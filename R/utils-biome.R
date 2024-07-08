#' Biome names
#'
#' Obtain biome names from the map created by
#' \insertCite{hengl2018global;textual}{smpds},
#' using the \code{ID_BIOME}.
#'
#' @param .data Numeric vector or data frame (\code{tibble} object with a column
#'     called \code{ID_BIOME}) with values linked to a biome provided by the
#'     map created by
#'     \insertCite{hengl2018global1kmres;textual}{smpds}.
#'     (See \code{\link{pnv_classes}}).
#' @param ... Optional parameters (not used).
#'
#' @return Table (\code{tibble} object) with biome metadata.
#' @export
#' @rdname biome_name
#' @family utils biome
#' @examples
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                         latitude = 51.4414,
#'                         longitude = -0.9418)
#' data %>%
#'   extract_biome() %>%
#'   biome_name()
#'
#' biome_name(1:10)
#'
#' @references
#' \insertAllCited{}
biome_name <- function(.data, ...) {
  UseMethod("biome_name", .data)
}

#' @export
#' @rdname biome_name
biome_name.tbl_df <- function(.data, ...) {
  biome_tbl <- .data$ID_BIOME %>% biome_name
  .data %>%
    dplyr::left_join(biome_tbl,
                     by = "ID_BIOME")
}

#' @export
#' @rdname biome_name
biome_name.numeric <- function(.data, ...) {
  # Local binding
  ID_BIOME <- NULL
  # PNV_classes %>% # Internal dataset
  smpds::pnv_classes() %>%
    dplyr::filter(ID_BIOME %in% !!.data)
}

#' Extract biome
#'
#' Extracts biome for a point (\code{latitude}, \code{longitude}) based on the
#' map with Potential Natural Vegetation (PNV) created by Hengl et al., 2018.
#'
#' @param .data Table containing columns for \code{latitude} and
#'     \code{longitude} (\code{tibble} object) or table with a \code{geometry}
#'     column (\code{sf} object).
#' @inheritDotParams extract_biome.tbl_df
#'
#' @return Table with the original data and matched biome(s):
#' \itemize{
#'  \item if \code{all = FALSE} (default): Only returns the dominant biome:
#'  \code{ID_BIOME}
#'  \item if \code{all = TRUE}: Returns all the detected biome -
#'  \code{ID_BIOME} and \code{px}, the number of pixels detected for each
#'  biome.
#' }
#'
#' @rdname extract_biome
#' @export
#' @family utils biome
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                         latitude = 51.44140,
#'                         longitude = -0.9418)
#' data %>%
#'   extract_biome()
extract_biome <- function(.data, ...) {
  UseMethod("extract_biome", .data)
}

#' @param reference Reference map with biomes, default: \code{smpds::PNV_1km}.
#' @param all Boolean flag to indicate whether or not to return all the detected
#'     biomes, default: \code{FALSE} (dominant biome only).
#' @inherit raster::extract
#'
#' @rdname extract_biome
#' @export
extract_biome.tbl_df <- function(.data,
                                 ...,
                                 reference = smpds::PNV_1km,
                                 buffer = 12000,
                                 all = FALSE) {
  # Local bindings
  . <- geometry <- longitude <- NULL
  if (!all(c("latitude", "longitude") %in% colnames(.data)))
    stop("The given data object does not contain a latitude and/or longitude.",
         call. = FALSE)
  .data %>%
    sf::st_as_sf(x = ., coords = c("longitude", "latitude"), crs = 4326) %>%
    extract_biome(reference = reference, buffer = buffer, all = all, ...) %>%
    dplyr::mutate(latitude = as.numeric(sf::st_coordinates(.)[, 2]),
                  longitude = as.numeric(sf::st_coordinates(.)[, 1]),
                  .after = geometry) %>%
    sf::st_drop_geometry()
}

#' @rdname extract_biome
#' @export
extract_biome.sf <- function(.data,
                             ...,
                             reference = smpds::PNV_1km,
                             buffer = 12000,
                             all = FALSE) {
  # Local bindings
  ID_BIOME <- px <- NULL
  biomes <- raster::extract(reference, .data, buffer = buffer, na.rm = TRUE)
  biomes %>%
    purrr::map_df(function(bio) {
      tmp <- tibble::as_tibble(bio) %>%
        magrittr::set_names("ID_BIOME") %>%
        dplyr::group_by(ID_BIOME) %>%
        dplyr::summarise(px = dplyr::n()) %>%
        dplyr::arrange(dplyr::desc(px)) %>%
        dplyr::filter(!is.na(ID_BIOME))
      if (nrow(tmp) == 0)
        tmp <- tibble::tibble(ID_BIOME = NA, px = NA)
      if (!all)
        tmp <- tmp %>% dplyr::select(-px) %>% dplyr::slice(1)
      .data %>%
        dplyr::bind_cols(tmp)
    })
}

#' @param cpus Number of CPUs to be used in parallel, default = 2.
#'
#' @rdname extract_biome
#'
#' @export
parallel_extract_biome <- function(.data,
                                   reference = smpds::PNV_1km,
                                   buffer = 12000,
                                   cpus = 2,
                                   all = FALSE) {
  # Local bindings
  . <- .ID_PAR_BIO <- ID <- latitude <- longitude <- NULL
  # Create data subset to improve performance
  .data <- .data %>%
    dplyr::mutate(.ID_PAR_BIO = seq_along(latitude))
  .data_sub <- .data %>%
    dplyr::select(latitude, longitude, .ID_PAR_BIO)
  oplan <- future::plan(future::multisession, workers = cpus)
  on.exit(future::plan(oplan), add = TRUE)
  {
    pb <- progressr::progressor(steps = nrow(.data_sub))
    output <- seq_len(nrow(.data_sub)) %>%
      furrr::future_map_dfr(function(i) {
        if (.data_sub %>% # Return NA if  latitude or longitude are missing (NA)
            dplyr::slice(i) %>%
            dplyr::mutate(LATLON = is.na(latitude) | is.na(longitude)) %>%
            .$LATLON) {
          return(.data_sub %>%
                   dplyr::slice(i) %>%
                   dplyr::mutate(ID = i, .before = 1))
        }
        # return(tibble::tibble(ID = i, ID_BIOME = NA, px = NA))
        tmp <- .data_sub %>%
          dplyr::slice(i) %>%
          # sf::st_as_sf(x = ., coords = c("longitude", "latitude")) %>%
          extract_biome(reference = reference, buffer = buffer, all = all) %>%
          dplyr::mutate(ID = i, .before = 1)
        pb()
        tmp
      },
      .options = furrr::furrr_options(seed = TRUE))
  }
  if (!all)  # Remove the ID
    output <- output %>% dplyr::select(-ID)
  # Combine the original data, .data, and the subset, .data_sub
  if (any(!(colnames(.data) %in% colnames(.data_sub)))) {
    output <- .data %>%
      dplyr::left_join(output,
                       by = c("latitude", "longitude", ".ID_PAR_BIO"))
  }
  output %>%
    dplyr::select(-dplyr::contains(".ID_PAR_BIO"))
}

#' PNV classes
#'
#' Potential Natural Vegetation (PNV) classes based on the map created by
#' \insertCite{hengl2018global1kmres;textual}{smpds}.
#'
#' @return Data frame (\code{tibble} object) with three columns:
#' \itemize{
#'  \item \code{ID_BIOME}: an unique identification number for each biome.
#'  \item \code{description}: string witha description of each biome.
#'  \item \code{colour}: hexadecimal colour code used to represent each biome.
#' }
#' @export
#'
#' @references
#' \insertAllCited{}
pnv_classes <- function() {
  tibble::tribble(
    ~ID_BIOME,                                   ~description,   ~colour,
    1,                  "tropical evergreen broadleaf forest", "#1C5510",
    2,             "tropical semi-evergreen broadleaf forest", "#659208",
    3,     "tropical deciduous broadleaf forest and woodland", "#AE7D20",
    4,  "warm-temperate evergreen broadleaf and mixed forest", "#000065",
    7,                            "cool-temperate rainforest", "#BBCB35",
    8,                     "cool evergreen needleleaf forest", "#009A18",
    9,                                    "cool mixed forest", "#CAFFCA",
    13,                "temperate deciduous broadleaf forest", "#55EB49",
    14,                               "cold deciduous forest", "#65B2FF",
    15,                    "cold evergreen needleleaf forest", "#0020CA",
    16,        "temperate sclerophyll woodland and shrubland", "#8EA228",
    17,        "temperate evergreen needleleaf open woodland", "#FF9ADF",
    18,                                    "tropical savanna", "#BAFF35",
    20,                              "xerophytic woods/scrub", "#FFBA9A",
    22,                                              "steppe", "#FFBA35",
    27,                                              "desert", "#F7FFCA",
    28,                                              "tundra", "#BFC9CA",
    30,                                              "tundra", "#BFC9CA",
    31,                                              "tundra", "#BFC9CA",
    32,                                              "tundra", "#BFC9CA",
    NA,                                      "not applicable", "#CC0033",
    -888888,                                 "not applicable", "#CC0033",
    -999999,                                       "not known", "#FFFFFF"
  )
}
