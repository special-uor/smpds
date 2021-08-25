#' Biome names
#'
#' Obtain biome names from the Hengl et al., 2018, using the \code{ID_BIOME}.
#'
#' @param .data Numeric vector or data frame (\code{tibble} object with a column
#'     called \code{ID_BIOME}) with values linked to a Biome provided by
#'     Hengl et al., 2018. (See \code{\link{smpds:::PNV_classes}}).
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
  smpds:::PNV_classes %>%
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
#'  \item{if \code{all = FALSE} (default) }{ Only returns the dominant biome:
#'  \code{ID_BIOME}}
#'  \item{if \code{all = TRUE} }{ Returns all the detected biomes:
#'  \code{ID_BIOME} and \code{px}, the number of pixels detected for each
#'  biome.}
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

#' @param reference Reference map with biomes, default: \code{smpds::PNV}.
#' @param all Boolean flag to indicate whether or not to return all the detected
#'     biomes, default: \code{FALSE} (dominant biome only).
#' @inherit raster::extract
#'
#' @rdname extract_biome
#' @export
extract_biome.tbl_df <- function(.data,
                                 ...,
                                 reference = smpds::PNV,
                                 buffer = 12000,
                                 all = FALSE) {
  if (!all(c("latitude", "longitude") %in% colnames(.data)))
    stop("The given data object does not contain a latitude and/or longitude.",
         call. = FALSE)
  # if (!("sf" %in% class(.data)))
  #   .data <- .data %>% sf::st_as_sf(x = ., coords = c("longitude", "latitude"))
  .data %>%
    dplyr::mutate(geometry = NA, .after = longitude) %>%
    sf::st_as_sf(x = ., coords = c("longitude", "latitude")) %>%
    extract_biome(reference = reference, buffer = buffer, all = all, ...) %>%
    dplyr::mutate(latitude = sf::st_coordinates(.)[, 2],
                  longitude = sf::st_coordinates(.)[, 1],
                  .after = geometry) %>%
    sf::st_set_geometry(NULL)
}

#' @rdname extract_biome
#' @export
extract_biome.sf <- function(.data,
                             ...,
                             reference = smpds::PNV,
                             buffer = 12000,
                             all = FALSE) {
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
                                   reference = smpds::PNV,
                                   buffer = 12000,
                                   cpus = 2,
                                   all = FALSE) {
  oplan <- future::plan(future::multisession, workers = cpus)
  on.exit(future::plan(oplan), add = TRUE)
  seq_len(nrow(.data)) %>%
    furrr::future_map_dfr(function(i) {
      if (.data %>% # Return NA if  latitude or longitude are missing (NA)
          dplyr::slice(i) %>%
          dplyr::mutate(LATLON = is.na(latitude) | is.na(longitude)) %>%
          .$LATLON)
        return(.data %>% dplyr::slice(i) %>% dplyr::mutate(ID = i, .before = 1))
        # return(tibble::tibble(ID = i, ID_BIOME = NA, px = NA))
      .data %>%
        dplyr::slice(i) %>%
        # sf::st_as_sf(x = ., coords = c("longitude", "latitude")) %>%
        extract_biome(reference = reference, buffer = buffer, all = all) %>%
        dplyr::mutate(ID = i, .before = 1)
    },
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE))
}


#' Plot biomes/entities
#'
#' Plot biomes linked to entities/sites, in addition to \code{latitude} and
#' \code{longitude}, a column with the \code{ID_BIOME} is required. This can
#' be obtained using the function \code{\link{extract_biome}} or its faster
#' version (for large datasets), \code{\link{parallel_extract_biome}}.
#'
#' @param .data Data frame with spatial data and biome classification.
#' @inheritParams ggplot2::coord_sf
#' @inheritDotParams ggplot2::coord_sf -xlim -ylim
#'
#' @return \code{ggplot} object with the plot.
#' @export
#' @family utils biome
plot_biome <- function(.data, xlim = c(-180, 180), ylim = c(-60, 90), ...) {
  # create the breaks- and label vectors
  ewbrks <- seq(-180,180,30)
  nsbrks <- seq(-90,90,30)
  # ewlbls <- ewbrks %>%
  #   purrr::map_chr(~ifelse(.x < 0,
  #                          paste(.x, "\u00B0E"),
  #                          ifelse(.x > 0, paste(.x, "\u00B0W"), "0")))
  # nslbls <- nsbrks %>%
  #   purrr::map_chr(~ifelse(.x < 0,
  #                          paste(.x, "\u00B0S"),
  #                          ifelse(.x > 0, paste(.x, "\u00B0N"), "0")))
  # world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")
  basemap <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(fill = "white", size = 0.25) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, ..., expand = FALSE)
  .data_biome <- .data$ID_BIOME %>%
    smpds::biome_name() %>%
    dplyr::distinct(description, .keep_all = TRUE)
  .data <- .data %>%
    dplyr::mutate(ID_BIOME = ifelse(ID_BIOME %in% c(30:32),
                                    28, # Amalgamate tundras
                                    ID_BIOME)) %>%
    dplyr::left_join(.data_biome,
                     by = "ID_BIOME") %>%
    dplyr::group_by(ID_BIOME) %>% # Reorder by ID_BIOME
    dplyr::mutate(n = length(ID_BIOME)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(n))
  p <- basemap +
    ggplot2::geom_point(mapping = ggplot2::aes(x = longitude,
                                               y = latitude,
                                               fill = description
                                               ),
                        data = .data,
                        size = 1.5,
                        shape = 21,
                        stroke = 0.2) +
    ggplot2::scale_fill_manual(name =
                                 "BIOME classification \n(Hengl et al., 2018)",
                               breaks = .data_biome$description,
                               values = .data_biome$colour) +
    ggplot2::scale_x_continuous(breaks = ewbrks) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(legend.position = c(.13, .225),
                   legend.background = ggplot2::element_rect(colour = "black",
                                                             fill = "white"),
                   legend.key = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = gray(.8),
                                                            linetype = "dashed",
                                                            size = 0.4),
                   panel.border = ggplot2::element_rect(colour = "black",
                                                        fill = NA),
                   # panel.background = ggplot2::element_rect(fill = "aliceblue"))
                   panel.background = ggplot2::element_rect(fill = NA))
  print(p)
  return(invisible(p))
}
