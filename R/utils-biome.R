#' Biome names
#'
#' Obtain Biome names from the Hengl et al., 2018, using the \code{ID_BIOME}.
#'
#' @param ID Numeric value linked to a Biome in provided by Hengl et al., 2018.
#'
#' @return Table (\code{tibble} object) with Biome metadata.
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                         latitude = 51.4414,
#'                         longitude = 0.9418) %>%
#'   sf::st_as_sf(x = ., coords = c("longitude", "latitude"))
#' data %>%
#'   extract_biome() %>%
#'   .$ID_BIOME %>%
#'   biome_name()
biome_name <- function(ID) {
  smpds:::PNV_classes %>%
    dplyr::filter(ID_BIOME %in% ID)
}

#' Extract biome
#'
#' Extracts biome for a point (latitude, longitude) based on the map created by
#' Hengl et al., 2018.
#'
#' @param .data Table containing a geometry column.
#' @param reference Reference map with biomes, default: \code{smpds::PNV}.
#' @param all Boolean flag to indicate whether or not to return all the detected
#'     biomes, default: \code{FALSE} (dominant biome only).
#' @inherit raster::extract
#'
#' @return Tibble with matched biomes: dominant, sub-dominant, etc.
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                         latitude = 51.44140,
#'                         longitude = 0.9418)
#' data %>%
#'   extract_biome()
extract_biome <- function(.data,
                          reference = smpds::PNV,
                          buffer = 12000,
                          all = FALSE) {
  if (!("sf" %in% class(.data)))
    .data <- .data %>% sf::st_as_sf(x = ., coords = c("longitude", "latitude"))
  biomes <- raster::extract(reference, .data, buffer = buffer, na.rm = TRUE)
  biomes %>%
    purrr::map_df(function(bio) {
      tmp <- tibble::as_tibble(bio) %>%
        magrittr::set_names("ID_BIOME") %>%
        dplyr::group_by(ID_BIOME) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        dplyr::filter(!is.na(ID_BIOME))
      if (!all)
        tmp <- tmp %>% dplyr::slice(1)
      # tmp <- tmp %>% dplyr::slice(which(dplyr::row_number() == 1))
      if (nrow(tmp) == 0)
        tmp <- tibble::tibble(ID_BIOME = NA, n = NA)
      tmp
    })
}

#' @param cpus Number of CPUs to be used in parallel, default = 2.
#'
#' @rdname extract_biome
#'
#' @export
parallel_extract_biome <- function(.data, reference = smpds::PNV,
                                   buffer = 12000,
                                   cpus = 2,
                                   all = TRUE) {
  if (!all(c("latitude", "longitude") %in% colnames(.data)))
    stop("The given data object does not contain a latitude and longitude columns.",
         call. = FALSE)
  oplan <- future::plan(future::multisession, workers = cpus)
  on.exit(future::plan(oplan), add = TRUE)
  seq_len(nrow(.data)) %>%
    furrr::future_map_dfr(function(i) {
      if (.data %>% # Return NA if  latitude or longitude are missing (NA)
          dplyr::slice(i) %>%
          dplyr::mutate(LATLON = is.na(latitude) | is.na(longitude)) %>%
          .$LATLON)
        return(tibble::tibble(ID = i, ID_BIOME = NA, n = NA))
      .data %>%
        dplyr::slice(i) %>%
        sf::st_as_sf(x = ., coords = c("longitude", "latitude")) %>%
        extract_biome(reference = reference, buffer = buffer) %>%
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
                     by = "ID_BIOME")
  p <- basemap +
    ggplot2::geom_point(mapping = ggplot2::aes(x = longitude,
                                               y = latitude,
                                               fill = colour
                                               ),
                        data = .data,
                        size = 1.5,
                        shape = 21,
                        stroke = 0.2) +
    ggplot2::scale_fill_manual(name =
                                 "BIOME classification \n(Hengl et al., 2018)",
                               labels = .data_biome$description,
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
}
