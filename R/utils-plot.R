#' Plot biomes/entities
#'
#' Plot biomes linked to entities/sites, in addition to \code{latitude} and
#' \code{longitude}, a column with the \code{ID_BIOME} is required. This can
#' be obtained using the function \code{\link{extract_biome}} or its faster
#' version (for large datasets), \code{\link{parallel_extract_biome}}.
#'
#' @param .data Data frame with spatial data and biome classification.
#' @param size Numeric value for the \code{size} aesthetic.
#' @param stroke Numeric value for the \code{stroke} aesthetic.
#' @inheritParams ggplot2::theme
#' @inheritParams ggplot2::coord_sf
#' @inheritDotParams ggplot2::coord_sf -xlim -ylim
#'
#' @importFrom grDevices gray
#'
#' @return \code{ggplot} object with the plot.
#' @export
#' @family utils biome
plot_biome <- function(.data,
                       size = 1,
                       stroke = 0.1,
                       legend.position = "bottom",
                       xlim = c(-180, 180),
                       ylim = c(-60, 90), ...) {
  # Local bindings
  description <- ID_BIOME <- n <- latitude <- longitude <- NULL
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
  basemap <- rnaturalearth::ne_countries(scale = "small",
                                         returnclass = "sf") %>%
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
    size = size,
    shape = 21,
    stroke = stroke) +
    ggplot2::scale_fill_manual(name =
                                 "BIOME classification \n(Hengl et al., 2018)",
                               breaks = .data_biome$description,
                               values = .data_biome$colour) +
    ggplot2::scale_x_continuous(breaks = ewbrks) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes =
                                                   list(size = 2))) +
    ggplot2::theme(legend.position = legend.position,
                   legend.background = ggplot2::element_rect(colour = "black",
                                                             fill = "white"),
                   legend.key = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = gray(.8),
                                                            linetype = "dashed",
                                                            size = 0.4),
                   panel.border = ggplot2::element_rect(colour = "black",
                                                        fill = NA),
                   panel.background = ggplot2::element_rect(fill = NA))
  print(p)
  return(invisible(p))
}

#' Plot climate variable
#'
#' Plot climate variable (like \code{MAT}, \code{MTCO}, etc.), in addition to
#' \code{latitude} and \code{longitude}, a column with the value of \code{var}
#' is required.
#'
#' @param .data Data frame with spatial and climate data.
#' @param var String with the name of the climate variable to plot. Default:
#'     \code{"mat"} (Mean Annual Temperature).
#' @param units String with the units to display next to the legend tittle.
#'     Default: \code{NA}.
#' @param fill_scale \code{ggplot2} compatible object with information on how to
#'     fill the individual points for the climate variable. Default:
#'     \code{ggplot2::scale_fill_viridis_c(name = toupper(var))}.
#' @param size Numeric value for the \code{size} aesthetic.
#' @param stroke Numeric value for the \code{stroke} aesthetic.
#' @inheritParams ggplot2::theme
#' @inheritParams ggplot2::coord_sf
#' @inheritDotParams ggplot2::coord_sf -xlim -ylim
#'
#' @importFrom grDevices gray
#'
#' @return \code{ggplot} object with the plot.
#' @rdname plot_climate
#' @export
plot_climate <- function(.data,
                         var = "mat",
                         units = NA,
                         fill_scale =
                           ggplot2::scale_fill_viridis_c(name = toupper(var)),
                         size = 1,
                         stroke = 0.1,
                         legend.key.width = ggplot2::unit(3, "cm"),
                         legend.position = "bottom",
                         xlim = c(-180, 180),
                         ylim = c(-60, 90),
                         ...) {
  # Local bindings
  latitude <- longitude <- NULL
  # Check for latitude, longitude and var
  idx <- c("latitude", "longitude", var) %in% colnames(.data)
  if (sum(!idx) != 0) {
    stop("The following variable",
         ifelse(sum(!idx) == 1, " was ", "s were "),
         "not found in the `.data` object:",
         paste0("\n  - ", c("latitude", "longitude", var)[!idx]),
         call. = FALSE)
  }

  # create the breaks- and labels vector
  ewbrks <- seq(-180,180,30)
  basemap <- rnaturalearth::ne_countries(scale = "small",
                                         returnclass = "sf") %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(fill = "white", size = 0.25) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, ..., expand = FALSE)

  if (!is.na(units))
    fill_scale$name <- paste0(fill_scale$name, " [", units, "]")
  p <- basemap +
    ggplot2::geom_point(mapping = ggplot2::aes(x = longitude,
                                               y = latitude,
                                               fill = var
    ),
    data = .data %>%
      dplyr::rename(var = !!var),
    size = size,
    shape = 21,
    stroke = stroke) +
    ggplot2::scale_x_continuous(breaks = ewbrks) +
    fill_scale +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(legend.position = legend.position,
                   # legend.key.width = ggplot2::unit(0.05, "npc"),
                   legend.key.width = legend.key.width,
                   legend.background = ggplot2::element_rect(colour = "black",
                                                             fill = "white"),
                   legend.key = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = gray(.8),
                                                            linetype = "dashed",
                                                            size = 0.4),
                   panel.border = ggplot2::element_rect(colour = "black",
                                                        fill = NA),
                   panel.background = ggplot2::element_rect(fill = NA))
  print(p)
  return(invisible(p))
}

#' @param baseline Numeric value to be used as the baseline for the calculation
#'     of the Growing Degree Days, default: \code{0}.
#' @rdname plot_climate
#' @export
plot_gdd <- function(.data,
                     baseline = 0,
                     size = 1,
                     stroke = 0.1,
                     legend.key.width = ggplot2::unit(3, "cm"),
                     legend.position = "bottom",
                     xlim = c(-180, 180),
                     ylim = c(-60, 90),
                     ...) {
  legend_name <- paste0("GDD", baseline)
  .data %>%
    plot_climate(var = paste0("gdd", baseline),
                 units = "\u00B0C days",
                 fill_scale = ggplot2::scale_fill_distiller(name = legend_name,
                                                            palette = "Greens",
                                                            direction = 1),
                 size = size,
                 stroke = stroke,
                 legend.key.width = legend.key.width,
                 legend.position = legend.position,
                 xlim = xlim,
                 ylim = ylim,
                 ...)
}

#' @rdname plot_climate
#' @export
plot_mat <- function(.data,
                     size = 1,
                     stroke = 0.1,
                     legend.key.width = ggplot2::unit(3, "cm"),
                     legend.position = "bottom",
                     xlim = c(-180, 180),
                     ylim = c(-60, 90),
                     ...) {
  .data %>%
    plot_climate(var = "mat",
                 units = "\u00B0C",
                 fill_scale =
                   ggplot2::scale_fill_distiller(name = "MAT",
                                                 palette = "Spectral",
                                                 direction = -1),
                 size = size,
                 stroke = stroke,
                 legend.key.width = legend.key.width,
                 legend.position = legend.position,
                 xlim = xlim,
                 ylim = ylim,
                 ...)
}

#' @rdname plot_climate
#' @export
plot_mi <- function(.data,
                    size = 1,
                    stroke = 0.1,
                    legend.key.width = ggplot2::unit(3, "cm"),
                    legend.position = "bottom",
                    xlim = c(-180, 180),
                    ylim = c(-60, 90),
                    ...) {
  .data %>%
    plot_climate(var = "mi",
                 units = "unitless",
                 fill_scale = ggplot2::scale_fill_distiller(name = "MI",
                                                            palette = "YlGnBu",
                                                            direction = 1),
                 size = size,
                 stroke = stroke,
                 legend.key.width = legend.key.width,
                 legend.position = legend.position,
                 xlim = xlim,
                 ylim = ylim,
                 ...)
}

#' @rdname plot_climate
#' @export
plot_mtco <- function(.data,
                      size = 1,
                      stroke = 0.1,
                      legend.key.width = ggplot2::unit(3, "cm"),
                      legend.position = "bottom",
                      xlim = c(-180, 180),
                      ylim = c(-60, 90),
                      ...) {
  .data %>%
    plot_climate(var = "mtco",
                 units = "\u00B0C",
                 fill_scale = ggplot2::scale_fill_distiller(name = "MTCO",
                                                            palette = "Blues"),
                 size = size,
                 stroke = stroke,
                 legend.key.width = legend.key.width,
                 legend.position = legend.position,
                 xlim = xlim,
                 ylim = ylim,
                 ...)
}

#' @rdname plot_climate
#' @export
plot_mtwa <- function(.data,
                      size = 1,
                      stroke = 0.1,
                      legend.key.width = ggplot2::unit(3, "cm"),
                      legend.position = "bottom",
                      xlim = c(-180, 180),
                      ylim = c(-60, 90),
                      ...) {
  .data %>%
    plot_climate(var = "mtwa",
                 units = "\u00B0C",
                 fill_scale = ggplot2::scale_fill_distiller(name = "MTWA",
                                                            palette = "Reds",
                                                            direction = 1),
                 size = size,
                 stroke = stroke,
                 legend.key.width = legend.key.width,
                 legend.position = legend.position,
                 xlim = xlim,
                 ylim = ylim,
                 ...)
}
