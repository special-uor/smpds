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
#' @return \code{ggplot} object with the plot.
#' @rdname plot_climate
#' @export
plot_climate <- function(.data,
                         var = "mat",
                         units = NA,
                         fill_scale = ggplot2::scale_fill_viridis_c(name = toupper(var)),
                         size = 1,
                         stroke = 0.1,
                         legend.position = "bottom",
                         xlim = c(-180, 180),
                         ylim = c(-60, 90),
                         ...) {
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
  basemap <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
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

#' @rdname plot_climate
#' @export
plot_mat <- function(.data,
                     size = 1,
                     stroke = 0.1,
                     legend.position = "bottom",
                     xlim = c(-180, 180),
                     ylim = c(-60, 90),
                     ...) {
  .data %>%
    plot_climate(var = "mat",
                 units = "\u00B0C",
                 fill_scale = ggplot2::scale_fill_distiller(name = "MAT",
                                                            palette = "Spectral",
                                                            direction = 1),
                 size = size,
                 stroke = stroke,
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
                 legend.position = legend.position,
                 xlim = xlim,
                 ylim = ylim,
                 ...)
}
