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
#' @param show_plot Boolean flag to indicate whether or not the graphic should
#'     be displayed, if \code{FALSE}, it only returns the \code{ggplot2} object.
#'     Default: \code{TRUE}.
#' @param tiled_legend Boolean flag to indicate whether or not the legend should
#'     be shown as tiles, if \code{TRUE}, or points, \code{FALSE}.
#'     Default: \code{TRUE}.
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
                       legend.key.height = ggplot2::unit(1, "cm"),
                       legend.key.width = ggplot2::unit(1, "cm"),
                       legend.position = "bottom",
                       xlim = c(-180, 180),
                       ylim = c(-60, 90),
                       show_plot = TRUE,
                       tiled_legend = TRUE,
                       ...) {
  # Local bindings
  description <- ID_BIOME <- n <- latitude <- longitude <- NULL
  .data <- .data %>% # Change missing ID_BIOME to -888888
    dplyr::mutate(ID_BIOME = ifelse(is.na(ID_BIOME), -888888, ID_BIOME))
  .data_biome <- .data$ID_BIOME %>%
    smpds::biome_name() %>%
    dplyr::distinct(description, .keep_all = TRUE) %>%
    dplyr::mutate(description = ifelse(description %>%
                                         stringr::str_detect("not"),
                                       description,
                                       description %>%
                                         stringr::str_replace_all(" ", "\n")))
  .data <- .data %>%
    dplyr::mutate(ID_BIOME = ifelse(ID_BIOME %in% c(30:32),
                                    28, # Amalgamate tundras
                                    ID_BIOME)) %>%
    dplyr::select(-dplyr::starts_with(c("colour", "description"))) %>%
    dplyr::left_join(.data_biome,
                     by = "ID_BIOME") %>%
    dplyr::group_by(ID_BIOME) %>% # Reorder by ID_BIOME
    dplyr::mutate(n = length(ID_BIOME)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(n))

  p <- .data %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = longitude,
                                           y = latitude,
                                           fill = description)) +
    ggplot2::geom_sf(data = rnaturalearth::ne_countries(scale = "small",
                                                        returnclass = "sf"),
                     fill = NA,
                     size = 0.25) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, ..., expand = FALSE) +
    ggplot2::geom_point(mapping = ggplot2::aes(fill = description),
                        size = size,
                        shape = 21,
                        stroke = stroke) +
    ggplot2::scale_fill_manual(name =
                                 "BIOME classification \n(Hengl et al., 2018)",
                               breaks = .data_biome$description,
                               values = .data_biome$colour) +
    ggplot2::labs(x = NULL, y = NULL)
  if (tiled_legend) {
    p <- p +
      ggplot2::geom_col(alpha = 0) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          # nrow = ifelse(unique(.data_biome$ID_BIOME) %>%
          #                 length() > 10,
          #               2,
          #               1),
          override.aes = list(alpha = 1),
          label.position = "right",
          label.hjust = 0
        )
      )
  } else {
    p <- p +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes =
                                                     list(size = 2),
                                                   label.hjust = 0))
  }
  p <- p +
    ggplot2::theme(legend.position = legend.position,
                   legend.key.height = legend.key.height,
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
  if (show_plot)
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
#' @inheritParams plot_biome
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
                           ggplot2::scale_fill_viridis_d(name = toupper(var)),
                         size = 1,
                         stroke = 0.1,
                         legend.key.width = ggplot2::unit(1, "cm"),
                         legend.position = "bottom",
                         xlim = c(-180, 180),
                         ylim = c(-60, 90),
                         show_plot = TRUE,
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

  if (!is.na(units))
    fill_scale$name <- paste0(fill_scale$name, " [", units, "]")
  .datav2 <- .data %>%
    dplyr::rename(var = !!var) %>%
    dplyr::filter(!is.na(var))

  # Create arbitrary factor for the input variable
  if (!is.factor(.datav2$var) |
      (!is.factor(.datav2$var) & !is.character(.datav2$var))) {
    .datav2 <- .datav2 %>%
      dplyr::mutate(
        var = var %>%
          cut(include.lowest = TRUE,
              .,
              breaks = c(
                -Inf,
                seq(
                  from = min(., na.rm = TRUE),
                  to = max(., na.rm = TRUE) -
                    round((max(., na.rm = TRUE) - min(., na.rm = TRUE)) / 9,
                          digits = 4),
                  by = round((max(., na.rm = TRUE) - min(., na.rm = TRUE)) / 9,
                             digits = 4))[-1] %>%
                  round(digits = 4),
                Inf),
              labels =
                seq(
                  from = min(., na.rm = TRUE),
                  to = max(., na.rm = TRUE) -
                    round((max(., na.rm = TRUE) - min(., na.rm = TRUE)) / 9,
                          digits = 4),
                  by = round((max(., na.rm = TRUE) - min(., na.rm = TRUE)) / 9,
                             digits = 4)) %>%
                round(digits = 4)
          )
      )
  }
  p <- .datav2 %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = longitude,
                                           y = latitude,
                                           fill = var)) +
    ggplot2::geom_sf(data = rnaturalearth::ne_countries(scale = "small",
                                                        returnclass = "sf"),
                     fill = NA,
                     size = 0.25) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, ..., expand = FALSE) +
    ggplot2::geom_point(mapping = ggplot2::aes(fill = var),
                        size = size,
                        shape = 21,
                        stroke = stroke) +
    ggplot2::geom_col(alpha = 0) +
    fill_scale +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        nrow = 1,
        override.aes = list(alpha = 1),
        label.position = "bottom",
        label.hjust = 0
      )
    ) +
    ggplot2::theme(legend.position = legend.position,
                   legend.key.width = legend.key.width,
                   legend.key.height = ggplot2::unit(1, "cm"),
                   legend.background = ggplot2::element_rect(colour = "black",
                                                             fill = "white"),
                   legend.key = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = gray(.8),
                                                            linetype = "dashed",
                                                            size = 0.4),
                   panel.border = ggplot2::element_rect(colour = "black",
                                                        fill = NA),
                   panel.background = ggplot2::element_rect(fill = NA))
  if (show_plot)
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
                     legend.key.width = ggplot2::unit(1, "cm"),
                     legend.position = "bottom",
                     xlim = c(-180, 180),
                     ylim = c(-60, 90),
                     show_plot = TRUE,
                     ...) {
  legend_name <- paste0("GDD", baseline)
  .data %>%
    plot_climate(var = paste0("gdd", baseline),
                 units = "\u00B0C days",
                 fill_scale = ggplot2::scale_fill_brewer(name = legend_name,
                                                         palette = "Spectral",
                                                         direction = -1),
                 size = size,
                 stroke = stroke,
                 legend.key.width = legend.key.width,
                 legend.position = legend.position,
                 xlim = xlim,
                 ylim = ylim,
                 show_plot = show_plot,
                 ...)
}

#' @rdname plot_climate
#' @export
plot_mat <- function(.data,
                     size = 1,
                     stroke = 0.1,
                     legend.key.width = ggplot2::unit(1, "cm"),
                     legend.position = "bottom",
                     xlim = c(-180, 180),
                     ylim = c(-60, 90),
                     show_plot = TRUE,
                     ...) {
  .data %>%
    plot_climate(var = "mat",
                 units = "\u00B0C",
                 fill_scale = ggplot2::scale_fill_brewer(name = "MAT",
                                                         palette = "Spectral",
                                                         direction = -1),
                 size = size,
                 stroke = stroke,
                 legend.key.width = legend.key.width,
                 legend.position = legend.position,
                 xlim = xlim,
                 ylim = ylim,
                 show_plot = show_plot,
                 ...)
}

#' @rdname plot_climate
#' @export
plot_mi <- function(.data,
                    size = 1,
                    stroke = 0.1,
                    legend.key.width = ggplot2::unit(1, "cm"),
                    legend.position = "bottom",
                    xlim = c(-180, 180),
                    ylim = c(-60, 90),
                    show_plot = TRUE,
                    ...) {
  .data %>%
    plot_climate(var = "mi",
                 units = "unitless",
                 fill_scale = ggplot2::scale_fill_brewer(name = "MI",
                                                         palette = "YlGnBu"),
                 size = size,
                 stroke = stroke,
                 legend.key.width = legend.key.width,
                 legend.position = legend.position,
                 xlim = xlim,
                 ylim = ylim,
                 show_plot = show_plot,
                 ...)
}

#' @rdname plot_climate
#' @export
plot_mtco <- function(.data,
                      size = 1,
                      stroke = 0.1,
                      legend.key.width = ggplot2::unit(1, "cm"),
                      legend.position = "bottom",
                      xlim = c(-180, 180),
                      ylim = c(-60, 90),
                      show_plot = TRUE,
                      ...) {
  .data %>%
    plot_climate(var = "mtco",
                 units = "\u00B0C",
                 fill_scale = ggplot2::scale_fill_brewer(name = "MTCO",
                                                         palette = "Blues"),
                 size = size,
                 stroke = stroke,
                 legend.key.width = legend.key.width,
                 legend.position = legend.position,
                 xlim = xlim,
                 ylim = ylim,
                 show_plot = show_plot,
                 ...)
}

#' @rdname plot_climate
#' @export
plot_mtwa <- function(.data,
                      size = 1,
                      stroke = 0.1,
                      legend.key.width = ggplot2::unit(1, "cm"),
                      legend.position = "bottom",
                      xlim = c(-180, 180),
                      ylim = c(-60, 90),
                      show_plot = TRUE,
                      ...) {
  .data %>%
    plot_climate(var = "mtwa",
                 units = "\u00B0C",
                 fill_scale = ggplot2::scale_fill_brewer(name = "MTWA",
                                                         palette = "Reds",
                                                         direction = 1),
                 size = size,
                 stroke = stroke,
                 legend.key.width = legend.key.width,
                 legend.position = legend.position,
                 xlim = xlim,
                 ylim = ylim,
                 show_plot = show_plot,
                 ...)
}
