#' @keywords internal
check_coords <- function(.data, var, skip = FALSE) {
  if (skip) # Avoid double checking data
    return(.data)
  # Local bindings
  idx <- var_names <- NULL
  lat_var_names <- c("latitude", "lat", "y")
  lon_var_names <- c("longitude", "long", "lon", "x")
  main_var_names <- var
  idx_lat <- lat_var_names %in% colnames(.data)
  idx_lon <- lon_var_names %in% colnames(.data)
  idx_main <- main_var_names %in% colnames(.data)
  missing_lat <- sum(idx_lat) != 1
  missing_lon <- sum(idx_lon) != 1
  missing_main <- sum(idx_main) != 1
  if (missing_lat ||
      missing_lon ||
      missing_main) {
    avail_lat <- lat_var_names[idx_lat]
    avail_lon <- lon_var_names[idx_lon]
    avail_main <- main_var_names[idx_main]
    lat_var_names2 <- purrr::map2_chr(lat_var_names,
                                      idx_lat,
                                      function(name, avail) {
                                        if (avail)
                                          return(paste0(name, " [\u2713]"))
                                        return(paste0(name, " [\u2715]"))
                                      })
    lon_var_names2 <- purrr::map2_chr(lon_var_names,
                                      idx_lon,
                                      function(name, avail) {
                                        if (avail)
                                          return(paste0(name, " [\u2713]"))
                                        return(paste0(name, " [\u2715]"))
                                      })
    stop("Check your coordinates and main variable:",
         ifelse(missing_lat,
                paste0("\n\n- Latitude [only one needed]: \n",
                       paste0("   + ", lat_var_names2, collapse = "\n")),
                ""),
         ifelse(missing_lon,
                paste0("\n\n- Longitude [only one needed]: \n",
                       paste0("   + ", lon_var_names2, collapse = "\n")),
                ""),
         ifelse(missing_main,
                paste0("\n\n- Main: \n",
                       paste0("   + ", main_var_names, collapse = "\n")),
                ""),
         call. = FALSE)

    stop("The following variable",
         ifelse(sum(!idx) == 1, " was ", "s were "),
         "not found in the `.data` object:",
         paste0("\n  - ", var_names[!idx]),
         call. = FALSE)
  } else {
    .data <- .data %>%
      dplyr::rename(latitude = !!lat_var_names[idx_lat][1],
                    longitude = !!lon_var_names[idx_lon][1],
                    var = !!main_var_names[idx_main][1])
  }
  invisible(.data)
}

#' @keywords internal
climate_theme <- function(fill_sea,
                          legend.key.width,
                          legend.position,
                          legend.key.height = ggplot2::unit(1, "cm")) {
  ggplot2::theme(legend.position = legend.position,
                 legend.key.width = legend.key.width,
                 legend.key.height = legend.key.height,
                 legend.background = ggplot2::element_rect(colour = "black",
                                                           fill = "white"),
                 legend.key = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(colour = gray(.8),
                                                          linetype = "dashed",
                                                          linewidth = 0.4),
                 panel.border = ggplot2::element_rect(colour = "black",
                                                      fill = NA),
                 panel.background = ggplot2::element_rect(fill = fill_sea))
}


# ggplot2::theme(legend.position = legend.position,
#                legend.key.height = legend.key.height,
#                legend.key.width = legend.key.width,
#                legend.background = ggplot2::element_rect(colour = "black",
#                                                          fill = "white"),
#                legend.key = ggplot2::element_blank(),
#                panel.grid.major = ggplot2::element_line(colour = gray(.8),
#                                                         linetype = "dashed",
#                                                         linewidth = 0.4),
#                panel.border = ggplot2::element_rect(colour = "black",
#                                                     fill = fill_sea),
#                panel.background = ggplot2::element_rect(fill = NA))

#' @keywords internal
create_factor <- function(.data) {
  if (!is.factor(.data$var) |
      (!is.factor(.data$var) & !is.character(.data$var))) {
    # Local bindings
    . <- var <- NULL
    .data <- .data %>%
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
                round(digits = 4) %>%
                sort()
          ) %>%
          factor(levels = unique(sort(.)))
      )
  }
  invisible(.data)
}

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
#' @param legend.key.height Height of legend key (`ggplot2::unit` object).
#' @param legend.key.width Width of legend key (`ggplot2::unit` object).
#' @param show_plot Boolean flag to indicate whether or not the graphic should
#'     be displayed, if \code{FALSE}, it only returns the \code{ggplot2} object.
#'     Default: \code{TRUE}.
#' @param tiled_legend Boolean flag to indicate whether or not the legend should
#'     be shown as tiles, if \code{TRUE}, or points, \code{FALSE}.
#'     Default: \code{TRUE}.
#' @param land_borders Data frame with polygons to represent land borders
#'     (e.g. continents, countries, counties, etc.).
#'     Default: `rnaturalearth::ne_countries`.
#' @param land_borders_colour String containing a colour code (HEX value) or
#'     colour name, to be used as colour for the land borders.
#'     Default: `"black"`.
#' @param land_borders_size Numeric value of the line size to draw the land
#'     borders. Default: `0.25`
#' @param fill_land String containing a colour code (HEX value) or colour
#'     name, to be used as filling for the countries.
#'     Default: `NA` (transparent).
#' @param fill_sea String containing a colour code (HEX value) or colour
#'     name, to be used as filling for the seas
#'     Default: `NA` (transparent).
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
                       land_borders =
                         rnaturalearth::ne_countries(scale = "small",
                                                     returnclass = "sf"),
                       land_borders_colour = "black",
                       land_borders_size = 0.25,
                       fill_land = "white",
                       fill_sea = "#CFE2F3",
                       ...) {
  # Local bindings
  description <- ID_BIOME <- n <- latitude <- longitude <- var <- NULL

  # Check for latitude, longitude and var (one of each expected)
  .data <- .data %>%
    check_coords(var = "ID_BIOME") %>%
    dplyr::mutate(ID_BIOME = var)

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
    ggplot2::geom_sf(data = land_borders,
                     colour = land_borders_colour,
                     fill = fill_land,
                     inherit.aes = FALSE,
                     size = land_borders_size) +
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
    climate_theme(fill_sea, legend.key.width, legend.position,
                  legend.key.height = NULL)
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
#' @param .data Data frame with spatial and climate data. The following are
#'     expected:
#'     \itemize{
#'         \item **Latitude**, named: \code{latitude}, \code{lat} or \code{y}.
#'         \item **Longitude**, named: \code{longitude}, \code{long}, \code{lon}
#'         or \code{y}.
#'         \item **Main variable**, named: value of \code{var}.
#'     }
#' @param var String with the name of the climate variable to plot. Default:
#'     \code{"mat"} (Mean Annual Temperature).
#' @param units String with the units to display next to the legend tittle.
#'     Default: \code{NA}.
#' @param fill_scale \code{ggplot2} compatible object with information on how to
#'     fill the individual points for the climate variable. Default:
#'     \code{ggplot2::scale_fill_viridis_c(name = toupper(var))}.
#' @param elevation_cut Numeric value to use as the threshold of the elevation
#'     at which the sites will be represented with different shapes.
#' @param contour Boolean flag to indicate if a contour should be overlaid
#'     on the individual sites/points. It uses the function [akima::interp()]
#'     to interpolate values from the nearest available points.
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
                         elevation_cut = NULL,
                         land_borders =
                           rnaturalearth::ne_countries(scale = "small",
                                                       returnclass = "sf"),
                         land_borders_colour = "black",
                         land_borders_size = 0.25,
                         fill_land = "white",
                         fill_sea = "#CFE2F3",
                         contour = FALSE,
                         ...) {
  # Local bindings
  caption <- latitude <- longitude <- NULL

  # Check if the map requested is a contour
  if (contour) {
    aux <- .data %>% dplyr::rename(var = !!var) %>% dplyr::select(var)
    if (is.factor(aux$var) |
        !(typeof(aux$var) %in% c("double", "integer", "numeric"))) {
      warning("The target variable, `", var, "`, must be numeric. ",
              "Plotting simple climate map.", call. = FALSE)
    } else {
      output <- plot_climate_countour(.data = .data,
                                      var = var,
                                      units = units,
                                      fill_scale = fill_scale,
                                      size = size,
                                      stroke = max(0.3, stroke),
                                      legend.key.width = legend.key.width,
                                      legend.position = legend.position,
                                      xlim = xlim,
                                      ylim = ylim,
                                      show_plot = show_plot,
                                      elevation_cut = elevation_cut,
                                      land_borders = land_borders,
                                      fill_land = fill_land,
                                      fill_sea = fill_sea,
                                      ...)
      return(output)
    }
  }

  # Check for latitude, longitude and var (one of each expected)
  .data <- .data %>%
    check_coords(var)

  # Check if the units were provided
  if (!is.na(units))
    fill_scale$name <- paste0(fill_scale$name, " [", units, "]")

  # Create clean version of the original dataset
  .datav2 <- .data %>%
    dplyr::filter(!is.na(var))

  # Use different shapes if elevation_cut is given by the user
  shape <- rep(21, nrow(.datav2))
  if (!is.null(elevation_cut) & "elevation" %in% colnames(.datav2)) {
    caption <- paste0("Circles: elevation < ",
                      elevation_cut,
                      "m -- Triangles: elevation >= ",
                      elevation_cut,
                      "m")
    shape <- ifelse(.datav2$elevation >= elevation_cut, 24, 21)
  }

  # Create arbitrary factor for the input variable
  .datav2 <- .datav2 %>%
    create_factor()

  # Create plot
  p <- .datav2 %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = longitude,
                                           y = latitude,
                                           fill = var)) +
    ggplot2::geom_sf(data = land_borders,
                     colour = land_borders_colour,
                     fill = fill_land,
                     inherit.aes = FALSE,
                     size = land_borders_size) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, ..., expand = FALSE) +
    ggplot2::geom_point(mapping = ggplot2::aes(fill = var),
                        size = size,
                        shape = shape,
                        stroke = stroke) +
    ggplot2::geom_col(alpha = 0) +
    fill_scale +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        nrow = 1,
        override.aes = list(alpha = 1),
        label.position = "bottom",
        label.hjust = 0
      )
    ) +
    ggplot2::labs(x = NULL, y = NULL, caption = caption) +
    climate_theme(fill_sea, legend.key.width, legend.position)

  if (show_plot)
    print(p)
  return(invisible(p))
}

#' @param resolution Numeric value for the grid resolution.
#' @rdname plot_climate
#' @export
plot_climate_countour <-
  function(.data,
           var = "mat",
           units = NA,
           fill_scale =
             ggplot2::scale_fill_gradientn(
               name = toupper(var),
               colours = zissou1_palette()
             ),
           size = 1,
           stroke = 0.5,
           legend.key.width = ggplot2::unit(2, "cm"),
           legend.position = "bottom",
           xlim = c(-180, 180),
           ylim = c(-60, 90),
           show_plot = TRUE,
           elevation_cut = NULL,
           land_borders =
             rnaturalearth::ne_countries(scale = "small",
                                         returnclass = "sf"),
           land_borders_colour = "black",
           land_borders_size = 0.25,
           fill_land = "white",
           fill_sea = "#CFE2F3",
           resolution = 0.5,
           ...) {
    # Local bindings
    caption <- latitude <- longitude <- NULL

    # Check for latitude, longitude and var
    .data <- .data %>%
      check_coords(var)

    # Check the fill_scale
    if (fill_scale$is_discrete()) {
      warning("Changing fill_scale to a continuous scale ...",
              call. = FALSE)
      fill_scale  <-
        ggplot2::scale_fill_gradientn(
          name = toupper(var),
          colours = zissou1_palette()
        )
    }

    # Check if the units were provided
    if (!is.na(units))
      fill_scale$name <- paste0(fill_scale$name, " [", units, "]")

    # Create clean version of the dataset
    .datav2 <- .data %>%
      dplyr::filter(!is.na(var))

    # Create interpolated subset
    .datav3 <- .datav2 %>%
      smpds::tps(var = var,
                 resolution = resolution,
                 land_borders =  land_borders,
                 check_data = FALSE) %>%
      dplyr::rename(var = !!var)

    # Use different shapes if elevation_cut is given by the user
    shape <- rep(21, nrow(.datav2))
    if (!is.null(elevation_cut) & "elevation" %in% colnames(.datav2)) {
      caption <- paste0("Circles: elevation < ",
                        elevation_cut,
                        "m -- Triangles: elevation >= ",
                        elevation_cut,
                        "m")
      shape <- ifelse(.datav2$elevation >= elevation_cut, 24, 21)
    }

    # Create plot
    p <- .datav3 %>%
      ggplot2::ggplot(mapping = ggplot2::aes(x = longitude,
                                             y = latitude,
                                             fill = var)) +
      ggplot2::geom_sf(data = land_borders,
                       fill = fill_land,
                       inherit.aes = FALSE,
                       size = 0) +
      ggplot2::geom_tile() +
      # ggplot2::geom_raster(interpolate = FALSE) +
      ggplot2::geom_sf(data = land_borders,
                       colour = land_borders_colour,
                       fill = NA,
                       inherit.aes = FALSE,
                       size = land_borders_size) +
      ggplot2::coord_sf(xlim = xlim, ylim = ylim, ..., expand = FALSE) +
      ggplot2::geom_point(mapping = ggplot2::aes(longitude,
                                                 latitude,
                                                 fill = var),
                          data = .datav2,
                          size = size,
                          shape = shape,
                          stroke = stroke,
                          position = "jitter") +
      ggplot2::scale_shape_identity() +
      fill_scale +
      ggplot2::labs(x = NULL, y = NULL, caption = caption) +
      climate_theme(fill_sea, legend.key.width, legend.position)
    if (show_plot)
      print(p)
    return(invisible(p))
  }

#' @param .overlay_data (Optional) Data frame with original observations, to be
#'     to be overlaid on top of the tiles in `.data`.
#' @param continuous (Optional) Boolean flag to indicate whether or not to use
#'     a continuous (`TRUE`) fill scale. Default: `FALSE`.
#' @rdname plot_climate
#' @export
plot_climate_tiles <-
  function(.data,
           var = "mat",
           units = NA,
           fill_scale =
             ggplot2::scale_fill_manual(
               name = toupper(var),
               values = zissou1_palette(9)
             ),
           size = 1,
           stroke = 0.1,
           legend.key.width = ggplot2::unit(1, "cm"),
           legend.position = "bottom",
           xlim = c(-180, 180),
           ylim = c(-60, 90),
           show_plot = TRUE,
           elevation_cut = NULL,
           land_borders =
             rnaturalearth::ne_countries(scale = "small",
                                         returnclass = "sf"),
           land_borders_colour = "black",
           land_borders_size = 0.25,
           fill_land = "white",
           fill_sea = "#CFE2F3",
           .overlay_data = NULL,
           continuous = FALSE,
           ...) {
  # Local bindings
  .is_tile <- caption <- latitude <- longitude <- NULL

  # Check for latitude, longitude and var (one of each expected)
  .data <- .data %>%
    check_coords(var)

  # Check the fill_scale
  if (fill_scale$is_discrete() & continuous) {
    warning("Changing fill_scale to a continuous scale ...",
            call. = FALSE)
    fill_scale  <-
      ggplot2::scale_fill_gradientn(
        name = toupper(var),
        colours = zissou1_palette()
      )
  }

  # Check if the units were provided
  if (!is.na(units))
    fill_scale$name <- paste0(fill_scale$name, " [", units, "]")

  # Create clean version of the original dataset
  .datav2 <- .data %>%
    dplyr::filter(!is.na(var))

  # Use different shapes if elevation_cut is given by the user
  shape <- rep(21, nrow(.datav2))
  if (!is.null(elevation_cut) & "elevation" %in% colnames(.datav2)) {
    caption <- paste0("Circles: elevation < ",
                      elevation_cut,
                      "m -- Triangles: elevation >= ",
                      elevation_cut,
                      "m")
    shape <- ifelse(.datav2$elevation >= elevation_cut, 24, 21)
  }

  # Create arbitrary factor for the input variable
  if (!missing(.overlay_data) && !continuous) {
    .overlay_data <- .overlay_data %>%
      check_coords(var) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::mutate(.is_tile = FALSE)

    .data_all <- .datav2 %>%
      dplyr::mutate(.is_tile = TRUE) %>%
      dplyr::bind_rows(.overlay_data) %>%
      create_factor()

    .overlay_data <- .data_all %>%
      dplyr::filter(!.is_tile) %>%
      dplyr::select(-.is_tile) %>%
      dplyr::arrange(var)
    .datav2 <- .data_all %>%
      dplyr::filter(.is_tile) %>%
      dplyr::select(-.is_tile) %>%
      dplyr::arrange(var)
  } else if (!continuous) {
    .datav2 <- .datav2 %>%
      create_factor()
  }

  # Create plot
  p <- .datav2 %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = longitude,
                                           y = latitude,
                                           fill = var)) +
    ggplot2::geom_sf(data = land_borders,
                     fill = fill_land,
                     inherit.aes = FALSE,
                     size = 0) +
    ggplot2::geom_tile() +
    ggplot2::geom_sf(data = land_borders,
                     colour = land_borders_colour,
                     fill = NA,
                     inherit.aes = FALSE,
                     size = land_borders_size) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, ..., expand = FALSE)
  if (!missing(.overlay_data)) {
    # Use different shapes if elevation_cut is given by the user
    shape <- rep(21, nrow(.overlay_data))
    if (!is.null(elevation_cut) & "elevation" %in% colnames(.overlay_data)) {
      caption <- paste0("Circles: elevation < ",
                        elevation_cut,
                        "m -- Triangles: elevation >= ",
                        elevation_cut,
                        "m")
      shape <- ifelse(.overlay_data$elevation >= elevation_cut, 24, 21)
    }

    p <- p +
      ggplot2::geom_point(mapping = ggplot2::aes(x = longitude,
                                                 y = latitude,
                                                 fill = var),
                          data = .overlay_data,
                          size = size,
                          shape = shape,
                          stroke = stroke,
                          position = "jitter")
  }

  p <- p +
    ggplot2::geom_col(alpha = 0) +
    fill_scale +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        nrow = 1,
        override.aes = list(alpha = 1),
        label.position = "bottom",
        label.hjust = 0
      )
    ) +
    ggplot2::labs(x = NULL, y = NULL, caption = caption) +
    climate_theme(fill_sea, legend.key.width, legend.position)

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

#' Zissou1 colour palette
#'
#' This is a colour palette created with the following function:
#' `wesanderson::wes_palette("Zissou1", 100, type = "continuous")`
#' Wrapped as a standalone function to remove dependency of the `wesanderson`
#' package.
#' @keywords internal
zissou1_palette <- function(n = 100) {
  if (n != 100) {
    return(
      structure(c("#3A9AB2", "#77B3BE", "#9BBDAC", "#B7C689", "#DCCB4E",
                  "#E3AF0D", "#E98905", "#EE5F03", "#F11B00"),
                class = "palette", name = "Zissou1")
    )
  }
  structure(c("#3A9AB2", "#3F9CB3", "#449EB5", "#4AA1B6", "#4FA3B8",
              "#54A6B9", "#5AA8BB", "#5FAABC", "#64ADBE", "#6AAFBF", "#6FB2C0",
              "#72B2BF", "#76B3BE", "#79B4BD", "#7DB5BC", "#80B6BB", "#83B6BA",
              "#87B7B9", "#8AB8B8", "#8EB9B6", "#91BAB5", "#93BAB3", "#95BBB1",
              "#97BCAF", "#99BDAD", "#9BBEAC", "#9DBFAA", "#9FBFA8", "#A1C0A6",
              "#A3C1A4", "#A5C2A1", "#A8C29E", "#AAC39B", "#ADC397", "#AFC494",
              "#B1C590", "#B4C58D", "#B6C689", "#B9C786", "#BBC783", "#BEC87E",
              "#C1C879", "#C4C874", "#C7C96F", "#CAC96A", "#CDC965", "#D1C960",
              "#D4CA5A", "#D7CA55", "#DACA50", "#DCC94A", "#DDC744", "#DDC53E",
              "#DEC338", "#DFC131", "#DFBF2B", "#E0BD25", "#E1BB1F", "#E2B918",
              "#E2B712", "#E3B50F", "#E3B10E", "#E4AE0D", "#E4AB0B", "#E4A80A",
              "#E5A509", "#E5A208", "#E69F07", "#E69C06", "#E69805", "#E79505",
              "#E79205", "#E88F05", "#E88C05", "#E98905", "#E98605", "#EA8305",
              "#EA8005", "#EB7D05", "#EB7A05", "#EC7704", "#EC7304", "#EC7004",
              "#ED6C04", "#ED6904", "#ED6503", "#EE6103", "#EE5E03", "#EE5A03",
              "#EE5703", "#EF5102", "#EF4B02", "#EF4502", "#EF3F01", "#EF3901",
              "#F03301", "#F02D00", "#F02700", "#F02100", "#F11B00"),
            class = "palette",
            name = "Zissou1")
}
