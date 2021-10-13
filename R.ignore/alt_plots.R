path <- "~/OneDrive - University of Reading/UoR/Data/CRU/4.04/"
# Daily ----
ncfile <- file.path(path, "cru_ts4.04.1901.2019.pre.dat-new-clim-1961-1990-int.nc")
nc_in <- ncdf4::nc_open(ncfile)
xlim <- c(78, 96)
ylim <- c(49, 57)
lat <- ncdf4::ncvar_get(nc_in, "lat")
lon <- ncdf4::ncvar_get(nc_in, "lon")
time <- ncdf4::ncvar_get(nc_in, "time")
coords_x <- which(lon >= xlim[1] & lon <= xlim[2]) %>%
  c(min(.), length(.)) %>% .[-seq_len(length(.) - 2)]
coords_y <- which(lat >= ylim[1] & lat <= ylim[2]) %>%
  c(min(.), length(.)) %>% .[-seq_len(length(.) - 2)]
pre_subset <- ncdf4::ncvar_get(nc_in, "pre",
                 start = c(coords_x[1], coords_y[1], min(time)),
                 count = c(coords_x[2], coords_y[2], max(time)))
dim(pre_subset)
var_atts <- ncdf4::ncatt_get(nc_in, "pre")
pre_sum <- rowSums(pre_subset, na.rm = TRUE, dims = 2)
dim(pre_sum)
ncdf4::nc_close(nc_in)

# Daily: NetCDF ----
lon_dim <- ncdf4::ncdim_def("longitude", "degrees_east",
                            lon[coords_x[1] + seq_len(coords_x[2])])
lat_dim <- ncdf4::ncdim_def("latitude", "degrees_north",
                            lat[coords_y[1] + seq_len(coords_y[2])])
time_dim <- ncdf4::ncdim_def("time", "days", 1)
pre_var <- ncdf4::ncvar_def("pre", "mm/year",
                            dim = list(lon_dim, lat_dim, time_dim))

# ncdf4::ncatt_put(nc_out, "pre")
nc_out <- ncdf4::nc_create("~/Downloads/SMPDSv2/Maarten/CRU_TS_from_daily_pre.nc",
                       vars = list(pre_var))
ncdf4::ncvar_put(nc_out, "pre", pre_sum)
ncdf4::nc_close(nc_out)

# Monthly ----
ncfile <- file.path(path, "cru_ts4.04.1901.2019.pre.dat-new-clim-1961-1990.nc")
nc_in <- ncdf4::nc_open(ncfile)
xlim <- c(78, 96)
ylim <- c(49, 57)
lat <- ncdf4::ncvar_get(nc_in, "lat")
lon <- ncdf4::ncvar_get(nc_in, "lon")
time <- ncdf4::ncvar_get(nc_in, "time")
coords_x <- which(lon >= xlim[1] & lon <= xlim[2]) %>%
  c(min(.), length(.)) %>% .[-seq_len(length(.) - 2)]
coords_y <- which(lat >= ylim[1] & lat <= ylim[2]) %>%
  c(min(.), length(.)) %>% .[-seq_len(length(.) - 2)]
pre_subset <- ncdf4::ncvar_get(nc_in, "pre",
                               start = c(coords_x[1], coords_y[1], min(time)),
                               count = c(coords_x[2], coords_y[2], max(time)))
dim(pre_subset)
var_atts <- ncdf4::ncatt_get(nc_in, "pre")
pre_sum <- rowSums(pre_subset, na.rm = TRUE, dims = 2)
dim(pre_sum)
ncdf4::nc_close(nc_in)

# Monthly: NetCDF ----
lon_dim <- ncdf4::ncdim_def("longitude", "degrees_east",
                            lon[coords_x[1] + seq_len(coords_x[2])])
lat_dim <- ncdf4::ncdim_def("latitude", "degrees_north",
                            lat[coords_y[1] + seq_len(coords_y[2])])
time_dim <- ncdf4::ncdim_def("time", "years", 1)
pre_var <- ncdf4::ncvar_def("pre", "mm/day",
                            dim = list(lon_dim, lat_dim, time_dim))

# ncdf4::ncatt_put(nc_out, "pre")
nc_out <- ncdf4::nc_create("~/Downloads/SMPDSv2/Maarten/CRU_TS_from_montly_pre.nc",
                           vars = list(pre_var))
ncdf4::ncvar_put(nc_out, "pre", pre_sum)
ncdf4::nc_close(nc_out)

# New function ----

plot_climate <- function(.data,
                         var = "mat",
                         units = NA,
                         fill_scale =
                           ggplot2::scale_fill_brewer(name = toupper(var),
                                                      palette = "Spectral"),
                           # ggplot2::scale_fill_viridis_d(name = toupper(var)),
                         size = 1,
                         stroke = 0.1,
                         legend.key.width = ggplot2::unit(1, "cm"),
                         legend.position = "bottom",
                         xlim = c(-180, 180),
                         ylim = c(-60, 90),
                         show_plot = TRUE,
                         ...) {
  # browser()
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

  shape <- ifelse(.data$elevation >= 1000, 24, 21)
  max_breaks <- 9
  if (!is.na(units))
    fill_scale$name <- paste0(fill_scale$name, " [", units, "]")
  p <- .data %>%
    dplyr::mutate(labels = dplyr::c_across(elevation),
                  labels = round(labels, digits =2)) %>%
    dplyr::rename(var = !!var) %>%
    dplyr::filter(!is.na(var)) %>%
    # dplyr::mutate(
    #   var = var %>%
    #     cut(include.lowest = TRUE,
    #         .,
    #         breaks = c(
    #           -Inf,
    #           seq(
    #             from = min(., na.rm = TRUE),
    #             # from = signif(min(., na.rm = TRUE) * 0.99999, digits = 5),
    #             to = max(., na.rm = TRUE) -
    #               round((max(., na.rm = TRUE) - min(., na.rm = TRUE)) / max_breaks,
    #                     digits = 4),
    #             by = round((max(., na.rm = TRUE) - min(., na.rm = TRUE)) / max_breaks,
    #                        digits = 4))[-1] %>%
    #             round(digits = 4),
    #           Inf),
    #         labels =
    #           seq(
    #             from = min(., na.rm = TRUE),
    #             to = max(., na.rm = TRUE) -
    #               round((max(., na.rm = TRUE) - min(., na.rm = TRUE)) / max_breaks,
    #                     digits = 4),
    #             by = round((max(., na.rm = TRUE) - min(., na.rm = TRUE)) / max_breaks,
    #                        digits = 4)) %>%
    #           round(digits = 4)
    #     )
    # ) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = longitude,
                                           y = latitude,
                                           fill = var)) +
    ggplot2::geom_sf(data = rnaturalearth::ne_countries(scale = "small",
                                                        returnclass = "sf"),
                     fill = NA, size = 0.25) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, ..., expand = FALSE) +
    ggplot2::geom_point(mapping = ggplot2::aes(fill = var),
                        size = size,
                        shape = shape,
                        stroke = stroke,
                        position = "jitter") +
    # ggrepel::geom_text_repel(mapping = ggplot2::aes(longitude,
    #                                                 latitude,
    #                                                 label = labels),
    #                          # data = .data %>%
    #                          #   dplyr::rename(labels = !!var) %>%
    #                          #   dplyr::mutate(labels = round(labels, 2)),
    #                          size = 1.5,
    #                          max.overlaps = 20) +
    ggplot2::geom_col(alpha = 0) +
    fill_scale +
    ggplot2::labs(x = NULL, y = NULL,
                  caption = "Circles: elevation < 1000m -- Triangles: elevation >= 1000m") +
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

plot_climate_countour <- function(.data,
                                  var = "mat",
                                  units = NA,
                                  fill_scale =
                                    ggplot2::scale_fill_distiller(name = toupper(var),
                                                                  palette = "Spectral"),
                                  size = 1,
                                  stroke = 0.5,
                                  legend.key.width = ggplot2::unit(2, "cm"),
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

  shape <- ifelse(.data$elevation >= 1000, 24, 21)
  if (!is.na(units))
    fill_scale$name <- paste0(fill_scale$name, " [", units, "]")
  .datav2 <- .data %>%
    dplyr::rename(var = !!var) %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::distinct(longitude, latitude, .keep_all = TRUE) %>%
    with(akima::interp(x = longitude, y = latitude, z = var))
  # prepare data in long format
  .datav3 <- reshape2::melt(.datav2$z, na.rm = TRUE) %>%
    tibble::as_tibble() %>%
    magrittr::set_names(c("longitude", "latitude", "var")) %>%
    dplyr::mutate(longitude = .datav2$x[longitude],
                  latitude = .datav2$y[latitude])
  p <- .datav3 %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = longitude,
                                           y = latitude,
                                           fill = var)) +
    ggplot2::geom_tile(ggplot2::aes(fill = var)) +
    ggplot2::geom_sf(data = rnaturalearth::ne_countries(scale = "small",
                                                        returnclass = "sf"),
                     fill = NA, size = 0.25) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, ..., expand = FALSE) +
    # ggplot2::stat_contour(ggplot2::aes(z = var),
    #                       colour = "black",
    #                       size = 0.1) +
    ggplot2::geom_point(mapping = ggplot2::aes(longitude,
                                               latitude,
                                               fill = var),
                        data = .data %>%
                          dplyr::rename(var = !!var) %>%
                          dplyr::filter(!is.na(var)),
                        size = size,
                        shape = shape,
                        stroke = stroke,
                        position = "jitter") +
    # ggplot2::geom_col(alpha = 0) +
    fill_scale +
    ggplot2::labs(x = NULL, y = NULL,
                  caption = "Circles: elevation < 1000m -- Triangles: elevation >= 1000m") +
    # ggplot2::guides(
    #   fill = ggplot2::guide_legend(
    #     nrow = 1,
    #     override.aes = list(alpha = 1),
    #     label.position = "bottom",
    #     label.hjust = 0
    #   )
    # ) +
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
#
# fld <- .data %>%
#   dplyr::rename(var = !!var) %>%
#   dplyr::filter(!is.na(var)) %>%
#   dplyr::distinct(longitude, latitude, .keep_all = TRUE) %>%
#   with(akima::interp(x = longitude, y = latitude, z = var))
#
# filled.contour(x = fld$x,
#                y = fld$y,
#                z = fld$z,
#                color.palette =
#                  colorRampPalette(c("white", "blue")),
#                xlab = "Longitude",
#                ylab = "Latitude",
#                main = "Rwandan rainfall",
#                key.title = title(main = "Rain (mm)", cex.main = 1))
#
# # prepare data in long format
# df <- reshape2::melt(fld$z, na.rm = TRUE)
# names(df) <- c("x", "y", "var")
# df$longitude <- fld$x[df$x]
# df$latitude <- fld$y[df$y]
#
# ggplot2::ggplot(data = df, ggplot2::aes(x = longitude, y = latitude, z = var)) +
#   ggplot2::geom_tile(ggplot2::aes(fill = var)) +
#   ggplot2::stat_contour() +
#   # ggplot2::ggtitle("Rwandan rainfall") +
#   ggplot2::xlab("Longitude") +
#   ggplot2::ylab("Latitude") +
#   ggplot2::theme(plot.title = ggplot2::element_text(size = 25, face = "bold"),
#                  legend.title = ggplot2::element_text(size = 15),
#                  axis.text = ggplot2::element_text(size = 15),
#                  axis.title.x = ggplot2::element_text(size = 20, vjust = -0.5),
#                  axis.title.y = ggplot2::element_text(size = 20, vjust = 0.2),
#                  legend.text = ggplot2::element_text(size = 10))
