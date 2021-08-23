# library(ncdf4)
# library(spgwr)


# a <- gwr(NULL,
#     varid = "SWdown",
#     reference = "~/Downloads/SWdown_daily_WFD_196001.nc",
#     coordinates = "~/Downloads/WFD-land-lat-long-z.nc")

#' @keywords internal
cru_mask <- function(res = 0.5,
                     coordinates = smpds::CRU_coords) {
  x <- seq(-180 + res / 2, 180 - res / 2, res)
  y <- seq(-90 + res / 2, 90 - res / 2, res)
  tibble::tibble(
    latitude = rep(y, length(x)),
    longitude = rep(x, each = length(y)),
    land = FALSE
  ) %>%
    dplyr::left_join(coordinates,
                     by = c("latitude", "longitude")) %>%
    dplyr::mutate(land = ifelse(!is.na(elevation), TRUE, land),
                  sea = !land) %>%
    dplyr::select(-elevation)
}

#' Title
#'
#' @param .data
#' @param varid
#' @param reference
#' @param coordinates
#' @param res
#'
#' @return
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                        latitude = 51.44140,
#'                        longitude = -0.9418)
gwr <- function(.data,
                varid,
                reference = NULL,
                coordinates = smpds::CRU_coords,
                res = 0.5) {
  # browser()
  # # load a nc file including lon,lat,z
  # ncin <- ncdf4::nc_open(coordinates)
  # lon <-
  #   ncdf4::ncvar_get(ncin, "Longitude") #length:67420 (NA values removed in advance)
  # lat <-
  #   ncdf4::ncvar_get(ncin, "Latitude") #length:67420 (NA values removed in advance)
  # Z <-
  #   ncdf4::ncvar_get(ncin, "Z")#length:67420 (NA values removed in advance)
  #
  # #Now, input climate gridded data from WFDEI
  # #We used Shortwave radiation (SWdown) as an example, it is a daily average data from a monthly nc file.

  reference <- "~/OneDrive - University of Reading/UoR/Data/CRU/4.04/cru_ts4.04-clim-1961-1990-daily.tmp.nc"
  ncin <- ncdf4::nc_open(reference)
  reference_tbl <- ncdf4::ncvar_get(ncin, varid) %>%
    mask_nc(mask = cru_mask(res = res, coordinates = coordinates)) %>%
    dplyr::filter(land) %>%
    dplyr::select(-land, -sea)
  ncdf4::nc_close(ncin)

  # Combine this daily gridded data with coordinates
  # climate_grid <- cbind(lon, lat, Z, pre.array)
  climate_grid <- coordinates %>%
    dplyr::right_join(reference_tbl,
                      by = c("latitude", "longitude"))

  # Start implementing Geographically Weighted Regression

  delta <- 1.5 # Here a is 1.5 degrees. It means that when extract values on a site, we only need site ¡À 1.5 lat/lon degrees of grid.
  # This is reasonable, Because on below, we set our bandwidth = 1.06 in our gwr function, considered as the best value in previous research

  # # Now input our extracted site, let's create a site as an example
  plot <- data.frame(matrix(NA)) # for final sites climate data
  plot$Latitude <- 44.26
  plot$Longitude <- -122.2
  plot$Z <- 780

  .data <- tibble::tibble(entity_name = "University of Reading",
                          latitude = 51.44140,
                          longitude = -0.9418,
                          elevation = 61)

  # specify gridded area, where grid is ¡À1.5 degree of focus sites. See description above
  climate_grid2 <- climate_grid %>%
    dplyr::filter(latitude > !!.data$latitude - delta,
                  latitude < !!.data$latitude + delta,
                  longitude > !!.data$longitude - delta,
                  longitude < !!.data$longitude + delta)

  sp::coordinates(.data) <- c("longitude", "latitude")
  sp::coordinates(climate_grid2) <- c("longitude", "latitude")
  # sp::gridded(climate_grid2) <- TRUE

  #Finally, extracted values by GWR
  #SWdown_1960_01_01 represent the first day of the month
  #Change V1 to V1-V31, get the value for day 1 to day 31 of the nc file

  output <- names(climate_grid2) %>%
    stringr::str_subset("^T[0-9]*$") %>%
    stringr::str_c(" ~ elevation") %>%
    purrr::map_dbl(~spgwr::gwr(formula = .x,
                               data = climate_grid2,
                               bandwidth = 1.06,
                               fit.points = .data,
                               predictions = TRUE)$SDF$pred)
  path <- "~/OneDrive - University of Reading/UoR/Data/CRU/4.04/"
  tmp <- codos:::nc_var_get(file.path(path, "cru_ts4.04.1901.2019.daily.tmp.nc"),
                            "tmp")$data
  idx_y <- which.min(abs(codos::lat$data - 51.44140))
  idx_x <- which.min(abs(codos::lon$data - -0.9418))
  tmp_cru_ts <- tmp[c(idx_x, idx_x + 1), c(idx_y - 1, idx_y), ]
  rownames(tmp_cru_ts) <- codos::lat$data[c(idx_y, idx_y - 1)]
  colnames(tmp_cru_ts) <- codos::lon$data[c(idx_x, idx_x + 1)]
  tmp_cru_ts <- colMeans(tmp_cru_ts, dims = 2)

  tibble::tibble(x = rep(1:365, 2),
                 y = c(tmp_cru_ts, output),
                 z = rep(c("4 grid mean", "GWR"), each = 365)) %>%
    ggplot2::ggplot(ggplot2::aes(x, y, colour = z)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_brewer(name = "CRU TS 4.04", palette = "Set1") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(12)) +
    ggplot2::labs(x = "[days]",
                  y = "Daily Temperature [°C]",
                  title = "UoR (51.44140, -0.9418 @ 61 m)") +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = NA),
                   panel.border = ggplot2::element_rect(fill = NA,
                                                        colour = "grey20"),
                   panel.grid = ggplot2::element_line(colour = "grey92"),
                   panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.5)),
                   strip.background = ggplot2::element_rect(fill = "grey85",
                                                            colour = "grey20"),
                   legend.key = ggplot2::element_rect(fill = "white",
                                                      colour = NA),
                   complete = TRUE,
                   legend.position = "bottom")
  # SWdown_1960_01_01 <- (spgwr::gwr(V1 ~ Z,
  #                                  climate_grid2,
  #                                  bandwidth = 1.06,
  #                                  fit.points = plot,
  #                                  predictions = TRUE))$SDF$pred

  p <- basemap +
    ggplot2::geom_point(mapping = ggplot2::aes(x = longitude,
                                               y = latitude,
                                               fill = T100
    ),
    data = climate_grid2,
    size = .5,
    shape = 21,
    stroke = 0)
  p
}

#' @keywords internal
mask_nc <- function(.data, mask = cru_mask()) {
  mask %>%
    dplyr::bind_cols(seq_len(dim(.data)[3]) %>%
                       purrr::map_dfc(function(t) {
                         list(matrix(t(.data[, , t]), ncol = 1)[, 1]) %>%
                           magrittr::set_names(paste0("T", t))
                       })
    )
}

# tictoc::tic()
# b <- mask_nc(reference_tbl, cpus = 1)
# tictoc::toc()
