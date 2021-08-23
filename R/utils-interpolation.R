#' Create land-sea mask for the CRU TS dataset
#'
#' @param res Numeric value for the mask resolution. Default: 0.5 degrees.
#' @param coordinates Reference data set with columns for \code{latitude},
#'     \code{longitude} and \code{elevation}. Default:
#'     \code{\link{smpds::CRU_coords}}.
#'
#' @return Table with land-sea mask:
#' \itemize{
#'  \item{\code{land = TRUE} }{ Grid cell with data provided by the CRU TS.}
#'  \item{\code{land = FALSE} }{ Grid cell where data is not provided by the
#'  CRU TS.}
#' }
#'
#' @keywords internal
cru_mask <- function(res = 0.5,
                     coordinates = smpds::CRU_coords) {
  x <- seq(-180 + res / 2, 180 - res / 2, res)
  y <- seq(-90 + res / 2, 90 - res / 2, res)
  if (!all(c("latitude", "longitude", "elevation") %in%
           colnames(coordinates))) {
    stop("The `coordinates` table is expected to have columns called:",
         "\n- `elevation`",
         "\n- `latitude`",
         "\n- `longitude`",
         call. = FALSE)
  }
  tibble::tibble(latitude = rep(y, length(x)),
                 longitude = rep(x, each = length(y)),
                 land = FALSE) %>%
    dplyr::left_join(coordinates,
                     by = c("latitude", "longitude")) %>%
    dplyr::mutate(land = ifelse(!is.na(elevation), TRUE, land),
                  sea = !land) %>%
    dplyr::select(-elevation)
}

#' Geographically Weighted Regression
#'
#' @param .data Table with geographical data, including: \code{latitude},
#'     \code{longitude} and \code{elevation}.
#' @param varid Main variable name for the NetCDF in \code{reference}
#' @param reference Path to the NetCDF file from which \code{varid} will be
#'     loaded.
#' @inheritParams cru_mask
#'
#' @return Table with interpolated values from \code{varid} for each record/row
#'     in \code{.data}.
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                        latitude = 51.44140,
#'                        longitude = -0.9418,
#'                        elevation = c(61, 161, 261, 361))
#' data %>% dplyr::slice(1) %>%
#'   smpds::gwr(varid = "tmp",
#'              reference = "inst/extdata/cru_ts4.04-clim-1961-1990-daily_tmp_1-5.nc")
gwr <- function(.data,
                varid,
                reference = NULL,
                coordinates = smpds::CRU_coords,
                res = 0.5) {
  # reference <- "~/OneDrive - University of Reading/UoR/Data/CRU/4.04/cru_ts4.04-clim-1961-1990-daily.tmp.nc"
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

  # # # Now input our extracted site, let's create a site as an example
  # plot <- data.frame(matrix(NA)) # for final sites climate data
  # plot$Latitude <- 44.26
  # plot$Longitude <- -122.2
  # plot$Z <- 780
  #
  # .data <- tibble::tibble(entity_name = "University of Reading",
  #                         latitude = 51.44140,
  #                         longitude = -0.9418,
  #                         elevation = 61)

  # specify gridded area, where grid is ¡À1.5 degree of focus sites. See description above
  climate_grid2 <- climate_grid %>%
    dplyr::filter(latitude > min(!!.data$latitude - delta),
                  latitude < max(!!.data$latitude + delta),
                  longitude > min(!!.data$longitude - delta),
                  longitude < max(!!.data$longitude + delta))

  .data_coords <- .data
  sp::coordinates(.data_coords) <- c("longitude", "latitude")
  sp::coordinates(climate_grid2) <- c("longitude", "latitude")
  # sp::gridded(climate_grid2) <- TRUE

  #Finally, extracted values by GWR
  #SWdown_1960_01_01 represent the first day of the month
  #Change V1 to V1-V31, get the value for day 1 to day 31 of the nc file

  fm_suffix <- " ~ elevation"
  output <- names(climate_grid2) %>%
    stringr::str_subset("^T[0-9]*$") %>%
    stringr::str_c(fm_suffix) %>%
    purrr::map_dfc(~spgwr::gwr(formula = .x,
                               data = climate_grid2,
                               bandwidth = 1.06,
                               fit.points = .data_coords,
                               predictions = TRUE)$SDF$pred %>%
                     list() %>%
                     magrittr::set_names(.x %>%
                                           stringr::str_remove(fm_suffix)))

  # SWdown_1960_01_01 <- (spgwr::gwr(V1 ~ Z,
  #                                  climate_grid2,
  #                                  bandwidth = 1.06,
  #                                  fit.points = plot,
  #                                  predictions = TRUE))$SDF$pred
  .data %>%
    dplyr::bind_cols(output)
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