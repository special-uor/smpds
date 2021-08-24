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
#' @param buffer Numeric value to be used as the boundary for the search area:
#'     \itemize{
#'      \item \code{latitude} < \code{.data$latitude + buffer}
#'      \item \code{latitude} > \code{.data$latitude - buffer}
#'      \item \code{longitude} < \code{.data$longitude + buffer}
#'      \item \code{longitude} > \code{.data$longitude - buffer}
#'     }
#' @param cpus Number of CPUs to be used in parallel, default = 1.
#' @inheritParams spgwr::gwr
#' @inheritDotParams spgwr::gwr -formula -data -bandwidth -fit.points -predictions
#'
#' @return Table with interpolated values from \code{varid} for each record/row
#'     in \code{.data}.
#' @export
#'
#' @references
#' Peng, Y., Bloomfield, K.J. and Prentice, I.C., 2020. A theory of plant
#' function helps to explain leaf‐trait and productivity responses to elevation.
#' New Phytologist, 226(5), pp.1274-1284. doi:10.1111/nph.16447
#'
#' @source
#' https://github.com/yunkepeng/gwr
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                        latitude = 51.44140,
#'                        longitude = -0.9418,
#'                        elevation = c(61, 161, 261, 361))
#' data %>%
#'   smpds::gwr(varid = "tmp",
#'              reference = "inst/extdata/cru_ts4.04-clim-1961-1990-daily_tmp_1-5.nc")
gwr <- function(.data,
                varid,
                reference = NULL,
                coordinates = smpds::CRU_coords,
                res = 0.5,
                buffer = 1.5,
                cpus = 1,
                bandwidth = 1.06,
                ...) {
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
#
#   # specify gridded area, where grid is ¡À1.5 degree of focus sites. See description above
#   climate_grid2 <- climate_grid %>%
#     dplyr::filter(latitude > min(!!.data$latitude - buffer),
#                   latitude < max(!!.data$latitude + buffer),
#                   longitude > min(!!.data$longitude - buffer),
#                   longitude < max(!!.data$longitude + buffer))
#   sp::coordinates(climate_grid2) <- c("longitude", "latitude")

  .data_coords <- .data
  sp::coordinates(.data_coords) <- c("longitude", "latitude")
  # sp::gridded(climate_grid2) <- TRUE

  #Finally, extracted values by GWR
  #SWdown_1960_01_01 represent the first day of the month
  #Change V1 to V1-V31, get the value for day 1 to day 31 of the nc file

  oplan <- future::plan(future::multisession, workers = cpus)
  on.exit(future::plan(oplan), add = TRUE)
  fm_suffix <- " ~ elevation"
  output <- seq_len(nrow(.data)) %>%
    furrr:::future_map_dfr(function(i) {
      climate_grid2 <- subset_coords(climate_grid,
                                     .data$latitude[i],
                                     .data$longitude[i],
                                     buffer)
      fms <-  names(climate_grid2) %>%
        stringr::str_subset("^T[0-9]*$") %>%
        stringr::str_c(fm_suffix)
      fms %>%
        purrr::map_dfc(~spgwr::gwr(formula = .x,
                                   data = climate_grid2,
                                   bandwidth = bandwidth,
                                   fit.points = .data_coords[i, ],
                                   predictions = TRUE,
                                   ...)$SDF$pred %>%
                         list() %>%
                         magrittr::set_names(.x %>%
                                               stringr::str_remove(fm_suffix)))
  },
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE))

  # output <- names(climate_grid2) %>%
  #   stringr::str_subset("^T[0-9]*$") %>%
  #   stringr::str_c(fm_suffix) %>%
  #   purrr::map_dfc(~spgwr::gwr(formula = .x,
  #                              data = climate_grid2,
  #                              bandwidth = 1.06,
  #                              fit.points = .data_coords,
  #                              predictions = TRUE)$SDF$pred %>%
  #                    list() %>%
  #                    magrittr::set_names(.x %>%
  #                                          stringr::str_remove(fm_suffix)))

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

#' @keywords internal
subset_coords <- function(.data, latitude, longitude, buffer) {
  .data_coords <- .data %>%
    dplyr::filter(latitude > min(!!latitude - buffer),
                  latitude < max(!!latitude + buffer),
                  longitude > min(!!longitude - buffer),
                  longitude < max(!!longitude + buffer))
  sp::coordinates(.data_coords) <- c("longitude", "latitude")
  return(.data_coords)
}
