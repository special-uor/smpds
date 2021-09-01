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
#'
#' @return Table with interpolated values from \code{varid} for each record/row
#'     in \code{.data}.
#' @export
#'
#' @references
#' Peng, Y., Bloomfield, K.J. and Prentice, I.C., 2020. A theory of plant
#' function helps to explain leaf‐trait and productivity responses to elevation.
#' New Phytologist, 226(5), pp.1274-1284. \doi{10.1111/nph.16447}
#'
#' @source
#' This function was adapted from a code developed by Yunke Peng
#' (\email{yunke.peng@usys.ethz.ch}) - ETH Zürich:
#' \url{https://github.com/yunkepeng/gwr}
#'
#' @examples
#' \dontrun{
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                        latitude = 51.44140,
#'                        longitude = -0.9418,
#'                        elevation = c(61, 161, 261, 361))
#' data %>%
#'   smpds::gwr(varid = "tmp",
#'              reference = "/path/to/reference-tmp.nc")
#' }
gwr <- function(.data,
                varid,
                reference = NULL,
                coordinates = smpds::CRU_coords,
                res = 0.5,
                buffer = 1.5,
                cpus = 1,
                bandwidth = 1.06) {
  ncin <- ncdf4::nc_open(reference)
  reference_tbl <- ncdf4::ncvar_get(ncin, varid) %>%
    mask_nc(mask = cru_mask(res = res, coordinates = coordinates)) %>%
    dplyr::filter(land) %>%
    dplyr::select(-land, -sea)
  ncdf4::nc_close(ncin)

  # Combine the daily gridded data with coordinates
  climate_grid <- coordinates %>%
    dplyr::right_join(reference_tbl,
                      by = c("latitude", "longitude"))

  # Start implementing Geographically Weighted Regression
  .data_coords <- .data
  sp::coordinates(.data_coords) <- c("longitude", "latitude")
  # sp::gridded(climate_grid2) <- TRUE

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
                                   predictions = TRUE)$SDF$pred %>%
                         list() %>%
                         magrittr::set_names(.x %>%
                                               stringr::str_remove(fm_suffix)))
    },
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE))

  .data %>%
    dplyr::bind_cols(output)
}

#' Mask NetCDF
#'
#' Mask NetCDF variable.
#'
#' @param .data 3D matrix with \code{latitude}, \code{longitude} and
#'     \code{time}.
#' @param mask 2D matrix with all combinations of \code{latitude},
#'     \code{longitude} and a third variable called \code{land} with logical
#'     values to indicated whether a grid cell should be used or ignored.
#'     \itemize{
#'      \item{\code{land = TRUE} }{ use this value.}
#'      \item{\code{land = FALSE} }{ ignore this value.}
#'     }
#'
#' @return 2D version of \code{.data}, including \code{latitute},
#'     \code{longitude}, and variables with the format \code{T#}, for each
#'     time step in \code{.data}: \code{T1}, \code{T2}, ..., \code{Tk}.
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

#' Pivot data
#'
#' Pivot data obtained with the function \code{\link{gwr}}. Groups all the data
#' points for each entity/row into a single list of elements.
#'
#' @param .data Data frame (\code{tibble} object) obtained with
#'     \code{\link{gwr}}.
#' @param timestep String with the pattern used to name each data point at each
#'     time-step. Default: \code{"^T[0-9]*"}.
#' @param cols Metadata columns (\code{latitude}, \code{longitude}, etc.), to
#'     be excluded from the data pivoting. Default: excluded columns matching
#'     the \code{timestep} pattern, for the default pattern, ignores columns
#'     with the prefix \code{T} and digits as suffix (e.g., \code{T1},
#'     \code{T2}, ..., \code{Tt}, etc.).
#' @param scale Numeric value to scale the data. Default: \code{1}, no scaling.
#' @param add Numeric value to be added/subtracted from the data points.
#'     Default: \code{0}, don't add anything.
#' @param varname Output variable name. Default: \code{"value"}.
#'
#' @return Data frame (\code{tibble} object) with a new column named according
#'     to the string passed with \code{value}, this new column contains a list
#'     of the data points at each time step for each observation/row.
#' @export
#'
#' @examples
#' \dontrun{
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                        latitude = 51.44140,
#'                        longitude = -0.9418,
#'                        elevation = 61)
#' data %>%
#'   smpds::gwr(varid = "tmp",
#'              reference = "/path/to/reference-tmp.nc") %>%
#'   smpds::pivot_data(varname = "tmp")
#' }
pivot_data <- function(.data,
                       timestep = "^T[0-9]*",
                       cols = colnames(.data) %>%
                         stringr::str_detect(timestep, negate = TRUE) %>%
                         which(),
                       scale = 1,
                       add = 0,
                       varname = "value") {
  # Local bindings
  . <- .ID <- value <- NULL

  .data %>%
    dplyr::mutate(.ID = seq_len(nrow(.))) %>% # Create unique ID per row/entity
    tidyr::pivot_longer(c(-dplyr::all_of(cols),
                          -.ID)) %>% # Pivot longer excluding cols and ID
    dplyr::group_by(.ID) %>% # Group by the unique ID assigned to each entity
    dplyr::mutate(value = list(value * scale + add)) %>%
    dplyr::ungroup() %>%
    magrittr::set_names(colnames(.) %>%
                          stringr::str_replace_all("value", varname)) %>%
    dplyr::distinct(.ID, .keep_all = TRUE) %>%
    dplyr::select(-name, -.ID)
}

#' Subset data
#'
#' Subset data using coordinates (\code{latitute} and \code{longitude}) and
#' \code{buffer}.
#'
#' @param .data 2D matrix with columns called \code{latitude} and
#'     \code{longitude}.
#' @param latitude Numeric value for reference \code{latitude}.
#' @param longitude Numeric value for reference \code{longitude}.
#' @inheritParams gwr
#'
#' @return Filtered 2D matrix.
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
