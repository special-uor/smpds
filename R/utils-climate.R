#' Calculate GDD
#'
#' Calculate Growing Degree Days above a baseline  \code{baseline}, (GDD) from
#' daily values of temperature.
#'
#' @param .data Object with daily values of temperature. The object can be a:
#' \itemize{
#'  \item Numeric vector
#'  \item Data frame (\code{tibble} object) with a column called \code{tmp},
#'  with values of daily temperature.
#' }
#' @param ... Optional parameters, including the \code{baseline} for the
#'     calculation of the Growing Degree Days, \code{cpus} and \code{pb}.
#'
#' @return If the input (\code{.data}) is a
#' \itemize{
#'  \item Numeric vector, it returns a single value for GDD.
#'  \item Data frame (\code{tibble} object), it returns the same data frame
#'  with an additional column called \code{gddX} (where \code{X} represents
#'  the value of the \code{baseline}; e.g., \code{gdd0} when
#'  \code{baseline = 0}), containing values for GDD.
#' }
#' @export
#' @rdname gdd
#' @seealso \code{\link{plot_gdd}}
#' @family utils climate
#'
#' @examples
#' \dontrun{
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                        latitude = 51.44140,
#'                        longitude = -0.9418,
#'                        elevation = 61)
#' smpds::gwr(.ref = "/path/to/reference-tmp.nc",
#'            .tar = data,
#'            varid = "tmp") %>%
#'   smpds::pivot_data(varname = "tmp") %>%
#'   smpds::gdd()
#' }
gdd <- function(.data, ...) {
  UseMethod("gdd", .data)
}

#' @param baseline Numeric value to be used as the baseline for the calculation
#'     of the Growing Degree Days, default: \code{0}.
#' @param pb Function to signal the execution of one cycle (e.g.
#'     \code{progressr::progressor(...)}). Default: \code{NULL}, do nothing.
#' @export
#' @rdname gdd
gdd.numeric <- function(.data, baseline = 0, pb = NULL, ...) {
  # Local binding
  tmp <- NULL
  output <- tibble::tibble(tmp = !!.data) %>%
    dplyr::filter(!is.na(tmp), tmp >= baseline) %>%
    dplyr::mutate(tmp = tmp - baseline) %>%
    dplyr::summarise(gdd =  sum(tmp, na.rm = TRUE)) %>%
    purrr::flatten_dbl()
  if (!is.null(pb))
    pb() # Signal progress
  return(output)
}

#' @param cpus Numeric value with the number of CPUs to be used for the
#'     computation. Default: \code{1}, serial computation.
#' @export
#' @rdname gdd
gdd.tbl_df <- function(.data, baseline = 0, cpus = 1, ...) {
  # Local bindings
  . <- tmp <- NULL
  # Create data subset to improve performance
  .data <- .data %>%
    dplyr::mutate(.ID_CLIM_VAR = seq_along(latitude))
  .data_sub <- .data %>%
    dplyr::select(tmp, .ID_CLIM_VAR)
  oplan <- future::plan(future::multisession, workers = cpus)
  {
    pb <- progressr::progressor(steps = nrow(.data_sub))
    output <- .data_sub %>%
      dplyr::mutate(gdd = tmp %>%
                      furrr::future_map_dbl(gdd,
                                            baseline = baseline,
                                            pb = pb)) %>%
      magrittr::set_names(c(colnames(.)[-ncol(.)],
                            colnames(.)[ncol(.)] %>%
                              stringr::str_replace("gdd",
                                                   paste0("gdd", baseline))))
  }
  future::plan(oplan)
  # Combine original data, .data, and the output
  .data %>%
    dplyr::left_join(output,
                     by = c("tmp", ".ID_CLIM_VAR")) %>%
    dplyr::select(-dplyr::contains(".ID_CLIM_VAR"))
}

#' Calculate MAT
#'
#' Calculate Mean Annual Temperature (MAT) from daily values of temperature.
#'
#' @param .data Object with daily values of temperature. The object can be a:
#' \itemize{
#'  \item Numeric vector
#'  \item Data frame (\code{tibble} object) with a column called \code{tmp},
#'  with values of daily temperature (a \code{list}).
#' }
#' @param ... Optional parameters, including \code{cpus} and \code{pb}.
#'
#' @return If the input (\code{.data}) is a
#' \itemize{
#'  \item Numeric vector, it returns a single value for MAT.
#'  \item Data frame (\code{tibble} object), it returns the same data frame
#'  with an additional column called \code{mat}, containing values for MAT.
#' }
#' @export
#' @rdname mat
#' @seealso \code{\link{plot_mat}}
#' @family utils climate
mat <- function(.data, ...) {
  UseMethod("mat", .data)
}

#' @inheritParams gdd.numeric
#' @export
#' @rdname mat
mat.numeric <- function(.data, pb = NULL, ...) {
  # Local binding
  tmp <- NULL
  output <- tibble::tibble(tmp = !!.data) %>%
    dplyr::summarise(tmp = mean(tmp,  na.rm = TRUE)) %>%
    purrr::flatten_dbl()
  if (!is.null(pb))
    pb() # Signal progress
  return(output)
}

#' @inheritParams gdd.tbl_df
#' @export
#' @rdname mat
mat.tbl_df <- function(.data, cpus = 1, ...) {
  # Local binding
  tmp <- NULL
  # Create data subset to improve performance
  .data <- .data %>%
    dplyr::mutate(.ID_CLIM_VAR = seq_along(latitude))
  .data_sub <- .data %>%
    dplyr::select(tmp, .ID_CLIM_VAR)
  oplan <- future::plan(future::multisession, workers = cpus)
  {
    pb <- progressr::progressor(steps = nrow(.data_sub))
    output <- .data_sub %>%
      dplyr::mutate(mat = tmp %>%
                      furrr::future_map_dbl(mat, pb = pb))
  }
  future::plan(oplan)
  # Combine original data, .data, and the output
  .data %>%
    dplyr::left_join(output,
                     by = c("tmp", ".ID_CLIM_VAR")) %>%
    dplyr::select(-dplyr::contains(".ID_CLIM_VAR"))
}

#' Calculate MI
#'
#' Calculate Moisture Index (MI) from daily values of precipitation, sunshine
#' fraction and temperature.
#'
#' @details The input (\code{.data} object) data, should contain the following
#' columns:
#' \itemize{
#'  \item{\code{latitude}: }{ numeric values for latitude (decimal degrees).}
#'  \item{\code{elevation}: }{ numeric values for elevation (m A.S.L.)}
#'  \item{\code{pre}: }{ list of numeric values for daily precipitation
#'  (mm / day).}
#'  \item{\code{sf}: }{ list of numeric values for daily sunshine fraction (%).}
#'  \item{\code{tmp}: }{ list of numeric values for daily temperature
#'  (degrees Celsius).}
#' }
#'
#' @param .data Data frame (\code{tibble} object) (see the details section).
#' @param ... Optional parameters, including \code{cpus}.
#'
#' @return Data frame (\code{tibble} object) with the same input data and the
#'  addition of a column called \code{mi}, containing values for MI.
#' @export
#' @rdname mi
#' @seealso \code{\link{plot_mi}}
#' @family utils climate
mi <- function(.data, ...) {
  UseMethod("mi", .data)
}

#' @importFrom magrittr `%$%`
#' @inheritParams gdd.tbl_df
#' @export
#' @rdname mi
mi.tbl_df <- function(.data, cpus = 1, ...) {
  # Local bindings
  elevation <- latitude <- pet_mm <- sf <- tmp <- NULL
  # Create data subset to improve performance
  .data <- .data %>%
    dplyr::mutate(.ID_CLIM_VAR = seq_along(latitude))
  .data_sub <- .data %>%
    dplyr::select(latitude, elevation, sf, tmp, .ID_CLIM_VAR)
  # orb_params <- .data$age_BP %>%
  #   as.double() %>%
  #   tidyr::replace_na(0) %>%
  #   purrr::map_df(~palinsol::astro(-.x, degree = TRUE))
  #
  # .data <- .data %>%
  #   dplyr::bind_cols(orb_params)

  year <- 1961 # This is only used to denote a non-leap year (365 days).
  oplan <- future::plan(future::multisession, workers = cpus)
  {
    p <- progressr::progressor(steps = nrow(.data_sub))
    .pet <- seq_len(nrow(.data_sub)) %>%
      furrr::future_map(function(k) {
        output <- seq_len(365) %>% # Daily values
          purrr::map_dbl(function(i) {
            if (any(is.na(.data_sub$sf[[1]][i]), # Prevent error inside SPLASH
                    is.na(.data_sub$tmp[[1]][i]))) {
              return(NA_real_)
            }
            .data_sub %>%
              dplyr::slice(k) %$%
              splash::calc_daily_evap(lat = latitude,
                                      n = i,
                                      elv = elevation,
                                      y = year,
                                      sf = sf[[1]][i],
                                      tc = tmp[[1]][i]) %$%
              pet_mm # Extract PET
          })
        p()
        output
      })
  }
  future::plan(oplan)

  # Calculate MI
  .data %>%
    dplyr::mutate(mi = seq_along(tmp) %>%
                    purrr::map_dbl(~sum(pre[[.x]], na.rm = TRUE) /
                                     sum(.pet[[.x]], na.rm = TRUE))) %>%
    dplyr::select(-dplyr::contains(".ID_CLIM_VAR"))
}

#' Calculate MTCO
#'
#' Calculate Mean Temperature of the COldest month (MTCO) from daily values
#' of temperature.
#'
#' @param .data Object with daily values of temperature. The object can be a:
#' \itemize{
#'  \item Numeric vector
#'  \item Data frame (\code{tibble} object) with a column called \code{tmp},
#'  with values of daily temperature (a \code{list}).
#' }
#' @param ... Optional parameters, including \code{cpus} and \code{pb}.
#'
#' @return If the input (\code{.data}) is a
#' \itemize{
#'  \item Numeric vector, it returns a single value for MTCO.
#'  \item Data frame (\code{tibble} object), it returns the same data frame
#'  with an additional column called \code{mtco}, containing values for MTCO.
#' }
#' @export
#' @rdname mtco
#' @seealso \code{\link{plot_mtco}}
#' @family utils climate
mtco <- function(.data, ...) {
  UseMethod("mtco", .data)
}

#' @inheritParams gdd.numeric
#' @export
#' @rdname mtco
mtco.numeric <- function(.data, pb = NULL, ...) {
  # Local bindings
  .date <- .month <- tmp <- NULL
  output <-
    tibble::tibble(tmp = !!.data,
                   .date = (seq_along(tmp) - 1) %>% lubridate::as_date(),
                   .month = lubridate::month(.date)) %>%
    dplyr::group_by(.month) %>%
    dplyr::summarise(tmp = mean(tmp,  na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(tmp = min(tmp, na.rm = TRUE)) %>%
    purrr::flatten_dbl()
  if (!is.null(pb))
    pb() # Signal progress
  return(output)
}

#' @inheritParams gdd.tbl_df
#' @export
#' @rdname mtco
mtco.tbl_df <- function(.data, cpus = 1, ...) {
  # Local binding
  tmp <- NULL
  # Create data subset to improve performance
  .data <- .data %>%
    dplyr::mutate(.ID_CLIM_VAR = seq_along(latitude))
  .data_sub <- .data %>%
    dplyr::select(tmp, .ID_CLIM_VAR)
  oplan <- future::plan(future::multisession, workers = cpus)
  {
    pb <- progressr::progressor(steps = nrow(.data_sub))
    output <- .data_sub %>%
      dplyr::mutate(mtco = tmp %>%
                      furrr::future_map_dbl(mtco, pb = pb))
  }
  future::plan(oplan)
  # Combine original data, .data, and the output
  .data %>%
    dplyr::left_join(output,
                     by = c("tmp", ".ID_CLIM_VAR")) %>%
    dplyr::select(-dplyr::contains(".ID_CLIM_VAR"))
}

#' Calculate MTWA
#'
#' Calculate Mean Temperature of the WArmest month (MTWA) from daily values
#' of temperature.
#'
#' @param .data Object with daily values of temperature. The object can be a:
#' \itemize{
#'  \item Numeric vector
#'  \item Data frame (\code{tibble} object) with a column called \code{tmp},
#'  with values of daily temperature.
#' }
#' @param ... Optional parameters, including \code{cpus} and \code{pb}.
#'
#' @return If the input (\code{.data}) is a
#' \itemize{
#'  \item Numeric vector, it returns a single value for MTWA.
#'  \item Data frame (\code{tibble} object), it returns the same data frame
#'  with an additional column called \code{mtco}, containing values for MTWA.
#' }
#' @export
#' @rdname mtwa
#' @seealso \code{\link{plot_mtwa}}
#' @family utils climate
mtwa <- function(.data, ...) {
  UseMethod("mtwa", .data)
}

#' @inheritParams gdd.numeric
#' @export
#' @rdname mtwa
mtwa.numeric <- function(.data, pb = NULL, ...) {
  # Local bindings
  .date <- .month <- tmp <- NULL
  output <-
    tibble::tibble(tmp = !!.data,
                   .date = (seq_along(tmp) - 1) %>% lubridate::as_date(),
                   .month = lubridate::month(.date)) %>%
    dplyr::group_by(.month) %>%
    dplyr::summarise(tmp = mean(tmp,  na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(tmp = max(tmp, na.rm = TRUE)) %>%
    purrr::flatten_dbl()
  if (!is.null(pb))
    pb() # Signal progress
  return(output)
}

#' @inheritParams gdd.tbl_df
#' @export
#' @rdname mtwa
mtwa.tbl_df <- function(.data, cpus = 1, ...) {
  # Local binding
  tmp <- NULL
  # Create data subset to improve performance
  .data <- .data %>%
    dplyr::mutate(.ID_CLIM_VAR = seq_along(latitude))
  .data_sub <- .data %>%
    dplyr::select(tmp, .ID_CLIM_VAR)
  oplan <- future::plan(future::multisession, workers = cpus)
  {
    pb <- progressr::progressor(steps = nrow(.data_sub))
    output <- .data_sub %>%
      dplyr::mutate(mtwa = tmp %>%
                      furrr::future_map_dbl(mtwa, pb = pb))
  }
  future::plan(oplan)
  # Combine original data, .data, and the output
  .data %>%
    dplyr::left_join(output,
                     by = c("tmp", ".ID_CLIM_VAR")) %>%
    dplyr::select(-dplyr::contains(".ID_CLIM_VAR"))
}
