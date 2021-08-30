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
#' @param ... Optional parameters. Including the \code{baseline} for the
#'     calculation of the Growing Degree Days.
#'
#' @return If the input is a
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
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                        latitude = 51.44140,
#'                        longitude = -0.9418,
#'                        elevation = 61)
#' data %>%
#'   smpds::gwr(varid = "tmp",
#'              reference = "inst/extdata/cru_ts4.04-clim-1961-1990-daily_tmp_1-5.nc") %>%
#'   smpds::pivot_data(varname = "tmp") %>%
#'   smpds::gdd()
gdd <- function(.data, ...) {
  UseMethod("gdd", .data)
}

#' @param baseline Numeric value to be used as the baseline for the calculation
#'     of the Growing Degree Days, default: \code{0}.
#' @export
#' @rdname gdd
gdd.numeric <- function(.data, baseline = 0, ...) {
  tibble::tibble(tmp = !!.data) %>%
    dplyr::filter(!is.na(tmp), tmp >= baseline) %>%
    dplyr::mutate(tmp = tmp - baseline) %>%
    dplyr::summarise(gdd =  sum(tmp, na.rm = TRUE)) %>%
    purrr::flatten_dbl()
}

#' @export
#' @rdname gdd
gdd.tbl_df <- function(.data, baseline = 0, ...) {
  .data %>%
    dplyr::mutate(gdd = tmp %>%
                    purrr::map_dbl(gdd, baseline = baseline)) %>%
    magrittr::set_names(colnames(.) %>%
                          stringr::str_replace_all("gdd",
                                                   paste0("gdd", baseline)))
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
#' @param ... Optional parameters (not used).
#'
#' @return If the input is a
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

#' @export
#' @rdname mtco
mtco.numeric <- function(.data, ...) {
  tibble::tibble(tmp = !!.data,
                 .date = (seq_along(tmp) - 1) %>% lubridate::as_date(),
                 .month = lubridate::month(.date)) %>%
    dplyr::group_by(.month) %>%
    dplyr::summarise(tmp = mean(tmp,  na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(tmp = min(tmp, na.rm = TRUE)) %>%
    purrr::flatten_dbl()
}

#' @export
#' @rdname mtco
mtco.tbl_df <- function(.data, ...) {
  .data %>%
    dplyr::mutate(mtco = tmp %>%
                    purrr::map_dbl(mtco))
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
#' @param ... Optional parameters (not used).
#'
#' @return If the input is a
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

#' @export
#' @rdname mtwa
mtwa.numeric <- function(.data, ...) {
  tibble::tibble(tmp = !!.data,
                 .date = (seq_along(tmp) - 1) %>% lubridate::as_date(),
                 .month = lubridate::month(.date)) %>%
    dplyr::group_by(.month) %>%
    dplyr::summarise(tmp = mean(tmp,  na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(tmp = min(tmp, na.rm = TRUE)) %>%
    purrr::flatten_dbl()
}

#' @export
#' @rdname mtwa
mtwa.tbl_df <- function(.data, ...) {
  .data %>%
    dplyr::mutate(mtwa = tmp %>%
                    purrr::map_dbl(mtwa))
}
