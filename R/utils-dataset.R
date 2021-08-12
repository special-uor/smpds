#' Compare two datasets using \code{latitude} and \code{longitude}
#'
#' Compare two datasets using \code{latitude} and \code{longitude}, the
#' comparison is used by the function \code{fx}, this can be a custom function
#' or an existing function like \code{round}, \code{trunc}, etc.
#'
#' @param x Reference dataset.
#' @param y Target dataset, this will be compared with \code{x}.
#' @param fx Comparison function, it defaults to an internal function called
#'     \code{tolerance} that drops decimals (using the parameter \code{digits})
#'     without rounding the numbers. Suggested base functions are \code{round},
#'     \code{trunc}, \code{floor}, \code{ceiling} and \code{signif}.
#' @param ... Optional parameters passed to \code{fx}.
#'
#' @return Table with records of \code{y} that exist in {x}.
#' @export
compare_latlon <- function(x, y, fx = tolerance, ...) {
  x %>%
    dplyr::filter(fx(latitude, ...) %in% fx(y$latitude, ...),
                  fx(longitude, ...) %in% fx(y$longitude, ...)) %>%
    dplyr::select(1:latitude,
                  longitude,
                  dplyr::starts_with("eleva")) %>%
    dplyr::mutate(lat = fx(latitude, ...),
                  lon = fx(longitude, ...)) %>%
    dplyr::inner_join(y %>%
                        dplyr::select(1:latitude,
                                      longitude,
                                      dplyr::starts_with("eleva")) %>%
                        dplyr::mutate(lat = fx(latitude, ...),
                                      lon = fx(longitude, ...)),
                      by = c("lat", "lon")) %>%
    dplyr::select(-lat, -lon)
}

#' @keywords internal
tolerance <- function(x, digits = 1) {
  # round(x, digits = digits)
  trunc(x * 10 ^ digits) / 10 ^ digits
}
