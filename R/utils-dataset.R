#' Taxa amalgamation
#'
#' @return Table with taxa amalgamations.
#' @export
amalgamate_taxa <- function() {
  taxa_amalgamation # internal dataset
}

#' Taxa clean-ups
#'
#' @return Table with taxa clean-ups.
#' @export
clean_taxa <- function() {
  taxa_clean # internal dataset
}

#' Compare two datasets using \code{latitude} and \code{longitude}
#'
#' Compare two datasets using \code{latitude} and \code{longitude}, the
#' comparison is done using the function \code{fx}, this can be a custom
#' function or an existing function like \code{round}, \code{trunc}, etc.
#'
#' @param x Reference dataset.
#' @param y Target dataset, this will be compared with \code{x}.
#' @param join_method Function to be used as the joining method between
#'     \code{x} and \code{y}. Default: \code{dplyr::inner_join}.
#' @param fx Comparison function, it defaults to an internal function called
#'     \code{tolerance} that the drops decimals (using the parameter
#'     \code{digits}) without rounding the numbers. Suggested base functions
#'     are \code{round}, \code{trunc}, \code{floor}, \code{ceiling} and
#'     \code{signif}.
#' @param ... Optional parameters passed to \code{fx}.
#'
#' @return Table with records of \code{y} that exist in \code{x}.
#' @export
compare_latlon <- function(x,
                           y,
                           join_method = dplyr::inner_join,
                           fx = tolerance,
                           ...) {
  # Local bindings
  lat <- latitude <- lon <- longitude <- NULL
  x %>%
    dplyr::filter(fx(latitude, ...) %in% fx(y$latitude, ...),
                  fx(longitude, ...) %in% fx(y$longitude, ...)) %>%
    dplyr::select(1:latitude, # Select all the columns from position 1 to lat.
                  longitude,
                  dplyr::starts_with("elev")) %>%
    dplyr::mutate(lat = fx(latitude, ...),
                  lon = fx(longitude, ...)) %>%
    join_method(y %>%
                  dplyr::select(1:latitude,
                                longitude,
                                dplyr::starts_with("elev")) %>%
                  dplyr::mutate(lat = fx(latitude, ...),
                                lon = fx(longitude, ...)),
                by = c("lat","lon")) %>%
    dplyr::select(-lat, -lon)
}

#' Compare two datasets using \code{latitude}, \code{longitude}, and
#' \code{elevation}
#'
#' Compare two datasets using \code{latitude}, \code{longitude}, and
#' \code{elevation}, the
#' comparison is done using the function \code{fx}, this can be a custom
#' function or an existing function like \code{round}, \code{trunc}, etc.
#'
#'
#' @inheritParams compare_latlon
#'
#' @return Table with records of \code{y} that exist in \code{x}.
#' @export
compare_latlonelv <- function(x,
                              y,
                              join_method = dplyr::inner_join,
                              fx = tolerance,
                              ...) {
  # Local bindings
  elv <- elevation <- lat <- latitude <- lon <- longitude <- NULL
  x %>%
    dplyr::filter(fx(latitude, ...) %in% fx(y$latitude, ...),
                  fx(longitude, ...) %in% fx(y$longitude, ...),
                  fx(elevation, ...) %in% fx(y$elevation, ...)) %>%
    dplyr::select(1:latitude, # Select all the columns from position 1 to lat.
                  longitude,
                  elevation) %>%
    dplyr::mutate(lat = fx(latitude, ...),
                  lon = fx(longitude, ...),
                  elv = fx(elevation, ...)) %>%
    join_method(y %>%
                  dplyr::select(1:latitude,
                                longitude,
                                elevation) %>%
                  dplyr::mutate(lat = fx(latitude, ...),
                                lon = fx(longitude, ...),
                                elv = fx(elevation, ...)),
                by = c("lat","lon","elv")) %>%
    dplyr::select(-lat, -lon, -elv)
}

#' Normalise taxa/columns counts
#'
#' Normalises the taxa/columns counts by entity/row. The \code{scale} parameter
#' can be used to scale the normalised values, defaults to 100.
#'
#' @param .data Data frame.
#' @param cols Numeric vector with the columns to be ignored (metadata columns).
#' @param scale Numeric scale factor, default = 100.
#'
#' @return Data frame with normalised taxa/columns counts.
#' @export
normalise_taxa <- function(.data, cols = 1, scale = 100) {
  # Local bindings
  . <- ID_ <- value <- total <- NULL
  # Create temporal object with 'internal ID' (ID_) and pivot columns to rows
  aux <- .data %>%
    dplyr::mutate(ID_ = seq_len(nrow(.))) %>%
    tidyr::pivot_longer(cols = -c("ID_", names(.)[cols]))

  # Use the 'internal ID' (ID_) to normalise the taxa by group
  aux2 <- aux %>%
    dplyr::select(-names(.)[cols]) %>% # ignore ID columns
    dplyr::group_by(ID_) %>% # group by 'internal ID'
    dplyr::mutate(total = dplyr::c_across(value) %>%
                    sum(na.rm = TRUE),
                  value = as.double(value) / total * scale) %>%
    dplyr::select(-total) %>%
    dplyr::ungroup() %>%
    dplyr::select(-ID_) # drop 'internal ID'

  # Combine initial input (long format) with standardised taxa and pivot data
  aux %>%
    dplyr::select(names(.)[cols]) %>%
    dplyr::bind_cols(aux2) %>%
    tidyr::pivot_wider(id_cols = names(.)[cols])
}

#' Remove missing taxa/columns
#'
#' Removes taxa/columns that are missing.
#'
#' @param .data Data frame.
#' @param cols Numeric vector with the columns to be ignored (metadata columns).
#'
#' @return Reduced data frame with non-missing taxa/columns.
#' @export
rm_na_taxa <- function(.data, cols = 1) {
  # Local binding
  . <- value <- NULL
  .data %>%
    tidyr::pivot_longer(cols = -names(.)[cols]) %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::pivot_wider(id_cols = names(.)[cols])
}

#' Remove taxa/columns with zeros
#'
#' Removes taxa/columns that are equal to zero
#'
#' @param .data Data frame.
#' @param cols Numeric vector with the columns to be ignored (metadata columns).
#'
#' @return Reduced data frame without taxa/columns of zeros.
#' @export
rm_zero_taxa <- function(.data, cols = 1) {
  # Local binding
  . <- value <- NULL
  .data %>%
    tidyr::pivot_longer(cols = -names(.)[cols]) %>%
    dplyr::filter(value != 0) %>%
    tidyr::pivot_wider(id_cols = names(.)[cols])
}

#' Sort taxa/columns alphabetically
#'
#' Sorts taxa/columns alphabetically, ignoring the columns in the positions
#' indicated by \code{col}.
#'
#' @param .data Data frame.
#' @param cols Numeric vector with the columns to be ignored (metadata columns).
#'
#' @return Data frame with taxa/columns alphabetically sorted.
#' @export
sort_taxa <- function(.data, cols = 1) {
  # Local binding
  . <- NULL
  .data %>%
    dplyr::select(names(.)[c(cols, order(colnames(.)[-cols]) + length(cols))])
}

#' @keywords internal
tolerance <- function(x, digits = 1) {
  # round(x, digits = digits)
  trunc(x * 10 ^ digits) / 10 ^ digits
}

#' Add the taxa/columns counts by entity/row
#'
#' Adds the taxa/columns counts by entity/row and inserts the a new column,
#' \code{total}, after the last index in \code{cols}, the metadata columns.
#'
#' @param .data Data frame.
#' @param cols Numeric vector with the columns to be ignored (metadata columns).
#'
#' @return Data frame including a new column, \code{total}, with taxa counts.
#' @export
total_taxa <- function(.data, cols = 1) {
  # Local binding
  . <- NULL
  .data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total = dplyr::c_across(names(.)[-cols]) %>%
                    sum(na.rm = TRUE),
                  .after = max(cols)) %>%
    dplyr::ungroup()
}
