#' Process the African Pollen Database
#'
#' This function loads all the raw data from the African Pollen Database into
#' a single object.
#'
#' @param path Path to where the raw data files are located.
#' @param ext Extension of the raw data files.
#' @inheritParams readr::read_delim
#' @inheritDotParams readr::read_delim -delim -comment
#'
#' @return List with a tibble for each of the raw data files
#' @export
#'
# @examples
process_apd <- function(path, ext = "ascii", delim = ";", comment = "#", ...) {
  files <- list.files(path = path,
                      pattern = paste0(ext, "$"),
                      full.names = TRUE)
  files %>%
    # .[1] %>%
    purrr::map(function(f) {
      header <- f %>%
        readr::read_lines() %>%
        stringr::str_subset("^#") %>%
        .[-c(1, length(.) - 1, length(.))] %>%
        stringr::str_remove_all("^#") %>%
        stringr::str_split_fixed("=", 2) %>%
        matrix(ncol = 2) %>%
        magrittr::set_colnames(c("key", "value")) %>%
        tibble::as_tibble() %>%
        # magrittr::set_names(c("key", "value")) %>%
        tidyr::pivot_wider(names_from = "key", values_from = "value") %>%
        magrittr::set_names(colnames(.) %>% stringr::str_to_lower()) %>%
        dplyr::mutate(latitude = latitude %>%
                        sp::char2dms(chd = "°", chm = "'", chs = "\"") %>%
                        as.double(),
                      longitude = longitude %>%
                        sp::char2dms(chd = "°", chm = "'", chs = "\"") %>%
                        as.double(),
                      altitude = altitude %>%
                        stringr::str_extract("-*[0-9]*") %>%
                        as.double()) %>%
        dplyr::rename(elevation = altitude) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(publication =
                        dplyr::across(dplyr::starts_with("reference")) %>%
                        .[!is.na(.)] %>%
                        unique() %>%
                        stringr::str_c(collapse = ";\n"),
                      publication =
                        ifelse(publication == "", NA, publication)) %>%
        dplyr::select(-dplyr::starts_with("reference"))

      data <- f %>%
        readr::read_delim(delim = delim,
                          comment = comment,
                          ...) %>%
        dplyr::mutate(sigle = basename(f) %>%
                        stringr::str_remove_all(ext) %>%
                        stringr::str_remove_all(".$"))
      header %>%
        dplyr::inner_join(data,
                          by = "sigle")
    })
}
