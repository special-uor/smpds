#' @export
process_apd <- function(path, ext = "ascii", delim = ";", comment = "#", ...) {
  files <- list.files(path = path,
                      pattern = paste0(ext, "$"),
                      full.names = TRUE)
  files %>%
    # .[1] %>%
    purrr::map(function(f) {
      f %>%
        readr::read_delim(delim = delim,
                          comment = comment,
                          ...) %>%
        dplyr::mutate(name = basename(f) %>%
                        stringr::str_remove_all(ext) %>%
                        stringr::str_remove_all(".$"))
    })
}
output <- process_apd("~/Downloads/SMPDSv2/APD/",
                      col_names = c("Taxon Name [APD]",
                                    "Taxon Name [Author]",
                                    "Depth [m]",
                                    "Radiocarbon Chronology [yrs.BP.]",
                                    "Calendar Chronology [yrs.BP.]",
                                    "Count"),
                      col_types = c(readr::col_character(),
                                    readr::col_character(),
                                    readr::col_double(),
                                    readr::col_double(),
                                    readr::col_double(),
                                    readr::col_double()))
output_df <- output %>%
  purrr::map_df(~.x)
