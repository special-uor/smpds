## code to prepare `APD` dataset goes here
`%>%` <- magrittr::`%>%`
output <- smpds::process_apd("~/Downloads/SMPDSv2/APD/",
                             col_names = c("Taxon Name [APD]",
                                           "Taxon Name [Author]",
                                           "Depth [m]",
                                           "Radiocarbon Chronology [yrs BP]",
                                           "Calendar Chronology [yrs BP]",
                                           "Count"),
                             col_types = c(readr::col_character(),
                                           readr::col_character(),
                                           readr::col_double(),
                                           readr::col_double(),
                                           readr::col_double(),
                                           readr::col_double()))
APD <- output %>%
  purrr::map_df(~.x) %>%
  dplyr::rename(taxon_name = `Taxon Name [APD]`,
                entity_name = name)

usethis::use_data(APD, overwrite = TRUE)
