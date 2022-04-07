## code to prepare `LAMPD` dataset goes here
# The Latin America Modern Pollen Data (LAMPD)
LAMPD <- readxl::read_xls("~/Downloads/SMPDSv2/Latin America/Latin American modern 2008.xls",
                          sheet = 3) %>%
  dplyr::mutate(A135 = as.double(A135)) %>%
  readr::type_convert() %>%
# LAMPD <- LAMPD %>%
  tidyr::pivot_longer(-1) %>%
  tidyr::pivot_wider(names_from = 1, values_from = value) %>%
  magrittr::set_colnames(c("name", "lat", "lon", colnames(.)[-c(1:3)]))
  # magrittr::set_colnames(c("name", "lon", "lat", colnames(.)[-c(1:3)]))

usethis::use_data(LAMPD, overwrite = TRUE)
