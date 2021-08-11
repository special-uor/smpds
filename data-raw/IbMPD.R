## code to prepare `IbMPD` dataset goes here
# Source:
# Harrison, S.P., Shen, Y. and Sweeney, L., 2021. Pollen data and charcoal data
# of the Iberian Peninsula. University of Reading. Dataset.
# http://doi.org/10.17864/1947.294

IbMPD <- readr::read_csv("inst/extdata/iberia_pollen_records.csv",
                         col_types = paste0("ccdddcc",
                                            paste0(rep("d", 213),
                                                   collapse = ""),
                                            collapse = "")) %>%
  magrittr::set_names(colnames(.) %>%
                        stringr::str_replace_all("\\.\\.", " ") %>%
                        stringr::str_remove_all("\\.")) %>%
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(entity_name = entity_name %>%
                  stringr::str_c("_", seq_along(entity_name))) %>%
  dplyr::select(-dplyr::starts_with("INTCAL")) %>%
  dplyr::rename(source = souce) %>%
  dplyr::relocate(source, .before = 1)

usethis::use_data(Herzschuh, overwrite = TRUE, compress = "xz")
