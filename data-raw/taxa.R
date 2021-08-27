## code to prepare `taxa` dataset goes here
taxa_all <-
  readxl::read_xlsx("~/Downloads/SMPDSv2/SMPDSv2-taxa-list_2021-08-21.xlsx",
                    sheet = 1) %>%
  dplyr::select(-3, -6) %>%
  magrittr::set_names(c("taxon_name",
                        "clean_name",
                        "unique_clean_name",
                        "species_amalgamation",
                        "unique_species_amalgamation",
                        "non_woody_genus_amalgamation",
                        "retrocompatible_name")) %>%
  # dplyr::distinct() %>%
  dplyr::mutate(
    action = ifelse(clean_name %>%
                      stringr::str_to_lower() %>%
                      stringr::str_detect("excl"),
                    "delete", "update"),
    # clean_name = ifelse(action == "delete", NA, clean_name),
    .after = clean_name
  ) %>%
  dplyr::mutate(clean_name = clean_name %>%
                  stringr::str_squish()) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name)

taxa_all %>%
  readr::write_excel_csv("inst/extdata/all_taxa.csv", na = "")
usethis::use_data(taxa, overwrite = TRUE)
