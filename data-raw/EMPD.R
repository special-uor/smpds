## code to prepare `EMPD` dataset goes here
sheets <- c("metadata", "climate", "ecosystems", "counts", "p_vars", "sampleContexts", "sampleTypes", "sampleMethods", "workerRoles", "countries", "ageUncertainties", "locationReliabilities", "groupID")
sheets <- c("metadata", "counts", "p_vars")
empdv2_workbook <- sheets %>%
  purrr::map(function(s) {
    readxl::read_xlsx(path = "~/Downloads/SMPDSv2/EMPDv2/Data/Eurasian Modern Pollen Database (former European Modern Pollen Database). .xlsx",
                      sheet = s)
  }) %>%
  magrittr::set_names(sheets)

empdv2_str <- empdv2_workbook %>%
  purrr::map(~names(.x))
empdv2_metadata <- empdv2_workbook$metadata %>%
  dplyr::select(-dplyr::starts_with("Worker")) %>%
  dplyr::group_by(SampleName) %>%
  dplyr::mutate(DOI = c(DOI1, DOI2, DOI3, DOI4, DOI5) %>%
                  .[!is.na(.)] %>%
                  stringr::str_c(collapse = ";\n"),
                DOI = ifelse(DOI == "", NA, DOI),
                Publication = c(Publication1, Publication2, Publication3, Publication4, Publication5) %>%
                  .[!is.na(.)] %>%
                  stringr::str_c(collapse = ";\n"),
                Publication = ifelse(Publication == "", NA, Publication)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(DOI1, DOI2, DOI3, DOI4, DOI5),
                -c(Publication1, Publication2, Publication3, Publication4, Publication5))
empdv2_metadata_workers <- empdv2_workbook$metadata %>%
  dplyr::select(dplyr::starts_with("Worker"))

# usethis::use_data(EMPD, overwrite = TRUE)
