## code to prepare `EMPD` dataset goes here
sheets <- c("metadata", "climate", "ecosystems", "counts", "p_vars", "sampleContexts", "sampleTypes", "sampleMethods", "workerRoles", "countries", "ageUncertainties", "locationReliabilities", "groupID")
sheets <- c("metadata", "counts", "p_vars")
`%>%` <- magrittr::`%>%`
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
                -c(Publication1, Publication2, Publication3, Publication4, Publication5),
                -Country,
                -LocationReliability) %>%
  dplyr::rename(entity_name = SampleName,
                original_entity_name = OriginalSampleName,
                site_name = SiteName,
                latitude = Latitude,
                longitude = Longitude,
                elevation = Elevation,
                location_notes = LocationNotes,
                area_of_site = AreaOfSite,
                entity_context = SampleContext,
                site_description = SiteDescription,
                vegetation_description = VegDescription,
                entity_type = SampleType,
                sample_method = SampleMethod,
                age_BP = AgeBP)

empdv2_metadata_workers <- empdv2_workbook$metadata %>%
  dplyr::select(dplyr::starts_with("Worker"))

empdv2_counts <- empdv2_workbook$counts %>%
  dplyr::rename(entity_name = SampleName,
                taxon_name = acc_varname) %>%
  dplyr::mutate(ID_COUNT = seq_along(entity_name), .before = 1)

# Find any matches in the SMPDSv1
aux <- SMPDSv1_long %>%
  dplyr::filter(entity_name %in% empdv2_counts$entity_name)
aux <- empdv2_counts %>%
  dplyr::filter(entity_name %in% SMPDSv1_long$entity_name,
                taxon_name %in% SMPDSv1_long$taxon_name)
empdv2_counts_filtered <- empdv2_counts %>%
  dplyr::filter(!(ID_COUNT %in% aux$ID_COUNT))

SMPDSv1_prefix <- SMPDSv1 %>%
  dplyr::mutate(short_entity_name = entity_name %>%
                  stringr::str_extract("[a-zA-Z]*"))
# aux0 <- SMPDSv1_long %>%
#   dplyr::filter(entity_name %in% aux$entity_name,
#                 taxon_name %in% aux$taxon_name)


# usethis::use_data(EMPD, overwrite = TRUE)
