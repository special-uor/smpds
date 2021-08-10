## code to prepare `SMPDSv1` dataset goes here
# SMPDSv1 <- readr::read_csv("~/Downloads/SMPDSv2/SMPDS_Feb2019.csv")
# SMPDSv1_long <- SMPDSv1 %>%
#   tidyr::pivot_longer(6:252, names_to = "taxon_name")
`%>%` <- magrittr::`%>%`
SMPDSv1 <- readxl::read_xlsx("~/Downloads/SMPDSv2/Sandy_s MPDS_20_October_expanded.xlsx",
                             sheet = 1,
                             col_types = c(rep("guess", 10),
                                           rep("numeric", 1555))) %>%
  dplyr::rename(source = SOURCE,
                site_name = `Site Name`,
                entity_name = `Entity name`,
                latitude = Latitude,
                longitude = Longitude,
                elevation = Elevation,
                basin_size = `Basin size  (Km2)`,
                site_type = `Site Type`,
                entity_type = `Entity Type`,
                age_BP = AgeBP) %>%
  dplyr::mutate(ID_SMPDSv1 = seq_along(entity_name), .before = 1) %>%
  # dplyr::mutate(short_entity_name = entity_name %>%
  #                 stringr::str_extract("[a-zA-Z]*"),
  #               .after = entity_name)# %>%
  # dplyr::mutate(age_BP = as.double(age_BP)) %>%
  dplyr::select(1:11, order(colnames(.)[-c(1:11)]) + 11) # Sort the taxon_names alphabetically

usethis::use_data(SMPDSv1, overwrite = TRUE, compress = "xz")


# ------------------------------------------------------------------------------
# |                                  Sandbox                                   |
# ------------------------------------------------------------------------------
SMPDSv1 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_count = dplyr::c_across(Abies:Zygophyllum) %>%
                  sum(na.rm = TRUE), .before = Abies) #%>%
# dplyr::arrange(total_count) %>%
# dplyr::filter(total_count < 99)


# Find duplicated entity_name
idx <- duplicated(SMPDSv1$entity_name)
SMPDSv1_dup <- SMPDSv1 %>%
  dplyr::filter(entity_name %in% SMPDSv1$entity_name[idx])

SMPDSv1 <- SMPDSv1 %>%
  dplyr::filter(!(ID_SMPDSv1 %in% SMPDSv1_dup$ID_SMPDSv1))

SMPDSv1_long <- SMPDSv1 %>%
  tidyr::pivot_longer(cols = -c(1:12), names_to = "taxon_name") %>%
  dplyr::filter(!is.na(value))

SMPDSv1_wide <- SMPDSv1_long %>%
  tidyr::pivot_wider(1:12, names_from = "taxon_name")
# duplicated_taxa <- SMPDSv1 %>%
#   dplyr::select(dplyr::contains("..."))
# idx <- rowSums(is.na(duplicated_taxa[, 4:6])) != 3
# duplicated_taxa[idx,]
