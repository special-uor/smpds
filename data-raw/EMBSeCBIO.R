## code to prepare `EMBSeCBIO` dataset goes here
`%>%` <- magrittr::`%>%`
# all_taxa <- readr::read_csv("inst/extdata/all_taxa.csv") %>%
#   dplyr::select(1:3)
# embsecbio_taxa <- readr::read_csv("inst/extdata/embsecbio_taxa.csv")
# mpd_taxa <- readr::read_csv("inst/extdata/empdv2_taxa.csv")
# ref_taxa <- embsecbio_taxa %>%
#   dplyr::bind_rows(all_taxa) %>%
#   dplyr::distinct(taxon_name, .keep_all = TRUE)
# ref_taxa <- readr::read_csv("inst/extdata/taxa_clean.csv")
ref_taxa <- smpds::clean_taxa()

# EMBSeCBIO records in the SMPDSv1
smpdsv1_embsecbio <- smpds::SMPDSv1 %>%
  dplyr::filter(toupper(source) %>% stringr::str_detect("EMB|EMBS")) %>%
  smpds::total_taxa(1:14)

# embsecbio::site %>%
#   dplyr::filter(site_name %in% smpdsv1_embsecbio$site_name)
# Esmeralda's updates to the EMBSeCBIO
ID_ENTITY_ECS <- c(
  1157, # Updated depths and age errors,
  8, 479, 1062, 1104, 1111, 1114, 1322, 1323, 1324, 1330, 1338, 1346, 1351, 1354, 1359, 1413, 1825, 1828, 2063, 2065, 2066, 2067, 2069, 2070, 2071, 2073, # New age models
  479,1067,1114,1354,1323,1324, # Added missing dated ages
  1827, # Updated depth units
  2105, # Update comments
  2, 3, 4, 5, 6, 7, 9, 10, 11, 1053, 1059, 1074, 1091, 1107, 1109, 1110, 1113, 1115, 1118, 1132, 1148, 1157, 1158, 1342, 1361, 1411, 1412, # New age models
  2074, 2075, 2076, 2077, 2079, 2080, # New age models
  1115, # Update dates and age model
  2115, # New record
  1688, # avg_depth updated
  1053, # depths units changed
  1505, # Added new reference
  1320, # Added new reference
  1114, # Added new reference
  1694, # Added new reference
  1713, # New dating information added
  1103, # Uploaded new age model
  1062, # Age model reviewed
  1354, # Possible duplicated sampling depth identified
  479, # Possible duplicated sampling depth identified
  1154, # Comment removed
  1108,
  1052,
  1713,
  1692,
  1506, # Updated ages
  2116:2124 # New entities
) %>%
  sort() %>%
  unique()

embsecbio_metadata <-
  readr::read_csv("inst/extdata/embsecbio_metadata.csv") #%>%
  # dplyr::filter(ID_ENTITY >= 2115)
  # dplyr::group_by(entity_name) %>%
  # dplyr::mutate(n = length(ID_SAMPLE),
  #               entity_name = ifelse(n > 1,
  #                                    paste0(entity_name, "_", seq_along(n)),
  #                                    entity_name)) %>%
  # dplyr::select(-n)

embsecbio_counts <-
  readr::read_csv("inst/extdata/embsecbio_counts.csv") %>%
  dplyr::mutate(taxon_name = taxon_name %>%
                  stringr::str_remove_all("undiff\\.") %>%
                  stringr::str_squish()) %>%
  dplyr::filter(ID_SAMPLE %in% embsecbio_metadata$ID_SAMPLE)

embsecbio_pubs <-
  readr::read_csv("inst/extdata/embsecbio_pubs.csv") %>%
  dplyr::filter(ID_ENTITY %in% embsecbio_metadata$ID_ENTITY) %>%
  dplyr::group_by(ID_ENTITY) %>%
  dplyr::mutate(publication = citation %>%
                  unique() %>%
                  sort() %>%
                  paste0(collapse = ";\n")) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(ID_ENTITY, .keep_all = TRUE) %>%
  dplyr::select(-ID_ENTITY_PUB,
                -ID_PUB,
                -citation)


embsecbio_unused <- embsecbio_metadata %>%
  dplyr::filter(age_BP < -72)

embsecbio_metadata2 <- embsecbio_metadata %>%
  dplyr::filter(ID_ENTITY >= 2115)
  # dplyr::filter(ID_ENTITY %in% ID_ENTITY_ECS)
  # dplyr::filter(ID_SAMPLE %in% embsecbio_counts$ID_SAMPLE)
embsecbio_counts2 <- embsecbio_counts %>%
  dplyr::filter(ID_SAMPLE %in% embsecbio_metadata2$ID_SAMPLE) %>%
  dplyr::distinct(.keep_all = TRUE)

embsecbio_counts_missing_taxa <- embsecbio_counts2 %>%
  dplyr::left_join(ref_taxa, by = "taxon_name") %>%
  dplyr::filter(is.na(action))

# embsecbio_missing_taxa <- embsecbio_counts_missing_taxa$taxon_name %>%
#   unique() %>%
#   sort()
# tibble::tibble(taxon_name = embsecbio_counts_missing_taxa$taxon_name %>%
#                  unique() %>%
#                  sort(),
#                clean_name = taxon_name) %>%
#   readr::write_csv("~/Downloads/SMPDSv2/EMBSeCBIO_taxon_names_2020-08-31.csv", na = "")

embsecbio_counts3 <- embsecbio_counts2 %>%
  dplyr::left_join(ref_taxa, by = "taxon_name") %>%
  dplyr::filter(!is.na(action), action != "delete") %>%
  dplyr::select(-action) %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) %>%
  # dplyr::group_by(entity_name, taxon_name, age_BP) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(n = length(taxon_count),
                taxon_count = sum(as.double(taxon_count), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
                                    taxon_name_original,
                                    taxon_name)) %>%
  dplyr::distinct(ID_SAMPLE, taxon_name, .keep_all = TRUE) %>%
  # dplyr::distinct(entity_name, taxon_name, age_BP, .keep_all = TRUE) %>%
  dplyr::select(-taxon_name_original, -n) %>%
  dplyr::left_join(embsecbio_pubs,
                   by = "ID_ENTITY") %>%
  dplyr::relocate(publication,
                  .before = taxon_count)

embsecbio_counts_wide <- embsecbio_counts3 %>%
  tidyr::pivot_wider(1:16,
                     names_from = "taxon_name",
                     values_from = "taxon_count") %>%
  smpds::sort_taxa(1:16) %>%
  smpds::rm_na_taxa(1:16) %>%
  dplyr::arrange(entity_name) %>%
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(n = length(entity_name),
                entity_name2 = ifelse(n > 1,
                                      paste0(entity_name, "_", seq_along(n)),
                                      entity_name),
                .before = latitude) %>%
  dplyr::ungroup() %>%
  dplyr::select(-n) %>%
  smpds::parallel_extract_biome(cpus = 2) %>%
  progressr::with_progress()

EMBSeCBIO <- embsecbio_counts_wide %>%
  dplyr::select(-ID_SITE,
                -ID_ENTITY,
                -ID_SAMPLE,
                -entity_name,
                -sample_type,
                -avg_depth) %>%
  dplyr::relocate(ID_BIOME, .before = age_BP) %>%
  dplyr::rename(entity_name = entity_name2) %>%
  dplyr::mutate(source = ifelse(is.na(source), "EMBSeCBIO", source)) %>%
  dplyr::relocate(source, .before = 1)
usethis::use_data(EMBSeCBIO, overwrite = TRUE)

# ---- Comparisons -------------------------------------------------------------
# Find records in the SMPDSv1 from the EMBSeCBIO
count_decimals <- function(x) {
  as.character(x) %>%
    purrr::map_dbl(~.x %>% stringr::str_split_fixed("\\.", 2) %>% .[2] %>% nchar())
}

# Esmeralda's updates to the EMBSeCBIO
ID_ENTITY <- c(
  1157, # Updated depths and age errors,
  8, 479, 1062, 1104, 1111, 1114, 1322, 1323, 1324, 1330, 1338, 1346, 1351, 1354, 1359, 1413, 1825, 1828, 2063, 2065, 2066, 2067, 2069, 2070, 2071, 2073, # New age models
  479,1067,1114,1354,1323,1324, # Added missing dated ages
  1827, # Updated depth units
  2105, # Update comments
  2, 3, 4, 5, 6, 7, 9, 10, 11, 1053, 1059, 1074, 1091, 1107, 1109, 1110, 1113, 1115, 1118, 1132, 1148, 1157, 1158, 1342, 1361, 1411, 1412, # New age models
  2074, 2075, 2076, 2077, 2079, 2080, # New age models
  1115, # Update dates and age model
  2115, # New record
  1688, # avg_depth updated
  1053, # depths units changed
  1505, # Added new reference
  1320, # Added new reference
  1114, # Added new reference
  1694, # Added new reference
  1713, # New dating information added
  1103, # Uploaded new age model
  1062, # Age model reviewed
  1354, # Possible duplicated sampling depth identified
  479, # Possible duplicated sampling depth identified
  1154, # Comment removed
  1108,
  1052,
  1713,
  1692,
  1506, # Updated ages
  2116:2124 # New entities
) %>%
  sort() %>%
  unique()

embsecbio_metadata_ECS <- embsecbio::entity %>% #embsecbio_metadata %>%
  dplyr::filter(ID_ENTITY %in% !!ID_ENTITY)
matched_by_entity_name_ECS <- embsecbio_metadata_ECS %>%
  dplyr::filter(entity_name %in% smpds::SMPDSv1$entity_name)
unmatched_by_entity_name_ECS <- embsecbio_metadata_ECS %>%
  dplyr::filter(!(entity_name %in% matched_by_entity_name_ECS$entity_name))
unmatched_by_entity_name_ECS %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/EMBSeCBIO_records_updated_by_ECS.csv", na = "")

digits <- 4
aux_ECS <- smpds::compare_latlonelv(smpdsv1_embsecbio, #smpds::SMPDSv1,
                                    unmatched_by_entity_name_ECS,
                                    digits = digits)

aux_ECS2 <- aux_ECS %>%
  dplyr::filter(count_decimals(latitude.x) >= digits,
                count_decimals(latitude.y) >= digits,
                count_decimals(longitude.x) >= digits,
                count_decimals(longitude.x) >= digits)

## All the records
matched_by_entity_name_db <- embsecbio::entity %>%
  dplyr::filter(entity_name %in% smpds::SMPDSv1$entity_name)
unmatched_by_entity_name_db <- embsecbio::entity %>%
  dplyr::filter(!(entity_name %in% matched_by_entity_name_db$entity_name))
# matched_by_site_name_db <- unmatched_by_entity_name_db %>%
#   dplyr::filter(site_name %in% smpds::SMPDSv1$site_name)
digits <- 4
aux_db <- smpds::compare_latlon(smpdsv1_embsecbio, # smpds::SMPDSv1,
                                unmatched_by_entity_name_db,
                                join_method = dplyr::right_join,
                                digits = digits)
aux_db <- smpds::compare_latlonelv(smpdsv1_embsecbio, # smpds::SMPDSv1,
                                   unmatched_by_entity_name_db,
                                   join_method = dplyr::right_join,
                                   digits = digits)
aux_db2 <- aux_db %>%
  dplyr::filter(count_decimals(latitude.x) >= digits,
                count_decimals(latitude.y) >= digits,
                count_decimals(longitude.x) >= digits,
                count_decimals(longitude.x) >= digits)

## Subset
matched_by_entity_name_sub <- embsecbio_metadata %>%
  dplyr::filter(entity_name %in% smpds::SMPDSv1$entity_name)
unmatched_by_entity_name_sub <- embsecbio_metadata %>%
  dplyr::filter(!(entity_name %in% matched_by_entity_name_sub$entity_name))
# matched_by_site_name_sub <- unmatched_by_entity_name_sub %>%
#   dplyr::filter(site_name %in% smpds::SMPDSv1$site_name)
digits <- 4
smpds::compare_latlon(smpdsv1_embsecbio, # smpds::SMPDSv1,
                      unmatched_by_entity_name_sub,
                      digits = digits)

aux_sub <- smpds::compare_latlonelv(smpdsv1_embsecbio, # smpds::SMPDSv1,
                                    unmatched_by_entity_name_sub,
                                    digits = digits)

aux_sub2 <- aux_sub %>%
  dplyr::filter(count_decimals(latitude.x) >= digits,
                count_decimals(latitude.y) >= digits,
                count_decimals(longitude.x) >= digits,
                count_decimals(longitude.x) >= digits)

counts <- table(aux_sub2$ID_ENTITY)
idx <- table(aux_sub2$ID_ENTITY) == 1
aux2 <- aux_sub2 %>%
  dplyr::filter(ID_ENTITY %in% names(counts[idx]))


# ------------------------------------------------------------------------------
# |                                  Sandbox                                   |
# ------------------------------------------------------------------------------
embsecbio_taxa <- readxl::read_xlsx("~/Downloads/SMPDSv2/EMBSeCBIO/EMBSECBIO_clean taxon names.xlsx",
                                    sheet = 1) %>%
  magrittr::set_names(c("taxon_name", "clean_name")) %>%
  dplyr::mutate(action = ifelse(stringr::str_detect(tolower(clean_name),
                                                    "exclude"),
                                "delete", "update"),
                clean_name = ifelse(stringr::str_detect(tolower(clean_name),
                                                        "exclude"),
                                    NA, clean_name),
                clean_name = clean_name %>%
                  stringr::str_squish()) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::filter(!is.na(taxon_name)) %>%
  dplyr::distinct(taxon_name, clean_name, .keep_all = TRUE)

embsecbio_taxa %>%
  readr::write_excel_csv("inst/extdata/embsecbio_taxa.csv", na = "")

embsecbio_samples <- embsecbio::sample %>%
  dplyr::filter(ID_ENTITY %in% embsecbio_metadata$ID_ENTITY) %>%
  dplyr::select(-sample_name, -sample_type)
embsecbio_counts <- embsecbio::pollen_data %>%
  dplyr::filter(ID_SAMPLE %in% embsecbio_samples$ID_SAMPLE)


# ---- Extract data ------------------------------------------------------------
embsecbio_metadata <- embsecbio::age_model %>%
  dplyr::mutate(age_BP = dplyr::coalesce(est_age_bacon_intcal20_mean,
                                         est_age_original,
                                         est_age_provided),
                .after = 1) %>%
  dplyr::filter(age_BP <= 50) %>%
  dplyr::distinct(ID_SAMPLE, .keep_all = TRUE) %>%
  # dplyr::filter(est_age_bacon_intcal20_median <= 50) %>%
  dplyr::left_join(embsecbio::sample, by = "ID_SAMPLE") %>%
  dplyr::left_join(embsecbio::entity, by = "ID_ENTITY") %>%
  dplyr::left_join(embsecbio::site %>%
                     dplyr::select(ID_SITE, site_name, site_type, basin_size),
                   by = "ID_SITE") %>%
  dplyr::select(ID_SITE,
                ID_ENTITY,
                ID_SAMPLE,
                site_name,
                entity_name,
                sample_name,
                latitude,
                longitude,
                elevation,
                site_type,
                entity_type,
                sample_type,
                basin_size,
                source,
                age_BP,
                # age_BP = est_age_bacon_intcal20_median,
                avg_depth) %>%
  dplyr::arrange(ID_SITE, ID_ENTITY, ID_SAMPLE)

embsecbio_metadata %>%
  readr::write_excel_csv("inst/extdata/embsecbio_metadata.csv", na = "")

embsecbio_counts <- embsecbio_metadata %>%
  dplyr::left_join(embsecbio::pollen_data, by = "ID_SAMPLE") %>%
  dplyr::select(-ID_SAMPLE_TAXA) %>%
  dplyr::rename(taxon_name = taxon_clean) %>%
  dplyr::filter(!is.na(taxon_name))

embsecbio_counts %>%
  readr::write_excel_csv("inst/extdata/embsecbio_counts.csv", na = "")

embsecbio_pubs <- embsecbio::entity_pub %>%
  dplyr::filter(ID_ENTITY %in% embsecbio_metadata$ID_ENTITY) %>%
  dplyr::left_join(embsecbio::pub,
                   by = "ID_PUB")

embsecbio_pubs %>%
  readr::write_excel_csv("inst/extdata/embsecbio_pubs.csv", na = "")

embsecbio_metadata <-
  readr::read_csv("~/Downloads/SMPDSv2/EMBSeCBIO/modern_samples_EMBSECBIO_30Aug2021.csv") %>%
  dplyr::rename(age_BP = age) %>%
  dplyr::left_join(embsecbio::entity) %>%
  dplyr::select(-mod_or_0ka_class, -comments) %>%
  dplyr::left_join(embsecbio::site) %>%
  dplyr::select(-catch_size)

embsecbio_counts <- embsecbio_metadata %>%
  dplyr::left_join(embsecbio::sample,
                   by = c("ID_ENTITY", "avg_depth")) %>%
  dplyr::filter(ID_ENTITY %in% embsecbio_metadata$ID_ENTITY) %>%
  dplyr::left_join(embsecbio::pollen_data, by = "ID_SAMPLE") %>%
  dplyr::select(-ID_SAMPLE_TAXA, -sample_name, -sample_type) %>%
  dplyr::rename(taxon_name = taxon_clean) %>%
  dplyr::filter(!is.na(taxon_name))


# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    EMBSeCBIO %>%
                      dplyr::select(source:publication))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    EMBSeCBIO %>%
                      dplyr::select(-c(source:publication)) #%>%
                    # tidyr::unnest(clean)
)
# openxlsx::addWorksheet(wb, "intermediate")
# openxlsx::writeData(wb, "intermediate",
#                     EMPDv2 %>%
#                       dplyr::select(ID_EMPDv2, intermediate) %>%
#                       tidyr::unnest(intermediate))
# openxlsx::addWorksheet(wb, "amalgamated")
# openxlsx::writeData(wb, "amalgamated",
#                     EMPDv2 %>%
#                       dplyr::select(ID_EMPDv2, amalgamated) %>%
#                       tidyr::unnest(amalgamated))
openxlsx::addWorksheet(wb, "taxon_list")
openxlsx::writeData(wb, "taxon_list",
                    "inst/extdata/embsecbio_taxa.csv" %>%
                    readr::read_csv())
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/EMBSeCBIO_",
                              Sys.Date(),
                              ".xlsx"))
