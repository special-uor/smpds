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


# Extract data ------------------------------------------------------------
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

# SPH revisions ----
"The entities and their counts were manually inspected by SPH"

`%>%` <- magrittr::`%>%`
## Load data ----
EMBSeCBIO_all <-
  "data-raw/GLOBAL/D_embsecbio_records_additions_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  dplyr::rename(basin_size = `basin_size (km)`) %>%
  dplyr::mutate(
    publication = "Harrison, S.P., Marinova, E. and Cruz-Silva, E., 2021. EMBSeCBIO pollen database. University of Reading. Dataset.",
    doi = "10.17864/1947.309",
    .after = Publication,
  ) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .after = doi) %>%
  dplyr::select(-Publication)

### Metadata ----
EMBSeCBIO_metadata <-
  EMBSeCBIO_all %>%
  dplyr::select(source:ID_SAMPLE)

### Polen counts ----
EMBSeCBIO_counts <-
  EMBSeCBIO_all %>%
  dplyr::select(ID_SAMPLE) %>%
  dplyr::bind_cols(
    EMBSeCBIO_all %>% # Convert columns with counts to numeric type
      dplyr::select(-c(source:ID_SAMPLE)) %>%
      purrr::map_dfc(~.x %>% as.numeric)
  ) %>%
  magrittr::set_names(
    colnames(.) %>%
      stringr::str_replace_all("\\.\\.\\.", "#")
  ) %>%
  tidyr::pivot_longer(-ID_SAMPLE,
                      names_to = "taxon_name",
                      values_to = "taxon_count") %>%
  dplyr::mutate(taxon_name = taxon_name %>%
                  stringr::str_remove_all("\\#[0-9]+$") %>%
                  stringr::str_squish()) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::distinct() %>%
  dplyr::ungroup()

### Amalgamations ----
EMBSeCBIO_taxa_amalgamation <-
  "data-raw/GLOBAL/D_embsecbio_records_additions_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  purrr::map_df(stringr::str_squish)

### Combine counts and amalgamation ----
EMBSeCBIO_taxa_counts_amalgamation <-
  EMBSeCBIO_counts %>%
  dplyr::left_join(EMBSeCBIO_taxa_amalgamation,
                   by = c("taxon_name" = "clean")) %>%
  dplyr::relocate(taxon_count, .after = amalgamated) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1) %>%
  dplyr::rename(clean = taxon_name)
# dplyr::select(-taxon_name)

EMBSeCBIO_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated)) %>%
  dplyr::distinct(clean, intermediate, amalgamated)

## Find DOIs ----
EMBSeCBIO_metadata_pubs <-
  EMBSeCBIO_metadata %>%
  dplyr::distinct(publication, doi) %>%
  dplyr::arrange(publication) %>%
  dplyr::mutate(DOI = publication %>%
                  stringr::str_extract_all("\\[DOI\\s*(.*?)\\s*\\](;|$)") %>%
                  purrr::map_chr(~.x %>%
                                   stringr::str_remove_all("^\\[DOI:") %>%
                                   stringr::str_remove_all("\\]\\s*;\\s*$") %>%
                                   stringr::str_remove_all("\\]$") %>%
                                   stringr::str_remove_all("doi:") %>%
                                   stringr::str_squish() %>%
                                   stringr::str_c(collapse = ";\n"))
  ) %>%
  dplyr::mutate(ID_PUB = seq_along(publication)) %>%
  dplyr::mutate(updated_publication = NA, .before = publication) %>%
  dplyr::mutate(updated_DOI = NA, .before = DOI)
# EMBSeCBIO_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/EMBSeCBIO_modern-references.csv")

### Load cleaned publications list ----
EMBSeCBIO_clean_publications <-
  "data-raw/GLOBAL/EMBSeCBIO_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI)

## Append clean publications ----
EMBSeCBIO_metadata_2 <-
  EMBSeCBIO_metadata %>%
  dplyr::left_join(EMBSeCBIO_metadata_pubs %>%
                     dplyr::select(-DOI, -doi, -dplyr::contains("updated")),
                   by = "publication") %>%
  dplyr::left_join(EMBSeCBIO_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi, -ID_PUB) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication)

## Extract PNV/BIOME ----
EMBSeCBIO_metadata_3 <-
  EMBSeCBIO_metadata_2 %>%
  dplyr::select(-dplyr::starts_with("ID_BIOME")) %>%
  smpds::parallel_extract_biome(cpus = 10) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

EMBSeCBIO_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * c(0.9, 1.1),
                    ylim = range(.$latitude, na.rm = TRUE) * c(0.9, 1.1))

## Create count tables ----
### Clean ----
EMBSeCBIO_clean <-
  EMBSeCBIO_taxa_counts_amalgamation %>%
  dplyr::select(-intermediate, -amalgamated) %>%
  dplyr::rename(taxon_name = clean) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     names_sort = TRUE) %>%
  dplyr::arrange(ID_SAMPLE)

### Intermediate ----
EMBSeCBIO_intermediate <-
  EMBSeCBIO_taxa_counts_amalgamation %>%
  dplyr::select(-clean, -amalgamated) %>%
  dplyr::rename(taxon_name = intermediate) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     names_sort = TRUE) %>%
  dplyr::arrange(ID_SAMPLE)

### Amalgamated ----
EMBSeCBIO_amalgamated <-
  EMBSeCBIO_taxa_counts_amalgamation %>%
  dplyr::select(-clean, -intermediate) %>%
  dplyr::rename(taxon_name = amalgamated) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     names_sort = TRUE) %>%
  dplyr::arrange(ID_SAMPLE)

# Extract missing elevations ----
# EMBSeCBIO_metadata_4 <-
#   EMBSeCBIO_metadata_3 %>%
#   dplyr::rename(elevation_original = elevation) %>%
#   smpds:::get_elevation(cpus = 12)
#
# EMBSeCBIO_metadata_4 %>%
#   dplyr::select(ID_SAMPLE, entity_name, latitude, longitude, elevation_new = elevation, elevation_original) %>%
#   dplyr::mutate(diff =
#                   abs(elevation_new - elevation_original) / elevation_original) %>%
#   # readr::write_csv("data-raw/GLOBAL/EMBSeCBIO_elevations_only.csv", na = "")
#   dplyr::filter(diff >= 0.5)
# dplyr::mutate(within_90p =
#                 dplyr::between(elevation,
#                                min(c(0.9, 1.1) * elevation_original),
#                                max(c(0.9, 1.1) * elevation_original)),
#               within_95p =
#                 dplyr::between(elevation,
#                                min(c(0.95, 1.05) * elevation_original),
#                                max(c(0.95, 1.05) * elevation_original)),
#               within_975p =
#                 dplyr::between(elevation,
#                                min(c(0.975, 1.025) * elevation_original),
#                                max(c(0.975, 1.025) * elevation_original))
# ) %>%
#   dplyr::filter(!within_975p)

# Store subsets ----
EMBSeCBIO <-
  EMBSeCBIO_metadata_3 %>%
  dplyr::mutate(
    clean = EMBSeCBIO_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = EMBSeCBIO_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = EMBSeCBIO_amalgamated %>%
      dplyr::select(-c(ID_SAMPLE))
  ) %>%
  dplyr::mutate(
    basin_size_old = basin_size,
    basin_size_num = basin_size %>%
      as.numeric() %>%
      round(digits = 6) %>%
      as.character(),
    basin_size = dplyr::coalesce(
      basin_size_num,
      basin_size
    ),
    basin_size = basin_size %>%
      stringr::str_to_lower() %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("unknown|Unknown", "not known"),
    entity_type = entity_type %>%
      stringr::str_to_lower() %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("unknown|Unknown", "not known") %>%
      stringr::str_to_lower(),
    site_type = site_type %>%
      stringr::str_to_lower() %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("estuarine", "coastal, estuarine") %>%
      stringr::str_replace_all("drained/dry lake", "lacustrine, drained lake") %>%
      stringr::str_replace_all("terrestrial, other sediments", "terrestrial") %>%
      stringr::str_replace_all("terrestrial, soil", "soil") %>%
      stringr::str_replace_all("unknown", "not known")
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = clean) %>%
  # dplyr::relocate(basin_size_old, .after = basin_size) %>%
  dplyr::select(-basin_size_num, -basin_size_old)

usethis::use_data(EMBSeCBIO, overwrite = TRUE, compress = "xz")

# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/EMBSeCBIO_climate_reconstructions_2022-04-30.csv" %>%
  readr::read_csv()

climate_reconstructions_with_counts <- smpds::EMBSeCBIO %>%
  dplyr::bind_cols(
    climate_reconstructions %>%
      dplyr::select(sn = site_name,
                    en = entity_name,
                    new_elevation = elevation,
                    mi:mtwa)
  ) %>%
  dplyr::relocate(mi:mtwa, .before = clean) %>%
  dplyr::mutate(elevation = dplyr::coalesce(elevation, new_elevation))
climate_reconstructions_with_counts %>%
  dplyr::filter(site_name != sn | entity_name != en)
waldo::compare(smpds::EMBSeCBIO,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:mtwa))
)
EMBSeCBIO <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation)
usethis::use_data(EMBSeCBIO, overwrite = TRUE, compress = "xz")

climate_reconstructions %>%
  smpds::plot_climate_countour(
    var = "mat",
    xlim = range(.$longitude, na.rm = TRUE),
    ylim = range(.$latitude, na.rm = TRUE)
  )

# Inspect enumerates ----
### basin_size -----
EMBSeCBIO$basin_size %>%
  unique() %>%
  sort()

### site_type ----
EMBSeCBIO$site_type %>%
  unique() %>% sort()

### entity_type ----
EMBSeCBIO$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    EMBSeCBIO %>%
                      dplyr::select(source:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    EMBSeCBIO %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    EMBSeCBIO %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    EMBSeCBIO %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/EMBSeCBIO_",
                              Sys.Date(),
                              ".xlsx"))
