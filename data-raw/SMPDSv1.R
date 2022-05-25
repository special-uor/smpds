## code to prepare `SMPDSv1` dataset goes here
# The SPECIAL Modern Pollen Dataset
# Source:
# Harrison, Sandy (2019): Modern pollen data for climate reconstructions,
# version 1 (SMPDS). University of Reading. Dataset.
# http://dx.doi.org/10.17864/1947.194
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
  smpds::sort_taxa(1:11) %>% # Sort the taxon_names alphabetically
  dplyr::mutate(ID_BIOME = tibble::tibble(latitude, longitude) %>%
                  smpds::parallel_extract_biome(buffer = 12000, cpus = 6) %>%
                  .$ID_BIOME,
                publication =
                  ifelse(is.na(publication),
                         paste("Harrison, Sandy P., 2019. Modern pollen data",
                               "for climate reconstructions, version 1 (SMPDS).",
                               "University of Reading. Dataset."),
                         publication),
                DOI =
                  ifelse(publication %>%
                           stringr::str_starts("Harrison, Sandy P., 2019. Modern pollen data"),
                         "10.17864/1947.194",
                         DOI),
                .after = age_BP) %>%
  progressr::with_progress()
SMPDSv12 <- SMPDSv1 %>%
  dplyr::mutate(ID_BIOME = ifelse(entity_name %>%
                                    stringr::str_detect("Barboni") &
                                    is.na(ID_BIOME),
                                  -999999,
                                  ID_BIOME),
                ID_BIOME = ifelse(site_name %>%
                                    stringr::str_detect("Onego|Azov|Onegskoe") &
                                    is.na(ID_BIOME),
                                  -888888,
                                  ID_BIOME),
                ID_BIOME = ifelse(entity_type %>%
                                    stringr::str_detect("marine") &
                                    is.na(ID_BIOME),
                                  -888888,
                                  ID_BIOME),
                site_type = ifelse(entity_name %>%
                                     stringr::str_detect("Caspian SE") &
                                     is.na(ID_BIOME),
                                   "marine",
                                   site_type),
                ID_BIOME = ifelse(site_type %>%
                                    stringr::str_detect("marine") &
                                    is.na(ID_BIOME),
                                  -888888,
                                  ID_BIOME))
SMPDSv1 %>%
  dplyr::filter(is.na(ID_BIOME)) %>%
  dplyr::select(1:13)

SMPDSv12 %>%
  dplyr::filter(is.na(ID_BIOME)) %>%
  dplyr::select(1:13)

# SMPDSv1 %>%
#   dplyr::filter(is.na(ID_BIOME)) %>%
#   dplyr::select(1:13) %>%
#   dplyr::rename(biome = ID_BIOME) %>%
#   readr::write_excel_csv("~/Downloads/SMPDSv1_records_without_biome.csv", na = "")

SMPDSv1 <- SMPDSv12
usethis::use_data(SMPDSv1, overwrite = TRUE, compress = "xz")

# ------------------------------------------------------------------------------
# |                         Find matches in the EMPDv2                         |
# ------------------------------------------------------------------------------
compare_latlon(EMPDv2, SMPDSv1, digits = 2)
aux <- EMPDv2 %>%
  dplyr::filter(entity_name %in% SMPDSv1$entity_name) %>%
  smpds::rm_zero_taxa(1:14) %>%
  smpds::total_taxa(1:14)
aux_rev <- SMPDSv1 %>%
  dplyr::filter(entity_name %in% EMPDv2$entity_name) %>%
  smpds::rm_zero_taxa(1:13) %>%
  smpds::total_taxa(1:13)

waldo::compare(
aux[1, ] %>%
  smpds::rm_zero_taxa(1:15),
aux_rev[1, ] %>%
  smpds::rm_zero_taxa(1:14))

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


# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    SMPDSv1 %>%
                      dplyr::select(ID_SMPDSv1:DOI))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    SMPDSv1 %>%
                      dplyr::select(-c(source:DOI)) #%>%
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
                    "inst/extdata/all_taxa.csv" %>%
                      readr::read_csv())
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/SMPDSv1_",
                              Sys.Date(),
                              ".xlsx"))

# SPH revisions ----
"The entities and their counts were manually inspected by SPH"

`%>%` <- magrittr::`%>%`
## Load data ----
SMPDSv1_all <-
  "data-raw/GLOBAL/A_SMPDSv1_Iberia and EMBSECBIO cleanup done.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  dplyr::rename(doi = DOI) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .after = doi) %>%
  dplyr::select(-Publication)

### Metadata ----
SMPDSv1_metadata <-
  SMPDSv1_all %>%
  dplyr::select(source:ID_SAMPLE)

### Pollen counts ----
SMPDSv1_counts <-
  SMPDSv1_all %>%
  dplyr::select(ID_SAMPLE) %>%
  dplyr::bind_cols(
    SMPDSv1_all %>% # Convert columns with counts to numeric type
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
SMPDSv1_taxa_amalgamation <-
  "data-raw/GLOBAL/A_SMPDSv1_Iberia and EMBSECBIO cleanup done.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  purrr::map_df(stringr::str_squish)

### Combine counts and amalgamation ----
SMPDSv1_taxa_counts_amalgamation <-
  SMPDSv1_counts %>%
  dplyr::left_join(SMPDSv1_taxa_amalgamation,
                   by = c("taxon_name" = "clean")) %>%
  dplyr::relocate(taxon_count, .after = amalgamated) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1) %>%
  dplyr::rename(clean = taxon_name)
  # dplyr::select(-taxon_name)

### Additional taxonomic corrections (SPH - May 20th) ----
taxonomic_corrections <- "data-raw/GLOBAL/taxonomic_corrections.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  purrr::map_df(stringr::str_squish)

SMPDSv1_taxa_counts_amalgamation_rev <-
  SMPDSv1_taxa_counts_amalgamation %>%
  dplyr::mutate(ID_COUNT = seq_along(ID_SAMPLE)) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("clean",  "all")),
                   by = c("clean" =  "original_taxon")) %>%
  dplyr::mutate(clean = dplyr::coalesce(corrected_taxon_name,
                                        clean)) %>%
  dplyr::select(-corrected_taxon_name, -level) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("intermediate", "all")),
                   by = c("clean" =  "original_taxon")) %>%
  dplyr::mutate(intermediate = dplyr::coalesce(corrected_taxon_name,
                                               intermediate)) %>%
  dplyr::select(-corrected_taxon_name, -level) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("amalgamated", "all")),
                   by = c("clean" =  "original_taxon")) %>%
  dplyr::mutate(amalgamated = dplyr::coalesce(corrected_taxon_name,
                                              amalgamated)) %>%
  dplyr::select(-corrected_taxon_name, -level) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("all")),
                   by = c("clean" =  "original_taxon")) %>%
  dplyr::mutate(clean = dplyr::coalesce(corrected_taxon_name,
                                        clean)) %>%
  dplyr::select(-corrected_taxon_name, -level) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("all")),
                   by = c("intermediate" =  "original_taxon")) %>%
  dplyr::mutate(intermediate = dplyr::coalesce(corrected_taxon_name,
                                               intermediate)) %>%
  dplyr::select(-corrected_taxon_name, -level) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("all")),
                   by = c("amalgamated" =  "original_taxon")) %>%
  dplyr::mutate(amalgamated = dplyr::coalesce(corrected_taxon_name,
                                              amalgamated)) %>%
  dplyr::select(-corrected_taxon_name, -level)

SMPDSv1_taxa_counts_amalgamation_rev %>%
  dplyr::group_by(ID_COUNT) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::filter(n > 1)

waldo::compare(SMPDSv1_taxa_counts_amalgamation %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               SMPDSv1_taxa_counts_amalgamation_rev %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               max_diffs = Inf)

SMPDSv1_taxa_counts_amalgamation <- SMPDSv1_taxa_counts_amalgamation_rev %>%
  dplyr::filter(!is.na(taxon_count), taxon_count > 0) %>%
  dplyr::select(-ID_COUNT)

SMPDSv1_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated)) %>%
  dplyr::distinct(clean, intermediate, amalgamated)

## Find DOIs ----
SMPDSv1_metadata_pubs <-
  SMPDSv1_metadata %>%
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
# SMPDSv1_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/SMPDSv1_modern-references.csv")

### Load cleaned publications list ----
SMPDSv1_clean_publications <-
  "data-raw/GLOBAL/SMPDSv1_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI)

## Append clean publications ----
SMPDSv1_metadata_2 <-
  SMPDSv1_metadata %>%
  dplyr::left_join(SMPDSv1_metadata_pubs %>%
                     dplyr::select(-DOI, -doi, -dplyr::contains("updated")),
                   by = "publication") %>%
  dplyr::left_join(SMPDSv1_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi, -ID_PUB) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication)

## Extract PNV/BIOME ----
SMPDSv1_metadata_3 <-
  SMPDSv1_metadata_2 %>%
  dplyr::select(-dplyr::starts_with("ID_BIOME")) %>%
  smpds::parallel_extract_biome(cpus = 10) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

SMPDSv1_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * c(0.9, 1.1),
                    ylim = range(.$latitude, na.rm = TRUE) * c(0.9, 1.1))

## Create count tables ----
### Clean ----
SMPDSv1_clean <-
  SMPDSv1_taxa_counts_amalgamation %>%
  dplyr::select(-intermediate, -amalgamated) %>%
  dplyr::rename(taxon_name = clean) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     values_fill = 0,
                     names_sort = TRUE) %>%
  dplyr::arrange(ID_SAMPLE)

### Intermediate ----
SMPDSv1_intermediate <-
  SMPDSv1_taxa_counts_amalgamation %>%
  dplyr::select(-clean, -amalgamated) %>%
  dplyr::rename(taxon_name = intermediate) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     values_fill = 0,
                     names_sort = TRUE) %>%
  dplyr::arrange(ID_SAMPLE)

### Amalgamated ----
SMPDSv1_amalgamated <-
  SMPDSv1_taxa_counts_amalgamation %>%
  dplyr::select(-clean, -intermediate) %>%
  dplyr::rename(taxon_name = amalgamated) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     values_fill = 0,
                     names_sort = TRUE) %>%
  dplyr::arrange(ID_SAMPLE)

# Extract missing elevations ----
# SMPDSv1_metadata_4 <-
#   SMPDSv1_metadata_3 %>%
#   dplyr::rename(elevation_original = elevation) %>%
#   smpds:::get_elevation(cpus = 12)
#
# SMPDSv1_metadata_4 %>%
#   dplyr::select(ID_SAMPLE, entity_name, latitude, longitude, elevation_new = elevation, elevation_original) %>%
#   dplyr::mutate(diff =
#                   abs(elevation_new - elevation_original) / elevation_original) %>%
#   # readr::write_csv("data-raw/GLOBAL/SMPDSv1_elevations_only.csv", na = "")
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
SMPDSv1 <-
  SMPDSv1_metadata_3 %>%
  dplyr::mutate(
    clean = SMPDSv1_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = SMPDSv1_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = SMPDSv1_amalgamated %>%
      dplyr::select(-c(ID_SAMPLE))
  ) %>%
  dplyr::mutate(
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
  dplyr::select(-basin_size_num)

usethis::use_data(SMPDSv1, overwrite = TRUE, compress = "xz")


# Inspect enumerates ----
### basin_size -----
SMPDSv1$basin_size %>%
  # stringr::str_replace_all("not applicable", "-888888") %>%
  # stringr::str_replace_all("not known", "-999999") %>%
  # as.numeric() %>%
  # round(digits = 6) %>%
  unique() %>%
  sort()

### site_type ----
SMPDSv1$site_type %>%
  unique() %>% sort()

### entity_type ----
SMPDSv1$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    SMPDSv1 %>%
                      dplyr::select(source:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    SMPDSv1 %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    SMPDSv1 %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    SMPDSv1 %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/SMPDSv1_",
                              Sys.Date(),
                              ".xlsx"))


# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/SMPDSv1_climate_reconstructions_2022-05-12.csv" %>%
  readr::read_csv()

# Load daily values for precipitation to compute MAP (mean annual precipitation)
climate_reconstructions_pre <-
  "data-raw/reconstructions/SMPDSv1_climate_reconstructions_pre_2022-05-12.csv" %>%
  readr::read_csv() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE), .before = T1)

climate_reconstructions_2 <- climate_reconstructions %>%
  dplyr::bind_cols(climate_reconstructions_pre %>%
                     dplyr::select(map))

climate_reconstructions_with_counts <-
  SMPDSv1 %>%
  # smpds::SMPDSv1 %>%
  # dplyr::select(-c(mi:map)) %>%
  dplyr::bind_cols(
    climate_reconstructions_2 %>%
      dplyr::select(sn = site_name,
                    en = entity_name,
                    new_elevation = elevation,
                    mi:map)
  ) %>%
  dplyr::relocate(mi:map, .before = clean) %>%
  dplyr::mutate(elevation = dplyr::coalesce(elevation, new_elevation))
climate_reconstructions_with_counts %>%
  dplyr::filter(site_name != sn | entity_name != en)
waldo::compare(smpds::SMPDSv1,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:map, sn, en, new_elevation))
)

SMPDSv1 <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation)
usethis::use_data(SMPDSv1, overwrite = TRUE, compress = "xz")
waldo::compare(smpds::SMPDSv1,
               SMPDSv1,
               max_diffs = Inf)

climate_reconstructions_2 %>%
  smpds::plot_climate_countour(
    var = "mat",
    xlim = range(.$longitude, na.rm = TRUE),
    ylim = range(.$latitude, na.rm = TRUE)
  )

climate_reconstructions_2 %>%
  smpds::plot_climate(
    var = "map",
    xlim = range(.$longitude, na.rm = TRUE),
    ylim = range(.$latitude, na.rm = TRUE)
  )

rm(climate_reconstructions,
   climate_reconstructions_2,
   climate_reconstructions_pre,
   climate_reconstructions_with_counts)
