`%>%` <- magrittr::`%>%`
# Load data ----
## Metadata ----
additional_european_pollen_metadata <-
  "data-raw/GLOBAL/E_additional_Europe_clean.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  dplyr::relocate(source, .before = 1) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .after = age_BP)

## Pollen counts ----
additional_european_pollen_taxa_counts_amalgamation <-
  "data-raw/GLOBAL/E_additional_Europe_clean.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(
    c("entity_name", "clean", "intermediate", "amalgamated", "taxon_count")
  ) %>%
  dplyr::left_join(
    additional_european_pollen_metadata %>%
      dplyr::select(entity_name, ID_SAMPLE)
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1) %>%
  dplyr::select(-entity_name)

### Additional taxonomic corrections (SPH - May 20th) ----
taxonomic_corrections <- "data-raw/GLOBAL/taxonomic_corrections.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  purrr::map_df(stringr::str_squish)

additional_european_pollen_taxa_counts_amalgamation_rev <-
  additional_european_pollen_taxa_counts_amalgamation %>%
  dplyr::mutate(ID_COUNT = seq_along(ID_SAMPLE)) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("clean", "all")),
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

additional_european_pollen_taxa_counts_amalgamation_rev %>%
  dplyr::group_by(ID_COUNT) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::filter(n > 1)

waldo::compare(additional_european_pollen_taxa_counts_amalgamation,
               additional_european_pollen_taxa_counts_amalgamation_rev)
waldo::compare(additional_european_pollen_taxa_counts_amalgamation %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               additional_european_pollen_taxa_counts_amalgamation_rev %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               max_diffs = Inf)

additional_european_pollen_taxa_counts_amalgamation <-
  additional_european_pollen_taxa_counts_amalgamation_rev %>%
  dplyr::filter(!is.na(taxon_count), taxon_count > 0) %>%
  dplyr::select(-ID_COUNT)

# Extract PNV/BIOME ----
additional_european_pollen_metadata_3 <-
  # additional_european_pollen_metadata_2 %>%
  additional_european_pollen_metadata %>%
  dplyr::select(-dplyr::starts_with("ID_BIOME")) %>%
  smpds::parallel_extract_biome(cpus = 1) %>%
  # smpds::biome_name() %>%
  # dplyr::relocate(ID_BIOME, .after = doi) %>%
  dplyr::relocate(ID_BIOME, .after = ID_SAMPLE) %>%
  smpds::pb()

additional_european_pollen_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * c(0.9, 1.1),
                    ylim = range(.$latitude, na.rm = TRUE) * c(0.9, 1.1))

# Create count tables ----
## Clean ----
additional_european_pollen_clean <-
  additional_european_pollen_taxa_counts_amalgamation %>%
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

## Intermediate ----
additional_european_pollen_intermediate <-
  additional_european_pollen_taxa_counts_amalgamation %>%
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

## Amalgamated ----
additional_european_pollen_amalgamated <-
  additional_european_pollen_taxa_counts_amalgamation %>%
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


# Store subsets ----
additional_european_pollen <-
  additional_european_pollen_metadata_3 %>%
  dplyr::mutate(
    clean = additional_european_pollen_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = additional_european_pollen_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = additional_european_pollen_amalgamated %>%
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
  dplyr::select(-basin_size_num)

usethis::use_data(additional_european_pollen, overwrite = TRUE, compress = "xz")

# Inspect enumerates ----
## basin_size -----
additional_european_pollen$basin_size %>%
  unique() %>% sort()

## site_type ----
additional_european_pollen$site_type %>%
  unique() %>% sort()

## entity_type ----
additional_european_pollen$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    additional_european_pollen %>%
                      dplyr::select(source:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    additional_european_pollen %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    additional_european_pollen %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    additional_european_pollen %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/additional_european_pollen_",
                              Sys.Date(),
                              ".xlsx"))

# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/additional_european_pollen_climate_reconstructions_2022-05-12.csv" %>%
  readr::read_csv()

# Load daily values for precipitation to compute MAP (mean annual precipitation)
climate_reconstructions_pre <-
  "data-raw/reconstructions/additional_european_pollen_climate_reconstructions_pre_2022-05-12.csv" %>%
  readr::read_csv() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE), .before = T1)

climate_reconstructions_2 <- climate_reconstructions %>%
  dplyr::bind_cols(climate_reconstructions_pre %>%
                     dplyr::select(map))

climate_reconstructions_with_counts <-
  additional_european_pollen %>%
  # smpds::additional_european_pollen %>%
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
waldo::compare(smpds::additional_european_pollen,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:map, sn, en, new_elevation))
)

additional_european_pollen <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation)
usethis::use_data(additional_european_pollen, overwrite = TRUE, compress = "xz")
waldo::compare(smpds::additional_european_pollen,
               additional_european_pollen,
               max_diffs = Inf)

climate_reconstructions %>%
  smpds::plot_climate_countour(
    var = "mat",
    xlim = range(.$longitude, na.rm = TRUE),
    ylim = range(.$latitude, na.rm = TRUE)
  )

rm(climate_reconstructions,
   climate_reconstructions_2,
   climate_reconstructions_pre,
   climate_reconstructions_with_counts)
