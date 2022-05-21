# Modern data ----
`%>%` <- magrittr::`%>%`
## Load data ----
### Metadata ----
other_southern_hemisphere_metadata <-
  "data-raw/GLOBAL/other_southern_hemisphere_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  janitor::clean_names() %>%
  dplyr::rename(age_BP = age_bp) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name))

### Polen counts ----
other_southern_hemisphere_counts <-
  "data-raw/GLOBAL/other_southern_hemisphere_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2, col_names = FALSE) %>%
  magrittr::set_names(c(
    "entity_name", "taxon_name", "taxon_count"
  ))

### Amalgamations ----
other_southern_hemisphere_taxa_amalgamation <-
  "data-raw/GLOBAL/other_southern_hemisphere_SPH.xlsx" %>%
  readxl::read_excel(sheet = 3) %>%
  magrittr::set_names(c(
    "taxon_name", "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  dplyr::mutate(clean = clean %>% stringr::str_squish(),
                intermediate = intermediate %>% stringr::str_squish(),
                amalgamated = amalgamated %>% stringr::str_squish())

### Combine counts and amalgamation ----
other_southern_hemisphere_taxa_counts_amalgamation <-
  other_southern_hemisphere_counts %>%
  dplyr::left_join(other_southern_hemisphere_taxa_amalgamation,
                   by = c("taxon_name")) %>%
  dplyr::relocate(taxon_count, .after = amalgamated) %>%
  dplyr::left_join(other_southern_hemisphere_metadata %>%
                     dplyr::select(entity_name, ID_SAMPLE),
                   by = "entity_name") %>%
  dplyr::select(-entity_name, -taxon_name) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1)

### Additional taxonomic corrections (SPH - May 20th) ----
taxonomic_corrections <- "data-raw/GLOBAL/taxonomic_corrections.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  purrr::map_df(stringr::str_squish)

other_southern_hemisphere_taxa_counts_amalgamation_rev <-
  other_southern_hemisphere_taxa_counts_amalgamation %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("clean", "all")),
                   by = c("clean" =  "original_taxon")) %>%
  dplyr::mutate(clean = dplyr::coalesce(corrected_taxon_name,
                                        clean)) %>%
  dplyr::select(-corrected_taxon_name, -level) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("intermediate", "all")),
                   by = c("intermediate" =  "original_taxon")) %>%
  dplyr::mutate(intermediate = dplyr::coalesce(corrected_taxon_name,
                                               intermediate)) %>%
  dplyr::select(-corrected_taxon_name, -level) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("amalgamated", "all")),
                   by = c("amalgamated" =  "original_taxon")) %>%
  dplyr::mutate(amalgamated = dplyr::coalesce(corrected_taxon_name,
                                              amalgamated)) %>%
  dplyr::select(-corrected_taxon_name, -level)

waldo::compare(other_southern_hemisphere_taxa_counts_amalgamation,
               other_southern_hemisphere_taxa_counts_amalgamation_rev)
waldo::compare(other_southern_hemisphere_taxa_counts_amalgamation %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               other_southern_hemisphere_taxa_counts_amalgamation_rev %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               max_diffs = Inf)

other_southern_hemisphere_taxa_counts_amalgamation <- other_southern_hemisphere_taxa_counts_amalgamation_rev

other_southern_hemisphere_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated))

## Find DOIs ----
other_southern_hemisphere_metadata_pubs <-
  other_southern_hemisphere_metadata %>%
  dplyr::distinct(publication) %>%
  dplyr::arrange(publication) %>%
  dplyr::mutate(DOI = publication %>%
                  stringr::str_extract_all("\\[DOI\\s*(.*?)\\s*\\](;|$)") %>%
                  purrr::map_chr(~.x %>%
                                   stringr::str_remove_all("^\\[DOI:|\\]$") %>%
                                   stringr::str_squish() %>%
                                   stringr::str_c(collapse = ";\n"))
  ) %>%
  dplyr::mutate(ID_PUB = seq_along(publication))
# other_southern_hemisphere_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/other_southern_hemisphere_modern-references.csv")

### Load cleaned publications list ----
other_southern_hemisphere_clean_publications <-
  "data-raw/GLOBAL/other_southern_hemisphere_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI)
  # dplyr::mutate(ID_PUB = seq_along(publication))

## Append clean publications ----
other_southern_hemisphere_metadata_2 <-
  other_southern_hemisphere_metadata %>%
  dplyr::left_join(other_southern_hemisphere_metadata_pubs %>%
                     dplyr::select(-DOI),
                   by = "publication") %>%
  dplyr::left_join(other_southern_hemisphere_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication)

## Extract PNV/BIOME ----
other_southern_hemisphere_metadata_3 <-
  other_southern_hemisphere_metadata_2 %>%
  smpds::parallel_extract_biome(cpus = 5) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

other_southern_hemisphere_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * 1.1,
                    ylim = range(.$latitude, na.rm = TRUE) * 1.1)

## Create count tables ----
### Clean ----
other_southern_hemisphere_clean <-
  other_southern_hemisphere_taxa_counts_amalgamation %>%
  dplyr::select(-intermediate, -amalgamated) %>%
  dplyr::rename(taxon_name = clean) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     names_sort = TRUE)
### Intermediate ----
other_southern_hemisphere_intermediate <-
  other_southern_hemisphere_taxa_counts_amalgamation %>%
  dplyr::select(-clean, -amalgamated) %>%
  dplyr::rename(taxon_name = intermediate) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     names_sort = TRUE)

### Amalgamated ----
other_southern_hemisphere_amalgamated <-
  other_southern_hemisphere_taxa_counts_amalgamation %>%
  dplyr::select(-clean, -intermediate) %>%
  dplyr::rename(taxon_name = amalgamated) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     names_sort = TRUE)

# Store subsets ----
southern_hemisphere_pollen <-
  other_southern_hemisphere_metadata_3 %>%
  dplyr::mutate(
    clean = other_southern_hemisphere_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = other_southern_hemisphere_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = other_southern_hemisphere_amalgamated %>%
      dplyr::select(-c(ID_SAMPLE))
  ) %>%
  dplyr::mutate(
    basin_size = basin_size %>%
      stringr::str_replace_all("unknown", "not known"),
    entity_type = entity_type %>%
      stringr::str_replace_all("unknown", "not known"),
    site_type = site_type %>%
      stringr::str_replace_all("unknown", "not known")
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = clean) %>%
  dplyr::mutate(source = "Southern Hemisphere pollen", .before = 1) %>%
  dplyr::mutate(age_BP = as.character(age_BP))

usethis::use_data(southern_hemisphere_pollen, overwrite = TRUE, compress = "xz")

## Inspect enumerates ----
### basin_size -----
southern_hemisphere_pollen$basin_size %>%
  unique() %>% sort()

### site_type ----
southern_hemisphere_pollen$site_type %>%
  unique() %>% sort()

### entity_type ----
southern_hemisphere_pollen$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    southern_hemisphere_pollen %>%
                      dplyr::select(site_name:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    southern_hemisphere_pollen %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    southern_hemisphere_pollen %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    southern_hemisphere_pollen %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/southern_hemisphere_pollen_",
                              Sys.Date(),
                              ".xlsx"))

# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/southern_hemisphere_pollen_climate_reconstructions_2022-04-29.csv" %>%
  readr::read_csv()

# Load daily values for precipitation to compute MAP (mean annual precipitation)
climate_reconstructions_pre <-
  "data-raw/reconstructions/southern_hemisphere_pollen_climate_reconstructions_pre_2022-04-29.csv" %>%
  readr::read_csv() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE), .before = T1)

climate_reconstructions_2 <- climate_reconstructions %>%
  dplyr::bind_cols(climate_reconstructions_pre %>%
                     dplyr::select(map))

climate_reconstructions_with_counts <-
  southern_hemisphere_pollen %>%
  # smpds::southern_hemisphere_pollen %>%
  # dplyr::select(-c(mi:mtwa)) %>%
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
waldo::compare(smpds::southern_hemisphere_pollen,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:map, sn, en, new_elevation))
)

southern_hemisphere_pollen <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation)
usethis::use_data(southern_hemisphere_pollen, overwrite = TRUE, compress = "xz")
waldo::compare(smpds::southern_hemisphere_pollen,
               southern_hemisphere_pollen,
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
