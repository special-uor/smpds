# Raw data  ----
`%>%` <- magrittr::`%>%`
neotoma_south_america_pollen <-
  readxl::read_excel("data-raw/neotoma_south_america_pollen_2021-10-22.xlsx",
                     sheet = 1) %>%
  magrittr::set_names(c("site_id",
                        "dataset_id",
                        "chronology_id",
                        "sample_id",
                        "site_name",
                        "dataset_name",
                        "entity_name",
                        "unit_name",
                        "longitude",
                        "latitude",
                        "elevation",
                        "description",
                        "collection_type",
                        "dataset_type",
                        "citation",
                        "depth",
                        "thickness",
                        "age_older",
                        "age",
                        "age_younger",
                        "chronology_name",
                        "age_type",
                        # "Fungi",
                        colnames(.)[-c(1:22)]))

neotoma_south_america_pollen_na_age <-
  neotoma_south_america_pollen %>%
  dplyr::filter(is.na(age))
neotoma_south_america_pollen_na_age %>%
  dplyr::filter(!is.na(age_older) | !is.na(age_younger)) %>%
  dplyr::select(dataset_id, site_name, entity_name, age, age_older, age_younger)

neotoma_south_america_pollen_modern <- neotoma_south_america_pollen %>%
  dplyr::filter(!is.na(age) | !is.na(age_older) | !is.na(age_younger)) %>%
  dplyr::mutate(age_filter = dplyr::coalesce(age, age_older, age_younger),
                # age_filter = 1950 - age_filter,
                before = age) %>%
  dplyr::filter(age_filter <= 50) %>%
  dplyr::select(-age_filter)

neotoma_south_america_pollen_modern_2 <-
  neotoma_south_america_pollen_modern %>%
  smpds::rm_na_taxa(cols = 1:22)

neotoma_south_america_pollen_modern_2_meta <-
  neotoma_south_america_pollen_modern_2 %>%
  dplyr::select(site_id:citation) %>%
  dplyr::distinct() %>%
  dplyr::mutate(DOI = citation %>%
                  stringr::str_extract_all("\\[DOI\\s*(.*?)\\s*\\](;|$)") %>%
                  purrr::map_chr(~.x %>%
                                   stringr::str_remove_all("^\\[DOI:|\\]$") %>%
                                   stringr::str_squish() %>%
                                   stringr::str_c(collapse = ";\n"))
  )

neotoma_south_america_pollen_modern_2_samples <-
  neotoma_south_america_pollen_modern_2 %>%
  dplyr::select(-c(site_id,
                   chronology_id:dataset_name,
                   unit_name:citation))

neotoma_south_america_pollen_modern_2_taxon_list <- tibble::tibble(
  taxon_name = colnames(neotoma_south_america_pollen_modern_2_samples)[-c(1:9)],
  clean_name = taxon_name %>%
    stringr::str_squish()
) %>%
  dplyr::arrange(taxon_name)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "taxon_list")
openxlsx::writeData(wb, "taxon_list",
                    neotoma_south_america_pollen_modern_2_taxon_list)
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata", neotoma_south_america_pollen_modern_2_meta)
openxlsx::addWorksheet(wb, "samples")
openxlsx::writeData(wb, "samples", neotoma_south_america_pollen_modern_2_samples)
openxlsx::saveWorkbook(wb,
                       paste0("inst/extdata/neotoma_south_america_modern_",
                              Sys.Date(),
                              ".xlsx"),
                       overwrite = TRUE)

# Modern data ----
`%>%` <- magrittr::`%>%`
## Load data ----
### Metadata ----
neotoma_south_america_pollen_metadata <-
  readxl::read_excel("data-raw/GLOBAL/neotoma_south_america_pollen_modern_SPH.xlsx",
                     sheet = 1) %>%
  janitor::clean_names() %>%
  dplyr::rename(age_BP = age_bp,
                ID_SAMPLE = sample_id)
### Pollen counts ----
neotoma_south_america_pollen_counts <-
  readxl::read_excel("data-raw/GLOBAL/neotoma_south_america_pollen_modern_SPH.xlsx",
                     sheet = 2) %>%
  magrittr::set_names(
    colnames(.) %>%
      stringr::str_replace_all("sample.id", "sample_id") %>%
      stringr::str_replace_all("site.name", "site_name") %>%
      stringr::str_replace_all("\\.\\.\\.", "#")
  ) %>%
  dplyr::rename(ID_SAMPLE = sample_id) %>%
  dplyr::filter(!is.na(ID_SAMPLE))

# Combined duplicated columns
neotoma_south_america_pollen_counts_2 <-
  neotoma_south_america_pollen_counts %>%
  dplyr::select(ID_SAMPLE) %>%
  dplyr::bind_cols(
    neotoma_south_america_pollen_counts %>%
      dplyr::select(-c(1:3)) %>%
      purrr::map_dfc(~.x %>% as.numeric)
  ) %>%
  tidyr::pivot_longer(-ID_SAMPLE,
                      names_to = "clean",
                      values_to = "taxon_count") %>%
  dplyr::mutate(clean = clean %>%
                  stringr::str_remove_all("\\#[0-9]+$") %>%
                  stringr::str_squish()) %>%
  dplyr::group_by(ID_SAMPLE, clean) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::distinct() %>%
  dplyr::ungroup()
  # tidyr::pivot_wider(ID_SAMPLE)

### Amalgamations ----
neotoma_south_america_pollen_taxa_amalgamation <-
  readxl::read_excel("data-raw/GLOBAL/neotoma_south_america_pollen_modern_SPH.xlsx",
                     sheet = 3) %>%
  janitor::clean_names() %>%
  dplyr::distinct() %>%
  dplyr::mutate(clean = clean %>% stringr::str_squish())

### Combine counts and amalgamation ----
neotoma_south_america_pollen_taxa_counts_amalgamation <-
  neotoma_south_america_pollen_counts_2 %>%
  dplyr::left_join(neotoma_south_america_pollen_taxa_amalgamation,
                   by = c("clean")) %>%
  dplyr::relocate(taxon_count, .after = amalgamated)

### Additional taxonomic corrections (SPH - May 20th) ----
taxonomic_corrections <- "data-raw/GLOBAL/taxonomic_corrections.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  purrr::map_df(stringr::str_squish)

neotoma_south_america_pollen_taxa_counts_amalgamation_rev <-
  neotoma_south_america_pollen_taxa_counts_amalgamation %>%
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

neotoma_south_america_pollen_taxa_counts_amalgamation_rev %>%
  dplyr::group_by(ID_COUNT) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::filter(n > 1)

waldo::compare(neotoma_south_america_pollen_taxa_counts_amalgamation %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               neotoma_south_america_pollen_taxa_counts_amalgamation_rev %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               max_diffs = Inf)

neotoma_south_america_pollen_taxa_counts_amalgamation <-
  neotoma_south_america_pollen_taxa_counts_amalgamation_rev %>%
  dplyr::filter(!is.na(taxon_count), taxon_count > 0) %>%
  dplyr::select(-ID_COUNT)

neotoma_south_america_pollen_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated))

## Find DOIs ----
neotoma_south_america_pollen_metadata_pubs <-
  neotoma_south_america_pollen_metadata %>%
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
# neotoma_south_america_pollen_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/neotoma_south_america_pollen_modern-references.csv")

### Load cleaned publications list ----
neotoma_south_america_pollen_clean_publications <-
  "data-raw/GLOBAL/neotoma_south_america_pollen_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI) %>%
  dplyr::mutate(ID_PUB = seq_along(publication))

## Append clean publications ----
neotoma_south_america_pollen_metadata_2 <-
  neotoma_south_america_pollen_metadata %>%
  dplyr::left_join(neotoma_south_america_pollen_metadata_pubs %>%
                     dplyr::select(-DOI),
                   by = "publication") %>%
  dplyr::left_join(neotoma_south_america_pollen_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication) # %>%
  # dplyr::filter(is.na(updated_publication)) %>%
  # dplyr::filter(publication.x != publication.y) %>%
  # View()

## Extract PNV/BIOME ----
neotoma_south_america_pollen_metadata_3 <-
  neotoma_south_america_pollen_metadata_2 %>%
  smpds::parallel_extract_biome(cpus = 12) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

neotoma_south_america_pollen_metadata_3 %>%
  smpds::plot_biome()

## Create count tables ----
### Clean ----
neotoma_south_america_pollen_clean <-
  neotoma_south_america_pollen_taxa_counts_amalgamation %>%
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
                     names_sort = TRUE)
### Intermediate ----
neotoma_south_america_pollen_intermediate <-
  neotoma_south_america_pollen_taxa_counts_amalgamation %>%
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
                     names_sort = TRUE)

### Amalgamated ----
neotoma_south_america_pollen_amalgamated <-
  neotoma_south_america_pollen_taxa_counts_amalgamation %>%
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
                     names_sort = TRUE)

# Store subsets ----
south_america_pollen <-
  neotoma_south_america_pollen_metadata_3 %>%
  dplyr::select(-site_id, -dataset_id) %>%
  dplyr::relocate(ID_SAMPLE, .after = ID_BIOME) %>%
  dplyr::filter(ID_SAMPLE %in% neotoma_south_america_pollen_clean$ID_SAMPLE) %>%
  dplyr::mutate(
    clean = neotoma_south_america_pollen_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = neotoma_south_america_pollen_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = neotoma_south_america_pollen_amalgamated %>%
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
      stringr::str_replace_all("unknown", "not known"),
    entity_type = entity_type %>%
      stringr::str_replace_all("Core", "core") %>%
      stringr::str_replace_all("Section", "section"),
    site_type = site_type %>%
      stringr::str_replace_all("leka", "lake") %>%
      stringr::str_replace_all("unknown", "not known")
  ) %>%
  dplyr::mutate(source = "Neotoma", .before = 1) %>%
  dplyr::mutate(age_BP = as.character(age_BP)) %>%
  dplyr::select(-basin_size_num)

usethis::use_data(south_america_pollen, overwrite = TRUE, compress = "xz")

## Inspect enumerates ----
### basin_size -----
south_america_pollen$basin_size %>%
  unique() %>% sort()

### site_type ----
south_america_pollen$site_type %>%
  unique() %>% sort()

### entity_type ----
south_america_pollen$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    south_america_pollen %>%
                      dplyr::select(site_name:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    south_america_pollen %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    south_america_pollen %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    south_america_pollen %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/south_america_pollen_",
                              Sys.Date(),
                              ".xlsx"))


# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/south_america_pollen_climate_reconstructions_2022-04-29.csv" %>%
  readr::read_csv()

# Load daily values for precipitation to compute MAP (mean annual precipitation)
climate_reconstructions_pre <-
  "data-raw/reconstructions/south_america_pollen_climate_reconstructions_pre_2022-04-29.csv" %>%
  readr::read_csv() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE), .before = T1)

climate_reconstructions_2 <- climate_reconstructions %>%
  dplyr::bind_cols(climate_reconstructions_pre %>%
                     dplyr::select(map)) %>%
  dplyr::bind_cols(neotoma_south_america_pollen_metadata_3 %>%
                     dplyr::select(sn = site_name,
                                   en = entity_name,
                                   ID_SAMPLE))
climate_reconstructions_2 %>%
  dplyr::filter(site_name != sn | entity_name != en)

climate_reconstructions_with_counts <-
  south_america_pollen %>%
  # smpds::south_america_pollen %>%
  # dplyr::select(-c(mi:map)) %>%
  dplyr::bind_cols(
    climate_reconstructions_2 %>%
      dplyr::filter(ID_SAMPLE %in% south_america_pollen$ID_SAMPLE) %>%
      dplyr::select(-sn, -en, -ID_SAMPLE) %>%
      dplyr::select(sn = site_name,
                    en = entity_name,
                    new_elevation = elevation,
                    mi:map)
  ) %>%
  dplyr::relocate(mi:map, .before = clean) %>%
  dplyr::mutate(elevation = dplyr::coalesce(elevation, new_elevation))
climate_reconstructions_with_counts %>%
  dplyr::filter(site_name != sn | entity_name != en)
waldo::compare(smpds::south_america_pollen,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:map, sn, en, new_elevation))
)

south_america_pollen <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation)
usethis::use_data(south_america_pollen, overwrite = TRUE, compress = "xz")
waldo::compare(smpds::south_america_pollen,
               south_america_pollen,
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
