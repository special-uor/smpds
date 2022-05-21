# Modern data ----
`%>%` <- magrittr::`%>%`
## Load data ----
### Metadata ----
african_modern_samples_metadata <-
  "data-raw/GLOBAL/African modern surface samples_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  janitor::clean_names() %>%
  dplyr::rename(age_BP = age_bp) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .after = doi)

### Pollen counts ----
african_modern_samples_counts <-
  "data-raw/GLOBAL/African modern surface samples_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  janitor::clean_names() %>%
  dplyr::left_join(african_modern_samples_metadata %>%
                     dplyr::select(entity_name, ID_SAMPLE),
                   by = "entity_name") %>%
  dplyr::relocate(ID_SAMPLE, .before = 1)

### Amalgamations ----
african_modern_samples_taxa_amalgamation <-
  african_modern_samples_counts %>%
  dplyr::select(-entity_name, -original_taxon_name) %>%
  dplyr::rename(taxon_count = counts) %>%
  dplyr::distinct() %>%
  dplyr::mutate(clean = clean %>% stringr::str_squish(),
                intermediate = intermediate %>% stringr::str_squish(),
                amalgamated = amalgamated %>% stringr::str_squish())

### Additional taxonomic corrections (SPH - May 20th) ----
taxonomic_corrections <- "data-raw/GLOBAL/taxonomic_corrections.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  purrr::map_df(stringr::str_squish)

african_modern_samples_taxa_amalgamation_rev <-
  african_modern_samples_taxa_amalgamation %>%
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

waldo::compare(african_modern_samples_taxa_amalgamation,
               african_modern_samples_taxa_amalgamation_rev)

african_modern_samples_taxa_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated)) %>%
  dplyr::distinct(clean, intermediate, amalgamated)

## Find DOIs ----
african_modern_samples_metadata_pubs <-
  african_modern_samples_metadata %>%
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
# african_modern_samples_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/african_modern_samples_modern-references.csv")

### Load cleaned publications list ----
african_modern_samples_clean_publications <-
  "data-raw/GLOBAL/african_modern_samples_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI)

## Append clean publications ----
african_modern_samples_metadata_2 <-
  african_modern_samples_metadata %>%
  dplyr::left_join(african_modern_samples_metadata_pubs %>%
                     dplyr::select(-DOI, -doi, -dplyr::contains("updated")),
                   by = "publication") %>%
  dplyr::left_join(african_modern_samples_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi, -ID_PUB) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication)

## Extract PNV/BIOME ----
african_modern_samples_metadata_3 <-
  african_modern_samples_metadata_2 %>%
  smpds::parallel_extract_biome(cpus = 8) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

african_modern_samples_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * c(0.9, 1.1),
                    ylim = range(.$latitude, na.rm = TRUE) * c(0.9, 1.1))

## Create count tables ----
### Clean ----
african_modern_samples_clean <-
  african_modern_samples_taxa_amalgamation %>%
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
african_modern_samples_intermediate <-
  african_modern_samples_taxa_amalgamation %>%
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
african_modern_samples_amalgamated <-
  african_modern_samples_taxa_amalgamation %>%
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
AMSS <-
  african_modern_samples_metadata_3 %>%
  dplyr::mutate(
    clean = african_modern_samples_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = african_modern_samples_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = african_modern_samples_amalgamated %>%
      dplyr::select(-c(ID_SAMPLE))
  ) %>%
  dplyr::mutate(
    basin_size = basin_size %>%
      stringr::str_replace_all("unknown", "not known"),
    entity_type = entity_type %>%
      stringr::str_replace_all("terrestrial, soil", "soil") %>%
      stringr::str_replace_all("unknown", "not known"),
    site_type = site_type %>%
      stringr::str_replace_all("unknown", "not known")
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = clean) %>%
  dplyr::select(-dplyr::starts_with("ID_PUB")) %>%
  dplyr::mutate(source = "AMSS", .before = 1) %>%
  dplyr::mutate(age_BP = as.character(age_BP))

usethis::use_data(AMSS, overwrite = TRUE, compress = "xz")

# Inspect enumerates ----
### basin_size -----
AMSS$basin_size %>%
  unique() %>% sort()

### site_type ----
AMSS$site_type %>%
  unique() %>% sort()

### entity_type ----
AMSS$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    AMSS %>%
                      dplyr::select(source:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    AMSS %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    AMSS %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    AMSS %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/AMSS_",
                              Sys.Date(),
                              ".xlsx"))

# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/AMSS_climate_reconstructions_2022-04-28.csv" %>%
  readr::read_csv()

# Load daily values for precipitation to compute MAP (mean annual precipitation)
climate_reconstructions_pre <-
  "data-raw/reconstructions/AMSS_climate_reconstructions_pre_2022-04-28.csv" %>%
  readr::read_csv() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE), .before = T1)

climate_reconstructions_2 <- climate_reconstructions %>%
  dplyr::bind_cols(climate_reconstructions_pre %>%
                     dplyr::select(map))

climate_reconstructions_with_counts <-
  smpds::AMSS %>%
  dplyr::select(-c(mi:mtwa)) %>%
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
waldo::compare(smpds::AMSS,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:map, sn, en, new_elevation))
)

AMSS <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation)
usethis::use_data(AMSS, overwrite = TRUE, compress = "xz")
waldo::compare(smpds::AMSS,
               AMSS,
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
