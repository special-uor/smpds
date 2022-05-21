# Modern data ----
`%>%` <- magrittr::`%>%`
## Load data ----
### Metadata ----
dugerdil_metadata <-
  "data-raw/GLOBAL/Dugerdil_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .after = doi)

### Pollen counts ----
dugerdil_counts <-
  "data-raw/GLOBAL/Dugerdil_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  dplyr::rename(entity_name = `...1`) %>%
  dplyr::left_join(dugerdil_metadata %>%
                     dplyr::select(entity_name, ID_SAMPLE),
                   by = "entity_name") %>%
  dplyr::relocate(ID_SAMPLE, .before = 1)
dugerdil_counts_2 <- dugerdil_counts %>%
  dplyr::select(ID_SAMPLE) %>%
  dplyr::bind_cols(
    dugerdil_counts %>% # Convert columns with counts to numeric type
      dplyr::select(-c(ID_SAMPLE:entity_name)) %>%
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
dugerdil_taxa_amalgamation <-
  "data-raw/GLOBAL/Dugerdil_SPH.xlsx" %>%
  readxl::read_excel(sheet = 3) %>%
  magrittr::set_names(c(
    "taxon_name", "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  purrr::map_dfc(~.x %>% stringr::str_squish())

### Combine counts and amalgamation ----
dugerdil_taxa_counts_amalgamation <-
  dugerdil_counts_2 %>%
  dplyr::left_join(dugerdil_taxa_amalgamation,
                   by = c("taxon_name")) %>%
  dplyr::relocate(taxon_count, .after = amalgamated) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1) %>%
  dplyr::select(-taxon_name)

### Additional taxonomic corrections (SPH - May 20th) ----
taxonomic_corrections <- "data-raw/GLOBAL/taxonomic_corrections.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  purrr::map_df(stringr::str_squish)

dugerdil_taxa_counts_amalgamation_rev <-
  dugerdil_taxa_counts_amalgamation %>%
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

waldo::compare(dugerdil_taxa_counts_amalgamation,
               dugerdil_taxa_counts_amalgamation_rev)
waldo::compare(dugerdil_taxa_counts_amalgamation %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               dugerdil_taxa_counts_amalgamation_rev %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               max_diffs = Inf)

dugerdil_taxa_counts_amalgamation <- dugerdil_taxa_counts_amalgamation_rev

dugerdil_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated)) %>%
  dplyr::distinct(clean, intermediate, amalgamated)

## Find DOIs ----
dugerdil_metadata_pubs <-
  dugerdil_metadata %>%
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
# dugerdil_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/dugerdil_modern-references.csv")

### Load cleaned publications list ----
dugerdil_clean_publications <-
  "data-raw/GLOBAL/dugerdil_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI)

## Append clean publications ----
dugerdil_metadata_2 <-
  dugerdil_metadata %>%
  dplyr::left_join(dugerdil_metadata_pubs %>%
                     dplyr::select(-DOI, -doi, -dplyr::contains("updated")),
                   by = "publication") %>%
  dplyr::left_join(dugerdil_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi, -ID_PUB) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication)

## Extract PNV/BIOME ----
dugerdil_metadata_3 <-
  dugerdil_metadata_2 %>%
  smpds::parallel_extract_biome(cpus = 8) %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

dugerdil_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * c(0.9, 1.1),
                    ylim = range(.$latitude, na.rm = TRUE) * c(0.9, 1.1))

## Create count tables ----
### Clean ----
dugerdil_clean <-
  dugerdil_taxa_counts_amalgamation %>%
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
dugerdil_intermediate <-
  dugerdil_taxa_counts_amalgamation %>%
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
dugerdil_amalgamated <-
  dugerdil_taxa_counts_amalgamation %>%
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

# Store subsets ----
dugerdil_pollen <-
  dugerdil_metadata_3 %>%
  dplyr::mutate(
    clean = dugerdil_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = dugerdil_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = dugerdil_amalgamated %>%
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
  dplyr::select(-dplyr::starts_with("ID_PUB")) %>%
  dplyr::mutate(source = "Dugerdil et al., 2021", .before = 1) %>%
  dplyr::mutate(age_BP = as.character(age_BP))

usethis::use_data(dugerdil_pollen, overwrite = TRUE, compress = "xz")

# Inspect enumerates ----
### basin_size -----
dugerdil_pollen$basin_size %>%
  unique() %>% sort()

### site_type ----
dugerdil_pollen$site_type %>%
  unique() %>% sort()

### entity_type ----
dugerdil_pollen$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    dugerdil_pollen %>%
                      dplyr::select(source:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    dugerdil_pollen %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    dugerdil_pollen %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    dugerdil_pollen %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/dugerdil_pollen_",
                              Sys.Date(),
                              ".xlsx"))

# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/dugerdil_pollen_climate_reconstructions_2022-05-04.csv" %>%
  readr::read_csv()

# Load daily values for precipitation to compute MAP (mean annual precipitation)
climate_reconstructions_pre <-
  "data-raw/reconstructions/dugerdil_pollen_climate_reconstructions_pre_2022-05-04.csv" %>%
  readr::read_csv() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE), .before = T1)

climate_reconstructions_2 <- climate_reconstructions %>%
  dplyr::bind_cols(climate_reconstructions_pre %>%
                     dplyr::select(map))

climate_reconstructions_with_counts <-
  dugerdil_pollen %>%
  # smpds::dugerdil_pollen %>%
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
waldo::compare(smpds::dugerdil_pollen,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:map, sn, en, new_elevation))
)

dugerdil_pollen <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation)
usethis::use_data(dugerdil_pollen, overwrite = TRUE, compress = "xz")
waldo::compare(smpds::dugerdil_pollen, dugerdil_pollen, max_diffs = Inf)

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
