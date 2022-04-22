# Modern data ----
`%>%` <- magrittr::`%>%`
## Load data ----
### Metadata ----
moroccan_coretops_metadata <-
  "data-raw/GLOBAL/Moroccan core tops_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  janitor::clean_names() %>%
  dplyr::rename(age_BP = age,
                basin_size = basin_size_km2,
                publication = reference) %>%
  dplyr::mutate(doi = NA) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .after = doi)

### Pollen counts ----
moroccan_coretops_counts <-
  "data-raw/GLOBAL/Moroccan core tops_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "ID_SAMPLE", "entity_name", "clean", "taxon_count"
  )) %>%
  dplyr::mutate(clean = clean %>%
                  stringr::str_squish()) %>%
  dplyr::group_by(ID_SAMPLE, clean) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::distinct() %>%
  dplyr::ungroup()

### Amalgamations ----
moroccan_coretops_taxa_amalgamation <-
  "data-raw/GLOBAL/Moroccan core tops_SPH.xlsx" %>%
  readxl::read_excel(sheet = 3) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  purrr::map_df(~.x %>% stringr::str_squish())

### Combine counts and amalgamation ----
moroccan_coretops_taxa_counts_amalgamation <-
  moroccan_coretops_counts %>%
  dplyr::left_join(moroccan_coretops_taxa_amalgamation,
                   by = c("clean")) %>%
  dplyr::relocate(clean, .after = amalgamated) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1)

moroccan_coretops_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated)) %>%
  dplyr::distinct(clean, intermediate, amalgamated)

## Find DOIs ----
moroccan_coretops_metadata_pubs <-
  moroccan_coretops_metadata %>%
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
# moroccan_coretops_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/moroccan_coretops_modern-references.csv")

### Load cleaned publications list ----
moroccan_coretops_clean_publications <-
  "data-raw/GLOBAL/moroccan_coretops_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI)

## Append clean publications ----
moroccan_coretops_metadata_2 <-
  moroccan_coretops_metadata %>%
  dplyr::left_join(moroccan_coretops_metadata_pubs %>%
                     dplyr::select(-DOI, -doi, -dplyr::contains("updated")),
                   by = "publication") %>%
  dplyr::left_join(moroccan_coretops_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi, -ID_PUB) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication)

## Extract PNV/BIOME ----
moroccan_coretops_metadata_3 <-
  moroccan_coretops_metadata_2 %>%
  smpds::parallel_extract_biome(cpus = 1) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

moroccan_coretops_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * c(0.9, 1.1),
                    ylim = range(.$latitude, na.rm = TRUE) * c(0.9, 1.1))

## Create count tables ----
### Clean ----
moroccan_coretops_clean <-
  moroccan_coretops_taxa_counts_amalgamation %>%
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
moroccan_coretops_intermediate <-
  moroccan_coretops_taxa_counts_amalgamation %>%
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
moroccan_coretops_amalgamated <-
  moroccan_coretops_taxa_counts_amalgamation %>%
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
moroccan_pollen <-
  moroccan_coretops_metadata_3 %>%
  dplyr::mutate(
    clean = moroccan_coretops_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = moroccan_coretops_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = moroccan_coretops_amalgamated %>%
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
  dplyr::select(-dplyr::starts_with("ID_PUB"))

usethis::use_data(moroccan_pollen, overwrite = TRUE, compress = "xz")

## Inspect enumerates ----
### basin_size -----
moroccan_pollen$basin_size %>%
  unique() %>% sort()

### site_type ----
moroccan_pollen$site_type %>%
  unique() %>% sort()

### entity_type ----
moroccan_pollen$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    moroccan_pollen %>%
                      dplyr::select(source:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    moroccan_pollen %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    moroccan_pollen %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    moroccan_pollen %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/moroccan_pollen_",
                              Sys.Date(),
                              ".xlsx"))
