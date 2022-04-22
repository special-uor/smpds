# Modern data ----
`%>%` <- magrittr::`%>%`
## Load data ----
### Metadata ----
neotropics_samples_metadata <-
  "data-raw/GLOBAL/Neotropics_surface samples_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  janitor::clean_names() %>%
  dplyr::select(-1) %>%
  dplyr::rename(age_BP = age_bp) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .after = doi) %>%
  dplyr::mutate(doi = doi %>%
                  stringr::str_remove_all("DOI:") %>%
                  stringr::str_squish())

### Pollen counts ----
neotropics_samples_counts <-
  "data-raw/GLOBAL/Neotropics_surface samples_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  dplyr::rename(ID_SAMPLE = `sample number`,
                site_name = `site name`)
neotropics_samples_counts_2 <- neotropics_samples_counts %>%
  dplyr::select(ID_SAMPLE) %>%
  dplyr::bind_cols(
    neotropics_samples_counts %>% # Convert columns with counts to numeric type
      dplyr::select(-c(ID_SAMPLE:site_name)) %>%
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
neotropics_samples_taxa_amalgamation <-
  "data-raw/GLOBAL/Neotropics_surface samples_SPH.xlsx" %>%
  readxl::read_excel(sheet = 3) %>%
  magrittr::set_names(c(
    "taxon_name", "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  dplyr::mutate(taxon_name = taxon_name %>% stringr::str_squish(),
                clean = clean %>% stringr::str_squish(),
                intermediate = intermediate %>% stringr::str_squish(),
                amalgamated = amalgamated %>% stringr::str_squish())

### Combine counts and amalgamation ----
neotropics_samples_taxa_counts_amalgamation <-
  neotropics_samples_counts_2 %>%
  dplyr::left_join(neotropics_samples_taxa_amalgamation,
                   by = c("taxon_name")) %>%
  dplyr::relocate(taxon_count, .after = amalgamated) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1) %>%
  dplyr::select(-taxon_name)

neotropics_samples_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated)) %>%
  dplyr::distinct(clean, intermediate, amalgamated)

## Find DOIs ----
neotropics_samples_metadata_pubs <-
  neotropics_samples_metadata %>%
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
# neotropics_samples_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/neotropics_samples_modern-references.csv")

### Load cleaned publications list ----
neotropics_samples_clean_publications <-
  "data-raw/GLOBAL/neotropics_samples_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI)

## Append clean publications ----
neotropics_samples_metadata_2 <-
  neotropics_samples_metadata %>%
  dplyr::left_join(neotropics_samples_metadata_pubs %>%
                     dplyr::select(-DOI, -doi, -dplyr::contains("updated")),
                   by = "publication") %>%
  dplyr::left_join(neotropics_samples_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi, -ID_PUB) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication)

## Extract PNV/BIOME ----
neotropics_samples_metadata_3 <-
  neotropics_samples_metadata_2 %>%
  smpds::parallel_extract_biome(cpus = 8) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

neotropics_samples_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * c(0.9, 1.1),
                    ylim = range(.$latitude, na.rm = TRUE) * c(0.9, 1.1))

## Create count tables ----
### Clean ----
neotropics_samples_clean <-
  neotropics_samples_taxa_counts_amalgamation %>%
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
neotropics_samples_intermediate <-
  neotropics_samples_taxa_counts_amalgamation %>%
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
neotropics_samples_amalgamated <-
  neotropics_samples_taxa_counts_amalgamation %>%
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
neotropics_pollen <-
  neotropics_samples_metadata_3 %>%
  dplyr::mutate(
    clean = neotropics_samples_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = neotropics_samples_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = neotropics_samples_amalgamated %>%
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
  dplyr::mutate(source = "Bush et al., 2020", .before = 1)

usethis::use_data(neotropics_pollen, overwrite = TRUE, compress = "xz")

## Inspect enumerates ----
### basin_size -----
neotropics_pollen$basin_size %>%
  unique() %>% sort()

### site_type ----
neotropics_pollen$site_type %>%
  unique() %>% sort()

### entity_type ----
neotropics_pollen$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    neotropics_pollen %>%
                      dplyr::select(source:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    neotropics_pollen %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    neotropics_pollen %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    neotropics_pollen %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/neotropics_pollen_",
                              Sys.Date(),
                              ".xlsx"))
