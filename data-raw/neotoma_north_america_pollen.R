`%>%` <- magrittr::`%>%`
# Original -----
neotoma_north_america_pollen <-
  readxl::read_excel("data-raw/neotoma_north_america_pollen_2021-10-25.xlsx",
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

# List of taxon to be excluded (provided by SPH)
excluded_taxons_sph <-
  readxl::read_excel("data-raw/taxon_exclusions.xlsx", col_names = FALSE) %>%
  magrittr::set_names(c("taxon_name", "action")) %>%
  dplyr::mutate(action = stringr::str_to_upper(action))
sum(excluded_taxons_sph$action == "EXCLUDE") == nrow(excluded_taxons_sph)

neotoma_north_america_pollen_na_age <-
  neotoma_north_america_pollen %>%
  dplyr::filter(is.na(age))
neotoma_north_america_pollen_na_age %>%
  dplyr::filter(!is.na(age_older) | !is.na(age_younger)) %>%
  dplyr::select(dataset_id, site_name, entity_name, age, age_older, age_younger)

# Create index of taxa that should be ignored
idx <- excluded_taxons_sph$taxon_name %in%
  colnames(neotoma_north_america_pollen)
neotoma_north_america_pollen_modern <- neotoma_north_america_pollen %>%
  dplyr::select(-excluded_taxons_sph$taxon_name[idx]) %>% # Remove unwanted taxa
  dplyr::filter(!is.na(age) | !is.na(age_older) | !is.na(age_younger)) %>%
  dplyr::mutate(age_filter = dplyr::coalesce(age, age_older, age_younger),
                # age_filter = 1950 - age_filter,
                before = age) %>%
  dplyr::filter(age_filter <= 50) %>%
  dplyr::select(-age_filter)

neotoma_north_america_pollen_modern_2 <-
  neotoma_north_america_pollen_modern %>%
  smpds::rm_na_taxa(cols = 1:22)

neotoma_north_america_pollen_modern_2_meta <-
  neotoma_north_america_pollen_modern_2 %>%
  dplyr::select(site_id:citation) %>%
  dplyr::distinct() %>%
  dplyr::mutate(DOI = citation %>%
                  stringr::str_extract_all("\\[DOI\\s*(.*?)\\s*\\](;|$)") %>%
                  purrr::map_chr(~.x %>%
                                   stringr::str_remove_all("^\\[DOI:|\\]$") %>%
                                   stringr::str_squish() %>%
                                   stringr::str_c(collapse = ";\n"))
                )

neotoma_north_america_pollen_modern_2_samples <-
  neotoma_north_america_pollen_modern_2 %>%
  dplyr::select(-c(site_id,
                   chronology_id:dataset_name,
                   unit_name:citation))

neotoma_north_america_pollen_modern_2_taxon_list <- tibble::tibble(
  taxon_name = colnames(neotoma_north_america_pollen_modern_2_samples)[-c(1:9)],
  clean_name = taxon_name %>%
    stringr::str_squish()
) %>%
  dplyr::arrange(taxon_name)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "taxon_list")
openxlsx::writeData(wb, "taxon_list",
                    neotoma_north_america_pollen_modern_2_taxon_list)
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata", neotoma_north_america_pollen_modern_2_meta)
openxlsx::addWorksheet(wb, "samples")
openxlsx::writeData(wb, "samples", neotoma_north_america_pollen_modern_2_samples)
openxlsx::saveWorkbook(wb,
                       paste0("inst/extdata/neotoma_north_america_modern_",
                              Sys.Date(),
                              ".xlsx"),
                       overwrite = TRUE)


neotoma_north_america_pollen_modern_2_meta <-
  neotoma_north_america_pollen_modern_2 %>%
  dplyr::select(site_id:citation) %>%
  dplyr::distinct() %>%
  dplyr::mutate(DOI = citation %>%
                  stringr::str_extract_all("\\[DOI\\s*(.*?)\\s*\\](;|$)") %>%
                  purrr::map_chr(~.x %>%
                                   stringr::str_remove_all("^\\[DOI:|\\]$") %>%
                                   stringr::str_squish() %>%
                                   stringr::str_c(collapse = ";\n"))
  )

### Pollen surface ----
neotoma_north_america_pollen_modern_undated <- neotoma_north_america_pollen %>%
  dplyr::select(-excluded_taxons_sph$taxon_name[idx]) %>% # Remove unwanted taxa
  dplyr::mutate(age_filter = dplyr::coalesce(age, age_older, age_younger),
                before = age) %>%
  dplyr::filter(is.na(age_filter) &
                  stringr::str_detect(dataset_type, "pollen surface")) %>%
  dplyr::select(-age_filter)

neotoma_north_america_pollen_modern_undated_2 <-
  neotoma_north_america_pollen_modern_undated %>%
  smpds::rm_na_taxa(cols = 1:22)

neotoma_north_america_pollen_modern_undated_2_meta <-
  neotoma_north_america_pollen_modern_undated_2 %>%
  dplyr::select(site_id:citation) %>%
  dplyr::distinct() %>%
  dplyr::mutate(DOI = citation %>%
                  stringr::str_extract_all("\\[DOI\\s*(.*?)\\s*\\](;|$)") %>%
                  purrr::map_chr(~.x %>%
                                   stringr::str_remove_all("^\\[DOI:|\\]$") %>%
                                   stringr::str_squish() %>%
                                   stringr::str_c(collapse = ";\n"))
  )

neotoma_north_america_pollen_modern_undated_2_samples <-
  neotoma_north_america_pollen_modern_undated_2 %>%
  dplyr::select(-c(site_id,
                   chronology_id:dataset_name,
                   unit_name:citation))

neotoma_north_america_pollen_modern_undated_2_taxon_list <-
  tibble::tibble(
    taxon_name =
      colnames(neotoma_north_america_pollen_modern_undated_2_samples)[-c(1:9)],
    clean_name = taxon_name %>%
      stringr::str_squish()
  ) %>%
  dplyr::arrange(taxon_name)

# Filter taxon already in `neotoma_north_america_pollen_modern_2_taxon_list`
neotoma_north_america_pollen_modern_undated_2_taxon_list_exclusive <-
  neotoma_north_america_pollen_modern_undated_2_taxon_list %>%
  dplyr::filter(!(taxon_name %in%
                    neotoma_north_america_pollen_modern_2_taxon_list$taxon_name)
                )

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "taxon_list")
openxlsx::writeData(wb, "taxon_list",
                    neotoma_north_america_pollen_modern_undated_2_taxon_list
                    # neotoma_north_america_pollen_modern_undated_2_taxon_list_exclusive
                    )
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata", neotoma_north_america_pollen_modern_undated_2_meta)
openxlsx::addWorksheet(wb, "samples")
openxlsx::writeData(wb, "samples", neotoma_north_america_pollen_modern_undated_2_samples)
openxlsx::saveWorkbook(wb,
                       paste0("inst/extdata/neotoma_north_america_modern_",
                              Sys.Date(),
                              "_pollen_surface_only.xlsx"),
                       overwrite = TRUE)

# Modern data ----
`%>%` <- magrittr::`%>%`
## Load data ----
### Pollen counts ----
#### Metadata ----
neotoma_north_america_pollen_metadata <-
  "data-raw/GLOBAL/neotoma_north_america_modern_2022-04-05_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  janitor::clean_names() %>%
  dplyr::rename(age_BP = age_bp,
                ID_SAMPLE = sample_id,
                ID_ENTITY = dataset_id) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name))

#### Counts ----
neotoma_north_america_pollen_counts <-
  "data-raw/GLOBAL/neotoma_north_america_modern_2022-04-05_SPH.xlsx" %>%
  readxl::read_excel(sheet = 3) %>%
  magrittr::set_names(
    colnames(.) %>%
      stringr::str_replace_all("sample.id", "ID_SAMPLE") %>%
      stringr::str_replace_all("dataset.id", "ID_ENTITY") %>%
      stringr::str_replace_all("\\.\\.\\.", "#")
  ) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(ID_ENTITY),
                .before = 1) %>%
  dplyr::mutate(age = ifelse(is.na(age),
                             "assumed modern",
                             as.character(age))) %>%
  dplyr::rename(age_BP = age)

neotoma_north_america_pollen_metadata_2 <-
  neotoma_north_america_pollen_metadata %>%
  dplyr::select(-age_BP) %>%
  dplyr::left_join(neotoma_north_america_pollen_counts %>%
                     dplyr::select(ID_SAMPLE, ID_ENTITY, entity_name, age_BP),
                   by = c("ID_ENTITY", "ID_SAMPLE", "entity_name")) %>%
  dplyr::relocate(age_BP, .before = publication) %>%
  dplyr::relocate(ID_SAMPLE, .after = doi)

# Combined duplicated columns
neotoma_north_america_pollen_counts_2 <-
  neotoma_north_america_pollen_counts %>%
  dplyr::select(ID_SAMPLE) %>%
  dplyr::bind_cols(# Convert columns with counts to numeric type
    neotoma_north_america_pollen_counts %>%
      dplyr::select(-c(ID_SAMPLE:age_BP)) %>%
      purrr::map_dfc(~.x %>% as.numeric)
  ) %>%
  magrittr::set_names(
    colnames(.) %>%
      stringr::str_replace_all("\\.\\.\\.", "#")
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
  dplyr::ungroup() %>%
  dplyr::mutate(clean = clean %>% stringr::str_squish())

##### DUCKPOND ----
"SPH requested adding two missing surface samples linked to Duck Pond"
additional_surface_samples_metadata <-
  "data-raw/GLOBAL/additional_surface_samples_DUCKPOND.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  janitor::clean_names() %>%
  dplyr::rename(age_BP = age_bp) %>%
  dplyr::mutate(ID_SAMPLE = nrow(neotoma_north_america_pollen_counts) +
                  seq_along(entity_name),
                .before = 1)

additional_surface_samples_counts <-
  "data-raw/GLOBAL/additional_surface_samples_DUCKPOND.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(
    c("entity_name",
      "taxon_name",
      "clean",
      "intermediate",
      "amalgamated",
      "taxon_count"
      )
  ) %>%
  dplyr::left_join(
    additional_surface_samples_metadata %>%
      dplyr::select(entity_name, ID_SAMPLE),
    by = "entity_name"
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1) %>%
  dplyr::select(-entity_name, -taxon_name) %>%
  dplyr::mutate(clean = clean %>% stringr::str_squish(),
                intermediate = intermediate %>% stringr::str_squish(),
                amalgamated = amalgamated %>% stringr::str_squish())

### Pollen surfaces ----
#### Metadata ----
neotoma_north_america_pollen_surface_metadata <-
  "data-raw/GLOBAL/neotoma_north_america_modern_2022-04-20_pollen_surface_only_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  janitor::clean_names() %>%
  dplyr::rename(age_BP = age,
                basin_size = basin_size_km2,
                publication = reference,
                ID_ENTITY = dataset_id) %>%
  dplyr::mutate(ID_SAMPLE = nrow(neotoma_north_america_pollen_counts) +
                  seq_along(entity_name),
                .before = 1) %>%
  dplyr::mutate(age_BP = ifelse(is.na(age_BP), "Modern", age_BP),
                basin_size = ifelse(is.na(basin_size), "not known", basin_size))

#### Counts ----
neotoma_north_america_pollen_surface_counts <-
  "data-raw/GLOBAL/neotoma_north_america_modern_2022-04-20_pollen_surface_only_SPH.xlsx" %>%
  readxl::read_excel(sheet = 3) %>%
  magrittr::set_names(
    colnames(.) %>%
      # stringr::str_replace_all("sample.id", "ID_SAMPLE") %>%
      stringr::str_replace_all("dataset.id", "ID_ENTITY") %>%
      stringr::str_replace_all("\\.\\.\\.", "#")
  ) %>%
  dplyr::mutate(ID_SAMPLE = nrow(neotoma_north_america_pollen_counts) +
                  seq_along(entity_name),
                .before = 1)

# Combined duplicated columns
neotoma_north_america_pollen_surface_counts_2 <-
  neotoma_north_america_pollen_surface_counts %>%
  dplyr::select(ID_SAMPLE) %>%
  dplyr::bind_cols(# Convert columns with counts to numeric type
    neotoma_north_america_pollen_surface_counts %>%
      dplyr::select(-c(ID_SAMPLE:entity_name)) %>%
      purrr::map_dfc(~.x %>% as.numeric)
  ) %>%
  magrittr::set_names(
    colnames(.) %>%
      stringr::str_replace_all("\\.\\.\\.", "#")
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
  dplyr::ungroup() %>%
  dplyr::mutate(clean = clean %>% stringr::str_squish())

### Amalgamations ----
neotoma_north_america_pollen_taxa_amalgamation <-
  "data-raw/GLOBAL/neotoma_north_america_modern_2022-04-05_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  magrittr::set_names(c(
    "original", "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  purrr::map_df(~.x %>% stringr::str_squish())

neotoma_north_america_pollen_surface_taxa_amalgamation <-
  "data-raw/GLOBAL/neotoma_north_america_modern_2022-04-20_pollen_surface_only_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  magrittr::set_names(c(
    "original", "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  purrr::map_df(~.x %>% stringr::str_squish())

### Combine counts and amalgamation ----
neotoma_north_america_pollen_taxa_counts_amalgamation <-
  neotoma_north_america_pollen_counts_2 %>%
  dplyr::rename(original = clean) %>%
  dplyr::left_join(neotoma_north_america_pollen_taxa_amalgamation,
                   by = c("original")) %>%
  dplyr::relocate(taxon_count, .after = amalgamated) %>%
  dplyr::select(-original)

### Additional taxonomic corrections (SPH - May 20th) ----
taxonomic_corrections <- "data-raw/GLOBAL/taxonomic_corrections.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  purrr::map_df(stringr::str_squish)

neotoma_north_america_pollen_taxa_counts_amalgamation_rev <-
  neotoma_north_america_pollen_taxa_counts_amalgamation %>%
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

waldo::compare(neotoma_north_america_pollen_taxa_counts_amalgamation,
               neotoma_north_america_pollen_taxa_counts_amalgamation_rev)
waldo::compare(neotoma_north_america_pollen_taxa_counts_amalgamation %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               neotoma_north_america_pollen_taxa_counts_amalgamation_rev %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               max_diffs = Inf)

neotoma_north_america_pollen_taxa_counts_amalgamation <- neotoma_north_america_pollen_taxa_counts_amalgamation_rev

neotoma_north_america_pollen_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated))


neotoma_north_america_pollen_surface_taxa_counts_amalgamation <-
  neotoma_north_america_pollen_surface_counts_2 %>%
  dplyr::rename(original = clean) %>%
  dplyr::left_join(neotoma_north_america_pollen_surface_taxa_amalgamation,
                   by = c("original")) %>%
  dplyr::relocate(taxon_count, .after = amalgamated) %>%
  dplyr::select(-original)

### Additional taxonomic corrections (SPH - May 20th) ----
taxonomic_corrections <- "data-raw/GLOBAL/taxonomic_corrections.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  purrr::map_df(stringr::str_squish)

neotoma_north_america_pollen_surface_taxa_counts_amalgamation_rev <-
  neotoma_north_america_pollen_surface_taxa_counts_amalgamation %>%
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

waldo::compare(neotoma_north_america_pollen_surface_taxa_counts_amalgamation,
               neotoma_north_america_pollen_surface_taxa_counts_amalgamation_rev)
waldo::compare(neotoma_north_america_pollen_surface_taxa_counts_amalgamation %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               neotoma_north_america_pollen_surface_taxa_counts_amalgamation_rev %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               max_diffs = Inf)

neotoma_north_america_pollen_surface_taxa_counts_amalgamation <- neotoma_north_america_pollen_surface_taxa_counts_amalgamation_rev

neotoma_north_america_pollen_surface_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated))

## Find DOIs ----
neotoma_north_america_pollen_metadata_pubs <-
  neotoma_north_america_pollen_metadata_2 %>%
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
                                   stringr::str_c(collapse = ";\n")) #%>%
                  # stringr::str_replace_all("$\\s*\n0.1139/e80-122",
                  #                          "10.1139/e80-122")
  ) %>%
  dplyr::mutate(ID_PUB = seq_along(publication)) %>%
  dplyr::mutate(updated_publication = NA, .before = publication) %>%
  dplyr::mutate(updated_DOI = NA, .before = DOI)
# neotoma_north_america_pollen_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/neotoma_north_america_pollen_modern-references.csv")

neotoma_north_america_pollen_surface_metadata_pubs <-
  neotoma_north_america_pollen_surface_metadata %>%
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
# neotoma_north_america_pollen_surface_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/neotoma_north_america_pollen_surface_modern-references.csv")

### Load cleaned publications list ----
neotoma_north_america_pollen_clean_publications <-
  "data-raw/GLOBAL/neotoma_north_america_pollen_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI) %>%
  dplyr::mutate(
    updated_publication = updated_publication %>%
      stringr::str_remove_all("\\[DOI\\s*(.*?)\\s*\\](;|$)") %>%
      stringr::str_remove_all("^\\[DOI:") %>%
      stringr::str_remove_all("\\]\\s*;\\s*$") %>%
      stringr::str_remove_all("\\]$") %>%
      stringr::str_remove_all("doi:") %>%
      stringr::str_squish()
  )

neotoma_north_america_pollen_surface_clean_publications <-
  "data-raw/GLOBAL/neotoma_north_america_pollen_surface_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI) %>%
  dplyr::mutate(
    updated_publication = updated_publication %>%
      stringr::str_remove_all("\\[DOI\\s*(.*?)\\s*\\](;|$)") %>%
      stringr::str_remove_all("^\\[DOI:") %>%
      stringr::str_remove_all("\\]\\s*;\\s*$") %>%
      stringr::str_remove_all("\\]$") %>%
      stringr::str_remove_all("doi:") %>%
      stringr::str_squish()
  )

## Append clean publications ----
neotoma_north_america_pollen_metadata_3 <-
  neotoma_north_america_pollen_metadata_2 %>%
  dplyr::left_join(neotoma_north_america_pollen_metadata_pubs %>%
                     dplyr::select(-DOI, -dplyr::contains("updated")),
                   by = "publication") %>%
  dplyr::left_join(neotoma_north_america_pollen_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi.x, -doi.y, -ID_PUB) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication) %>%
  dplyr::mutate(publication = ifelse(is.na(publication),
                                     "Unpublished data", publication))

neotoma_north_america_pollen_surface_metadata_2 <-
  neotoma_north_america_pollen_surface_metadata %>%
  dplyr::left_join(neotoma_north_america_pollen_surface_metadata_pubs %>%
                     dplyr::select(-DOI, -dplyr::contains("updated")),
                   by = "publication") %>%
  dplyr::left_join(neotoma_north_america_pollen_surface_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi.x, -doi.y, -ID_PUB) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication) %>%
  dplyr::mutate(publication = ifelse(is.na(publication),
                                     "Unpublished data", publication))

## Extract PNV/BIOME ----
neotoma_north_america_pollen_metadata_4 <-
  neotoma_north_america_pollen_metadata_3 %>%
  smpds::parallel_extract_biome(cpus = 12) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

neotoma_north_america_pollen_surface_metadata_3 <-
  neotoma_north_america_pollen_surface_metadata_2 %>%
  smpds::parallel_extract_biome(cpus = 12) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

additional_surface_samples_metadata_2 <-
  additional_surface_samples_metadata %>%
  smpds::parallel_extract_biome() %>%
  dplyr::relocate(ID_BIOME, .after = doi)

neotoma_north_america_pollen_metadata_4 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * c(0.9, 1.1),
                    ylim = range(.$latitude, na.rm = TRUE) * c(0.9, 1.1))
neotoma_north_america_pollen_surface_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * c(0.9, 1.1),
                    ylim = range(.$latitude, na.rm = TRUE) * c(0.9, 1.1))

## Combine both subsets ----
neotoma_north_america_pollen_metadata_all <-
  dplyr::bind_rows(
    neotoma_north_america_pollen_metadata_4,
    neotoma_north_america_pollen_surface_metadata_3
    # additional_surface_samples_metadata_2
  )

neotoma_north_america_pollen_taxa_counts_amalgamation_all <-
  dplyr::bind_rows(
    neotoma_north_america_pollen_taxa_counts_amalgamation,
    neotoma_north_america_pollen_surface_taxa_counts_amalgamation
    # additional_surface_samples_counts
  )

neotoma_north_america_pollen_taxa_counts_amalgamation_all %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated))

## Create count tables ----
### Clean ----
neotoma_north_america_pollen_clean <-
  neotoma_north_america_pollen_taxa_counts_amalgamation_all %>%
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
neotoma_north_america_pollen_intermediate <-
  neotoma_north_america_pollen_taxa_counts_amalgamation_all %>%
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
neotoma_north_america_pollen_amalgamated <-
  neotoma_north_america_pollen_taxa_counts_amalgamation_all %>%
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
## Check if there are any missing elevations:
neotoma_north_america_pollen_metadata_all %>%
  dplyr::filter(is.na(elevation))

## Extract missing elevation values with `elevatr`
latlon_proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
neotoma_north_america_pollen_metadata_all_missing_elevations <-
  neotoma_north_america_pollen_metadata_all %>%
  dplyr::filter(is.na(elevation)) %>%
  dplyr::mutate(elevation = list(latitude, longitude) %>%
                  purrr::pmap_dbl(function(latitude, longitude) {
                    elv <- tibble::tibble(x = longitude, y = latitude) %>%
                      sp::SpatialPoints(proj4string = sp::CRS(latlon_proj)) %>%
                      elevatr::get_elev_point(prj = latlon_proj, src = "aws")
                    elv$elevation[1]
                  }))
neotoma_north_america_pollen_metadata_all_missing_elevations %>%
  dplyr::filter(is.na(elevation))
# neotoma_north_america_pollen_metadata_all_missing_elevations <-
#   neotoma_north_america_pollen_metadata_all %>%
#   dplyr::filter(is.na(elevation)) %>%
#   dplyr::mutate(elevation = list(latitude, longitude) %>%
#                   purrr::pmap_dbl(function(latitude, longitude) {
#                     rgbif::elevation(latitude = latitude,
#                                      longitude = longitude,
#                                      username = "villegar",
#                                      elevation_model = "srtm1") %>%
#                       .$elevation_geonames
#                   }))
neotoma_north_america_pollen_metadata_all_missing_elevations %>%
  smpds::plot_climate("elevation",
                      xlim = c(-180, -50),
                      ylim = c(90, 11))

neotoma_north_america_pollen_metadata_all %>%
  smpds::plot_climate_tiles("elevation",
                               xlim = -c(-20, 110),
                               ylim = c(70, 85))

north_america_pollen <-
  neotoma_north_america_pollen_metadata_all %>%
  dplyr::left_join(
    neotoma_north_america_pollen_metadata_all_missing_elevations %>%
      dplyr::select(ID_SAMPLE,
                    new_elevation = elevation),
    by = "ID_SAMPLE"
  ) %>%
  dplyr::mutate(elevation = dplyr::coalesce( # Extract new elevations
    elevation,
    new_elevation
  )) %>%
  dplyr::select(-sample_id, -site_id, -ID_ENTITY, -new_elevation) %>%
  dplyr::mutate(
    clean = neotoma_north_america_pollen_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = neotoma_north_america_pollen_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = neotoma_north_america_pollen_amalgamated %>%
      dplyr::select(-c(ID_SAMPLE))
  ) %>%
  dplyr::mutate(
    basin_size = basin_size %>%
      stringr::str_replace_all("unknown", "not known"),
    entity_type = entity_type %>%
      stringr::str_replace_all("unknown", "not known") %>%
      stringr::str_to_lower(),
    site_type = site_type %>%
      stringr::str_replace_all("coastal, estuary", "coastal, estuarine") %>%
      stringr::str_replace_all("drained lake", "lacustrine, drained lake") %>%
      stringr::str_replace_all("dry lake", "lacustrine, drained lake") %>%
      stringr::str_replace_all("llake", "lake") %>%
      stringr::str_replace_all("unknown", "not known")
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = clean) %>%
  dplyr::mutate(source = "Neotoma") %>%
  dplyr::relocate(source, .before = 1)

usethis::use_data(north_america_pollen, overwrite = TRUE, compress = "xz")


# Inspect enumerates ----
### basin_size -----
north_america_pollen$basin_size %>%
  unique() %>% sort()

### site_type ----
north_america_pollen$site_type %>%
  unique() %>% sort()

### entity_type ----
north_america_pollen$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    north_america_pollen %>%
                      dplyr::select(site_name:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    north_america_pollen %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    north_america_pollen %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    north_america_pollen %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/north_america_pollen_",
                              Sys.Date(),
                              ".xlsx"))


# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/north_america_pollen_climate_reconstructions_2022-04-30.csv" %>%
  readr::read_csv()

# Load daily values for precipitation to compute MAP (mean annual precipitation)
climate_reconstructions_pre <-
  "data-raw/reconstructions/north_america_pollen_climate_reconstructions_pre_2022-04-30.csv" %>%
  readr::read_csv() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE), .before = T1)

climate_reconstructions_2 <- climate_reconstructions %>%
  dplyr::bind_cols(climate_reconstructions_pre %>%
                     dplyr::select(map))

climate_reconstructions_with_counts <-
  north_america_pollen %>%
  # smpds::north_america_pollen %>%
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
waldo::compare(smpds::north_america_pollen,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:map, sn, en, new_elevation))
)

north_america_pollen <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation)
usethis::use_data(north_america_pollen, overwrite = TRUE, compress = "xz")
waldo::compare(smpds::north_america_pollen,
               north_america_pollen,
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
