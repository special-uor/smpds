## code to prepare `NEOTOMA` dataset goes here
# Original ----
neotoma_metadata <- readr::read_csv("inst/extdata/neotoma_metadata.csv")
neotoma_count <- readr::read_csv("inst/extdata/neotoma_count.csv")
neotoma_taxa <- readr::read_csv("inst/extdata/neotoma_taxa.csv")
NEOTOMA <- neotoma_metadata %>%
  dplyr::left_join(neotoma_count, by = "entity_name") %>%
  smpds::parallel_extract_biome(buffer = 12000, cpus = 2) %>%
  dplyr::relocate(ID_BIOME, .after = age_BP) %>%
  tidyr::pivot_longer(-c(1:13),
                      names_to = "taxon_name",
                      values_to = "count") %>%
  dplyr::filter(!is.na(count)) %>%
  dplyr::left_join(smpds::clean_taxa(), #neotoma_taxa,
                   by = "taxon_name") %>%
  dplyr::filter(action != "delete") %>%
  dplyr::select(-action) %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(count = sum(as.double(count), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
                                    taxon_name_original,
                                    taxon_name)) %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE) %>%
  dplyr::select(-taxon_name_original) %>%
  tidyr::pivot_wider(1:13,
                     names_from = "taxon_name",
                     values_from = "count") %>%
  smpds::sort_taxa(cols = 1:13) %>%
  progressr::with_progress()

aux <- NEOTOMA %>%
  dplyr::filter(entity_name %in% EMPDv2$entity_name)
aux_rev <- EMPDv2 %>%
  dplyr::filter(entity_name %in% aux$entity_name) %>%
  smpds::rm_na_taxa(1:14)

usethis::use_data(NEOTOMA, overwrite = TRUE)

# Export list of taxon names for clean-up
tibble::tibble(taxon_name = colnames(NEOTOMA)[-c(1:13)],
               clean_name = taxon_name) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/NEOTOMA_taxa_2021-08-24.csv", na = "")

# ------------------------------------------------------------------------------
# |                                  Sandbox                                   |
# ------------------------------------------------------------------------------
neotoma_metadata <-
  readxl::read_xlsx("~/Downloads/SMPDSv2/NEOTOMA/Modern samples_neotoma update.xlsx",
                    sheet = 1) %>%
  magrittr::set_names(c("source",
                        "site_name",
                        "entity_name",
                        "latitude",
                        "longitude",
                        "elevation",
                        "basin_size",
                        "site_type",
                        "entity_type",
                        "age_BP",
                        "publication")) %>%
  dplyr::group_by(site_name) %>%
  dplyr::mutate(publication = publication %>%
                  stringr::str_c(collapse = ";\n")) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(site_name, .keep_all = TRUE) %>%
  dplyr::mutate(elevation2 = list(latitude, longitude) %>%
                  purrr::pmap_dbl(function(latitude, longitude) {
                    rgbif::elevation(latitude = latitude,
                                     longitude = longitude,
                                     username = "villegar",
                                     elevation_model = "srtm1") %>%
                      .$elevation_geonames
                  }))
neotoma_metadata %>%
  dplyr::mutate(elevation = elevation2) %>%
  dplyr::select(-elevation2) %>%
  readr::write_excel_csv("inst/extdata/neotoma_metadata.csv", na = "")


neotoma_count <-
  readxl::read_xlsx("~/Downloads/SMPDSv2/NEOTOMA/Modern samples_neotoma update.xlsx",
                    sheet = 2,
                    col_names = FALSE)
neotoma_count_unused <- neotoma_count[seq(3, nrow(neotoma_count), 3), ]
neotoma_count2 <- neotoma_count %>%
  dplyr::slice(-seq(3, nrow(neotoma_count), 3))

neotoma_count_long <- seq(1, nrow(neotoma_count2), 2) %>%
  purrr::map_dfr(function(i) {
    names <- neotoma_count2[i, ] %>% purrr:::flatten_chr()
    values <- neotoma_count2[i + 1, ] %>% purrr:::flatten_chr()
    tibble::tibble(entity_name = values[1],
                   taxon_name = names[-1],
                   count = values[-1]) %>%
      dplyr::filter(!is.na(count))
  }) %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(count = as.double(count) %>%
                  sum(na.rm = FALSE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE)
neotoma_count_wide <- neotoma_count_long %>%
  tidyr::pivot_wider(entity_name,
                     names_from = "taxon_name",
                     values_from = "count") %>%
  smpds::sort_taxa() %>%
  smpds::rm_na_taxa()

neotoma_count_wide %>%
  readr::write_excel_csv("inst/extdata/neotoma_count.csv", na = "")

neotoma_taxa <- readr::read_csv("~/Downloads/SMPDSv2/NEOTOMA_taxa_2021-08-24_SPH.csv") %>%
  dplyr::distinct() %>%
  dplyr::mutate(action = ifelse(clean_name %>%
                                  stringr::str_detect("EXCLUDE"),
                                "delete", "update"),
                clean_name = ifelse(clean_name %>%
                                      stringr::str_detect("EXCLUDE"),
                                    NA, clean_name)) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::filter(!is.na(taxon_name))
neotoma_taxa %>%
  readr::write_excel_csv("inst/extdata/neotoma_taxa.csv", na = "")

# SPH revisions ----
"The entities and their counts were manually inspected by SPH"

`%>%` <- magrittr::`%>%`
## Load data ----
### Metadata ----
NEOTOMA_metadata <-
  "data-raw/GLOBAL/Neotoma_extras_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  dplyr::rename(doi = DOI) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .after = doi)

### Pollen counts ----
NEOTOMA_counts <-
  "data-raw/GLOBAL/Neotoma_extras_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  dplyr::left_join(NEOTOMA_metadata %>%
                     dplyr::select(entity_name, ID_SAMPLE),
                   by = "entity_name") %>%
  dplyr::relocate(ID_SAMPLE, .before = 1)
NEOTOMA_counts_2 <- NEOTOMA_counts %>%
  dplyr::select(ID_SAMPLE) %>%
  dplyr::bind_cols(
    NEOTOMA_counts %>% # Convert columns with counts to numeric type
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
NEOTOMA_taxa_amalgamation <-
  "data-raw/GLOBAL/Neotoma_extras_SPH.xlsx" %>%
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
NEOTOMA_taxa_counts_amalgamation <-
  NEOTOMA_counts_2 %>%
  dplyr::left_join(NEOTOMA_taxa_amalgamation,
                   by = c("taxon_name")) %>%
  dplyr::relocate(taxon_count, .after = amalgamated) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1) %>%
  dplyr::select(-taxon_name)

### Additional taxonomic corrections (SPH - May 20th) ----
taxonomic_corrections <- "data-raw/GLOBAL/taxonomic_corrections.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  purrr::map_df(stringr::str_squish)

NEOTOMA_taxa_counts_amalgamation_rev <-
  NEOTOMA_taxa_counts_amalgamation %>%
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

waldo::compare(NEOTOMA_taxa_counts_amalgamation,
               NEOTOMA_taxa_counts_amalgamation_rev)
waldo::compare(NEOTOMA_taxa_counts_amalgamation %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               NEOTOMA_taxa_counts_amalgamation_rev %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               max_diffs = Inf)

NEOTOMA_taxa_counts_amalgamation <- NEOTOMA_taxa_counts_amalgamation_rev

NEOTOMA_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated)) %>%
  dplyr::distinct(clean, intermediate, amalgamated)

## Find DOIs ----
NEOTOMA_metadata_pubs <-
  NEOTOMA_metadata %>%
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
# NEOTOMA_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/NEOTOMA_modern-references.csv")

### Load cleaned publications list ----
NEOTOMA_clean_publications <-
  "data-raw/GLOBAL/NEOTOMA_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI)

## Append clean publications ----
NEOTOMA_metadata_2 <-
  NEOTOMA_metadata %>%
  dplyr::left_join(NEOTOMA_metadata_pubs %>%
                     dplyr::select(-DOI, -doi, -dplyr::contains("updated")),
                   by = "publication") %>%
  dplyr::left_join(NEOTOMA_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi, -ID_PUB) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication)

## Extract PNV/BIOME ----
NEOTOMA_metadata_3 <-
  NEOTOMA_metadata_2 %>%
  dplyr::select(-dplyr::starts_with("ID_BIOME")) %>%
  smpds::parallel_extract_biome(cpus = 12) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

NEOTOMA_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * c(0.9, 1.1),
                    ylim = range(.$latitude, na.rm = TRUE) * c(0.9, 1.1))

## Create count tables ----
### Clean ----
NEOTOMA_clean <-
  NEOTOMA_taxa_counts_amalgamation %>%
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
NEOTOMA_intermediate <-
  NEOTOMA_taxa_counts_amalgamation %>%
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
NEOTOMA_amalgamated <-
  NEOTOMA_taxa_counts_amalgamation %>%
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
NEOTOMA <-
  NEOTOMA_metadata_3 %>%
  dplyr::mutate(
    clean = NEOTOMA_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = NEOTOMA_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = NEOTOMA_amalgamated %>%
      dplyr::select(-c(ID_SAMPLE))
  ) %>%
  dplyr::mutate(
    basin_size = basin_size %>%
      stringr::str_replace_all("unknown", "not known"),
    entity_type = entity_type %>%
      stringr::str_replace_all("ccore top", "core top") %>%
      stringr::str_replace_all("unknown", "not known") %>%
      stringr::str_to_lower(),
    site_type = site_type %>%
      stringr::str_replace_all("unknown", "not known")
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = clean)

usethis::use_data(NEOTOMA, overwrite = TRUE, compress = "xz")

# Inspect enumerates ----
### basin_size -----
NEOTOMA$basin_size %>%
  unique() %>% sort()

### site_type ----
NEOTOMA$site_type %>%
  unique() %>% sort()

### entity_type ----
NEOTOMA$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    NEOTOMA %>%
                      dplyr::select(source:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    NEOTOMA %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    NEOTOMA %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    NEOTOMA %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/NEOTOMA_",
                              Sys.Date(),
                              ".xlsx"))

# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/neotoma_climate_reconstructions_2022-04-29.csv" %>%
  readr::read_csv()

# Load daily values for precipitation to compute MAP (mean annual precipitation)
climate_reconstructions_pre <-
  "data-raw/reconstructions/neotoma_climate_reconstructions_pre_2022-04-29.csv" %>%
  readr::read_csv() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE), .before = T1)

climate_reconstructions_2 <- climate_reconstructions %>%
  dplyr::bind_cols(climate_reconstructions_pre %>%
                     dplyr::select(map))

climate_reconstructions_with_counts <-
  NEOTOMA %>%
  # smpds::NEOTOMA %>%
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
waldo::compare(smpds::NEOTOMA,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:map, sn, en, new_elevation))
)

NEOTOMA <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation)
usethis::use_data(NEOTOMA, overwrite = TRUE, compress = "xz")
waldo::compare(smpds::NEOTOMA,
               NEOTOMA,
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
