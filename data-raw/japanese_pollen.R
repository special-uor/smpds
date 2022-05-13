`%>%` <- magrittr::`%>%`
# Load data ----
## Metadata ----
japanese_pollen_metadata <-
  "data-raw/GLOBAL/japanese_pollen/Japan sites.xlsx" %>%
  readxl::read_excel(sheet = 1, n_max = 94) %>%
  janitor::clean_names() %>%
  dplyr::rename(age_BP = age_bp) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .after = doi)

## Pollen counts ----
### File 1 ----
japanese_pollen_taxa_counts_amalgamation_1 <-
  "data-raw/GLOBAL/japanese_pollen/Japan_dat files_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  magrittr::set_names(
    c("entity_name", "clean", "intermediate", "amalgamated", "taxon_count")
  ) %>%
  dplyr::left_join(
    japanese_pollen_metadata %>%
      dplyr::select(entity_name, ID_SAMPLE)
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1) %>%
  dplyr::select(-entity_name) %>%
  dplyr::mutate(
    clean = clean %>% stringr::str_squish(),
    intermediate = intermediate %>% stringr::str_squish(),
    amalgamated = amalgamated %>% stringr::str_squish()
  )

japanese_pollen_taxa_counts_amalgamation_1 %>%
  dplyr::filter(is.na(ID_SAMPLE))

japanese_pollen_metadata %>%
  dplyr::filter(ID_SAMPLE %in%
                  japanese_pollen_taxa_counts_amalgamation_1$ID_SAMPLE)

### File 2 ----
japanese_pollen_taxa_counts_2 <-
  "data-raw/GLOBAL/japanese_pollen/Japan0k_Morita_SPH_clean.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  dplyr::rename(taxon_name = Entity) %>%
  tidyr::pivot_longer(-taxon_name,
                      names_to = "entity_name",
                      values_to = "taxon_count") %>%
  dplyr::arrange(entity_name) %>%
  dplyr::left_join(
    japanese_pollen_metadata %>%
      dplyr::select(entity_name, ID_SAMPLE)
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1) %>%
  dplyr::select(-entity_name) %>%
  dplyr::filter(!is.na(taxon_count))

japanese_pollen_taxa_counts_2 %>%
  dplyr::filter(is.na(ID_SAMPLE))

japanese_pollen_taxa_amalgamations_2 <-
  "data-raw/GLOBAL/japanese_pollen/Japan0k_Morita_SPH_clean.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(
    c("taxon_name", "clean", "intermediate", "amalgamated")
  ) %>%
  purrr::map_df(stringr::str_squish)

japanese_pollen_taxa_counts_amalgamation_2 <-
  japanese_pollen_taxa_counts_2 %>%
  dplyr::left_join(japanese_pollen_taxa_amalgamations_2) %>%
  dplyr::select(-taxon_name) %>%
  dplyr::relocate(taxon_count, .after = amalgamated)

japanese_pollen_taxa_counts_amalgamation_2 %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated)) %>%
  dplyr::distinct(clean, intermediate, amalgamated)

japanese_pollen_metadata %>%
  dplyr::filter(ID_SAMPLE %in%
                  japanese_pollen_taxa_counts_amalgamation_2$ID_SAMPLE)

### File 3 ----
japanese_pollen_taxa_counts_3 <-
  "data-raw/GLOBAL/japanese_pollen/JPND01_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  dplyr::rename(taxon_name = Entity) %>%
  tidyr::pivot_longer(-taxon_name,
                      names_to = "entity_name",
                      values_to = "taxon_count") %>%
  dplyr::arrange(entity_name) %>%
  dplyr::left_join(
    japanese_pollen_metadata %>%
      dplyr::select(entity_name, ID_SAMPLE)
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1) %>%
  dplyr::select(-entity_name) %>%
  dplyr::filter(!is.na(taxon_count))

japanese_pollen_taxa_counts_3 %>%
  dplyr::filter(is.na(ID_SAMPLE))

japanese_pollen_taxa_amalgamations_3 <-
  "data-raw/GLOBAL/japanese_pollen/JPND01_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(
    c("taxon_name", "clean", "intermediate", "amalgamated")
  ) %>%
  purrr::map_df(stringr::str_squish)

japanese_pollen_taxa_counts_amalgamation_3 <-
  japanese_pollen_taxa_counts_3 %>%
  dplyr::left_join(japanese_pollen_taxa_amalgamations_3) %>%
  dplyr::select(-taxon_name) %>%
  dplyr::relocate(taxon_count, .after = amalgamated)

japanese_pollen_taxa_counts_amalgamation_3 %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated)) %>%
  dplyr::distinct(clean, intermediate, amalgamated)

japanese_pollen_metadata %>%
  dplyr::filter(ID_SAMPLE %in%
                  japanese_pollen_taxa_counts_amalgamation_3$ID_SAMPLE)

### File 4 ----
japanese_pollen_taxa_counts_4 <-
  "data-raw/GLOBAL/japanese_pollen/JPND02_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  dplyr::rename(taxon_name = Entity) %>%
  tidyr::pivot_longer(-taxon_name,
                      names_to = "entity_name",
                      values_to = "taxon_count") %>%
  dplyr::arrange(entity_name) %>%
  dplyr::left_join(
    japanese_pollen_metadata %>%
      dplyr::select(entity_name, ID_SAMPLE)
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1) %>%
  dplyr::select(-entity_name) %>%
  dplyr::filter(!is.na(taxon_count))

japanese_pollen_taxa_counts_4 %>%
  dplyr::filter(is.na(ID_SAMPLE))

japanese_pollen_taxa_amalgamations_4 <-
  "data-raw/GLOBAL/japanese_pollen/JPND02_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(
    c("taxon_name", "clean", "intermediate", "amalgamated")
  ) %>%
  purrr::map_df(stringr::str_squish)

japanese_pollen_taxa_counts_amalgamation_4 <-
  japanese_pollen_taxa_counts_4 %>%
  dplyr::left_join(japanese_pollen_taxa_amalgamations_4) %>%
  dplyr::select(-taxon_name) %>%
  dplyr::relocate(taxon_count, .after = amalgamated)

japanese_pollen_taxa_counts_amalgamation_4 %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated)) %>%
  dplyr::distinct(clean, intermediate, amalgamated)

japanese_pollen_metadata %>%
  dplyr::filter(ID_SAMPLE %in%
                  japanese_pollen_taxa_counts_amalgamation_4$ID_SAMPLE)

### File 5 ----
japanese_pollen_taxa_counts_5 <-
  "data-raw/GLOBAL/japanese_pollen/JPND03_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  dplyr::rename(taxon_name = Site) %>%
  tidyr::pivot_longer(-taxon_name,
                      names_to = "entity_name",
                      values_to = "taxon_count") %>%
  dplyr::arrange(entity_name) %>%
  dplyr::left_join(
    japanese_pollen_metadata %>%
      dplyr::select(entity_name, ID_SAMPLE)
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1) %>%
  dplyr::select(-entity_name) %>%
  dplyr::filter(!is.na(taxon_count))

japanese_pollen_taxa_counts_5 %>%
  dplyr::filter(is.na(ID_SAMPLE))

japanese_pollen_taxa_amalgamations_5 <-
  "data-raw/GLOBAL/japanese_pollen/JPND03_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(
    c("taxon_name", "clean", "intermediate", "amalgamated")
  ) %>%
  purrr::map_df(stringr::str_squish)

japanese_pollen_taxa_counts_amalgamation_5 <-
  japanese_pollen_taxa_counts_5 %>%
  dplyr::left_join(japanese_pollen_taxa_amalgamations_5) %>%
  dplyr::select(-taxon_name) %>%
  dplyr::relocate(taxon_count, .after = amalgamated)

japanese_pollen_taxa_counts_amalgamation_5 %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated)) %>%
  dplyr::distinct(clean, intermediate, amalgamated)

japanese_pollen_metadata %>%
  dplyr::filter(ID_SAMPLE %in%
                  japanese_pollen_taxa_counts_amalgamation_5$ID_SAMPLE)

## Combine pollen counts and amalgamations ----
"Check that all the entities in the metadata have pollen counts"
samples_with_counts <-
  dplyr::bind_rows(
    japanese_pollen_taxa_counts_amalgamation_1 %>%
      dplyr::distinct(ID_SAMPLE) %>%
      dplyr::mutate(group = 1),
    japanese_pollen_taxa_counts_amalgamation_2 %>%
      dplyr::distinct(ID_SAMPLE) %>%
      dplyr::mutate(group = 2),
    japanese_pollen_taxa_counts_amalgamation_3 %>%
      dplyr::distinct(ID_SAMPLE) %>%
      dplyr::mutate(group = 3),
    japanese_pollen_taxa_counts_amalgamation_4 %>%
      dplyr::distinct(ID_SAMPLE) %>%
      dplyr::mutate(group = 4),
    japanese_pollen_taxa_counts_amalgamation_5 %>%
      dplyr::distinct(ID_SAMPLE) %>%
      dplyr::mutate(group = 5)
  ) %>%
  dplyr::group_by(ID_SAMPLE) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::arrange(dplyr::desc(n), ID_SAMPLE) %>%
  dplyr::ungroup()

samples_with_counts$ID_SAMPLE %>% range()

all(japanese_pollen_metadata$ID_SAMPLE %in% samples_with_counts$ID_SAMPLE)

japanese_pollen_taxa_counts_amalgamation_all <-
  dplyr::bind_rows(
    japanese_pollen_taxa_counts_amalgamation_1,
    japanese_pollen_taxa_counts_amalgamation_2,
    japanese_pollen_taxa_counts_amalgamation_3,
    japanese_pollen_taxa_counts_amalgamation_4,
    japanese_pollen_taxa_counts_amalgamation_5
  ) %>%
  dplyr::arrange(ID_SAMPLE, clean)

# Find DOIs ----
japanese_pollen_metadata_pubs <-
  japanese_pollen_metadata %>%
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
# japanese_pollen_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/japanese_pollen_modern-references.csv")

## Load cleaned publications list ----
japanese_pollen_clean_publications <-
  "data-raw/GLOBAL/japanese_pollen_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI)

# Append clean publications ----
japanese_pollen_metadata_2 <-
  japanese_pollen_metadata %>%
  dplyr::left_join(japanese_pollen_metadata_pubs %>%
                     dplyr::select(-DOI, -doi, -dplyr::contains("updated")),
                   by = "publication") %>%
  dplyr::left_join(japanese_pollen_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi, -ID_PUB) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication)

# Extract PNV/BIOME ----
japanese_pollen_metadata_3 <-
  japanese_pollen_metadata_2 %>%
  dplyr::select(-dplyr::starts_with("ID_BIOME")) %>%
  smpds::parallel_extract_biome(cpus = 2) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

japanese_pollen_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * c(0.9, 1.1),
                    ylim = range(.$latitude, na.rm = TRUE) * c(0.9, 1.1))

# Create count tables ----
## Clean ----
japanese_pollen_clean <-
  japanese_pollen_taxa_counts_amalgamation_all %>%
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

## Intermediate ----
japanese_pollen_intermediate <-
  japanese_pollen_taxa_counts_amalgamation_all %>%
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

## Amalgamated ----
japanese_pollen_amalgamated <-
  japanese_pollen_taxa_counts_amalgamation_all %>%
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
japanese_pollen <-
  japanese_pollen_metadata_3 %>%
  dplyr::mutate(
    clean = japanese_pollen_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = japanese_pollen_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = japanese_pollen_amalgamated %>%
      dplyr::select(-c(ID_SAMPLE))
  ) %>%
  dplyr::mutate(
    basin_size_old = basin_size,
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
  dplyr::select(-basin_size_num, -basin_size_old)

usethis::use_data(japanese_pollen, overwrite = TRUE, compress = "xz")


# Inspect enumerates ----
## basin_size -----
japanese_pollen$basin_size %>%
  unique() %>%
  sort()

## site_type ----
japanese_pollen$site_type %>%
  unique() %>%
  sort()

## entity_type ----
japanese_pollen$entity_type %>%
  unique() %>%
  sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    japanese_pollen %>%
                      dplyr::select(source:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    japanese_pollen %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    japanese_pollen %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    japanese_pollen %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/japanese_pollen_",
                              Sys.Date(),
                              ".xlsx"))


# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/japanese_pollen_climate_reconstructions_2022-05-12.csv" %>%
  readr::read_csv()

# Load daily values for precipitation to compute MAP (mean annual precipitation)
climate_reconstructions_pre <-
  "data-raw/reconstructions/japanese_pollen_climate_reconstructions_pre_2022-05-12.csv" %>%
  readr::read_csv() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE), .before = T1)

climate_reconstructions_2 <- climate_reconstructions %>%
  dplyr::bind_cols(climate_reconstructions_pre %>%
                     dplyr::select(map))

climate_reconstructions_with_counts <-
  smpds::japanese_pollen %>%
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
waldo::compare(smpds::japanese_pollen,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:map, sn, en, new_elevation))
)

japanese_pollen <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation)
usethis::use_data(japanese_pollen, overwrite = TRUE, compress = "xz")

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
