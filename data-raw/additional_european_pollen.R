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

# # Find DOIs ----
# additional_european_pollen_metadata_pubs <-
#   additional_european_pollen_metadata %>%
#   dplyr::distinct(publication, doi) %>%
#   dplyr::arrange(publication) %>%
#   dplyr::mutate(DOI = publication %>%
#                   stringr::str_extract_all("\\[DOI\\s*(.*?)\\s*\\](;|$)") %>%
#                   purrr::map_chr(~.x %>%
#                                    stringr::str_remove_all("^\\[DOI:") %>%
#                                    stringr::str_remove_all("\\]\\s*;\\s*$") %>%
#                                    stringr::str_remove_all("\\]$") %>%
#                                    stringr::str_remove_all("doi:") %>%
#                                    stringr::str_squish() %>%
#                                    stringr::str_c(collapse = ";\n"))
#   ) %>%
#   dplyr::mutate(ID_PUB = seq_along(publication)) %>%
#   dplyr::mutate(updated_publication = NA, .before = publication) %>%
#   dplyr::mutate(updated_DOI = NA, .before = DOI)
# # additional_european_pollen_metadata_pubs %>%
# #   readr::write_excel_csv("data-raw/GLOBAL/additional_european_pollen_modern-references.csv")
#
# ## Load cleaned publications list ----
# additional_european_pollen_clean_publications <-
#   "data-raw/GLOBAL/additional_european_pollen_modern-references_clean.csv" %>%
#   readr::read_csv() %>%
#   dplyr::select(-DOI)
#
# # Append clean publications ----
# additional_european_pollen_metadata_2 <-
#   additional_european_pollen_metadata %>%
#   dplyr::left_join(additional_european_pollen_metadata_pubs %>%
#                      dplyr::select(-DOI, -doi, -dplyr::contains("updated")),
#                    by = "publication") %>%
#   dplyr::left_join(additional_european_pollen_clean_publications,
#                    by = "ID_PUB") %>%
#   dplyr::select(-publication.x, -publication.y, -doi, -ID_PUB) %>%
#   dplyr::rename(doi = updated_DOI,
#                 publication = updated_publication)

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
                     names_sort = TRUE) %>%
  dplyr::arrange(ID_SAMPLE)

# Extract missing elevations ----
# additional_european_pollen_metadata_4 <-
#   additional_european_pollen_metadata_3 %>%
#   dplyr::rename(elevation_original = elevation) %>%
#   smpds:::get_elevation(cpus = 12)
#
# additional_european_pollen_metadata_4 %>%
#   dplyr::select(ID_SAMPLE, entity_name, latitude, longitude, elevation_new = elevation, elevation_original) %>%
#   dplyr::mutate(diff =
#                   abs(elevation_new - elevation_original) / elevation_original) %>%
#   # readr::write_csv("data-raw/GLOBAL/additional_european_pollen_elevations_only.csv", na = "")
#   dplyr::filter(diff >= 0.5)
# dplyr::mutate(within_90p =
#                 dplyr::between(elevation,
#                                min(c(0.9, 1.1) * elevation_original),
#                                max(c(0.9, 1.1) * elevation_original)),
#               within_95p =
#                 dplyr::between(elevation,
#                                min(c(0.95, 1.05) * elevation_original),
#                                max(c(0.95, 1.05) * elevation_original)),
#               within_975p =
#                 dplyr::between(elevation,
#                                min(c(0.975, 1.025) * elevation_original),
#                                max(c(0.975, 1.025) * elevation_original))
# ) %>%
#   dplyr::filter(!within_975p)

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

usethis::use_data(additional_european_pollen, overwrite = TRUE, compress = "xz")

# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/additional_european_pollen_climate_reconstructions_2022-04-30.csv" %>%
  readr::read_csv()

climate_reconstructions_with_counts <- smpds::additional_european_pollen %>%
  dplyr::bind_cols(
    climate_reconstructions %>%
      dplyr::select(sn = site_name,
                    en = entity_name,
                    new_elevation = elevation,
                    mi:mtwa)
  ) %>%
  dplyr::relocate(mi:mtwa, .before = clean) %>%
  dplyr::mutate(elevation = dplyr::coalesce(elevation, new_elevation))
climate_reconstructions_with_counts %>%
  dplyr::filter(site_name != sn | entity_name != en)
waldo::compare(smpds::additional_european_pollen,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:mtwa))
)
additional_european_pollen <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation)
usethis::use_data(additional_european_pollen, overwrite = TRUE, compress = "xz")

climate_reconstructions %>%
  smpds::plot_climate_countour(
    var = "mat",
    xlim = range(.$longitude, na.rm = TRUE),
    ylim = range(.$latitude, na.rm = TRUE)
  )

# Inspect enumerates ----
## basin_size -----
additional_european_pollen$basin_size %>%
  unique() %>%
  sort()

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
