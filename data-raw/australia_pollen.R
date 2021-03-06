# code to create the australia_pollen dataset
`%>%` <- magrittr::`%>%`
# Load files ----
## File 1 ----
australia_pollen_1_s1 <-
  readxl::read_excel("data-raw/GLOBAL/AUSTRALIA/Post 1900_Australia.xlsx",
                     sheet = 1) %>%
  magrittr::set_names(c("ID_SAMPLE",
                        "clean", "intermediate", "amalgamated",
                        "taxon_count"))
australia_pollen_1_s2 <-
  readxl::read_excel("data-raw/GLOBAL/AUSTRALIA/Post 1900_Australia.xlsx",
                     sheet = 2) %>%
  magrittr::set_names(colnames(.) %>%
                        stringr::str_replace_all("basin size",
                                                 "basin_size")) %>%
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(sample_name = stringr::str_c(
    entity_name,
    "_",
    seq_along(entity_name)
  )) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(doi = doi %>%
                  stringr::str_remove_all("https://|http://|www\\.") %>%
                  stringr::str_remove_all("dx\\.|do\\i.org/"))
australia_pollen_1_s1
australia_pollen_1_s2
# Check the age_BP
australia_pollen_1_s2$age_BP %>% unique() %>% sort()
# Check DOIs
australia_pollen_1_s2$doi %>% unique()

### Missing counts ----
australia_pollen_1_missing_counts <-
  readxl::read_excel("data-raw/GLOBAL/AUSTRALIA/Missing counts_Australia.xlsx",
                     sheet = 1) %>%
  dplyr::select(1:5) %>%
  magrittr::set_names(c(
    "entity_name", "clean", "intermediate", "amalgamated", "counts"
    )) %>%
  dplyr::left_join(
    tibble::tribble(
      ~entity_name, ~ID_SAMPLE,
      "Alexander Morrison NP",    4,
      "Cave Bay Cave_1",        133,
      "Rotten Swamp_8",        7576,
      "Mueller's Rock",        7433
    ),
    by = "entity_name"
  ) %>%
  dplyr::select(-entity_name)

## File 2 ----
australia_pollen_2_s1 <-
  readxl::read_excel("data-raw/GLOBAL/AUSTRALIA/Post 1900_Australia2.xlsx",
                     sheet = 2) %>%
  magrittr::set_names(c("ID_SAMPLE",
                        "clean", "intermediate", "amalgamated",
                        "taxon_count"))
australia_pollen_2_s2 <-
  readxl::read_excel("data-raw/GLOBAL/AUSTRALIA/Post 1900_Australia2.xlsx",
                     sheet = 1) %>%
  magrittr::set_names(colnames(.) %>%
                        stringr::str_replace_all("basin size",
                                                 "basin_size")) %>%
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(sample_name = stringr::str_c(
    entity_name,
    "_",
    seq_along(entity_name)
  )) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(doi = doi %>%
                  stringr::str_remove_all("https://|http://|www\\.") %>%
                  stringr::str_remove_all("dx\\.|do\\i.org/"))
australia_pollen_2_s1
australia_pollen_2_s2
# Check the age_BP
australia_pollen_2_s2$age_BP %>% unique() %>% sort()
# Check DOIs
australia_pollen_2_s2$doi %>% unique()

## File 3 ----
australia_pollen_3_s1 <-
  readxl::read_excel("data-raw/GLOBAL/AUSTRALIA/Post 1900_Australia3.xlsx",
                     sheet = 2) %>%
  magrittr::set_names(c("entity_name", "ID_SAMPLE",
                        "clean", "intermediate", "amalgamated",
                        "taxon_count")) %>%
  dplyr::select(-ID_SAMPLE)
australia_pollen_3_s2 <-
  readxl::read_excel("data-raw/GLOBAL/AUSTRALIA/Post 1900_Australia3.xlsx",
                     sheet = 1) %>%
  magrittr::set_names(colnames(.) %>%
                        stringr::str_replace_all("basin size",
                                                 "basin_size")) %>%
  dplyr::select(-`...13`) %>%
  dplyr::rename(notes = `...14`) %>%
  dplyr::mutate(notes = notes %>%
                  stringr::str_replace_all("samplles", "samples"),
                age_BP = as.character(age_BP)) %>%
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(publication = publication %>%
                  stringr::str_c(collapse = ";\n")) %>%
  dplyr::distinct() %>%
  dplyr::mutate(sample_name = stringr::str_c(
                  entity_name,
                  "_",
                  seq_along(entity_name)
                )) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(doi = doi %>%
                  stringr::str_remove_all("https://|http://|www\\.") %>%
                  stringr::str_remove_all("dx\\.|do\\i.org/"))
australia_pollen_3_s1
australia_pollen_3_s2
# Check the age_BP
australia_pollen_2_s2$age_BP %>% unique() %>% sort()
# Check DOIs
australia_pollen_3_s2$doi %>% unique()

# Combine metadata with counts ----
## File 1 ----
australia_pollen_1_all <- australia_pollen_1_s2 %>%
  dplyr::full_join(
    australia_pollen_1_s1 %>%
      dplyr::bind_rows(australia_pollen_1_missing_counts), # Add missing counts
    by = "ID_SAMPLE")
australia_pollen_1_all %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated))
australia_pollen_1_all %>%
  dplyr::filter(is.na(entity_name))
australia_pollen_1_all %>%
  dplyr::filter(ID_SAMPLE %in% c(133, 7433, 7576))

## File 2 ----
australia_pollen_2_all <- australia_pollen_2_s2 %>%
  dplyr::mutate(sample_name = ifelse(stringr::str_detect(site_name,
                                                         "Fitzerald"),
                                     sample_name %>%
                                       stringr::str_replace_all("_1", "_2"),
                                     sample_name)) %>%
  dplyr::full_join(australia_pollen_2_s1,
                   by = "ID_SAMPLE")
australia_pollen_2_all %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated))
australia_pollen_2_all %>%
  dplyr::filter(is.na(entity_name))

## File 3 ----
australia_pollen_3_all <- australia_pollen_3_s2 %>%
  dplyr::full_join(australia_pollen_3_s1,
                   by = "entity_name")
australia_pollen_3_all %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated))
australia_pollen_3_all %>%
  dplyr::filter(is.na(entity_name))

dplyr::bind_rows(
  australia_pollen_1_s2,
  australia_pollen_2_s2,
  australia_pollen_3_s2
) %>%
  dplyr::arrange(site_name, entity_name) %>%
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::filter(n > 1) %>%
  View()
# Note: there are entities for the site 'Fitzerald River National Park FRNP'
# in two separate files, all have age_BP = 'modern'. To distinguish across them,
# the sample_name values in australia_pollen_2_s2 were updated (see above).

# Combine all the files ----
australia_pollen <- australia_pollen_1_all %>%
  dplyr::filter(!is.na(clean), !is.na(entity_name)) %>%
  dplyr::bind_rows(australia_pollen_2_all, australia_pollen_3_all) %>%
  dplyr::mutate(site_name = site_name %>%
                  stringr::str_squish(),
                entity_name = entity_name %>%
                  stringr::str_squish()) %>%
  dplyr::arrange(site_name, entity_name) %>%
  dplyr::select(-ID_SAMPLE)

# australia_pollen %>%
#   dplyr::distinct(sample_name, .keep_all = TRUE) %>%
#   smpds::plot_climate(var = "elevation")

### Additional taxonomic corrections (SPH - May 20th) ----
taxonomic_corrections <- "data-raw/GLOBAL/taxonomic_corrections.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  purrr::map_df(stringr::str_squish)

australia_pollen_rev <-
  australia_pollen %>%
  dplyr::mutate(ID_COUNT = seq_along(sample_name)) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("clean", "all")),
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

australia_pollen_rev %>%
  dplyr::group_by(ID_COUNT) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::filter(n > 1)

waldo::compare(australia_pollen %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               australia_pollen_rev %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               max_diffs = Inf)

australia_pollen <- australia_pollen_rev %>%
  dplyr::filter(!is.na(taxon_count), taxon_count > 0) %>%
  dplyr::select(-ID_COUNT)

# Extract PNV/BIOME ----
australia_pollen_biomes <- australia_pollen %>%
  dplyr::distinct(sample_name, latitude, longitude) %>%
  smpds::parallel_extract_biome(cpus = 12) %>%
  # smpds::biome_name() %>%
  smpds::pb()

australia_pollen_biomes %>%
  smpds::plot_biome()

australia_pollen_with_pnv <- australia_pollen %>%
  dplyr::left_join(australia_pollen_biomes %>%
                     dplyr::select(sample_name, ID_BIOME),
                   by = c("sample_name")) %>%
  dplyr::relocate(notes, ID_BIOME, .after = doi) #%>%
  # dplyr::group_by(sample_name) %>%
  # dplyr::mutate(ID_SAMPLE = seq_along(sample_name)) %>%
  # dplyr::relocate(ID_SAMPLE, .before = sample_name) %>%
  # dplyr::ungroup()

# Create count tables ----
## Clean ----
australia_pollen_clean <- australia_pollen_with_pnv %>%
  dplyr::select(-intermediate, -amalgamated) %>%
  dplyr::rename(taxon_name = clean) %>%
  dplyr::group_by(sample_name, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(site_name:sample_name,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     values_fill = 0,
                     names_sort = TRUE)
## Intermediate ----
australia_pollen_intermediate <- australia_pollen_with_pnv %>%
  dplyr::select(-clean, -amalgamated) %>%
  dplyr::rename(taxon_name = intermediate) %>%
  dplyr::group_by(sample_name, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(site_name:sample_name,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     values_fill = 0,
                     names_sort = TRUE)
## Amalgamated ----
australia_pollen_amalgamated <- australia_pollen_with_pnv %>%
  dplyr::select(-clean, -intermediate) %>%
  dplyr::rename(taxon_name = amalgamated) %>%
  dplyr::group_by(sample_name, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(site_name:sample_name,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     values_fill = 0,
                     names_sort = TRUE)

# Find missing elevations ----
australia_pollen_2022_04_11 <-
  "~/Downloads/ready to upload/australia_pollen_2022-04-11.xlsx" %>%
  readxl::read_excel()

australia_pollen_2022_04_11_missing_elevations <-
  australia_pollen_2022_04_11 %>%
  dplyr::filter(is.na(elevation)) %>%
  smpds:::get_elevation(cpus = 2)

australia_pollen_2022_04_11 %>%
  dplyr::left_join(australia_pollen_2022_04_11_missing_elevations %>%
                     dplyr::select(ID_SAMPLE,
                                   new_elevation = elevation),
                   by = "ID_SAMPLE") %>%
  # dplyr::filter(elevation != new_elevation | is.na(elevation)) %>%
  # dplyr::select(site_name:elevation, new_elevation)
  dplyr::mutate(elevation = dplyr::coalesce(elevation, new_elevation)) %>%
  dplyr::select(-new_elevation) %>%
  readr::write_csv(
    "~/Downloads/ready to upload/australia_pollen_2022-04-11_with_elevations.csv",
    na = ""
    )

australia_pollen_base <- australia_pollen_clean %>%
  dplyr::select(site_name:sample_name) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(sample_name), .before = sample_name)

# Store subsets ----
australia_pollen <-
  australia_pollen_base %>%
  # australia_pollen_clean %>%
  # dplyr::select(site_name:sample_name) %>%
  dplyr::mutate(
    clean = australia_pollen_clean %>%
      dplyr::select(-c(site_name:sample_name)),
    intermediate = australia_pollen_intermediate %>%
      dplyr::select(-c(site_name:sample_name)),
    amalgamated = australia_pollen_amalgamated %>%
      dplyr::select(-c(site_name:sample_name))
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
      stringr::str_replace_all("unknown", "not known"),
    site_type = site_type %>%
      stringr::str_replace_all("Cave", "cave") %>%
      stringr::str_replace_all("drained/dry lake|Drained/dry lake",
                               "lacustrine, drained lake") %>%
      stringr::str_replace_all("estuarine|Estuarine",
                               "coastal, estuarine") %>%
      stringr::str_replace_all("Cave", "cave") %>%
      stringr::str_replace_all("terrestrial, other sediments",
                               "terrestrial") %>%
      stringr::str_replace_all("terrestrial, soil", "soil") %>%
      stringr::str_replace_all("unknown", "not known")
  ) %>%
  dplyr::mutate(source = "Australian pollen", .before = 1) %>%
  dplyr::select(-dplyr::contains("notes")) %>%
  dplyr::select(-basin_size_num)

usethis::use_data(australia_pollen, overwrite = TRUE, compress = "xz")

# Check enumerates ----
## basin_size -----
australia_pollen$basin_size %>%
  unique() %>%
  sort()

## site_type -----
australia_pollen$site_type %>%
  unique() %>%
  sort()

## entity_type -----
australia_pollen$entity_type %>%
  unique() %>%
  sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    australia_pollen %>%
                      dplyr::select(site_name:sample_name))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    australia_pollen %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    australia_pollen %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    australia_pollen %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/AUSTRALIA/australia_pollen_",
                              Sys.Date(),
                              ".xlsx"))

# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/australia_pollen_climate_reconstructions_2022-04-30.csv" %>%
  readr::read_csv()

# Load daily values for precipitation to compute MAP (mean annual precipitation)
climate_reconstructions_pre <-
  "data-raw/reconstructions/australia_pollen_climate_reconstructions_pre_2022-04-30.csv" %>%
  readr::read_csv() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE), .before = T1)

climate_reconstructions_2 <- climate_reconstructions %>%
  dplyr::bind_cols(climate_reconstructions_pre %>%
                     dplyr::select(map)) %>%
  dplyr::filter(entity_name %in% australia_pollen$entity_name)

climate_reconstructions_with_counts <-
  australia_pollen %>%
  # smpds::australia_pollen %>%
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
waldo::compare(smpds::australia_pollen,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:map, sn, en, new_elevation))
)

australia_pollen <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation) %>%
  dplyr::select(-dplyr::starts_with("notes"))
usethis::use_data(australia_pollen, overwrite = TRUE, compress = "xz")
waldo::compare(smpds::australia_pollen,
               australia_pollen,
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
