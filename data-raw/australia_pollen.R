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
  dplyr::ungroup()
australia_pollen_1_s1
australia_pollen_1_s2
# Check the age_BP
australia_pollen_1_s2$age_BP %>% unique() %>% sort()

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
  dplyr::ungroup()
australia_pollen_2_s1
australia_pollen_2_s2
# Check the age_BP
australia_pollen_2_s2$age_BP %>% unique() %>% sort()

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
                  stringr::str_c(collapse = ";\n"),
                sample_name = stringr::str_c(
                  entity_name,
                  "_",
                  seq_along(entity_name)
                )) %>%
  dplyr::ungroup() %>%
  dplyr::distinct()
australia_pollen_3_s1
australia_pollen_3_s2
# Check the age_BP
australia_pollen_2_s2$age_BP %>% unique() %>% sort()

# Combine metadata with counts ----
## File 1 ----
australia_pollen_1_all <- australia_pollen_1_s2 %>%
  dplyr::full_join(australia_pollen_1_s1,
                   by = "ID_SAMPLE")
australia_pollen_1_all %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated))
australia_pollen_1_all %>%
  dplyr::filter(is.na(entity_name))
australia_pollen_1_s1 %>%
  dplyr::filter(ID_SAMPLE %in% c(4, 133, 7433, 7576))

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
# in to separate files, all have age_BP = 'modern'. To distinguish across them,
# the sample_name values in australia_pollen_2_s2 were updated (see above).

# Combine all the files ----
australia_pollen <- australia_pollen_1_all %>%
  dplyr::filter(!is.na(clean), !is.na(entity_name)) %>%
  dplyr::bind_rows(australia_pollen_2_all, australia_pollen_3_all) %>%
  # dplyr::group_by(entity_name, ID_SAMPLE) %>%
  # dplyr::mutate(sample_name = stringr::str_c(
  #   entity_name,
  #   "_",
  #   seq_along(entity_name)
  # )) %>%
  # dplyr::ungroup() %>%
  dplyr::mutate(site_name = site_name %>%
                  stringr::str_squish(),
                entity_name = entity_name %>%
                  stringr::str_squish()) %>%
  dplyr::arrange(site_name, entity_name) %>%
  dplyr::select(-ID_SAMPLE)
australia_pollen %>%
  dplyr::distinct(sample_name, .keep_all = TRUE) %>%
  smpds::plot_climate(var = "elevation")

# Extract PNV/BIOME ----
australia_pollen_biomes <- australia_pollen %>%
  dplyr::distinct(sample_name, latitude, longitude) %>%
  smpds::parallel_extract_biome(cpus = 12) %>%
  smpds::pb()

australia_pollen_biomes %>%
  smpds::plot_biome()

australia_pollen_with_pnv <- australia_pollen %>%
  dplyr::left_join(australia_pollen_biomes %>%
                     dplyr::select(sample_name, ID_BIOME),
                   by = c("sample_name")) %>%
  dplyr::relocate(notes, ID_BIOME, .after = doi)

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
                     names_sort = TRUE)
## Amalgamated
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
                     names_sort = TRUE)

# Climate reconstructions ----
path_to_cru_ts <- "~/OneDrive - University of Reading/UoR/Data/CRU/4.04/"
CPUS <- 1
australia_pollen_base <- australia_pollen_clean %>%
  dplyr::select(site_name:sample_name)
## Interpolate climate from the CRU TS dataset
## Cloud coverage ----
# ncin <- file.path(path_to_cru_ts,
#                   "cru_ts4.04.1901.2019.cld.dat-clim-1961-1990-int.nc") %>%
#   ncdf4::nc_open()
# cld_ref_nc <- ncdf4::ncvar_get(ncin, varid = "cld")
# ncdf4::nc_close(ncin)
australia_pollen_base_cld <- australia_pollen_base %>%
  dplyr::slice(1:10) %>%
  smpds::gwr(.ref = cld_ref_nc,
             cpus = 1) %>%
  progressr::with_progress()

australia_pollen_base_pre <- australia_pollen_base %>%
  dplyr::slice(1:10) %>%
  smpds::gwr(varid = "pre",
             .ref = file.path(path_to_cru_ts,
                              "cru_ts4.04.1901.2019.pre.dat-new-clim-1961-1990-int.nc"),
             cpus = CPUS)

australia_pollen_base_tmp <- australia_pollen_base %>%
  smpds::gwr(varid = "tmp",
             .ref = file.path(path_to_cru_ts,
                              "cru_ts4.04-clim-1961-1990-daily.tmp.nc"),
             cpus = CPUS)

## Transform climate reconstructions
australia_pollen_base_cld2 <- australia_pollen_base_cld %>%
  smpds::pivot_data(varname = "cld")
### Calculate sunshine fraction from cloud cover
australia_pollen_base_sf <- australia_pollen_base_cld %>%
  smpds::pivot_data(scale = -0.01, add = 1, varname = "sf")
australia_pollen_base_pre2 <- australia_pollen_base_pre %>%
  smpds::pivot_data(varname = "pre")
australia_pollen_base_tmp2 <- australia_pollen_base_tmp %>%
  smpds::pivot_data(varname = "tmp")

australia_pollen_basev2 <- australia_pollen_base_sf %>%
  dplyr::left_join(australia_pollen_base_pre2) %>%
  dplyr::left_join(australia_pollen_base_tmp2)

## Reconstruct climate variables
australia_pollen_basev3 <- australia_pollen_basev2 %>%
  smpds::mi(cpus = CPUS) %>%
  smpds::gdd() %>%
  smpds::mat() %>%
  smpds::mtco() %>%
  smpds::mtwa() %>%
  progressr::with_progress()

# Store subsets ----
australia_pollen <- australia_pollen_clean %>%
  dplyr::select(site_name:sample_name) %>%
  dplyr::mutate(
    clean = australia_pollen_clean %>%
      dplyr::select(-c(site_name:sample_name)),
    intermediate = australia_pollen_intermediate %>%
      dplyr::select(-c(site_name:sample_name)),
    amalgamated = australia_pollen_amalgamated %>%
      dplyr::select(-c(site_name:sample_name))
  ) %>%
  dplyr::mutate(
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
  )

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
