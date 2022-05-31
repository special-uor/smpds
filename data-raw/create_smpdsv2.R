# Setup ---
CPUS <- 8
path <- "/storage/shared/research/met/pacmedy/roberto.villegasdiaz/smpds"
output_path <- "/storage/shared/research/met/pacmedy/roberto.villegasdiaz/smpds"
`%>%` <- magrittr::`%>%`

# Create dataset -----
SMPDSv2 <- dplyr::bind_rows(
  smpds::additional_european_pollen,
  smpds::AMSS,
  smpds::APD,
  smpds::australia_pollen,
  smpds::CMPD,
  smpds::dugerdil_pollen,
  smpds::EMBSeCBIO,
  smpds::EMPDv2,
  smpds::gaillard_pollen,
  smpds::Herzschuh,
  smpds::IbMPD,
  smpds::japanese_pollen,
  smpds::moroccan_pollen,
  smpds::NEOTOMA,
  smpds::neotropics_pollen,
  smpds::north_america_pollen,
  smpds::Phelps,
  smpds::SMPDSv1,
  smpds::south_america_pollen,
  smpds::southern_hemisphere_pollen,
  smpds::tatiana_pollen
)

SMPDSv2 <- SMPDSv2 %>%
  smpds::biome_name() %>%
  dplyr::rename(PNV = description) %>%
  dplyr::relocate(PNV, .after = ID_BIOME) %>%
  dplyr::select(-colour, -sample_name, -ID_SAMPLE, -ID_PUB) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .before = clean)

# Categorical variable clean ups ----
categorical_variables_mapping <-
  "data-raw/GLOBAL/smpdsv2_categorical_variables_counts_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1, skip = 1) %>%
  magrittr::set_names(c(
    "column", "value", "n_entities", "new_value"
  )) %>%
  dplyr::mutate(value2 = dplyr::coalesce(new_value, value) %>%
                  stringr::str_squish()) %>%
  dplyr::filter(column != "source")

SMPDSv2 <-
  SMPDSv2 %>%
  dplyr::mutate(
    site_type = tibble::tibble(site_type) %>%
      dplyr::left_join(
        categorical_variables_mapping %>%
          dplyr::filter(column == "site_type"),
        by = c("site_type" = "value")
      ) %>%
      .$value2,
    entity_type = tibble::tibble(entity_type) %>%
      dplyr::left_join(
        categorical_variables_mapping %>%
          dplyr::filter(column == "entity_type"),
        by = c("entity_type" = "value")
      ) %>%
      .$value2
  )

# ## Arrange taxon_counts ----
# SMPDSv2 %>%
#   dplyr::select(ID_SAMPLE, clean) %>%
#   tidyr::unnest(clean) %>%
#   tidyr::pivot_longer(-ID_SAMPLE) %>%
#   tidyr::pivot_wider(ID_SAMPLE, names_sort = TRUE, values_fill = 0)

waldo::compare(smpds::SMPDSv2[1:19], SMPDSv2[1:19],
               tolerance = 1E-4, max_diffs = Inf)

# Remove climate and PNV reconstructions for marine entities ----
SMPDSv2_marine_entities_corrected <- SMPDSv2 %>%
  dplyr::mutate(
    ID_BIOME =
      ifelse(!is.na(site_type) & site_type == "marine", NA, ID_BIOME),
    PNV =
      ifelse(!is.na(site_type) & site_type == "marine", "not applicable", PNV),
    mi =
      ifelse(!is.na(site_type) & site_type == "marine", NA, mi),
    gdd0 =
      ifelse(!is.na(site_type) & site_type == "marine", NA, gdd0),
    mat =
      ifelse(!is.na(site_type) & site_type == "marine", NA, mat),
    mtco =
      ifelse(!is.na(site_type) & site_type == "marine", NA, mtco),
    mtwa =
      ifelse(!is.na(site_type) & site_type == "marine", NA, mtwa),
    map =
      ifelse(!is.na(site_type) & site_type == "marine", NA, map)
  )
waldo::compare(SMPDSv2, SMPDSv2_marine_entities_corrected, max_diffs = Inf)
SMPDSv2_marine_entities_corrected %>%
  dplyr::filter(is.na(ID_BIOME)) %>%
  View()
SMPDSv2 <- SMPDSv2_marine_entities_corrected
usethis::use_data(SMPDSv2, overwrite = TRUE, compress = "xz")

# Update reconstructions for dodgy entities ----
dodgy_entities <-
  "data-raw/smpdsv2_sites_with_dodgy_climate_reconstructions_2022-05-25_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1)

climate_reconstructions <-
  "data-raw/reconstructions/smpdsv2_sites_with_dodgy_climate_reconstructions_climate_reconstructions_2022-05-30.csv" %>%
  readr::read_csv()

# Load daily values for precipitation to compute MAP (mean annual precipitation)
climate_reconstructions_pre <-
  "data-raw/reconstructions/smpdsv2_sites_with_dodgy_climate_reconstructions_climate_reconstructions_pre_2022-05-30.csv" %>%
  readr::read_csv() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE), .before = T1)
climate_reconstructions_2 <- climate_reconstructions %>%
  dplyr::bind_cols(climate_reconstructions_pre %>%
                     dplyr::select(map, ID_BIOME, PNV))
View(climate_reconstructions_2)

# Remove reconstructed values where mi < 0 (not accurate reconstructions)
climate_reconstructions_3 <- climate_reconstructions_2 %>%
  dplyr::mutate(
    mi = ifelse(is.na(mi) | mi < 0, NA, mi),
    gdd0 = ifelse(is.na(mi) | mi < 0, NA, gdd0),
    mat = ifelse(is.na(mi) | mi < 0, NA, mat),
    mtco = ifelse(is.na(mi) | mi < 0, NA, mtco),
    mtwa = ifelse(is.na(mi) | mi < 0, NA, mtwa),
    map = ifelse(is.na(mi) | mi < 0, NA, map)
  )
View(climate_reconstructions_3)

## Update the main dataset ----
climate_reconstructions_3 %>%
  dplyr::bind_cols(
    dodgy_entities %>%
      dplyr::select(source, age_BP, ID_SAMPLE)
  )

SMPDSv2_with_climate_corrections <- SMPDSv2 %>%
  dplyr::left_join(
    climate_reconstructions_3 %>%
      dplyr::distinct() %>%
      magrittr::set_names(paste0("new_", colnames(.))),
    by = c("site_name" = "new_site_name", "entity_name" = "new_entity_name")
  )

SMPDSv2_with_climate_corrections_2 <-
  SMPDSv2_with_climate_corrections %>%
  dplyr::mutate(
    mi = ifelse(!is.na(new_latitude), NA, mi),
    gdd0 = ifelse(!is.na(new_latitude), NA, gdd0),
    mat = ifelse(!is.na(new_latitude), NA, mat),
    mtco = ifelse(!is.na(new_latitude), NA, mtco),
    mtwa = ifelse(!is.na(new_latitude), NA, mtwa),
    map = ifelse(!is.na(new_latitude), NA, map),
    ID_BIOME = ifelse(!is.na(new_latitude), NA, ID_BIOME),
    PNV = ifelse(!is.na(new_latitude), NA, PNV)
  ) %>%
  dplyr::mutate(
    latitude = dplyr::coalesce(new_latitude, latitude),
    longitude = dplyr::coalesce(new_longitude, longitude),
    elevation = dplyr::coalesce(new_elevation, elevation),
    mi = dplyr::coalesce(new_mi, mi),
    gdd0 = dplyr::coalesce(new_gdd0, gdd0),
    mat = dplyr::coalesce(new_mat, mat),
    mtco = dplyr::coalesce(new_mtco, mtco),
    mtwa = dplyr::coalesce(new_mtwa, mtwa),
    map = dplyr::coalesce(new_map, map),
    ID_BIOME = dplyr::coalesce(new_ID_BIOME, ID_BIOME),
    PNV = dplyr::coalesce(new_PNV, PNV)
  ) #%>%
  # dplyr::filter(!is.na(new_latitude)) %>%
  # dplyr::select(-clean, -intermediate, -amalgamated) %>%
  # View()
  # dplyr::filter(!is.na(new_latitude)) %>% View()

waldo::compare(climate_reconstructions_3 %>%
                 dplyr::arrange(site_name, entity_name),
               SMPDSv2_with_climate_corrections_2 %>%
                 dplyr::filter(!is.na(new_latitude)) %>%
                 dplyr::arrange(site_name, entity_name) %>%
                 dplyr::select(site_name:elevation, mi:map, ID_BIOME:PNV))
waldo::compare(SMPDSv2,
               SMPDSv2_with_climate_corrections_2 %>%
                 dplyr::select(-dplyr::starts_with("new_")),
               max_diffs = Inf)
SMPDSv2_with_climate_corrections_2 %>%
  dplyr::filter(is.na(ID_BIOME)) %>%
  View()
SMPDSv2 <- SMPDSv2_with_climate_corrections_2 %>%
  dplyr::select(-dplyr::starts_with("new_"))
usethis::use_data(SMPDSv2, overwrite = TRUE, compress = "xz")

# Update citation for Harrison et al., 2022 ----
`%>%` <- magrittr::`%>%`
data("SMPDSv2", package = "smpds")
SMPDSv2_updated <- SMPDSv2 %>%
  dplyr::mutate(
    # source = ifelse(source == "Harrison et al., 2021",
    #                 "Harrison et al., 2022",
    #                 source),
    publication =
      ifelse(source == "Harrison et al., 2021",
             "Harrison, S.P., Villegas-Diaz, R., Cruz-Silva, E., Gallagher, D., Kesner, D., Lincoln, P., Shen, Y., Sweeney, L., Colombaroli, D., Ali, A., Barhoumi, C., Bergeron, Y., Blyakharchuk, T., Bobek, P., Bradshaw, R., Clear, J.L., Czerwiński, S., Daniau, A-L., Dodson, J., Edwards, K.J., Edwards, M.E., Feurdean, A., Foster, D., Gajewski, K., Gałka, M., Garneau, M., Giesecke, T.,  Gil Romera, G., Girardin, M.P., Hoefer, D., Huang, K., Inoue, J., Jamrichová, E., Jasiunas, N., Jiang, W., Jiménez-Moreno, G., Karpińska-Kołaczek, M., Kołaczek, P., Kuosmanen, N., Lamentowicz, M., Lavoie, M., Li, F., Li, J., Lisitsyna, O., López-Sáez, J.A., Luelmo-Lautenschlaeger, R., Magnan, G., Magyari, E.K., Maksims, A., Marcisz, K., Marinova, E., Marlon, J., Mensing, S., Miroslaw-Grabowska, J., Oswald, W., Pérez-Díaz, S., Pérez-Obiol, R., Piilo, S., Poska, A., Qin, X., Remy, C.C., Richard, P.J.H., Salonen, S., Sasaki, N., Schneider, H., Shotyk, W., Stancikaite, M., Šteinberga, D., Stivrins, N., Takahara, H., Tan, Z., Trasune, L., Umbanhowar, C.E., Väliranta, M., Vassiljev, J., Xiao, X., Xu, Q., Xu, X., Zawisza, E., Zhao, Y., Zhou, Z., Paillard, J., 2021/2022. The Reading Palaeofire database: an expanded global resource to document changes in fire regimes from sedimentary charcoal records Earth System Science Data Discussions https://doi.org/10.5194/essd-2021-272. Earth System Science Data 14: 1109-1124 https://doi.org/10.5194/essd-14-1109-2022",
             publication),
    doi =
      ifelse(source == "Harrison et al., 2021",
             "10.5194/essd-2021-272",
             doi),
  )

waldo::compare(SMPDSv2, SMPDSv2_updated)
SMPDSv2 <- SMPDSv2_updated
usethis::use_data(SMPDSv2, overwrite = TRUE, compress = "xz")

# Export list of amalgamations ----
## Additional taxonomic corrections (SPH - May 20th) ----
taxonomic_corrections <- "data-raw/GLOBAL/taxonomic_corrections.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  purrr::map_df(stringr::str_squish)

## additional_european_pollen ----
additional_european_pollen_taxa_counts_amalgamation <-
  "data-raw/GLOBAL/E_additional_Europe_clean.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(
    c("entity_name", "clean", "intermediate", "amalgamated", "taxon_count")
  ) %>%
  dplyr::select(-entity_name, -taxon_count) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "additional_european_pollen")

## AMSS ----
african_modern_samples_counts <-
  "data-raw/GLOBAL/African modern surface samples_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  janitor::clean_names() %>%
  dplyr::select(-entity_name,
                -original_taxon_name,
                -counts) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "AMSS")

## APD ----
APD_taxa_amalgamation <-
  "data-raw/GLOBAL/APD_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "APD")

## australia_pollen ----
### File 1 ----
australia_pollen_1_s1 <-
  readxl::read_excel("data-raw/GLOBAL/AUSTRALIA/Post 1900_Australia.xlsx",
                     sheet = 1) %>%
  magrittr::set_names(c("ID_SAMPLE",
                        "clean", "intermediate", "amalgamated",
                        "taxon_count")) %>%
  dplyr::select(-ID_SAMPLE, -taxon_count) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "australia_pollen")

### File 2 ----
australia_pollen_2_s1 <-
  readxl::read_excel("data-raw/GLOBAL/AUSTRALIA/Post 1900_Australia2.xlsx",
                     sheet = 2) %>%
  magrittr::set_names(c("ID_SAMPLE",
                        "clean", "intermediate", "amalgamated",
                        "taxon_count")) %>%
  dplyr::select(-ID_SAMPLE, -taxon_count) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "australia_pollen")

### File 3 ----
australia_pollen_3_s1 <-
  readxl::read_excel("data-raw/GLOBAL/AUSTRALIA/Post 1900_Australia3.xlsx",
                     sheet = 2) %>%
  magrittr::set_names(c("entity_name", "ID_SAMPLE",
                        "clean", "intermediate", "amalgamated",
                        "taxon_count")) %>%
  dplyr::select(-ID_SAMPLE, -entity_name, -taxon_count) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "australia_pollen")

## CMPD ----
CMPD_taxa_amalgamation <-
  "data-raw/GLOBAL/CMPD_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::mutate(clean = clean %>% stringr::str_squish(),
                intermediate = intermediate %>% stringr::str_squish(),
                amalgamated = amalgamated %>% stringr::str_squish()) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "CMPD")

## dugerdil_pollen ----
dugerdil_taxa_amalgamation <-
  "data-raw/GLOBAL/Dugerdil_SPH.xlsx" %>%
  readxl::read_excel(sheet = 3) %>%
  magrittr::set_names(c(
    "taxon_name", "clean", "intermediate", "amalgamated"
  )) %>%
  purrr::map_dfc(~.x %>% stringr::str_squish()) %>%
  dplyr::select(-taxon_name) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "dugerdil_pollen")

## EMBSeCBIO ----
EMBSeCBIO_taxa_amalgamation <-
  "data-raw/GLOBAL/D_embsecbio_records_additions_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "EMBSeCBIO")

## EMPDv2 ----
EMPDv2_taxa_amalgamation <-
  "data-raw/GLOBAL/EMPDv2_only_SPH_clean_no dups.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "taxon_name", "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::select(-taxon_name) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "EMPDv2")

## gaillard_pollen ----
gaillard_samples_taxa_amalgamation <-
  "data-raw/GLOBAL/Gaillard et al_SPH.xlsx" %>%
  readxl::read_excel(sheet = 3) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "gaillard_pollen")

## Herzschuh ----
Herzschuh_taxa_amalgamation <-
  "data-raw/GLOBAL/Herzschuh_clean_no dups_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "Herzschuh")

## IbMPD ----
IbMPD_taxa_counts_amalgamation <-
  "data-raw/GLOBAL/C_Iberian data_clean.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  janitor::clean_names() %>%
  dplyr::select(-entity, -taxon_count) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "IbMPD")

## japanese_pollen ----
### File 1 ----
japanese_pollen_taxa_amalgamations_1 <-
# japanese_pollen_taxa_counts_amalgamation_1 <-
  "data-raw/GLOBAL/japanese_pollen/Japan_dat files_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  magrittr::set_names(
    c("entity_name", "clean", "intermediate", "amalgamated", "taxon_count")
  ) %>%
  dplyr::select(-entity_name, -taxon_count) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "japanese_pollen")

### File 2 ----
japanese_pollen_taxa_amalgamations_2 <-
  "data-raw/GLOBAL/japanese_pollen/Japan0k_Morita_SPH_clean.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(
    c("taxon_name", "clean", "intermediate", "amalgamated")
  ) %>%
  dplyr::select(-taxon_name) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "japanese_pollen")

### File 3 ----
japanese_pollen_taxa_amalgamations_3 <-
  "data-raw/GLOBAL/japanese_pollen/JPND01_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(
    c("taxon_name", "clean", "intermediate", "amalgamated")
  ) %>%
  dplyr::select(-taxon_name) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "japanese_pollen")

### File 4 ----
japanese_pollen_taxa_amalgamations_4 <-
  "data-raw/GLOBAL/japanese_pollen/JPND02_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(
    c("taxon_name", "clean", "intermediate", "amalgamated")
  ) %>%
  dplyr::select(-taxon_name) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "japanese_pollen")

### File 5 ----
japanese_pollen_taxa_amalgamations_5 <-
  "data-raw/GLOBAL/japanese_pollen/JPND03_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(
    c("taxon_name", "clean", "intermediate", "amalgamated")
  ) %>%
  dplyr::select(-taxon_name) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "japanese_pollen")

## moroccan_pollen ----
moroccan_coretops_taxa_amalgamation <-
  "data-raw/GLOBAL/Moroccan core tops_SPH.xlsx" %>%
  readxl::read_excel(sheet = 3) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::distinct() %>%
  dplyr::mutate(source = "moroccan_pollen")

## neotoma_north_america_pollen ----
neotoma_north_america_pollen_taxa_amalgamation <-
  "data-raw/GLOBAL/neotoma_north_america_modern_2022-04-05_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  magrittr::set_names(c(
    "original", "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::select(-original) %>%
  dplyr::distinct() %>%
  purrr::map_df(~.x %>% stringr::str_squish()) %>%
  dplyr::mutate(source = "neotoma_north_america_pollen")

neotoma_north_america_pollen_surface_taxa_amalgamation <-
  "data-raw/GLOBAL/neotoma_north_america_modern_2022-04-20_pollen_surface_only_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  magrittr::set_names(c(
    "original", "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::select(-original) %>%
  dplyr::distinct() %>%
  purrr::map_df(~.x %>% stringr::str_squish()) %>%
  dplyr::mutate(source = "neotoma_north_america_pollen")

## neotoma_south_america_pollen ----
neotoma_south_america_pollen_taxa_amalgamation <-
  readxl::read_excel("data-raw/GLOBAL/neotoma_south_america_pollen_modern_SPH.xlsx",
                     sheet = 3) %>%
  janitor::clean_names() %>%
  dplyr::distinct() %>%
  purrr::map_df(~.x %>% stringr::str_squish()) %>%
  dplyr::mutate(source = "neotoma_south_america_pollen")

## neotoma_extra ----
NEOTOMA_taxa_amalgamation <-
  "data-raw/GLOBAL/Neotoma_extras_SPH.xlsx" %>%
  readxl::read_excel(sheet = 3) %>%
  magrittr::set_names(c(
    "taxon_name", "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::select(-taxon_name) %>%
  dplyr::distinct() %>%
  purrr::map_df(~.x %>% stringr::str_squish()) %>%
  dplyr::mutate(source = "neotoma_extra")

## neotropics_pollen ----
neotropics_samples_taxa_amalgamation <-
  "data-raw/GLOBAL/Neotropics_surface samples_SPH.xlsx" %>%
  readxl::read_excel(sheet = 3) %>%
  magrittr::set_names(c(
    "taxon_name", "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::select(-taxon_name) %>%
  dplyr::distinct() %>%
  purrr::map_df(~.x %>% stringr::str_squish()) %>%
  dplyr::mutate(source = "neotropics_pollen")

## Phelps ----
Phelps_taxa_amalgamation <-
  "data-raw/GLOBAL/Phelps_2021-09-24_version 2_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  purrr::map_df(~.x %>% stringr::str_squish()) %>%
  dplyr::mutate(source = "Phelps")

## SMPDSv1 ----
SMPDSv1_taxa_amalgamation <-
  "data-raw/GLOBAL/A_SMPDSv1_Iberia and EMBSECBIO cleanup done.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  purrr::map_df(~.x %>% stringr::str_squish()) %>%
  dplyr::mutate(source = "SMPDSv1")

## southern_hemisphere_pollen ----
southern_hemisphere_taxa_amalgamation <-
  "data-raw/GLOBAL/other_southern_hemisphere_SPH.xlsx" %>%
  readxl::read_excel(sheet = 3) %>%
  magrittr::set_names(c(
    "taxon_name", "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::select(-taxon_name) %>%
  dplyr::distinct() %>%
  purrr::map_df(~.x %>% stringr::str_squish()) %>%
  dplyr::mutate(source = "southern_hemisphere_pollen")

## tatiana_pollen ----
tatiana_samples_taxa_amalgamation <-
  "data-raw/GLOBAL/Tatiana_samples_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  purrr::map_df(~.x %>% stringr::str_squish()) %>%
  dplyr::mutate(source = "tatiana_pollen")

smpdsv2_pollen_amalgamations <-
  dplyr::bind_rows(
    additional_european_pollen_taxa_counts_amalgamation,
    african_modern_samples_counts,
    APD_taxa_amalgamation,
    australia_pollen_1_s1,
    australia_pollen_2_s1,
    australia_pollen_3_s1,
    CMPD_taxa_amalgamation,
    dugerdil_taxa_amalgamation,
    EMBSeCBIO_taxa_amalgamation,
    EMPDv2_taxa_amalgamation,
    gaillard_samples_taxa_amalgamation,
    Herzschuh_taxa_amalgamation,
    IbMPD_taxa_counts_amalgamation,
    japanese_pollen_taxa_amalgamations_1,
    japanese_pollen_taxa_amalgamations_2,
    japanese_pollen_taxa_amalgamations_3,
    japanese_pollen_taxa_amalgamations_4,
    japanese_pollen_taxa_amalgamations_5,
    moroccan_coretops_taxa_amalgamation,
    neotoma_north_america_pollen_taxa_amalgamation,
    neotoma_north_america_pollen_surface_taxa_amalgamation,
    neotoma_south_america_pollen_taxa_amalgamation,
    NEOTOMA_taxa_amalgamation,
    neotropics_samples_taxa_amalgamation,
    Phelps_taxa_amalgamation,
    SMPDSv1_taxa_amalgamation,
    southern_hemisphere_taxa_amalgamation,
    tatiana_samples_taxa_amalgamation
  ) %>%
  # dplyr::left_join(taxonomic_corrections %>%
  #                    dplyr::filter(level %in% c("clean", "all")),
  #                  by = c("clean" =  "original_taxon")) %>%
  # dplyr::mutate(clean = dplyr::coalesce(corrected_taxon_name,
  #                                       clean)) %>%
  # dplyr::select(-corrected_taxon_name, -level) %>%
  # dplyr::left_join(taxonomic_corrections %>%
  #                    dplyr::filter(level %in% c("intermediate", "all")),
  #                  by = c("clean" =  "original_taxon")) %>%
  # dplyr::mutate(intermediate = dplyr::coalesce(corrected_taxon_name,
  #                                              intermediate)) %>%
  # dplyr::select(-corrected_taxon_name, -level) %>%
  # dplyr::left_join(taxonomic_corrections %>%
  #                    dplyr::filter(level %in% c("amalgamated", "all")),
  #                  by = c("clean" =  "original_taxon")) %>%
  # dplyr::mutate(amalgamated = dplyr::coalesce(corrected_taxon_name,
  #                                             amalgamated)) %>%
  # dplyr::select(-corrected_taxon_name, -level) %>%
dplyr::left_join(taxonomic_corrections %>%
                   dplyr::filter(level %in% c("clean",  "all")),
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
  dplyr::select(-corrected_taxon_name, -level) %>%
  dplyr::arrange(clean, intermediate, amalgamated) %>%
  dplyr::distinct() %>%
  dplyr::group_by(clean, intermediate, amalgamated) %>%
  dplyr::mutate(source = source %>%
                  stringr::str_c(collapse = ", ")) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::arrange(clean, intermediate, amalgamated) %>%
  dplyr::rename(`included in` = source)


smpdsv2_pollen_amalgamations %>%
  purrr::map_df(stringr::str_squish) %>%
  dplyr::group_by(clean, intermediate) %>%
  # dplyr::group_by(clean, amalgamated) %>%
  # dplyr::group_by(intermediate, amalgamated) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::filter(n > 1) %>%
  View()

smpdsv2_pollen_amalgamations %>%
  readr::write_excel_csv(paste0("data-raw/smpdsv2_pollen_amalgamations_",
                                Sys.Date(),
                                ".csv"), na = "")

data("SMPDSv2", package = "smpds")

# Export data to MySQL database ----
`%>%` <- magrittr::`%>%`
conn <- dabr::open_conn_mysql("SMPDSv2",
                              password = rstudioapi::askForPassword())

idx_pairs <- function(max, step) {
  tibble::tibble(x = seq(1, max, step), y = c(x[-1] - 1, max))
}

SMPDSv2_db <- SMPDSv2 %>%
  dplyr::arrange(site_name, entity_name, age_BP, elevation) %>%
  dplyr::group_by(site_name) %>%
  dplyr::mutate(ID_SITE = dplyr::cur_group_id(), .before = 1) %>%
  dplyr::group_by(site_name, entity_name) %>%
  dplyr::mutate(ID_ENTITY = dplyr::cur_group_id(), .before = 2) %>%
  dplyr::ungroup()

## entity ----
idx_entity <- idx_pairs(nrow(SMPDSv2_db), 1000)
pb <- progress::progress_bar$new(total = nrow(idx_entity))
meta_neo_res <-
  purrr::map2(idx_entity$x,
              idx_entity$y,
              ~ {
                pb$tick()
                SMPDSv2_db %>%
                  dplyr::select(ID_SITE:age_BP, ID_SAMPLE, publication, doi) %>%
                  dplyr::relocate(ID_SAMPLE, .after = ID_ENTITY) %>%
                  dplyr::slice(.x:.y) %>%
                  rpd:::update_records(conn = conn, table = "entity",
                                       dry_run = TRUE, quiet = TRUE,
                                       PK = 1:3)
              })

###### Validate -----
entity_tb <- conn %>%
  dabr::select_all("entity")
waldo::compare(SMPDSv2_db %>%
                 dplyr::select(ID_SITE:age_BP, ID_SAMPLE, publication) %>%
                 dplyr::arrange(ID_SITE, ID_ENTITY, ID_SAMPLE) %>%
                 .[order(colnames(.))],
               entity_tb %>%
                 dplyr::arrange(ID_SITE, ID_ENTITY, ID_SAMPLE) %>%
                 .[order(colnames(.))],
               tolerance = 1E-9,
               max_diffs = Inf)

## climate ----
idx_climate <- idx_pairs(nrow(SMPDSv2_db), 1000)
pb <- progress::progress_bar$new(total = nrow(idx_climate))
meta_neo_res <-
  purrr::map2(idx_climate$x,
              idx_climate$y,
              ~ {
                pb$tick()
                SMPDSv2_db %>%
                  dplyr::select(ID_SAMPLE, ID_BIOME:map) %>%
                  dplyr::mutate(ID_BIOME = ifelse(ID_BIOME < 0, NA, ID_BIOME)) %>%
                  dplyr::slice(.x:.y) %>%
                  rpd:::update_records(conn = conn, table = "climate",
                                       dry_run = TRUE, quiet = TRUE,
                                       PK = 1)
              })

###### Validate -----
climate_tb <- conn %>%
  dabr::select_all("climate")
waldo::compare(SMPDSv2_db %>%
                 dplyr::select(ID_SAMPLE, ID_BIOME:map) %>%
                 dplyr::mutate(ID_BIOME = ifelse(ID_BIOME < 0, NA, ID_BIOME)) %>%
                 dplyr::arrange(ID_SAMPLE) %>%
                 .[order(colnames(.))],
               climate_tb %>%
                 dplyr::arrange(ID_SAMPLE) %>%
                 .[order(colnames(.))],
               tolerance = 1E-9,
               max_diffs = Inf)

## count ----
### taxon_name ----
taxon_list <-
  dplyr::bind_rows(
    tibble::tibble(
      taxon_name = SMPDSv2$clean %>% colnames(),
      column = "clean"
    ),
    tibble::tibble(
      taxon_name = SMPDSv2$intermediate %>% colnames(),
      column = "intermediate"
    ),
    tibble::tibble(
      taxon_name = SMPDSv2$amalgamated %>% colnames(),
      column = "amalgamated"
    )
  ) %>%
  dplyr::mutate(taxon_name = taxon_name %>% stringr::str_squish()) %>%
  dplyr::distinct(taxon_name) %>%
  dplyr::arrange(taxon_name) %>%
  # dplyr::slice(-1) %>%
  dplyr::mutate(taxon_name_lc = taxon_name %>%
                  stringr::str_to_lower()) %>%
  dplyr::group_by(taxon_name_lc) %>%
  dplyr::mutate(ID_TAXON = dplyr::cur_group_id(), .before = 1) %>%
  dplyr::ungroup()
  # dplyr::mutate(ID_TAXON = seq_along(taxon_name), .before = 1)

taxon_list %>%
  # dplyr::mutate(taxon_name_lc = taxon_name %>%
  #                 stringr::str_to_lower()) %>%
  dplyr::group_by(taxon_name_lc) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::filter(n > 1)

# dabr::select(conn, "SELECT * FROM taxon_name")
# dabr::delete(conn, "DELETE FROM taxon_name WHERE ID_TAXON >= 1")

taxon_list %>%
  dplyr::select(-taxon_name_lc) %>%
  rpd:::update_records(conn = conn, table = "taxon_name",
                       dry_run = TRUE, quiet = TRUE,
                       PK = 1)

waldo::compare(taxon_list, dabr::select_all(conn, "taxon_name"))

### clean ----
counts_table_1_1 <- SMPDSv2_db %>%
  dplyr::slice(1:10000) %>%
  dplyr::select(ID_SAMPLE, clean) %>%
  tidyr::unnest(clean) %>%
  tidyr::pivot_longer(-ID_SAMPLE,
                      names_to = "taxon_name",
                      values_to = "count") %>%
  dplyr::filter(!is.na(count), count != 0) %>%
  dplyr::mutate(taxon_name = taxon_name %>% stringr::str_squish()) %>%
  dplyr::left_join(taxon_list %>%
                     dplyr::select(-taxon_name_lc),
                   by = "taxon_name") %>%
  dplyr::mutate(amalgamation_level = 0)

counts_table_1_2 <- SMPDSv2_db %>%
  dplyr::slice(10001:20000) %>%
  dplyr::select(ID_SAMPLE, clean) %>%
  tidyr::unnest(clean) %>%
  tidyr::pivot_longer(-ID_SAMPLE,
                      names_to = "taxon_name",
                      values_to = "count") %>%
  dplyr::filter(!is.na(count), count != 0) %>%
  dplyr::mutate(taxon_name = taxon_name %>% stringr::str_squish()) %>%
  dplyr::left_join(taxon_list %>%
                     dplyr::select(-taxon_name_lc),
                   by = "taxon_name") %>%
  dplyr::mutate(amalgamation_level = 0)

counts_table_1_3 <- SMPDSv2_db %>%
  dplyr::slice(-c(1:20000)) %>%
  dplyr::select(ID_SAMPLE, clean) %>%
  tidyr::unnest(clean) %>%
  tidyr::pivot_longer(-ID_SAMPLE,
                      names_to = "taxon_name",
                      values_to = "count") %>%
  dplyr::filter(!is.na(count), count != 0) %>%
  dplyr::mutate(taxon_name = taxon_name %>% stringr::str_squish()) %>%
  dplyr::left_join(taxon_list %>%
                     dplyr::select(-taxon_name_lc),
                   by = "taxon_name") %>%
  dplyr::mutate(amalgamation_level = 0)

counts_table_1 <-
  dplyr::bind_rows(
    counts_table_1_1,
    counts_table_1_2,
    counts_table_1_3
  )

counts_table_1 %>%
  dplyr::filter(is.na(ID_TAXON))

idx_stage1 <- idx_pairs(nrow(counts_table_1), 2000)
pb <- progress::progress_bar$new(total = nrow(idx_stage1))
meta_neo_res <-
  purrr::map2(idx_stage1$x,
              idx_stage1$y,
              ~ {
                pb$tick()
                counts_table_1[.x:.y, ] %>%
                  dplyr::select(-taxon_name) %>%
                  rpd:::add_records(conn = conn, table = "pollen_count",
                                    dry_run = TRUE, quiet = TRUE)
              })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
TAXA_1 <- conn %>%
  dabr::select("SELECT * FROM pollen_count WHERE ID_SAMPLE IN (",
               paste0(unique(counts_table_1$ID_SAMPLE), collapse = ", "),
               ") and amalgamation_level = 0")
waldo::compare(counts_table_1 %>%
                 dplyr::arrange(ID_SAMPLE, ID_TAXON) %>%
                 # dplyr::slice(1:1E-6) %>%
                 dplyr::select(-taxon_name) %>%
                 .[order(colnames(.))],
               TAXA_1 %>%
                 dplyr::arrange(ID_SAMPLE, ID_TAXON) %>%
                 # dplyr::slice(1:1E-6) %>%
                 .[order(colnames(.))],
               tolerance = 1E-9)

### intermediate ----
counts_table_2_1 <- SMPDSv2_db %>%
  dplyr::slice(1:10000) %>%
  dplyr::select(ID_SAMPLE, intermediate) %>%
  tidyr::unnest(intermediate) %>%
  tidyr::pivot_longer(-ID_SAMPLE,
                      names_to = "taxon_name",
                      values_to = "count") %>%
  dplyr::filter(!is.na(count), count != 0) %>%
  dplyr::mutate(taxon_name = taxon_name %>% stringr::str_squish()) %>%
  dplyr::left_join(taxon_list %>%
                     dplyr::select(-taxon_name_lc),
                   by = "taxon_name") %>%
  dplyr::mutate(amalgamation_level = 1)

counts_table_2_2 <- SMPDSv2_db %>%
  dplyr::slice(10001:20000) %>%
  dplyr::select(ID_SAMPLE, intermediate) %>%
  tidyr::unnest(intermediate) %>%
  tidyr::pivot_longer(-ID_SAMPLE,
                      names_to = "taxon_name",
                      values_to = "count") %>%
  dplyr::filter(!is.na(count), count != 0) %>%
  dplyr::mutate(taxon_name = taxon_name %>% stringr::str_squish()) %>%
  dplyr::left_join(taxon_list %>%
                     dplyr::select(-taxon_name_lc),
                   by = "taxon_name") %>%
  dplyr::mutate(amalgamation_level = 1)

counts_table_2_3 <- SMPDSv2_db %>%
  dplyr::slice(-c(1:20000)) %>%
  dplyr::select(ID_SAMPLE, intermediate) %>%
  tidyr::unnest(intermediate) %>%
  tidyr::pivot_longer(-ID_SAMPLE,
                      names_to = "taxon_name",
                      values_to = "count") %>%
  dplyr::filter(!is.na(count), count != 0) %>%
  dplyr::mutate(taxon_name = taxon_name %>% stringr::str_squish()) %>%
  dplyr::left_join(taxon_list %>%
                     dplyr::select(-taxon_name_lc),
                   by = "taxon_name") %>%
  dplyr::mutate(amalgamation_level = 1)

counts_table_2 <-
  dplyr::bind_rows(
    counts_table_2_1,
    counts_table_2_2,
    counts_table_2_3
  )
counts_table_2 %>%
  dplyr::filter(is.na(ID_TAXON))

idx_stage_2 <- idx_pairs(nrow(counts_table_2), 2000)
pb <- progress::progress_bar$new(total = nrow(idx_stage_2))
meta_neo_res <-
  purrr::map2(idx_stage_2$x,
              idx_stage_2$y,
              ~ {
                pb$tick()
                counts_table_2[.x:.y, ] %>%
                  dplyr::select(-taxon_name) %>%
                  rpd:::add_records(conn = conn, table = "pollen_count",
                                    dry_run = TRUE, quiet = TRUE)
              })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
TAXA_2 <- conn %>%
  dabr::select("SELECT * FROM pollen_count WHERE ID_SAMPLE IN (",
               paste0(unique(counts_table_2$ID_SAMPLE), collapse = ", "),
               ") and amalgamation_level = 1")
waldo::compare(counts_table_2 %>%
                 dplyr::arrange(ID_SAMPLE, ID_TAXON) %>%
                 dplyr::select(-taxon_name) %>%
                 .[order(colnames(.))],
               TAXA_2 %>%
                 dplyr::arrange(ID_SAMPLE, ID_TAXON) %>%
                 .[order(colnames(.))],
               tolerance = 1E-9)

### amalgamated ----
counts_table_3_1 <- SMPDSv2_db %>%
  dplyr::slice(1:10000) %>%
  dplyr::select(ID_SAMPLE, amalgamated) %>%
  tidyr::unnest(amalgamated) %>%
  tidyr::pivot_longer(-ID_SAMPLE,
                      names_to = "taxon_name",
                      values_to = "count") %>%
  dplyr::filter(!is.na(count), count != 0) %>%
  dplyr::mutate(taxon_name = taxon_name %>% stringr::str_squish()) %>%
  dplyr::left_join(taxon_list %>%
                     dplyr::select(-taxon_name_lc),
                   by = "taxon_name") %>%
  dplyr::mutate(amalgamation_level = 2)

counts_table_3_2 <- SMPDSv2_db %>%
  dplyr::slice(10001:20000) %>%
  dplyr::select(ID_SAMPLE, amalgamated) %>%
  tidyr::unnest(amalgamated) %>%
  tidyr::pivot_longer(-ID_SAMPLE,
                      names_to = "taxon_name",
                      values_to = "count") %>%
  dplyr::filter(!is.na(count), count != 0) %>%
  dplyr::mutate(taxon_name = taxon_name %>% stringr::str_squish()) %>%
  dplyr::left_join(taxon_list %>%
                     dplyr::select(-taxon_name_lc),
                   by = "taxon_name") %>%
  dplyr::mutate(amalgamation_level = 2)

counts_table_3_3 <- SMPDSv2_db %>%
  dplyr::slice(-c(1:20000)) %>%
  dplyr::select(ID_SAMPLE, amalgamated) %>%
  tidyr::unnest(amalgamated) %>%
  tidyr::pivot_longer(-ID_SAMPLE,
                      names_to = "taxon_name",
                      values_to = "count") %>%
  dplyr::filter(!is.na(count), count != 0) %>%
  dplyr::mutate(taxon_name = taxon_name %>% stringr::str_squish()) %>%
  dplyr::left_join(taxon_list %>%
                     dplyr::select(-taxon_name_lc),
                   by = "taxon_name") %>%
  dplyr::mutate(amalgamation_level = 2)

# counts_table_3 <- SMPDSv2_db %>%
#   dplyr::select(ID_SAMPLE, amalgamated) %>%
#   tidyr::unnest(amalgamated) %>%
#   tidyr::pivot_longer(-ID_SAMPLE,
#                       names_to = "taxon_name",
#                       values_to = "count") %>%
#   dplyr::filter(!is.na(count)) %>%
#   dplyr::mutate(taxon_name = taxon_name %>%
#                   stringr::str_squish() %>%
#                   stringr::str_replace_all("0", "Eucalyptus")
#                 ) %>%
#   dplyr::left_join(taxon_list,
#                    by = "taxon_name") %>%
#   dplyr::mutate(amalgamation_level = 2) %>%
#   dplyr::filter(count != 0)

counts_table_3 <-
  dplyr::bind_rows(
    counts_table_3_1,
    counts_table_3_2,
    counts_table_3_3
  )

counts_table_3 %>%
  dplyr::filter(is.na(ID_TAXON))

idx_stage_3 <- idx_pairs(nrow(counts_table_3), 1000)
pb <- progress::progress_bar$new(total = nrow(idx_stage_3))
meta_neo_res <-
  purrr::map2(idx_stage_3$x,
              idx_stage_3$y,
              ~ {
                pb$tick()
                counts_table_3[.x:.y, ] %>%
                  dplyr::select(-taxon_name) %>%
                  rpd:::add_records(conn = conn, table = "pollen_count",
                                    dry_run = TRUE, quiet = TRUE)
              })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
TAXA_3 <- conn %>%
  dabr::select("SELECT * FROM pollen_count WHERE ID_SAMPLE IN (",
               paste0(unique(counts_table_3$ID_SAMPLE), collapse = ", "),
               ") and amalgamation_level = 2")
waldo::compare(counts_table_3 %>%
                 dplyr::arrange(ID_SAMPLE, ID_TAXON) %>%
                 dplyr::select(-taxon_name) %>%
                 .[order(colnames(.))],
               TAXA_3 %>%
                 dplyr::arrange(ID_SAMPLE, ID_TAXON) %>%
                 .[order(colnames(.))],
               tolerance = 1E-9)

# Create map with marine and offshore entities ----
`%>%` <- magrittr::`%>%`
data("SMPDSv2", package = "smpds")
sf::sf_use_s2(FALSE)
land_borders <-
  rnaturalearth::ne_countries(scale = "large",
                              returnclass = "sf")
lakes <- rnaturalearth::ne_download(
  scale = "large", type = "lakes", category = "physical", returnclass = "sf")

SMPDSv2_offshore <- SMPDSv2 %>%
  dplyr::select(1:19) %>%
  dplyr::arrange(longitude, latitude) %>%
  dplyr::mutate(geometry = NA, .after = longitude) %>%
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs = "+proj=longlat +datum=WGS84 +no_defs"
  ) %>%
  dplyr::mutate(
    on_land = sf::st_within(geometry, land_borders) %>%
      lengths(),
    on_lake = sf::st_within(geometry, lakes) %>%
      lengths()
  )

SMPDSv2_offshore_2 <- SMPDSv2_offshore %>%
  # dplyr::filter(on_land != 1) %>%
  # dplyr::filter(on_lake == 1) %>%
  # dplyr::filter(on_land != 1 | on_lake == 1| map < 0 | mi < 0 | gdd0 < 0) %>%
  dplyr::mutate(latitude = sf::st_coordinates(.)[, 2],
                longitude = sf::st_coordinates(.)[, 1],
                .after = geometry) %>%
  sf::st_set_geometry(NULL)
SMPDSv2_offshore_2 %>%
  # dplyr::filter(on_land != 1) %>%
  dplyr::filter(site_type != "marine") %>%
  dplyr::filter(is.na(ID_BIOME) |
                  is.na(mi) |
                  is.na(gdd0) |
                  is.na(mat) |
                  is.na(mtco) |
                  is.na(mtwa) |
                  is.na(map) |
                  ID_BIOME < 0 |
                  map < 0 | mi < 0 | gdd0 < 0) %>%
  View()
  # smpds::plot_climate(var = "elevation", units = "m ASL", fill_sea = NA, fill_land = NA)

SMPDSv2_dodgy_entities <- SMPDSv2_offshore_2 %>%
  dplyr::filter(site_type != "marine") %>%
  dplyr::filter(is.na(ID_BIOME) |
                  is.na(mi) |
                  is.na(gdd0) |
                  is.na(mat) |
                  is.na(mtco) |
                  is.na(mtwa) |
                  is.na(map) |
                  ID_BIOME < 0 |
                  map < 0 | mi < 0 | gdd0 < 0) %>%
  dplyr::select(source:PNV, ID_SAMPLE, on_land, on_lake) %>%
  dplyr::left_join(
    SMPDSv2 %>%
      dplyr::select(ID_SAMPLE, publication, doi)
  )

SMPDSv2_dodgy_entities_2 <- SMPDSv2_dodgy_entities %>%
  smpds:::get_elevation(cpus = 4) %>%
  smpds::pb()

SMPDSv2_dodgy_entities_3 <- SMPDSv2_dodgy_entities_2 %>%
  dplyr::bind_cols(
    SMPDSv2_dodgy_entities %>%
      dplyr::select(old_elevation = elevation)
  ) %>%
  dplyr::relocate(old_elevation, .after = elevation) #%>%
  # dplyr::mutate(diff_elevation = old_elevation - elevation,
  #               .after = old_elevation)
View(SMPDSv2_dodgy_entities_3)

SMPDSv2_dodgy_entities_3 %>%
  readr::write_excel_csv(
    paste0("data-raw/smpdsv2_sites_with_dodgy_climate_reconstructions_",
           Sys.Date(), ".csv"), na = "")

## Load climate for dodgy entities ----
dodgy_entities <-
  "data-raw/smpdsv2_sites_with_dodgy_climate_reconstructions_2022-05-25_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1)

climate_reconstructions <-
  "data-raw/reconstructions/smpdsv2_sites_with_dodgy_climate_reconstructions_climate_reconstructions_2022-05-30.csv" %>%
  readr::read_csv()

# Load daily values for precipitation to compute MAP (mean annual precipitation)
climate_reconstructions_pre <-
  "data-raw/reconstructions/smpdsv2_sites_with_dodgy_climate_reconstructions_climate_reconstructions_pre_2022-05-30.csv" %>%
  readr::read_csv() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE), .before = T1)
climate_reconstructions_2 <- climate_reconstructions %>%
  dplyr::bind_cols(climate_reconstructions_pre %>%
                     dplyr::select(map, ID_BIOME, PNV))
View(climate_reconstructions_2)

# Remove reconstructed values where mi < 0 (not accurate reconstructions)
climate_reconstructions_3 <- climate_reconstructions_2 %>%
  dplyr::mutate(
    mi = ifelse(is.na(mi) | mi < 0, NA, mi),
    gdd0 = ifelse(is.na(mi) | mi < 0, NA, gdd0),
    mat = ifelse(is.na(mi) | mi < 0, NA, mat),
    mtco = ifelse(is.na(mi) | mi < 0, NA, mtco),
    mtwa = ifelse(is.na(mi) | mi < 0, NA, mtwa),
    map = ifelse(is.na(mi) | mi < 0, NA, map)
  )
View(climate_reconstructions_3)

# climate_reconstructions_z_buffer_2 <-
#   "data-raw/reconstructions/smpdsv2_sites_with_dodgy_climate_reconstructions_climate_reconstructions_2022-05-25_z_buffer_2.csv" %>%
#   readr::read_csv()
#
# # Load daily values for precipitation to compute MAP (mean annual precipitation)
# climate_reconstructions_z_buffer_2_pre <-
#   "data-raw/reconstructions/smpdsv2_sites_with_dodgy_climate_reconstructions_climate_reconstructions_pre_2022-05-25_z_buffer_2.csv" %>%
#   readr::read_csv() %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE), .before = T1)
# climate_reconstructions_z_buffer_2_2 <- climate_reconstructions_z_buffer_2 %>%
#   dplyr::bind_cols(climate_reconstructions_z_buffer_2_pre %>%
#                      dplyr::select(map))

# View(climate_reconstructions_z_buffer_2_2)

SMPDSv2_offshore_2 %>%
  dplyr::filter(on_land != 1) %>%
  dplyr::filter(site_type != "marine") %>%
  dplyr::filter(is.na(ID_BIOME) |
                  is.na(mi) |
                  is.na(gdd0) |
                  is.na(mat) |
                  is.na(mtco) |
                  is.na(mtwa) |
                  is.na(map) |
                  ID_BIOME < 0 |
                  map < 0 | mi < 0 | gdd0 < 0)

.data_pre_1 <- SMPDSv2_offshore_2 %>%
  # dplyr::filter(on_land != 1) %>%
  dplyr::filter(site_type != "marine") %>%
  dplyr::filter(map < 0) %>%
  smpds:::get_elevation() %>%
  smpds::gwr(
    varid = "pre",
    .ref =
      file.path("~/Downloads/climatologies/",
                # "cru_ts4.04.1901.2019.pre.dat-clim-1961-1990-int.nc"),
                "cru_ts4.04.1901.2019.pre.dat-new-clim-1961-1990-int.nc"),
    z_buffer = 2,
    cpus = 4) %>%
  smpds::pb()

.data_pre_1 %>%
  # smpds::pivot_data(varname = "pre_1") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE),
                .before = T1) %>%
  dplyr::select(-c(T1:T365), -basin_size, -age_BP)

p_offshore <- SMPDSv2_offshore %>%
  # dplyr::filter(on_land != 1) %>%
  # dplyr::filter(on_lake == 1) %>%
  dplyr::filter(on_land != 1 | on_lake == 1| map < 0 | mi < 0 | gdd0 < 0) %>%
  dplyr::mutate(latitude = sf::st_coordinates(.)[, 2],
                longitude = sf::st_coordinates(.)[, 1],
                .after = geometry) %>%
  sf::st_set_geometry(NULL) %>%
  smpds::plot_climate(var = "elevation", units = "m ASL",
                      land_borders = land_borders %>%
                        sf::st_difference(sf::st_union(lakes)))
ggplot2::ggsave(file.path("~/Downloads/",
                          paste0("SMPDSv2_offshore_and_lake_entities_", Sys.Date(), ".pdf")),
                plot = p_offshore,
                device = "pdf",
                width = 32,
                height = 15,
                units = "in")

SMPDSv2_offshore %>%
  dplyr::filter(on_land != 1 | on_lake == 1 | map < 0 | mi < 0 | gdd0 < 0) %>%
  dplyr::mutate(latitude = sf::st_coordinates(.)[, 2],
                longitude = sf::st_coordinates(.)[, 1],
                .after = geometry) %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::mutate(on_land = ifelse(on_land == 0, FALSE, TRUE),
                on_lake = ifelse(on_land == 0, FALSE, TRUE)) %>%
  readr::write_excel_csv(
    file.path("~/Downloads/",
              paste0("SMPDSv2_offshore_and_lake_entities_", Sys.Date(), ".csv"))
  )

# Export list of taxon names ----
taxon_list <-
  dplyr::bind_rows(
    tibble::tibble(
      taxon_name = SMPDSv2$clean %>% colnames(),
      column = "clean"
    ),
    tibble::tibble(
      taxon_name = SMPDSv2$intermediate %>% colnames(),
      column = "intermediate"
    ),
    tibble::tibble(
      taxon_name = SMPDSv2$amalgamated %>% colnames(),
      column = "amalgamated"
    )
  )

taxon_list %>%
  dplyr::group_by(taxon_name) %>%
  dplyr::mutate(
    column = column %>%
      stringr::str_c(collapse = ", ")
  ) %>%
  dplyr::rename(`included in` = column) %>%
  dplyr::arrange(taxon_name) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  # dplyr::slice(-1) %>%
  # dplyr::distinct(taxon_name, .keep_all = TRUE) %>%
  readr::write_excel_csv(
    paste0("smpdsv2_taxon_list_", Sys.Date(), ".csv"),
    na = "")

# Export list of categorical variables ----
site_type_tb <-
  SMPDSv2$site_type %>%
  table() %>%
  tibble::as_tibble() %>%
  magrittr::set_names(c("value", "n_entities")) %>%
  dplyr::mutate(column = "site_type",
                .before = 1)

entity_type_tb <-
  SMPDSv2$entity_type %>%
  table() %>%
  tibble::as_tibble() %>%
  magrittr::set_names(c("value", "n_entities")) %>%
  dplyr::mutate(column = "entity_type",
                .before = 1)

source_tb <-
  SMPDSv2$source %>%
  table() %>%
  tibble::as_tibble() %>%
  magrittr::set_names(c("value", "n_entities")) %>%
  dplyr::mutate(column = "source",
                .before = 1)

dplyr::bind_rows(
  site_type_tb,
  entity_type_tb,
  source_tb
) %>%
  View()
  # readr::write_excel_csv("smpdsv2_categorical_variables_counts.csv", na = "")


# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    SMPDSv2 %>%
                      dplyr::select(source:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    SMPDSv2 %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean) %>%
                      tidyr::pivot_longer(-ID_SAMPLE) %>%
                      dplyr::filter(!is.na(value)) %>%
                      tidyr::pivot_wider(names_sort = TRUE,
                                         values_fill = 0)
)
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    SMPDSv2 %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate) %>%
                      tidyr::pivot_longer(-ID_SAMPLE) %>%
                      dplyr::filter(!is.na(value)) %>%
                      tidyr::pivot_wider(names_sort = TRUE,
                                         values_fill = 0)
)
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    SMPDSv2 %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated) %>%
                      tidyr::pivot_longer(-ID_SAMPLE) %>%
                      dplyr::filter(!is.na(value)) %>%
                      tidyr::pivot_wider(names_sort = TRUE,
                                         values_fill = 0)
)
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/SMPDSv2_",
                              Sys.Date(),
                              ".xlsx"))
readr::write_rds(wb,
                 paste0("data-raw/SMPDSv2_",
                        Sys.Date(),
                        ".Rds"))
wb <- readr::read_rds(paste0("data-raw/SMPDSv2_",
                       Sys.Date(),
                       ".Rds"))

SMPDSv2 <-
  file.path(path,
            "SMPDSv2_2022-04-30.csv") %>%
  readr::read_csv()
# set.seed(1)
# idx <- sample(seq_len(nrow(SMPDSv2)), 2000)
# SMPDSv2[idx, ] %>%
#   smpds::plot_climate_countour(
#     var = "mat"
#     # xlim = range(.$longitude, na.rm = TRUE),
#     # ylim = range(.$latitude, na.rm = TRUE)
#   )


show_plot <- TRUE
size <- 1
stroke <- 0.1
width <- 14
xlim <- c(-180, 180)
ylim <- c(-60, 90)

# Plots ----
## GDD0 -----
p_gdd0 <- SMPDSv2 %>%
  dplyr::mutate(
    gdd0 = gdd0 %>%
      cut(
        breaks = c(-Inf, 250, 500, 750, 1000, 1500, 2000, 2500, 3000, 4000, 5000, 6000, 8000, 10000, 15000, Inf),
        labels = c("250", "500", "750", "1000", "1500", "2000", "2500", "3000", "4000", "5000", "6000", "8000", "10000", "15000", "15000+"),
      )
  ) %>%
  smpds::plot_climate(
    var = "gdd0",
    units = "\u00B0C days",
    fill_scale = ggplot2::scale_fill_manual(
      values = wesanderson::wes_palette("Zissou1", 15, type = "continuous")
    ),
    size = size,
    stroke = stroke,
    xlim = xlim,
    ylim = ylim,
    show_plot = show_plot
  )
  # smpds::plot_gdd(size = size,
  #                 stroke = stroke,
  #                 xlim = xlim,
  #                 ylim = ylim,
  #                 show_plot = show_plot)
ggplot2::ggsave(file.path(output_path,
                          paste0("SMPDSv2_gdd0_", Sys.Date(), ".pdf")),
                plot = p_gdd0,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")
# ## MAP ----
# p_map <- SMPDSv2 %>%
#   smpds::plot_climate(var = "map",
#                       units = "mm/year",
#                       size = size,
#                       stroke = stroke,
#                       xlim = xlim,
#                       ylim = ylim,show_plot = show_plot)
# ggplot2::ggsave(file.path(output_path,
#                           paste0("SMPDSv2_map_", Sys.Date(), ".pdf")),
#                 plot = p_map,
#                 device = "pdf",
#                 width = width,
#                 height = 8,
#                 units = "in")

## MAT ----
p_mat <- SMPDSv2 %>%
  smpds::plot_mat(size = size,
                  stroke = stroke,
                  xlim = xlim,
                  ylim = ylim,show_plot = show_plot)
ggplot2::ggsave(file.path(output_path,
                          paste0("SMPDSv2_mat_", Sys.Date(), ".pdf")),
                plot = p_mat,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

## MI ----
p_mi <- SMPDSv2 %>%
  smpds::plot_mi(size = size,
                 stroke = stroke,
                 xlim = xlim,
                 ylim = ylim,
                 show_plot = show_plot)
ggplot2::ggsave(file.path(output_path,
                          paste0("SMPDSv2_mi_", Sys.Date(), ".pdf")),
                plot = p_mi,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

## MTCO ----
p_mtco <- SMPDSv2 %>%
  smpds::plot_climate(var = "mtco",
                      units = "\u00B0C",
                      fill_scale =
                        ggplot2::scale_fill_brewer(name = "MTCO",
                                                   palette = "Spectral",
                                                   direction = -1),
                      # plot_mtco(
                      size = size,
                      stroke = stroke,
                      xlim = xlim,
                      ylim = ylim,
                      show_plot = show_plot)
ggplot2::ggsave(file.path(output_path,
                          paste0("SMPDSv2_mtco_", Sys.Date(), ".pdf")),
                plot = p_mtco,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

## MTWA ----
p_mtwa <- SMPDSv2 %>%
  smpds::plot_climate(var = "mtwa",
                      units = "\u00B0C",
                      fill_scale =
                        ggplot2::scale_fill_brewer(name = "MTWA",
                                                   palette = "Spectral",
                                                   direction = -1),
                      size = size,
                      stroke = stroke,
                      xlim = xlim,
                      ylim = ylim,
                      show_plot = show_plot)
ggplot2::ggsave(file.path(output_path,
                          paste0("SMPDSv2_mtwa_", Sys.Date(), ".pdf")),
                plot = p_mtwa,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

p_biome <- SMPDSv2 %>%
  smpds::plot_biome(size = size,
                    stroke = stroke,
                    xlim = xlim,
                    ylim = ylim,
                    legend.key.width = ggplot2::unit(1.3, "cm"),
                    show_plot = show_plot)
ggplot2::ggsave(file.path(output_path,
                          paste0("SMPDSv2_PNV_", Sys.Date(), ".pdf")),
                plot = p_biome,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# Contours ----
## GDD0 -----
p_gdd0 <- SMPDSv2 %>%
  smpds::plot_climate_countour("gdd0",
                               units = "\u00B0C days",
                               size = size,
                               stroke = stroke,
                               xlim = xlim,
                               ylim = ylim,
                               show_plot = show_plot,
                               resolution = 0.25)
ggplot2::ggsave(file.path(output_path,
                          paste0("SMPDSv2_gdd0_", Sys.Date(), "_tps.pdf")),
                plot = p_gdd0,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")
