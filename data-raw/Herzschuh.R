## code to prepare `Herzschuh` dataset goes here
# Source:
# Herzschuh, U., Cao, X., Laepple, T., Dallmeyer, A., Telford, R.J., Ni, J.,
# Chen, F., Kong, Z., Liu, G., Liu, K.B. and Liu, X., 2019. Position and
# orientation of the westerly jet determined Holocene rainfall patterns in China.
# Nature communications, 10(1), pp.1-8.
# https://doi.org/10.1038/s41467-019-09866-8
`%>%` <- magrittr::`%>%`
# "modern" -> when age_BP missing
Herzschuh_file1 <- readr::read_csv("~/Downloads/SMPDSv2/SourceData_China_Herschuh/SourceDataFile1.csv") %>%
  dplyr::rename(ID_HERZSCHUH = ID,
                entity_name = Site.name,
                longitude = Long,
                latitude = Lat,
                elevation = Elev) %>%
  dplyr::select(-Pann) %>%
  dplyr::mutate(age_BP = "modern", .after = elevation) %>%
  dplyr::mutate(entity_name = entity_name %>%
                  stringr::str_replace_all("s00-", "Alashan-00-") %>%
                  stringr::str_replace_all("s01-", "Alashan-01-") %>%
                  stringr::str_replace_all("Alashan-00-2$",
                                           "Alashan-00-02") %>%
                  stringr::str_replace_all("Alashan-00-4$",
                                           "Alashan-00-04") %>%
                  stringr::str_replace_all("Alashan-00-6$",
                                           "Alashan-00-06") %>%
                  stringr::str_replace_all("Alashan-00-7$",
                                           "Alashan-00-07") %>%
                  stringr::str_replace_all("Alashan-00-9$",
                                           "Alashan-00-09") %>%
                  stringr::str_replace_all("Alashan-01-3\\.97$",
                                           "Alashan-01-03.97") %>%
                  stringr::str_replace_all("Alashan-01-5\\.99$",
                                           "Alashan-01-05.99") %>%
                  stringr::str_replace_all("Alashan-01-6$",
                                           "Alashan-01-06") %>%
                  stringr::str_replace_all("Alashan-01-7$",
                                           "Alashan-01-07") %>%
                  stringr::str_replace_all("North-HL3",
                                           "China North-HL03") %>%
                  stringr::str_replace_all("North-JA1",
                                           "China North-JA01") %>%
                  stringr::str_replace_all("North-PH5",
                                           "China North-PH05") %>%
                  stringr::str_replace_all("North-PH7",
                                           "China North-PH07") %>%
                  stringr::str_remove_all("-7$")
                )

Herzschuh_file2 <- readr::read_csv("~/Downloads/SMPDSv2/SourceData_China_Herschuh/SourceDataFile2.csv") %>%
  dplyr::rename(ID_HERZSCHUH = ID,
                country = Country,
                province = Province,
                entity_name = Site,
                latitude = Latitude,
                longitude = Longitude,
                elevation = Altitude,
                age_BP = Cal.yr.BP) %>%
  dplyr::select(-country, -province)
Herzschuh_file2_modern <- Herzschuh_file2 %>%
  dplyr::filter(is.na(age_BP) | age_BP <= 50) %>%
  smpds::rm_na_taxa(1:8) %>%
  dplyr::mutate(age_BP = as.character(age_BP)) %>%
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(n = length(entity_name),
                entity_name2 = paste0(entity_name, " ", seq_along(entity_name)),
                entity_name = ifelse(n > 1, entity_name2, entity_name)) %>%
  dplyr::select(-entity_name2) %>%
  dplyr::ungroup()

aux <- Herzschuh_file2_modern %>%
  dplyr::filter(entity_name %in% Herzschuh_file1$entity_name) %>%
  smpds::rm_zero_taxa(1:6) %>%
  smpds::total_taxa(1:6)
aux_rev <- Herzschuh_file1 %>%
  dplyr::filter(entity_name %in% Herzschuh_file2_modern$entity_name) %>%
  smpds::rm_zero_taxa(1:6) %>%
  smpds::total_taxa(1:6)

aux <- compare_latlon(Herzschuh_file2_modern, Herzschuh_file1, digits = 2) %>%
  dplyr::distinct()
Herzschuh_file2_modern %>%
  dplyr::filter(entity_name %in% aux$entity_name.x)
Herzschuh_file1 %>%
  dplyr::filter(entity_name %in% aux$entity_name.y)


# con <- file("~/Downloads/SMPDSv2/SourceData_China_Herschuh/SourceDataFile3.dat", "rb")
# readBin(con, what = "raw", 10e6)
# Herzschuh_file3 <- readr::read_delim("~/Downloads/SMPDSv2/SourceData_China_Herschuh/SourceDataFile3.dat", delim = "\n")

## Filter taxon_names
Herzschuh_clean_taxon_names <- readr::read_csv("inst/extdata/herzschuh_taxa.csv")

Herzschuh_file1_long <- Herzschuh_file1 %>%
  tidyr::pivot_longer(-c(1:6), names_to = "taxon_name") %>%
  dplyr::left_join(Herzschuh_clean_taxon_names,
                   by = "taxon_name") %>%
  dplyr::filter(action != "delete") %>%
  dplyr::select(-action) %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) %>%
  dplyr::group_by(entity_name, taxon_name, age_BP) %>%
  dplyr::mutate(value = sum(as.double(value), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
                                    taxon_name_original,
                                    taxon_name)) %>%
  dplyr::distinct(entity_name, taxon_name, age_BP, .keep_all = TRUE) %>%
  dplyr::select(-taxon_name_original)

Herzschuh_file2_modern_long <- Herzschuh_file2_modern %>%
  tidyr::pivot_longer(-c(1:6), names_to = "taxon_name") %>%
  dplyr::left_join(Herzschuh_clean_taxon_names,
                   by = "taxon_name") %>%
  dplyr::filter(action != "delete") %>%
  dplyr::select(-action) %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) %>%
  dplyr::group_by(entity_name, taxon_name, age_BP) %>%
  dplyr::mutate(value = sum(as.double(value), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
                                    taxon_name_original,
                                    taxon_name)) %>%
  dplyr::distinct(entity_name, taxon_name, age_BP, .keep_all = TRUE) %>%
  dplyr::select(-taxon_name_original)

aux <- compare_latlon(Herzschuh_file2_modern, Herzschuh_file1, digits = 2) %>%
  dplyr::distinct()
Herzschuh_file2_modern_long %>%
  dplyr::filter(entity_name %in% aux$entity_name.x) %>%
  tidyr::pivot_wider(1:6, names_from = "taxon_name") %>%
  smpds::rm_zero_taxa(1:6) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total = dplyr::c_across(-c(1:6)) %>%
                  sum(na.rm = TRUE),
                .after = 6)
Herzschuh_file1_long %>%
  dplyr::filter(entity_name %in% aux$entity_name.y) %>%
  tidyr::pivot_wider(1:6, names_from = "taxon_name") %>%
  smpds::rm_zero_taxa(1:6) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total = dplyr::c_across(-c(1:6)) %>%
                  sum(na.rm = TRUE),
                .after = 6)

Herzschuh <- Herzschuh_file1_long %>%
  # dplyr::bind_rows(Herzschuh_file2_modern_long) %>%
  tidyr::pivot_wider(id_cols = 1:6, names_from = "taxon_name") %>%
  smpds::sort_taxa(cols = 1:6) %>% # Sort the taxon_names alphabetically
  dplyr::mutate(basin_size = NA,
                site_type = NA,
                entity_type = NA,
                # BiomeID = smpds::Herzschuh$BiomeID,
                BiomeID = tibble::tibble(latitude, longitude) %>%
                  smpds::parallel_extract_biome(buffer = 12000, cpus = 6) %>%
                  dplyr::filter(!is.na(BiomeID)) %>%
                  dplyr::distinct(ID, .keep_all = TRUE) %>%
                  dplyr::right_join(tibble::tibble(ID = seq_along(latitude)),
                                    by = "ID") %>%
                  .$BiomeID,
                .after = elevation)

usethis::use_data(Herzschuh, overwrite = TRUE, compress = "xz")

# ------------------------------------------------------------------------------
# |                          Find matches in the CMPD                          |
# ------------------------------------------------------------------------------
## Match by entity_name
Herzschuh_CMPD_entity_name <- Herzschuh %>%
  dplyr::mutate(entity_name = entity_name %>%
                  stringr::str_replace_all("s00-", "Alashan-00-") %>%
                  stringr::str_replace_all("s01-", "Alashan-01-") %>%
                  stringr::str_replace_all("Alashan-00-2$",
                                           "Alashan-00-02") %>%
                  stringr::str_replace_all("Alashan-00-4$",
                                           "Alashan-00-04") %>%
                  stringr::str_replace_all("Alashan-00-6$",
                                           "Alashan-00-06") %>%
                  stringr::str_replace_all("Alashan-00-7$",
                                           "Alashan-00-07") %>%
                  stringr::str_replace_all("Alashan-00-9$",
                                           "Alashan-00-09") %>%
                  stringr::str_replace_all("Alashan-01-3\\.97$",
                                           "Alashan-01-03.97") %>%
                  stringr::str_replace_all("Alashan-01-5\\.99$",
                                           "Alashan-01-05.99") %>%
                  stringr::str_replace_all("Alashan-01-6$",
                                           "Alashan-01-06") %>%
                  stringr::str_replace_all("Alashan-01-7$",
                                           "Alashan-01-07") %>%
                  stringr::str_replace("North-HL3",
                                       "China North-HL03") %>%
                  stringr::str_replace("North-JA1",
                                       "China North-JA01") %>%
                  stringr::str_replace("North-PH5",
                                       "China North-PH05") %>%
                  stringr::str_replace("North-PH7",
                                       "China North-PH07") %>%
                  stringr::str_replace("North-QU02",
                                       "China North-QU02") %>%
                  stringr::str_replace("North-QU03",
                                       "China North-QU03") %>%
                  stringr::str_replace("North-PO3",
                                       "China North-PO03") %>%
                  stringr::str_remove_all("-7$")
                ) %>%
  dplyr::filter(entity_name %in% CMPD$entity_name) %>%
  tidyr::pivot_longer(-c(1:10)) %>% # Use the pivot to remove empty taxon
  dplyr::filter(!(is.na(value) | value == 0)) %>%
  tidyr::pivot_wider(1:10) %>%
  dplyr::select(1:10, order(colnames(.)[-c(1:10)]) + 10) %>%
  dplyr::arrange(entity_name)

Herzschuh_CMPD_entity_name_rev <- CMPD %>%
  dplyr::filter(
    entity_name %in% Herzschuh_CMPD_entity_name$entity_name
  ) %>%
  tidyr::pivot_longer(-c(1:13)) %>% # Use the pivot to remove empty taxon
  dplyr::filter(!(is.na(value) | value == 0)) %>%
  tidyr::pivot_wider(1:13) %>%
  dplyr::select(1:13, order(colnames(.)[-c(1:13)]) + 13) %>%
  dplyr::arrange(entity_name)

Herzschuh_CMPD_entity_name[1, ] %>% tidyr::pivot_longer(-c(1:10)) %>% dplyr::filter(!is.na(value)) %>% dplyr::select(1:5, 11:12)
Herzschuh_CMPD_entity_name_rev[1, ] %>% tidyr::pivot_longer(-c(1:13)) %>% dplyr::filter(!is.na(value)) %>% dplyr::select(1:7, 14:15)

compare_latlon(CMPD,
               Herzschuh %>%
                 dplyr::filter(!(entity_name %in%
                                   Herzschuh_CMPD_entity_name$entity_name)) ,
               digits = 3)

aux <- compare_latlon(CMPD, Herzschuh, digits = 2)
aux2 <- aux %>%
  dplyr::select(ID_CMPD, ID_HERZSCHUH, dplyr::starts_with("entity"))
Herzschuh %>%
  dplyr::filter(!(entity_name %in% unique(aux2$entity_name.y)))

# ------------------------------------------------------------------------------
# |                         Find matches in the EMPDv2                         |
# ------------------------------------------------------------------------------
## Match by entity_name
Herzschuh_EMPDv2_entity_name <- Herzschuh %>%
  dplyr::mutate(entity_name = entity_name %>%
                  stringr::str_replace_all("Inner Mongolia 0", "Inner Mongolia ") %>%
                  stringr::str_replace_all("Inner Mongolia C0", "Inner Mongolia C")) %>%
  dplyr::filter(entity_name %in% EMPDv2$site_name) %>%
  tidyr::pivot_longer(-c(1:10)) %>% # Use the pivot to remove empty taxon
  dplyr::filter(!(is.na(value) | value == 0)) %>%
  tidyr::pivot_wider(1:10) %>%
  dplyr::select(1:10, order(colnames(.)[-c(1:10)]) + 10) # Sort the taxon_names alphabetically

Herzschuh_EMPDv2_entity_name_rev <- EMPDv2 %>%
  dplyr::filter(site_name %>%
                  stringr::str_detect("Inner Mongolia")) %>%
  tidyr::pivot_longer(-c(1:13)) %>% # Use the pivot to remove empty taxon
  dplyr::filter(!is.na(value)) %>%
  dplyr::group_by(entity_name) %>%
  # dplyr::mutate(total = sum(value, na.rm = TRUE), # Convert pollen counts to %
  #               value = value / total * 100) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(1:13) %>%
  dplyr::select(1:13, order(colnames(.)[-c(1:13)]) + 13) # Sort the taxon_names alphabetically

Herzschuh_EMPDv2_entity_name[1, ] %>% tidyr::pivot_longer(-c(1:10)) %>% dplyr::filter(!is.na(value)) %>% dplyr::select(1:5, 11:12)
Herzschuh_EMPDv2_entity_name_rev[1, ] %>% tidyr::pivot_longer(-c(1:13)) %>% dplyr::filter(!is.na(value)) %>% dplyr::select(1:7, 14:15)


aux <- compare_latlon(EMPDv2, Herzschuh, digits = 3) %>%
  dplyr::distinct(site_name.y, .keep_all = TRUE)
Herzschuh_subset <- Herzschuh %>%
  dplyr::filter(!(site_name %in% aux$site_name.y))
compare_latlon(EMPDv2, Herzschuh_subset, digits = 2)


idx <- CMPD_excluded$entity_name %in% Herzschuh_CMPD_entity_name_rev$entity_name
CMPD_excluded$entity_name[!idx]
# ------------------------------------------------------------------------------
# |                                  Sandbox                                   |
# ------------------------------------------------------------------------------
Herzschuh %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_count = dplyr::c_across(Abies:Zygophyllum) %>%
                  sum(na.rm = TRUE), .before = Abies) #%>%
  # dplyr::arrange(total_count) %>%
  # dplyr::filter(total_count < 99)

Herzschuh_clean_taxon_names <- readxl::read_xlsx("~/Downloads/SMPDSv2/smpdsv2-APD-Herzschuh-taxon-names-2021-08-05_SPH.xlsx",
                                                 sheet = 1) %>%
  dplyr::distinct() %>%
  dplyr::mutate(action = ifelse(stringr::str_detect(clean_name, "delete"), "delete", "update"),
                clean_name = ifelse(stringr::str_detect(clean_name, "delete"), NA, clean_name)) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::filter(!is.na(taxon_name))

tmp30 <- Herzschuh_file1_long %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(n = length(taxon_name),
                unique_count = length(unique(value))) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(entity_name, taxon_name) %>%
  dplyr::filter(n != 1)
tmp31 <- tmp30 %>%
  dplyr::filter(unique_count > 1)
tmp31 %>%
  dplyr::select(1:6, 8, 7) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/Herzschuh-multiple-records-same-taxon-entity.csv", na = "")

