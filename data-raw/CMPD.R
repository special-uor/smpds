## code to prepare `CMPD` dataset goes here
# The Chinese Modern Pollen Data by Ni Jian
china_modern_pollen_data <- readxl::read_xlsx("~/Downloads/SMPDSv2/Re_modern_pollen_data_China_NIJIAN/China modern pollen - data.xlsx",
                                              sheet = 2,
                                              skip = 1) %>%
  dplyr::rename(entity_name = `Site name`,
                longitude = `Longitude (°E)`,
                latitude = `Latitude(°N)`,
                elevation = `Elevation (m)`) %>%
  dplyr::mutate(entity_name = entity_name %>%
                  stringr::str_squish()) %>%
  dplyr::slice(-c(1:2))
china_modern_pollen_info <- readxl::read_xlsx("~/Downloads/SMPDSv2/Re_modern_pollen_data_China_NIJIAN/China modern pollen - info.xlsx",
                                              sheet = 2,
                                              skip = 1)  %>%
  dplyr::rename(site_name = `Site`,
                entity_name = `Site name`,
                longitude = `Longitude (°E)`,
                latitude = `Latitude(°N)`,
                elevation = `Elevation (m)`,
                site_type = `Sample type`,
                data_source = `Data source`,
                entity_type = `Data type`,
                publication = 21) %>% # 参考文献 (References)
  dplyr::mutate(ID_CMPD = seq_along(entity_name),
                entity_name = entity_name %>%
                  stringr::str_squish(),
                site_name = list(site_name, entity_name) %>%
                  purrr::pmap_chr(function(site_name, entity_name) {
                    site <- iconv(site_name, "latin1", "ASCII", sub = "") %>%
                      stringr::str_remove_all('\\*') %>%
                      stringr::str_remove_all("\\)") %>%
                      stringr::str_squish()
                    if (site != "")
                      entity_name <- entity_name %>%
                        stringr::str_remove_all(pattern = site)
                    entity_name %>%
                      stringr::str_squish() %>%
                      # stringr::str_remove_all("[-]*$") %>%
                      stringr::str_remove_all("HLJ-") %>%
                      stringr::str_remove_all("[0-9-\\*]*$") %>%
                      stringr::str_remove_all("25.*$") %>%
                      stringr::str_squish()

                  }),
                # site_name2 = iconv(site_name, "latin1", "ASCII", sub = "") %>%
                #   stringr::str_squish(),
                # site_name3 = entity_name,
                basin_size = NA,
                age_BP = NA,
                DOI = NA,
                source = "CMPD")
names(china_modern_pollen_data)
names(china_modern_pollen_info)

# Find duplicated records
idx <- duplicated(china_modern_pollen_data$entity_name)
china_modern_pollen_data_dup <- china_modern_pollen_data %>%
  dplyr::filter(entity_name %in% china_modern_pollen_data$entity_name[idx])

# ------------------------------------------------------------------------------
# |                                 Clean data                                 |
# ------------------------------------------------------------------------------
# Clean taxon names
cmpd_clean_taxon_names <- readxl::read_xlsx("~/Downloads/SMPDSv2/smpdsv2-taxon-names-2021-08-05_SPH.xlsx",
                                            sheet = 2,
                                            n_max = 713)

## Nonsense. categories, to check numbers before deciding whether to delete
cmpd_clean_taxon_names_nonsense <- readxl::read_xlsx("~/Downloads/SMPDSv2/smpdsv2-taxon-names-2021-08-05_SPH.xlsx",
                                                     sheet = 2,
                                                     col_names = c("taxon_name", "clean_name"),
                                                     skip = 716,
                                                     n_max = 30)
china_modern_pollen_data_nonsense <- china_modern_pollen_data %>%
  dplyr::select(1:6, !!cmpd_clean_taxon_names_nonsense$taxon_name) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_count = sum(dplyr::c_across(`Abies+Picea`:`Tilia+Ulmus`) %>%
                                    as.numeric(), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(total_count > 0)

china_modern_pollen_data_nonsense2 <- china_modern_pollen_info %>%
  dplyr::select(ID_CMPD,
                source,
                site_name,
                entity_name,
                latitude,
                longitude,
                elevation
  ) %>%
  dplyr::right_join(china_modern_pollen_data_nonsense %>%
                     dplyr::select(-c(1:2, 4:6))) # %>%
  # purrr::map_dfr(~suppressWarnings(sum(as.numeric(.x), na.rm = TRUE)))
china_modern_pollen_data_nonsense2 %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/CMPD-taxons-for-inspection_2021-08-09.csv", na = "")

# To be deleted
## Nonsense. categories, to check numbers before deciding whether to delete
cmpd_clean_taxon_names_delete <- readxl::read_xlsx("~/Downloads/SMPDSv2/smpdsv2-taxon-names-2021-08-05_SPH.xlsx",
                                                   sheet = 2,
                                                   col_names = c("taxon_name", "clean_name"),
                                                   skip = 747)

china_modern_pollen_data_long <- china_modern_pollen_data %>%
  tidyr::pivot_longer(cols = -c(1:6), names_to = "taxon_name") %>%
  dplyr::filter(!is.na(value))  %>%
  dplyr::filter(!(taxon_name %in% cmpd_clean_taxon_names_nonsense$taxon_name),
                !(taxon_name %in% cmpd_clean_taxon_names_delete$taxon_name)) %>%
  dplyr::left_join(cmpd_clean_taxon_names,
                   by = "taxon_name") %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name)
tmp20 <- china_modern_pollen_data_long %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(n = length(taxon_name),
                unique_count = length(unique(value))) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(entity_name, taxon_name) %>%
  dplyr::filter(n != 1)
tmp21 <- tmp20 %>%
  dplyr::filter(unique_count > 1)
tmp21 %>%
  dplyr::select(1:7, 9, 8) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/CMPD-multiple-records-same-taxon-entity.csv", na = "")

china_modern_pollen_data_wide <- china_modern_pollen_data_long %>%
  dplyr::select(-taxon_name_original) %>%
  tidyr::pivot_wider(1:6, names_from = "taxon_name")

china_modern_pollen_data2 <- china_modern_pollen_data %>%
  dplyr::select(-!!cmpd_clean_taxon_names_nonsense$taxon_name,
                -!!cmpd_clean_taxon_names_delete$taxon_name) %>%
  magrittr::set_names(c(colnames(.)[c(1:6)],
                        tibble::tibble(taxon_name = colnames(.)[-c(1:6)]) %>%
                          dplyr::left_join(cmpd_clean_taxon_names,
                                           by = "taxon_name") %>%
                          .$clean_name))
CMPD <- china_modern_pollen_info %>%
  dplyr::select(ID_CMPD,
                source,
                site_name,
                entity_name,
                latitude,
                longitude,
                elevation,
                basin_size,
                site_type,
                entity_type,
                age_BP,
                publication,
                DOI
              ) %>%
  dplyr::full_join(china_modern_pollen_data2 %>%
                     dplyr::select(-c(1:2, 4:6)))

usethis::use_data(CMPD, overwrite = TRUE)
