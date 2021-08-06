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
  dplyr::full_join(china_modern_pollen_data %>%
                     dplyr::select(-c(1:2, 4:6)))

usethis::use_data(CMPD, overwrite = TRUE)
