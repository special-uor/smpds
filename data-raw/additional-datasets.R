# Additional datasets in the "to check included" directory
# ------------------------------------------------------------------------------
# |                epdcore and finsinger other to check_pollen                 |
# ------------------------------------------------------------------------------
epdcore_finsinger <- readxl::read_xlsx("~/Downloads/SMPDSv2/To check included/epdcore and finsinger other to check_pollen.xlsx",
                                       sheet = 1) %>%
  dplyr::rename(entity_name = samplename,
                taxon_name = original_varname) %>%
  # dplyr::group_by(entity_name) %>%
  # dplyr::mutate(total = sum(count, na.rm = TRUE),
  #               percentage2 = count / total * 100) %>%
  # tidyr::pivot_wider(1, names_from = "taxon_name", values_from = "percentage2") %>%
  # tidyr::pivot_wider(1, names_from = "taxon_name", values_from = "percentage") %>%
  tidyr::pivot_wider(1, names_from = "taxon_name", values_from = "count") %>%
  smpds::sort_taxa(cols = 1) %>% # Sort the taxon_names alphabetically
  dplyr::rowwise() %>%
  dplyr::mutate(total = dplyr::c_across(Abies:`Zea mais`) %>%
                  sum(na.rm = TRUE))

aux <- epdcore_finsinger %>%
  dplyr::filter(entity_name %in% EMPDv2$entity_name) %>%
  smpds::rm_na_taxa()
aux_rev <- EMPDv2 %>%
  dplyr::filter(entity_name %in% aux$entity_name) %>%
  smpds::rm_na_taxa(1:13)

aux[1, ] %>%
  smpds::rm_na_taxa(cols = 1)

aux_rev[1, ] %>%
  smpds::rm_na_taxa(cols = 1:13)

aux_rev %>%
  dplyr::select(-1) %>%
  dplyr::bind_rows(aux %>%
                     dplyr::mutate(source = "epdcore_finsinger")) %>%
  dplyr::arrange(entity_name) %>%
  smpds::sort_taxa(cols = 1:12) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/EMPDv2-epdcore_finsinger.csv", na = "")


# ------------------------------------------------------------------------------
# |                      feurdeana3 and epdcoretop extras                      |
# ------------------------------------------------------------------------------
feurdeana3_epdcoretop <- readxl::read_xlsx("~/Downloads/SMPDSv2/To check included/feurdeana3 and epdcoretop extras.xlsx",
                                           sheet = 1) %>%
  dplyr::rename(entity_name = samplename,
                taxon_name = original_varname) %>%
  # dplyr::group_by(entity_name) %>%
  # dplyr::mutate(total = sum(count, na.rm = TRUE),
  #               percentage2 = count / total * 100) %>%
  # tidyr::pivot_wider(1, names_from = "taxon_name", values_from = "percentage2") %>%
  # tidyr::pivot_wider(1, names_from = "taxon_name", values_from = "percentage") %>%
  tidyr::pivot_wider(1, names_from = "taxon_name", values_from = "count") %>%
  smpds::sort_taxa(cols = 1) %>% # Sort the taxon_names alphabetically
  dplyr::filter(!is.na(entity_name)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total = dplyr::c_across(Abies:`Zea mays`) %>%
                  sum(na.rm = TRUE))
feurdeana3_epdcoretop2 <- readxl::read_xlsx("~/Downloads/SMPDSv2/To check included/feurdeana3 and epdcoretop extras.xlsx",
                                            sheet = 2,
                                            col_names = FALSE)

aux <- feurdeana3_epdcoretop %>%
  dplyr::filter(entity_name %in% EMPDv2$entity_name) %>%
  smpds::rm_na_taxa()
aux_rev <- EMPDv2 %>%
  dplyr::filter(entity_name %in% aux$entity_name) %>%
  smpds::rm_na_taxa(1:13)

aux[1, ] %>%
  smpds::rm_na_taxa(cols = 1)

aux_rev[1, ] %>%
  smpds::rm_na_taxa(cols = 1:13)

aux_rev %>%
  dplyr::select(-1) %>%
  dplyr::bind_rows(aux %>%
                     dplyr::mutate(source = "feurdeana3_epdcoretop")) %>%
  dplyr::arrange(entity_name) %>%
  smpds::sort_taxa(cols = 1:12) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/EMPDv2-feurdeana3_epdcoretop.csv", na = "")


# ------------------------------------------------------------------------------
# |                   iberia_pollen_records_9April_to check                    |
# ------------------------------------------------------------------------------

iberian_pollen_records_9april <- readxl::read_xlsx("~/Downloads/SMPDSv2/To check included/iberia_pollen_records_9April_to check.xlsx",
                                                   sheet = 1)

compare_latlon(IbMPD, iberian_pollen_records_9april, digits = 2) %>%
  dplyr::distinct(site_name.y, .keep_all = TRUE)


# ------------------------------------------------------------------------------
# |                      Juodonys pollen data from Migle                       |
# ------------------------------------------------------------------------------
juodonys <- readxl::read_xls("~/Downloads/SMPDSv2/To check included/Juodonys pollen data from Migle.xls",
                             sheet = 1) %>%
  dplyr::select(2:3) %>%
  dplyr::slice(-c(1:2, 70:76)) %>%
  magrittr::set_names(c("name", "value")) %>%
  dplyr::mutate(entity_name = "juodonys", .before = 1) %>%
  tidyr::pivot_wider(1, names_from = "name") %>%
  smpds::sort_taxa() %>%
  dplyr::mutate(longitude = NA,
                latitude = NA,
                elevation = NA,
                basin_size = NA,
                site_type = NA,
                entity_type = NA,
                age_BP = -83,
                publication = NA, .after = 1)

EMPDv2 %>%
  dplyr::filter(stringr::str_detect(entity_name, "Juodonys"))

juodonys %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/juodonys.csv", na = "")

# ------------------------------------------------------------------------------
# |                           Novenko diagnosis.xlsx                           |
# ------------------------------------------------------------------------------
novenko <- readxl::read_xlsx("~/Downloads/SMPDSv2/To check included/Novenko diagnosis.xlsx",
                             sheet = 1,
                             col_names = FALSE) %>%
  dplyr::select(-c(4:6)) %>%
  magrittr::set_names(c("entity_name", "X2", "X3", "X4"))

aux <- novenko %>%
  dplyr::filter(entity_name %in% EMPDv2$entity_name)
aux_rev <- EMPDv2 %>%
  dplyr::filter(entity_name %in% aux$entity_name) %>%
  smpds::rm_na_taxa(1:13)

EMPDv2 %>%
  dplyr::filter(entity_name %>% stringr::str_detect("Novenko"))
EMPDv2 %>%
  dplyr::filter(entity_name %in% novenko$entity_name)


# ------------------------------------------------------------------------------
# |                     Petrasiunai_pollen_data_from Migle                     |
# ------------------------------------------------------------------------------
petresiunai <- readxl::read_xls("~/Downloads/SMPDSv2/To check included/Petrasiunai_pollen_data_from Migle.xls",
                                sheet = 1) %>%
  dplyr::rename(taxon_name = `...1`) %>%
  dplyr::slice(-c(1, 80:85)) %>%
  tidyr::pivot_longer(-1, names_to = "depth") %>%
  dplyr::mutate(ID = seq_along(taxon_name),
                entity_name = paste0("petresiunai_", depth), .before = 1) %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(value = sum(value, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE) %>%
  # dplyr::mutate(rows = length(entity_name))
  tidyr::pivot_wider(2, names_from = "taxon_name") %>%
  smpds::sort_taxa()

petresiunai %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/petresiunai.csv", na = "")


# ------------------------------------------------------------------------------
# |                               POLNET_ESP_PRT                               |
# ------------------------------------------------------------------------------
POLNET_EST_PRT <- readxl::read_xlsx("~/Downloads/SMPDSv2/To check included/POLNET_ESP_PRT_Sites copy.xlsx",
                                    sheet = 1) %>%
  dplyr::rename(site_name = sitename,
                longitude = londd,
                latitude = latdd)
POLNET_EST_PRT2 <- readxl::read_xlsx("~/Downloads/SMPDSv2/To check included/POLNET_ESP_PRT_Sites copy.xlsx",
                                     sheet = 2) %>%
  dplyr::rename(site_name = sitename,
                longitude = londd,
                latitude = latdd)

aux <- POLNET_EST_PRT %>%
  dplyr::filter(site_name %in% EMPDv2$site_name)
aux_rev <- EMPDv2 %>%
  dplyr::filter(site_name %in% aux$site_name) %>%
  smpds::rm_na_taxa(1:13)

EMPDv2 %>%
  dplyr::filter(entity_name %>% stringr::str_detect("Novenko"))
EMPDv2 %>%
  dplyr::filter(entity_name %in% novenko$...1)


# ------------------------------------------------------------------------------
# |                               Spanish sites                                |
# ------------------------------------------------------------------------------
Spanish_sites <- readxl::read_xlsx("~/Downloads/SMPDSv2/To check included/Spanish sites.xlsx",
                                   sheet = 1) %>%
  magrittr::set_names(c("source",
                        "site_name",
                        "entity_name",
                        "latitude",
                        "longitude",
                        "elevation",
                        "basin_size_km2",
                        "site_type",
                        "entity_type",
                        "age_BP",
                        colnames(.)[-c(1:10)]))

aux <- Spanish_sites %>%
  dplyr::filter(entity_name %in% SMPDSv1$entity_name)
aux_rev <- EMPDv2 %>%
  dplyr::filter(site_name %in% aux$entity_name) %>%
  smpds::rm_na_taxa(1:13)
