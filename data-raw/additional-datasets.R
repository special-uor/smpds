# Additional datasets in the "to check included" directory
# ______________________________________________________________________________
# |                epdcore and finsinger other to check_pollen                 |
# ______________________________________________________________________________
# EPDcore_Fisinger -------------------------------------------------------------
epdcore_finsinger_taxa <- readr::read_csv("~/Downloads/SMPDSv2/epdcore_finsinger-taxon_names_2021-08-24_SPH.csv") %>%
  dplyr::distinct() %>%
  dplyr::mutate(action = ifelse(clean_name %>%
                                  stringr::str_detect("EXCLUDE|exclude"),
                                "delete", "update"),
                clean_name = ifelse(clean_name %>%
                                      stringr::str_detect("EXCLUDE|exclude"),
                                    NA, clean_name)) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::filter(!is.na(taxon_name))
epdcore_finsinger_taxa %>%
  readr::write_excel_csv("inst/extdata/epdcore_finsinger_taxa.csv", na = "")

epdcore_finsinger <- readxl::read_xlsx("~/Downloads/SMPDSv2/To check included/epdcore and finsinger other to check_pollen.xlsx",
                                       sheet = 1) %>%
  dplyr::rename(entity_name = samplename,
                taxon_name = original_varname) %>%
  dplyr::left_join(epdcore_finsinger_taxa,
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
  tidyr::pivot_wider(1, names_from = "taxon_name", values_from = "count") %>%
  smpds::sort_taxa(cols = 1) %>% # Sort the taxon_names alphabetically
  smpds::total_taxa(cols = 1)

aux <- epdcore_finsinger %>%
  dplyr::filter(entity_name %in% EMPDv2$entity_name) %>%
  smpds::rm_na_taxa()
aux_rev <- EMPDv2 %>%
  dplyr::filter(entity_name %in% aux$entity_name) %>%
  smpds::rm_na_taxa(1:14)

epdcore_finsinger2 <- aux_rev %>%
  dplyr::select(1:14) %>%
  dplyr::right_join(epdcore_finsinger,
                    by = "entity_name") %>%
  dplyr::select(-total)

# # Export list of taxon names for clean-up
# tibble::tibble(taxon_name = colnames(epdcore_finsinger2)[-c(1:14)],
#                clean_name = taxon_name) %>%
#   readr::write_excel_csv("~/Downloads/SMPDSv2/epdcore_finsinger-taxon_names_2021-08-24.csv", na = "")

epdcore_finsinger2 %>%
  readr::write_excel_csv("inst/extdata/epdcore_finsinger.csv", na = "")

aux[1, ] %>%
  smpds::rm_na_taxa(cols = 1)

aux_rev[1, ] %>%
  smpds::rm_na_taxa(cols = 1:14)

# aux_rev %>%
#   dplyr::select(-1) %>%
#   dplyr::bind_rows(aux %>%
#                      dplyr::mutate(source = "epdcore_finsinger")) %>%
#   dplyr::arrange(entity_name) %>%
#   smpds::sort_taxa(cols = 1:12) %>%
#   readr::write_excel_csv("~/Downloads/SMPDSv2/EMPDv2-epdcore_finsinger.csv", na = "")


# ______________________________________________________________________________
# |                      feurdeana3 and epdcoretop extras                      |
# ______________________________________________________________________________
# Feurdeana3_EPDcoretop --------------------------------------------------------
feurdeana3_epdcoretop_taxa <- readr::read_csv("~/Downloads/SMPDSv2/feurdeana3_epdcoretop-taxon_names_2021-08-24_SPH.csv") %>%
  dplyr::distinct() %>%
  dplyr::mutate(action = ifelse(clean_name %>%
                                  stringr::str_detect("EXCLUDE|exclude"),
                                "delete", "update"),
                clean_name = ifelse(clean_name %>%
                                      stringr::str_detect("EXCLUDE|exclude"),
                                    NA, clean_name)) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::filter(!is.na(taxon_name))
feurdeana3_epdcoretop_taxa %>%
  readr::write_excel_csv("inst/extdata/feurdeana3_epdcoretop_taxa.csv", na = "")

feurdeana3_epdcoretop <- readxl::read_xlsx("~/Downloads/SMPDSv2/To check included/feurdeana3 and epdcoretop extras.xlsx",
                                           sheet = 1) %>%
  dplyr::rename(entity_name = samplename,
                taxon_name = original_varname) %>%
  dplyr::left_join(feurdeana3_epdcoretop_taxa,
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
  tidyr::pivot_wider(1, names_from = "taxon_name", values_from = "count") %>%
  smpds::sort_taxa(cols = 1) # Sort the taxon_names alphabetically

feurdeana3_epdcoretop2 <- readxl::read_xlsx("~/Downloads/SMPDSv2/To check included/feurdeana3 and epdcoretop extras.xlsx",
                                            sheet = 2,
                                            col_names = FALSE)

aux <- feurdeana3_epdcoretop %>%
  dplyr::filter(entity_name %in% EMPDv2$entity_name) %>%
  smpds::rm_na_taxa()
aux_rev <- EMPDv2 %>%
  dplyr::filter(entity_name %in% aux$entity_name) %>%
  smpds::rm_na_taxa(1:14)

feurdeana3_epdcoretop2 <- aux_rev %>%
  dplyr::select(1:14) %>%
  dplyr::right_join(feurdeana3_epdcoretop,
                    by = "entity_name") %>%
  smpds::rm_na_taxa(1:14) %>%
  smpds::sort_taxa(1:14)

# # Export list of taxon names for clean-up
# tibble::tibble(taxon_name = colnames(feurdeana3_epdcoretop2)[-c(1:14)],
#                clean_name = taxon_name) %>%
#   readr::write_excel_csv("~/Downloads/SMPDSv2/feurdeana3_epdcoretop-taxon_names_2021-08-24.csv", na = "")

feurdeana3_epdcoretop2 %>%
  readr::write_excel_csv("inst/extdata/feurdeana3_epdcoretop.csv", na = "")

aux[1, ] %>%
  smpds::rm_na_taxa(cols = 1)

aux_rev[1, ] %>%
  smpds::rm_na_taxa(cols = 1:13)

# aux_rev %>%
#   dplyr::select(-1) %>%
#   dplyr::bind_rows(aux %>%
#                      dplyr::mutate(source = "feurdeana3_epdcoretop")) %>%
#   dplyr::arrange(entity_name) %>%
#   smpds::sort_taxa(cols = 1:12) %>%
#   readr::write_excel_csv("~/Downloads/SMPDSv2/EMPDv2-feurdeana3_epdcoretop.csv", na = "")


# ______________________________________________________________________________
# |                   iberia_pollen_records_9April_to check                    |
# ______________________________________________________________________________
# Iberia pollen records --------------------------------------------------------
iberian_pollen_records_9april <- readxl::read_xlsx("~/Downloads/SMPDSv2/To check included/iberia_pollen_records_9April_to check.xlsx",
                                                   sheet = 1) %>%
  dplyr::rename(source = souce,
                avg_depth = avg_depth..cm.,
                IPE_age_cal = IPE.age..cal.,
                age_BP_mean = INTCAL2020_mean,
                age_BP_median = INTCAL2020_median)

a <- compare_latlon(IbMPD, iberian_pollen_records_9april, digits = 2) %>%
  dplyr::distinct(site_name.y, .keep_all = TRUE)

aux <- iberian_pollen_records_9april %>%
  dplyr::filter(site_name %in% IbMPD$site_name)
aux_rev <- IbMPD %>%
  dplyr::filter(site_name %in% aux$site_name) # %>%
  # smpds::rm_zero_taxa(1:10)

iberian_pollen_records_9april %>%
  dplyr::filter(!(site_name %in% aux$site_name))

tmp <- aux %>%
  dplyr::select(1, 3:10) %>%
  dplyr::right_join(smpds::IbMPD %>%
                      dplyr::select(1:10),
                    by = c("site_name", "latitude", "longitude", "elevation", "source", "avg_depth"))
aux_rev %>%
  dplyr::distinct(site_name, .keep_all = TRUE)



# ______________________________________________________________________________
# |                      Juodonys pollen data from Migle                       |
# ______________________________________________________________________________
# Juodonys ---------------------------------------------------------------------
# Pre-processing
juodonys_clean_ups <- readxl::read_xlsx("~/Downloads/SMPDSv2/Juodonys clean up.xlsx",
                                        sheet = 1,
                                        col_names = c("taxon_name", "clean_name")) %>%
  dplyr::distinct() %>%
  dplyr::mutate(action = ifelse(stringr::str_detect(clean_name, "EXC|exc"),
                                "delete", "update"),
                clean_name = ifelse(stringr::str_detect(clean_name, "EXC|exc"),
                                    NA, clean_name)) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::filter(!is.na(taxon_name))
juodonys_clean_ups %>%
  readr::write_excel_csv("inst/extdata/juodonys_taxa.csv", na = "")

# Load and clean raw data
juodonys_taxa <- readr::read_csv("inst/extdata/juodonys_taxa.csv")

juodonys <- readxl::read_xls("~/Downloads/SMPDSv2/To check included/Juodonys pollen data from Migle.xls",
                             sheet = 1) %>%
  dplyr::select(2:3) %>%
  dplyr::slice(-c(1:2, 70:76)) %>%
  magrittr::set_names(c("taxon_name", "value")) %>%
  dplyr::left_join(juodonys_taxa,
                   by = "taxon_name") %>%
  dplyr::filter(action != "delete") %>%
  dplyr::select(-action) %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) %>%
  dplyr::group_by(taxon_name) %>%
  dplyr::mutate(value = sum(as.double(value), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
                                    taxon_name_original,
                                    taxon_name)) %>%
  dplyr::distinct(taxon_name, .keep_all = TRUE) %>%
  dplyr::select(-taxon_name_original) %>%
  dplyr::mutate(entity_name = "juodonys", .before = 1) %>%
  tidyr::pivot_wider(1, names_from = "taxon_name") %>%
  smpds::sort_taxa() %>%
  dplyr::mutate(latitude = 55.74, # Metadata extracted from the RPD
                longitude = 25.44,
                elevation = 20,
                site_type = "bog",
                entity_type = "core top",
                basin_size = "small (0.01-1 km2)",
                age_BP = "modern", #-83,
                publication = paste0("Stančikaitė, M., Kisielienė, D., Strimaitienė, A., 2004. Vegetation response to the climatic and human impact changes during the Late Glacial and Holocene: case study of the marginal area of Baltija Upland, NE Lithuania. Baltica 17, 17–33.",
                                     "\n",
                                     "Stančikaitė, M., Kisielienė, D., Moe, D., Vaikutienė, G., 2009. Lateglacial and early Holocene environmental changes in northeastern Lithuania. Quaternary International 207, 80–92."
                                     ),
                DOI = paste0("\n10.1016/j.quaint.2008.10.009"),
                source = "Stančikaitė et al., 2004 and 2009",
                .after = 1)

EMPDv2 %>%
  dplyr::filter(stringr::str_detect(entity_name, "Juodonys"))

juodonys %>%
  readr::write_excel_csv("inst/extdata/juodonys.csv", na = "")

# ______________________________________________________________________________
# |                           Novenko diagnosis.xlsx                           |
# ______________________________________________________________________________
# Novenko ----------------------------------------------------------------------
novenko <- readxl::read_xlsx("~/Downloads/SMPDSv2/To check included/Novenko diagnosis.xlsx",
                             sheet = 1,
                             col_names = FALSE) %>%
  dplyr::select(-c(4:6)) %>%
  magrittr::set_names(c("entity_name", "X2", "X3", "X4")) %>%
  dplyr::filter(!is.na(entity_name))

aux <- novenko %>%
  dplyr::filter(entity_name %in% EMPDv2$entity_name)
aux_rev <- EMPDv2 %>%
  dplyr::filter(entity_name %in% aux$entity_name) %>%
  smpds::rm_na_taxa(1:14)

EMPDv2 %>%
  dplyr::filter(entity_name %>% stringr::str_detect("Novenko"))
EMPDv2 %>%
  dplyr::filter(entity_name %in% novenko$entity_name)


# ______________________________________________________________________________
# |                     Petrasiunai_pollen_data_from Migle                     |
# ______________________________________________________________________________
# Petrasiunai ------------------------------------------------------------------
# Pre-processing
petresiunai_clean_ups <- readxl::read_xlsx("~/Downloads/SMPDSv2/petrrasiunai clean up.xlsx",
                                        sheet = 1,
                                        col_names = c("taxon_name", "clean_name")) %>%
  dplyr::distinct() %>%
  dplyr::mutate(action = ifelse(stringr::str_detect(clean_name, "EXC|exc"),
                                "delete", "update"),
                clean_name = ifelse(stringr::str_detect(clean_name, "EXC|exc"),
                                    NA, clean_name)) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::filter(!is.na(taxon_name))
petresiunai_clean_ups %>%
  readr::write_excel_csv("inst/extdata/petresiunai_taxa.csv", na = "")

# Load and clean raw data
petresiunai_taxa <- readr::read_csv("inst/extdata/petresiunai_taxa.csv")

petresiunai <- readxl::read_xls("~/Downloads/SMPDSv2/To check included/Petrasiunai_pollen_data_from Migle.xls",
                                sheet = 1) %>%
  dplyr::rename(taxon_name = `...1`) %>%
  dplyr::slice(-c(1, 80:85)) %>%
  tidyr::pivot_longer(-1, names_to = "depth") %>%
  dplyr::mutate(ID = seq_along(taxon_name),
                entity_name = paste0("petresiunai_", depth), .before = 1) %>%
  dplyr::left_join(petresiunai_taxa,
                   by = "taxon_name") %>%
  dplyr::filter(action != "delete") %>%
  dplyr::select(-action) %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(value = sum(as.double(value), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
                                    taxon_name_original,
                                    taxon_name)) %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE) %>%
  dplyr::select(-taxon_name_original) %>%
  tidyr::pivot_wider(2, names_from = "taxon_name") %>%
  smpds::sort_taxa() %>%
  dplyr::slice(1) %>% # Only the top depth
  dplyr::mutate(latitude = 55.85, # Metadata extracted from the RPD
                longitude = 25.7028,
                elevation = 107,
                site_type = "lake",
                entity_type = "core top",
                basin_size = "0.02",
                age_BP = "20",
                publication =
                  paste("Stančikaitė, M., Simniškytė, A., Skuratovič, Gedminienė, L.,",
                        "Kazakauskas, V., Uogintas, D., 2019. Reconstruction of the mid-to",
                        "Late- Holocene history of vegetation and land-use in Petrešiūnai,",
                        "north-east Lithuania: implications from palaeobotanical and",
                        "archaeological data. Quaternary International 516, 5–20."),
                DOI = "10.1016/j.quaint.2018.09.029",
                source = "Stančikaitė et al., 2019",
                .after = 1) %>%
  smpds::rm_zero_taxa(1:11)

petresiunai %>%
  readr::write_excel_csv("inst/extdata/petresiunai.csv", na = "")

# petresiunai %>%
#   readr::write_excel_csv("~/Downloads/SMPDSv2/petresiunai-2021-08-18.csv", na = "")


# ______________________________________________________________________________
# |                               POLNET_ESP_PRT                               |
# ______________________________________________________________________________
# POLNET -----------------------------------------------------------------------
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

a <- compare_latlon(SMPDSv2, POLNET_EST_PRT, digits = 4) %>%
  dplyr::distinct(site_name.y, .keep_all = TRUE)

POLNET_EST_PRT_missing <- POLNET_EST_PRT %>%
  dplyr::filter(!(site_name %in% a$site_name.y)) %>%
  dplyr::filter(!(site_name %in% c("Conquezuela palaeolake",
                                   "Hoya del Castillo",
                                   "La Molina mire",
                                   "Laguna Guallar",
                                   "Lake Banyoles",
                                   "Las Pardillas Lake",
                                   "Marbore Lake",
                                   "Pedrido",
                                   "Posidonia Lligat",
                                   "PRD-4",
                                   "Puerto de Belate",
                                   "Puerto de Serranillos",
                                   "Tubilla del Lago",
                                   "Verdeospesoa mire",
                                   "Beliche-Guadiana CM5",
                                   "Charco da Candieira")))

b <- compare_latlon(SMPDSv2, POLNET_EST_PRT2, digits = 4) %>%
  dplyr::distinct(site_name.y, .keep_all = TRUE)

POLNET_EST_PRT_missing2 <- POLNET_EST_PRT %>%
  dplyr::filter(!(site_name %in% b$site_name.y)) %>%
  # dplyr::filter(!(site_name %in% a$site_name.y)) %>%
  dplyr::filter(!(site_name %in% POLNET_EST_PRT_missing$site_name)) %>%
  dplyr::filter(!(site_name %in% c("Conquezuela palaeolake",
                                   "Hoya del Castillo",
                                   "La Molina mire",
                                   "Laguna Guallar",
                                   "Lake Banyoles",
                                   "Las Pardillas Lake",
                                   "Marbore Lake",
                                   "Pedrido",
                                   "Posidonia Lligat",
                                   "PRD-4",
                                   "Puerto de Belate",
                                   "Puerto de Serranillos",
                                   "Tubilla del Lago",
                                   "Verdeospesoa mire",
                                   "Beliche-Guadiana CM5",
                                   "Charco da Candieira")))

POLNET_EST_PRT_missing %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/POLNET_missing_records.csv", na = "")

aux <- POLNET_EST_PRT_missing %>%
  dplyr::filter(site_name %in% SMPDSv2$site_name)

POLNET_EST_PRT_missing %>%
  dplyr::filter(!(site_name %in% aux$site_name))

compare_latlon(SMPDSv2, POLNET_EST_PRT_missing, digits = 1) %>%
  dplyr::distinct(site_name.y, .keep_all = TRUE)

aux <- POLNET_EST_PRT %>%
  dplyr::filter(site_name %in% SMPDSv2$site_name)
# aux_rev <- SMPDSv2 %>%
#   dplyr::filter(site_name %in% aux$site_name) %>%
#   smpds::rm_na_taxa(1:15)

POLNET_EST_PRT %>%
  dplyr::filter(!(site_name %in% aux$site_name))

EMPDv2 %>%
  dplyr::filter(entity_name %>% stringr::str_detect("Novenko"))
EMPDv2 %>%
  dplyr::filter(entity_name %in% novenko$...1)


# ______________________________________________________________________________
# |                               Spanish sites                                |
# ______________________________________________________________________________
# Spanish sites ----------------------------------------------------------------
Spanish_sites <- readxl::read_xlsx("~/Downloads/SMPDSv2/To check included/Spanish sites.xlsx",
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
                        colnames(.)[-c(1:10)])) %>%
  dplyr::filter(age_BP <= 50) %>%
  dplyr::mutate(publication = paste("Harrison, S.P., Shen, Y. and",
                                    "Sweeney, L., 2021. Pollen data and",
                                    "charcoal data of the Iberian Peninsula.",
                                    "University of Reading. Dataset."),
                DOI = "10.17864/1947.294",
                source = "Harrison et al., 2021",
                .after = age_BP)

aux <- Spanish_sites %>%
  dplyr::filter(entity_name %in% IbMPD$entity_name)
aux_rev <- EMPDv2 %>%
  dplyr::filter(site_name %in% aux$entity_name) %>%
  smpds::rm_na_taxa(1:14)

Spanish_sites %>%
  readr::write_excel_csv("inst/extdata/spanish_sites.csv", na = "")
