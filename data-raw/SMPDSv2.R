## code to prepare `SMPDSv2` dataset goes here

SMPDSv2_all <- smpds::SMPDSv1 %>%
  dplyr::mutate(original = "SMPDSv1", .before = 1) %>%
  dplyr::bind_rows(smpds::SSMPD %>%
                     dplyr::mutate(original = "SSMPD", .before = 1)) %>%
  dplyr::bind_rows(smpds::Herzschuh %>%
                     dplyr::mutate(original = "Herzschuh", .before = 1)) %>%
  dplyr::bind_rows(smpds::EMPDv2 %>%
                     dplyr::mutate(basin_size = as.character(basin_size),
                                   age_BP = as.character(age_BP)) %>%
                     dplyr::mutate(original = "EMPDv2", .before = 1)) %>%
  dplyr::bind_rows(smpds::CMPD %>%
                     dplyr::mutate(original = "CMPD", .before = 1)) %>%
  dplyr::bind_rows(smpds::APD %>%
                     dplyr::mutate(original = "APD", .before = 1))
idx <- duplicated(SMPDSv2_all$entity_name)
SMPDSv2_all_dup <- SMPDSv2_all %>%
  dplyr::filter(entity_name %in% SMPDSv2_all$entity_name[idx]) %>%
  dplyr::arrange(entity_name)

SMPDSv2_all2 <- SMPDSv2_all %>%
  dplyr::distinct(entity_name, .keep_all = TRUE)
SMPDSv2 <- SMPDSv2_all2
usethis::use_data(SMPDSv2, overwrite = TRUE)
