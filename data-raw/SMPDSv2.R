## code to prepare `SMPDSv2` dataset goes here

epdcore_finsinger <- readr::read_csv("inst/extdata/epdcore_finsinger.csv")
feurdeana3_epdcoretop <- readr::read_csv("inst/extdata/feurdeana3_epdcoretop.csv")
juodonys <- readr::read_csv("inst/extdata/juodonys.csv")
petresiunai <- readr::read_csv("inst/extdata/petresiunai.csv")

SMPDSv2_all <- smpds::SMPDSv1 %>% ############################ SMPDSv1
  dplyr::mutate(original = "SMPDSv1", .before = 1) %>%
  dplyr::bind_rows(smpds::SSMPD %>% ########################## SSMPD
                     dplyr::mutate(original = "SSMPD", .before = 1)) %>%
  dplyr::bind_rows(smpds::Herzschuh %>% ###################### Herzschuh
                     dplyr::mutate(original = "Herzschuh", .before = 1)) %>%
  dplyr::bind_rows(epdcore_finsinger %>% ##################### Finsinger
                     dplyr::mutate(basin_size = as.character(basin_size),
                                   age_BP = as.character(age_BP)) %>%
                     dplyr::mutate(original = "Finsinger et al., 2007",
                                   .before = 1)) %>%
  dplyr::bind_rows(feurdeana3_epdcoretop %>% ################# Feurdean
                     dplyr::mutate(basin_size = as.character(basin_size),
                                   age_BP = as.character(age_BP)) %>%
                     dplyr::mutate(original = "Feurdean et al., 2009",
                                   .before = 1)) %>%
  dplyr::bind_rows(juodonys %>% ############################# Juodonys
                     dplyr::mutate(basin_size = as.character(basin_size),
                                   age_BP = as.character(age_BP)) %>%
                     dplyr::mutate(original = "Stančikaitė et al., 2004",
                                   .before = 1)) %>%
  dplyr::bind_rows(petresiunai %>% ########################## Petresiunai
                     dplyr::mutate(basin_size = as.character(basin_size),
                                   age_BP = as.character(age_BP)) %>%
                     dplyr::mutate(original = "Stančikaitė et al., 2019",
                                   before = 1)) %>%
  dplyr::bind_rows(NEOTOMA %>% ############################## NEOTOMA
                     dplyr::mutate(basin_size = as.character(basin_size),
                                   age_BP = as.character(age_BP)) %>%
                     dplyr::mutate(original = "NEOTOMA",
                                   .before = 1)) %>%
  dplyr::bind_rows(smpds::EMPDv2 %>% ######################### EMPDv2
                     dplyr::mutate(basin_size = as.character(basin_size),
                                   age_BP = as.character(age_BP)) %>%
                     dplyr::mutate(original = "EMPDv2", .before = 1)) %>%
  dplyr::bind_rows(smpds::CMPD %>% ########################### CMPD
                     dplyr::mutate(basin_size = as.character(basin_size),
                                   age_BP = as.character(age_BP)) %>%
                     dplyr::mutate(original = "CMPD", .before = 1)) %>%
  dplyr::bind_rows(smpds::APD %>% ############################ APD
                     dplyr::mutate(age_BP = as.character(age_BP)) %>%
                     dplyr::mutate(original = "APD", .before = 1)) %>%
  dplyr::bind_rows(smpds::IbMPD %>% ########################## IbMPD
                     dplyr::mutate(age_BP = as.character(age_BP)) %>%
                     dplyr::mutate(original = "IbMPD", .before = 1))

idx <- duplicated(SMPDSv2_all$entity_name)
SMPDSv2_all_dup <- SMPDSv2_all %>%
  dplyr::filter(entity_name %in% SMPDSv2_all$entity_name[idx]) %>%
  dplyr::arrange(entity_name)

SMPDSv2_all2 <- SMPDSv2_all %>%
  dplyr::distinct(entity_name, .keep_all = TRUE)
SMPDSv2 <- SMPDSv2_all2
usethis::use_data(SMPDSv2, overwrite = TRUE, compress = "xz")

taxa <- colnames(SMPDSv2_all)[-c(1:14)] %>% sort()
tibble::tibble(
  taxon_name = taxa,
  amalgamate_name = taxa
) %>% readr::write_excel_csv("~/Downloads/SMPDSv2/SMPDSv2-taxa-list_2021-08-21.csv", na = "")

p <- SMPDSv2_all2 %>% plot_biome()
ggplot2::ggsave("~/Downloads/SMPDSv2/SMPDSv2_biome_map_2021-08-21.pdf",
                p,
                device = "pdf",
                width = 14,
                height =  8)
