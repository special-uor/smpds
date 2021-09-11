## code to prepare `SMPDSv2` dataset goes here

epdcore_finsinger <- readr::read_csv("inst/extdata/epdcore_finsinger.csv")
feurdeana3_epdcoretop <- readr::read_csv("inst/extdata/feurdeana3_epdcoretop.csv")
juodonys <- readr::read_csv("inst/extdata/juodonys.csv")
petresiunai <- readr::read_csv("inst/extdata/petresiunai.csv")
spanish_sites <- readr::read_csv("inst/extdata/spanish_sites.csv")

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
                                   .before = 1)) %>%
  dplyr::bind_rows(spanish_sites %>% ######################## Spanish sites
                     dplyr::mutate(basin_size = as.character(basin_size),
                                   age_BP = as.character(age_BP)) %>%
                     dplyr::mutate(original = "EPD_Yicheng",
                                   .before = 1)) %>%
  dplyr::bind_rows(NEOTOMA %>% ############################## NEOTOMA
                     dplyr::mutate(basin_size = as.character(basin_size),
                                   age_BP = as.character(age_BP)) %>%
                     dplyr::mutate(original = "NEOTOMA",
                                   .before = 1)) %>%
  dplyr::bind_rows(EMBSeCBIO %>% ############################## EMBSeCBIO
                     dplyr::mutate(basin_size = as.character(basin_size),
                                   age_BP = as.character(age_BP)) %>%
                     dplyr::mutate(original = "EMBSeCBIO",
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

# Filter records missing latitude and longitude
SMPDSv2_all_missing_location <- SMPDSv2_all %>%
  dplyr::filter(is.na(latitude) | is.na(longitude) | is.na(elevation))

SMPDSv2_all2 <- SMPDSv2_all %>%
  dplyr::filter(!is.na(latitude), !is.na(longitude), !is.na(elevation))

idx <- duplicated(SMPDSv2_all2$entity_name)
SMPDSv2_all2_dup <- SMPDSv2_all2 %>%
  dplyr::filter(entity_name %in% SMPDSv2_all2$entity_name[idx]) %>%
  dplyr::arrange(entity_name)

# aux <- SMPDSv2_all2 %>%
#   dplyr::group_by(entity_name, age_BP) %>%
#   dplyr::mutate(n = length(entity_name),
#                 .after = DOI) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(n > 1) %>%
#   dplyr::arrange(entity_name, age_BP)

SMPDSv2_all3 <- SMPDSv2_all2 %>%
  # dplyr::mutate(age_BP = ifelse(is.na(age_BP), "assumed modern", age_BP)) %>%
  # dplyr::mutate(age_BP = ifelse(age_BP == "<50", "assumed modern", age_BP)) %>%
  dplyr::distinct(entity_name, .keep_all = TRUE) %>%
  smpds::sort_taxa(1:15) %>%
  dplyr::mutate(ID_BIOME = ifelse(is.na(ID_BIOME), -888888, ID_BIOME)) %>%
  dplyr::left_join(smpds::pnv_classes() %>%
                     dplyr::select(-colour),
                   by = "ID_BIOME") %>%
  dplyr::relocate(description, .after = ID_BIOME) %>%
  dplyr::rename(BIOME_description = description) %>%
  dplyr::select(-ID_SMPDSv1, -ID_CMPD, -ID_EMPDv2, -ID_SSMPD, -depth) %>%
  dplyr::select(-c("Aegiceras",
                   "Carallia",
                   "Ceriops",
                   "Citrus",
                   "Corylus/Myrica",
                   "Excoecaria",
                   "Excoecaria agallocha",
                   "Glyceria",
                   "Hippuris vulgaris",
                   "Liriodendron",
                   "Lumnitzera",
                   "Pilularia",
                   "Podocarpus",
                   "Ranunculus flammula type",
                   "Ricinus",
                   "Sonneratia",
                   "Symplocos",
                   "Transeauina",
                   "Tsuga"))

# Verify taxon names not in the clean column:
idx <- colnames(SMPDSv2_all3) %in% smpds::clean_taxa()$clean_name

SMPDSv2_all3v2 <- SMPDSv2_all3 %>%
  dplyr::select(1:15, which(idx)) %>%
  tidyr::pivot_longer(-c(1:15)) %>%
  dplyr::filter(!is.na(value))

SMPDSv2_all4 <-
  SMPDSv2_all3 %>% dplyr::select(-which(idx)) %>%
  # smpds::rm_na_taxa(1:15) %>%
  # smpds::sort_taxa(1:15) %>%
  tidyr::pivot_longer(-c(1:15)) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::left_join(smpds::clean_taxa(),
                   by = c("name" = "taxon_name")) %>%
  dplyr::filter(action != "delete")

SMPDSv2_all4 %>%
  dplyr::distinct(name, clean_name, action) %>%
  dplyr::filter(name != clean_name) %>%
  dplyr::arrange(name) %>%
  glue::glue_data('"{clean_name}" = "{name}", \n')

SMPDSv2_all4v2 <- SMPDSv2_all4 %>%
  dplyr::mutate(name = clean_name) %>%
  dplyr::select(-action, -clean_name)

SMPDSv2_all5 <- SMPDSv2_all3v2 %>%
  dplyr::bind_rows(SMPDSv2_all4v2) %>%
  dplyr::group_by(entity_name, name) %>%
  dplyr::mutate(n = length(entity_name),
                value = value %>%
                  sum(na.rm = TRUE)) %>%
  dplyr::ungroup()

SMPDSv2_all5v2 <- SMPDSv2_all5 %>%
  dplyr::distinct(entity_name, name, .keep_all = TRUE) %>%
  tidyr::pivot_wider(1:15) %>%
  smpds::sort_taxa(1:15)

SMPDSv2_all3 %>%
  dplyr::select(sort(unique(SMPDSv2_all4$name)))
SMPDSv2_all5v2 %>%
  dplyr::select(sort(unique(SMPDSv2_all4$name)))

sum(sort(unique(SMPDSv2_all4$name)) %in% colnames(SMPDSv2_all5v2))

SMPDSv2 <- SMPDSv2_all5v2
usethis::use_data(SMPDSv2, overwrite = TRUE, compress = "xz")

taxa <- colnames(SMPDSv2_all3)[-c(1:15)] %>% sort()
waldo::compare(colnames(SMPDSv2_all3)[-c(1:15)], taxa)
idx <- taxa %in% smpds::clean_taxa()$clean_name
taxa[!idx]
smpds::clean_taxa() %>%
  dplyr::filter(taxon_name %in% taxa[!idx])
tibble::tibble(
  taxon_name = taxa,
  amalgamate_name = taxa
) %>% readr::write_excel_csv(paste0("~/Downloads/SMPDSv2/SMPDSv2-taxa-list_",
                                    Sys.Date(),
                                    ".csv"),
                             na = "")

p <- SMPDSv2_all3 %>% plot_biome()
ggplot2::ggsave(paste0("~/Downloads/SMPDSv2/SMPDSv2_biome_map_",
                       Sys.Date(), ".pdf"),
                p,
                device = "pdf",
                width = 14,
                height =  8)

# This code was used to verify that the following  taxa was removed as they had
# all the entries missing:
c("Casuarina type", "Centaurea aspera type", "Doronicum type",
  "Dryopteris dilitata", "Fallopia dumetorum type", "Onoclea struthiopteris",
  "Sambucus/Viburnum")

a <- colnames(smpds::SMPDSv2)
b <- colnames(SMPDSv2)
d <- a[!(a %in% b)]
idx <- d %in% c("Aegiceras",
                "Carallia",
                "Ceriops",
                "Citrus",
                "Corylus/Myrica",
                "Excoecaria",
                "Excoecaria agallocha",
                "Glyceria",
                "Hippuris vulgaris",
                "Liriodendron",
                "Lumnitzera",
                "Pilularia",
                "Podocarpus",
                "Ranunculus flammula type",
                "Ricinus",
                "Sonneratia",
                "Symplocos",
                "Transeauina",
                "Tsuga")
e <- d[!idx]
idx <- e %in% unique(SMPDSv2_all4$name)
smpds::SMPDSv2 %>%
  dplyr::select(e[!idx]) %>%
  dplyr::mutate(ID = seq_len(nrow(.)), .before = 1) %>%
  tidyr::pivot_longer(-ID) %>%
  dplyr::filter(!is.na(value), name != "depth")
