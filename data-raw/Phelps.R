## code to prepare `Phelps` dataset goes here
phelps_a1 <- readr::read_csv("~/Downloads/SMPDSv2/Phelps_Appendix1-9/APPENDIX_1_sites-1.csv")
phelps_a2 <- readr::read_csv("~/Downloads/SMPDSv2/Phelps_Appendix1-9/APPENDIX_2_entities-1.csv")
phelps_a3 <- readr::read_csv("~/Downloads/SMPDSv2/Phelps_Appendix1-9/APPENDIX_3_citations-1.csv") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(publication = dplyr::c_across(ref1:ref4) %>%
                  .[!is.na(.)] %>%
                  stringr::str_c(collapse = ";\n"))
phelps_a4 <- readr::read_csv("~/Downloads/SMPDSv2/Phelps_Appendix1-9/APPENDIX_4_taxalist-1.csv")
phelps_a5 <- readr::read_csv("~/Downloads/SMPDSv2/Phelps_Appendix1-9/APPENDIX_5_samples-1.csv")
phelps_a6 <- readr::read_csv("~/Downloads/SMPDSv2/Phelps_Appendix1-9/APPENDIX_6_counts-1.csv")
phelps_a7 <- readr::read_csv("~/Downloads/SMPDSv2/Phelps_Appendix1-9/APPENDIX_7_date_ages-1.csv")
phelps_a8 <- readr::read_csv("~/Downloads/SMPDSv2/Phelps_Appendix1-9/APPENDIX_8_CLAM_age-1.csv") %>%
  dplyr::filter(calBP <= 50) %>%
  dplyr::mutate(ENT_SAMP = paste0(entitynum, ",", samplenum))
phelps_a9 <- readr::read_csv("~/Downloads/SMPDSv2/Phelps_Appendix1-9/APPENDIX_9_harmonized_biomization-1.csv")

# Filter the tables samples and counts to only modern CLAM ages
phelps_a52 <- phelps_a5 %>%
  dplyr::mutate(ENT_SAMP = paste0(entitynum, ",", samplenum)) %>%
  dplyr::filter(ENT_SAMP %in% phelps_a8$ENT_SAMP)
phelps_a62 <- phelps_a6 %>%
  dplyr::mutate(ENT_SAMP = paste0(entitynum, ",", samplenum)) %>%
  dplyr::filter(ENT_SAMP %in% phelps_a8$ENT_SAMP)

phelps_sample_ages <- phelps_a52 %>%
  dplyr::inner_join(phelps_a62,
                    by = c("entitynum", "samplenum", "status", "ENT_SAMP"))
phelps_site_entity <- phelps_a1 %>%
  dplyr::left_join(phelps_a2 %>%
                     dplyr::select(-data_source),
                   by = "sitenum") %>%
  dplyr::left_join(phelps_a3 %>%
                     dplyr::select(1:2, publication),
                   by = c("sitenum", "entitynum")) %>%
  dplyr::filter(entitynum %in% phelps_a8$entitynum)

# Verify sites included in the APD
phelps_site_entity %>%
  dplyr::filter(sitename %in% smpds::APD$site_name)
phelps_site_entity %>%
  dplyr::filter(data_source %>%
                  stringr::str_detect("apd"))

# Combine all the appendices
phelps_all <- phelps_site_entity %>%
  dplyr::inner_join(phelps_sample_ages,
                    by = c("data_source", "entitynum", "status")) %>%
  dplyr::inner_join(phelps_a8,
                   by = c("entitynum", "status", "samplenum", "depth", "ENT_SAMP")) %>%
  dplyr::select(sitenum,
                entitynum,
                samplenum,
                poldiv1,
                site_name = sitename,
                entity_name = sigle,
                latitude,
                longitude,
                data_source,
                status,
                publication,
                age_BP = calBP,
                depth,
                taxon_name = original_varname,
                # accepted_varname,
                count) %>%
  dplyr::group_by(site_name) %>%
  dplyr::mutate(n = length(unique(depth)),
                entity_name = ifelse(n > 1,
                                     paste0(entity_name, "_", depth),
                                     entity_name)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-n) %>%
  dplyr::mutate(ID_PHELPS = seq_along(sitenum), .before = 1) %>%
  dplyr::arrange(sitenum, entitynum, taxon_name)

apd_taxa <- readr::read_csv("inst/extdata/apd_taxa.csv")
phelps_all2 <- phelps_all %>%
  dplyr::left_join(apd_taxa,
                   by = "taxon_name") %>%
  dplyr::filter(is.na(action) | action != "delete") %>%
  dplyr::select(-action) %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name)

# Export list of taxon names for clean-up
tibble::tibble(taxon_name = sort(unique(phelps_all$taxon_name))) %>%
  dplyr::left_join(apd_taxa,
                   by = "taxon_name") %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/phelps-taxon_names_2021-08-25.csv", na = "")

phelps_all_sum <- phelps_all %>%
  dplyr::group_by(site_name, depth, taxon_name) %>%
  dplyr::mutate(count2 = sum(count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(site_name, depth, taxon_name, .keep_all = TRUE) %>%
  dplyr::select(-ID_PHELPS)

phelps_all_sum_wide <- phelps_all_sum %>%
  tidyr::pivot_wider(6:14, names_from = "taxon_name", values_from = "count2")

phelps_all_unique <- phelps_all %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE)

phelps_all_dups <- phelps_all %>%
  dplyr::filter(!(ID_PHELPS %in% phelps_all_unique$ID_PHELPS))

phelps_all %>%
  tidyr::pivot_wider(6:14, names_from = "taxon_name", values_from = "count")
usethis::use_data(Phelps, overwrite = TRUE)

phelps_all %>%
  dplyr::slice(1:5) %>%
  dplyr::mutate(elevation = list(latitude, longitude) %>%
                  purrr::pmap_dbl(function(latitude, longitude) {
                    rgbif::elevation(latitude = latitude,
                                     longitude = longitude,
                                     username = "villegar",
                                     elevation_model = "srtm1") %>%
                      .$elevation_geonames
                  }))
