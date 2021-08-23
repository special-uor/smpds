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
phelps_a8 <- readr::read_csv("~/Downloads/SMPDSv2/Phelps_Appendix1-9/APPENDIX_8_CLAM_age-1.csv")
phelps_a9 <- readr::read_csv("~/Downloads/SMPDSv2/Phelps_Appendix1-9/APPENDIX_9_harmonized_biomization-1.csv")

phelps_all <- phelps_a1 %>%
  dplyr::left_join(phelps_a2 %>%
                     dplyr::select(-data_source),
                   by = "sitenum") %>%
  dplyr::left_join(phelps_a3 %>%
                     dplyr::select(1:2, publication),
                   by = c("sitenum", "entitynum")) %>%
  dplyr::left_join(phelps_a6 %>%
                     dplyr::select(1:4, count),
                   by = "entitynum") %>%
  dplyr::left_join(phelps_a5 %>%
                     dplyr::distinct(entitynum, .keep_all = TRUE) %>%
                     dplyr::select(1:6),
                   by = c("entitynum", "samplenum")) %>%
  dplyr::left_join(phelps_a8 %>%
                     dplyr::select(1:6),
                   by = c("entitynum", "samplenum", "depth")) %>%
  dplyr::select(sitenum,
                entitynum,
                samplenum,
                poldiv1,
                sigle,
                site_name = sitename,
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
  dplyr::filter(is.na(age_BP) | age_BP <= 50) %>%
  dplyr::mutate(entity_name = paste0(site_name, "_", depth),
                .after = site_name) %>%
  dplyr::mutate(ID_PHELPS = seq_along(sitenum))

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
