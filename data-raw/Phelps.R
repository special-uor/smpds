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
phelps_a6 <- readr::read_csv("~/Downloads/SMPDSv2/Phelps_Appendix1-9/APPENDIX_6_counts-1.csv") %>%
  dplyr::filter(!is.na(original_varname))
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
                sigle,
                source = data_source,
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
  dplyr::group_by(site_name) %>%
  # dplyr::mutate(taxon_name = taxon_name %>%
  #                 stringr::str_replace_all("-type", " type")) %>%
  dplyr::mutate(n = length(unique(depth)),
                entity_name = ifelse(n > 1,
                                     paste0(sigle, "_", depth),
                                     sigle),
                .after = site_name) %>%
  dplyr::ungroup() %>%
  dplyr::select(-n) %>%
  dplyr::mutate(ID_PHELPS = seq_along(sitenum), .before = 1) %>%
  dplyr::arrange(sitenum, entitynum, taxon_name)

phelps_apd <- phelps_all %>%
  dplyr::filter(source %>% stringr::str_detect("apd")) %>%
  dplyr::select(2:14) %>%
  dplyr::distinct(.keep_all = TRUE) %>%
  dplyr::arrange(sigle)

apd_taxa <- readr::read_csv("inst/extdata/apd_taxa.csv")
phelps_taxa <- readr::read_csv("inst/extdata/phelps_taxa.csv")
all_taxa <- readr::read_csv("inst/extdata/all_taxa.csv")
ref_taxa <- apd_taxa %>%
  dplyr::bind_rows(phelps_taxa) %>%
  dplyr::bind_rows(all_taxa %>%
                     dplyr::select(1:3)) %>%
  dplyr::mutate(clean_name = clean_name %>%
                  stringr::str_squish()) %>%
  dplyr::filter(!is.na(action)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(dplyr::desc(action), taxon_name)
phelps_all2 <- phelps_all %>%
  dplyr::mutate(taxon_name = taxon_name %>%
                  stringr::str_replace_all("undiff\\.|undif", "") %>%
                  stringr::str_squish()) %>%
  dplyr::left_join(ref_taxa,
                   by = "taxon_name") %>%
  dplyr::filter(is.na(action) | action != "delete") %>%
  dplyr::select(-action) %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name)
phelps_all2 %>%
  dplyr::filter(is.na(taxon_name)) %>%
  dplyr::select(16, 18) %>%
  dplyr::distinct() %>%
  dplyr::arrange(taxon_name_original)

# Export list of taxon names for clean-up
tibble::tibble(taxon_name = sort(unique(phelps_all$taxon_name))) %>%
  dplyr::mutate(taxon_name = taxon_name %>%
                  stringr::str_replace_all("undiff\\.|undif", "") %>%
                  stringr::str_squish()) %>%
  dplyr::left_join(ref_taxa,
                   by = "taxon_name") %>%
  dplyr::filter(is.na(clean_name), is.na(action)) %>%
  dplyr::mutate(clean_name = taxon_name) %>%
  dplyr::distinct() %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/phelps-taxon_names_2021-08-25v2.csv", na = "")

phelps_all_sum <- phelps_all2 %>%
  dplyr::group_by(site_name, depth, taxon_name) %>%
  dplyr::mutate(count = sum(count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(site_name, depth, taxon_name, .keep_all = TRUE) %>%
  dplyr::select(-ID_PHELPS)


Phelps_APD <- phelps_all_sum %>%
  tidyr::pivot_wider(6:14, names_from = "taxon_name", values_from = "count") %>%
  dplyr::filter((stringr::str_remove_all(Phelps$entity_name, "_[.0-9]*$") %in%
                   stringr::str_remove_all(smpds::APD$entity_name, "_[0-9]*$")))

Phelps <- phelps_all_sum %>%
  tidyr::pivot_wider(6:14, names_from = "taxon_name", values_from = "count") %>%
  dplyr::filter(!(stringr::str_remove_all(Phelps$entity_name, "_[.0-9]*$") %in%
                    stringr::str_remove_all(smpds::APD$entity_name, "_[0-9]*$")))

phelps_all_unique <- phelps_all2 %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE)

phelps_all_dups <- phelps_all2 %>%
  dplyr::filter(!(ID_PHELPS %in% phelps_all_unique$ID_PHELPS))

phelps_all %>%
  tidyr::pivot_wider(6:14, names_from = "taxon_name", values_from = "count")

usethis::use_data(Phelps, overwrite = TRUE)

# ------------------------------------------------------------------------------
# |                                  Sand-box                                  |
# ------------------------------------------------------------------------------
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

phelps_taxa <- readxl::read_xlsx("~/Downloads/SMPDSv2/phelps-taxon_names_2021-08-25_v2_SPH.xlsx",
                                 sheet = 1,
                                 skip = 1) %>%
  dplyr::mutate(clean_name = action,
                action = ifelse(stringr::str_detect(tolower(clean_name),
                                                    "exclude"),
                                "delete", "update"),
                clean_name = ifelse(stringr::str_detect(tolower(clean_name),
                                                        "exclude"),
                                    NA, clean_name),
                action = ifelse(is.na(action), "update", action),
                clean_name = ifelse(is.na(clean_name),
                                    taxon_name, clean_name) %>%
                  stringr::str_squish()) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::filter(!is.na(taxon_name)) %>%
  dplyr::distinct(taxon_name, clean_name, .keep_all = TRUE)

phelps_taxa %>%
  readr::write_excel_csv("inst/extdata/phelps_taxa.csv", na = "")

