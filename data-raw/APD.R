## code to prepare `APD` dataset goes here
`%>%` <- magrittr::`%>%`

apd_clean_taxon_names <- readr::read_csv("inst/extdata/apd_taxa.csv")
apd_publications <- readr::read_csv("inst/extdata/APD_publications.csv")

APD_SPH_50_m72 <- readxl::read_xlsx("~/Downloads/SMPDSv2/APD-modern-records_diagnosed/APD-modern-records_50_-72_SPH.xlsx",
                                    sheet = 1) %>%
  dplyr::mutate(age_BP2 = age_BP %>%
                  stringr::str_replace_all("modern", "0") %>%
                  as.double()) %>%
  dplyr::filter(age_BP2 >= -72, age_BP2 <= 50) %>%
  dplyr::mutate(ID_APD = seq_along(sigle)) %>%
  dplyr::select(-age_BP2) %>%
  dplyr::filter(!(sigle %in% c("LT98-37M")))
APD_SPH_over_m72 <- readxl::read_xlsx("~/Downloads/SMPDSv2/APD-modern-records_diagnosed/APD-modern-records_over_-72_SPH.xlsx",
                                      sheet = 1) %>%
  dplyr::mutate(age_BP2 = age_BP %>%
                  stringr::str_replace_all("modern", "0") %>%
                  as.double()) %>%
  dplyr::filter(age_BP2 >= -72, age_BP2 <= 50) %>%
  dplyr::mutate(ID_APD = seq_along(sigle)) %>%
  dplyr::select(-age_BP2)
APD_SPH <- readxl::read_xlsx("~/Downloads/SMPDSv2/APD-modern-records_diagnosed/APD-modern-records_all_SPH_diagnosed.xlsx",
                             sheet = 1) %>%
  dplyr::rename(SPH_comment = ...14) %>%
  # dplyr::filter(!is.na(age_BP)) %>%
  dplyr::mutate(age_BP2 = age_BP %>%
                  stringr::str_replace_all("modern", "0") %>%
                  as.double()) %>%
  dplyr::filter(age_BP2 >= -72, age_BP2 <= 50) %>%
  dplyr::mutate(ID_APD = seq_along(sigle)) %>%
  dplyr::select(-age_BP2)

a <- unique(APD_SPH_50_m72$sigle)
b <- unique(APD_SPH_over_m72$sigle)
d <- unique(APD_SPH$sigle)
a %in% b
a %in% d

APD_SPH2 <- APD_SPH %>%
  dplyr::bind_rows(APD_SPH_50_m72) %>%
  dplyr::bind_rows(APD_SPH_over_m72 %>%
                     dplyr::mutate(age_BP = as.character(age_BP))) %>%
  dplyr::mutate(ID_APD = seq_along(sigle)) %>%
  dplyr::filter(!is.na(age_BP))

APD_SPH_unused2 <- APD_SPH %>%
  dplyr::bind_rows(APD_SPH_50_m72) %>%
  dplyr::bind_rows(APD_SPH_over_m72 %>%
                     dplyr::mutate(age_BP = as.character(age_BP))) %>%
  dplyr::mutate(ID_APD = seq_along(sigle)) %>%
  dplyr::filter(!(ID_APD %in% APD_SPH2$ID_APD)) %>%
  dplyr::arrange(sigle)

APD_all <- APD_SPH2 %>%
  dplyr::filter(taxon_name != "Podocarpus") %>%
  dplyr::mutate(taxon_name = taxon_name %>%
                  stringr::str_squish()) %>%
  dplyr::filter(taxon_name != "[ODD taxon]") %>%
  dplyr::select(-taxon_name_original,-taxon_name_author, -SPH_comment, -ID_APD) %>%
  dplyr::group_by(site_name) %>% # Add suffix based on site_name and depth
  # dplyr::mutate(entity_name = paste0(sigle, "_", depth_in_m, "_", age_BP))
  dplyr::mutate(n = length(unique(depth_in_m)),
                entity_name = paste0(sigle, "_", depth_in_m),
                # entity_name = ifelse(n > 1,
                #                      paste0(sigle, "_", depth_in_m), sigle),
                .after = site_name) %>%
  dplyr::group_by(entity_name) %>% # Add suffix based on entity_name and age_BP
  dplyr::mutate(n = length(unique(age_BP)),
                entity_name = ifelse(n > 1,
                                     paste0(entity_name,
                                            c("", paste0("_", seq_len(n)[-1]))),
                                     entity_name),
                .after = site_name) %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(count = sum(count, na.rm = TRUE),
                n = length(count)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE) %>%
  dplyr::select(site_name, entity_name, latitude, longitude, elevation, age_BP, publication, taxon_name, count) %>%
  dplyr::mutate(ID_APD = seq_along(site_name), .before = 1)

APD_all_wide <- APD_all %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(2:10, names_from = "taxon_name", values_from = "count") %>%
  smpds::parallel_extract_biome(buffer = 12000, cpus = 6) %>%
  dplyr::relocate(ID_BIOME, .after = age_BP) %>%
  progressr::with_progress()

APD_all_wide %>%
  dplyr::filter(is.na(ID_BIOME))

APD <- APD_all_wide %>%
  dplyr::mutate(ID_BIOME = ifelse(is.na(ID_BIOME),
                                  -888888,
                                  ID_BIOME)) %>%
  dplyr::rename(publication_old = publication) %>%
  dplyr::left_join(apd_publications,
                   by = "site_name") %>%
  dplyr::relocate(publication, .after = publication_old) %>%
  dplyr::select(-publication_old)
  # dplyr::relocate(publication, DOI, .after = publication_old) %>%
  # dplyr::select(-publication_old, -publicationv2)

usethis::use_data(APD, overwrite = TRUE, compress = "xz")

# RAW DATA ----
# ------------------------------------------------------------------------------
# |                             Raw data (.ASCII)                              |
# ------------------------------------------------------------------------------
output <- smpds:::process_apd("~/Downloads/SMPDSv2/APD/",
                              col_names = c("Taxon Name [APD]",
                                            "Taxon Name [Author]",
                                            "Depth [m]",
                                            "Radiocarbon Chronology [yrs BP]",
                                            "Calendar Chronology [yrs BP]",
                                            "Count"),
                              col_types = c(readr::col_character(),
                                            readr::col_character(),
                                            readr::col_double(),
                                            readr::col_double(),
                                            readr::col_double(),
                                            readr::col_double()))

APD_old <- output %>%
  purrr::map_df(~.x) %>%
  dplyr::ungroup() %>%
  dplyr::rename(taxon_name = `Taxon Name [APD]`,
                taxon_name_author = `Taxon Name [Author]`,
                site_name = sitename,
                depth_in_m = `Depth [m]`,
                age_BP = `Calendar Chronology [yrs BP]`,
                count = Count) %>%
  dplyr::select(-`Radiocarbon Chronology [yrs BP]`) %>%
  dplyr::filter(is.na(age_BP) | age_BP <= 100) %>%
  dplyr::mutate(taxon_name = taxon_name %>%
                  stringr::str_squish()) %>%
  dplyr::left_join(apd_clean_taxon_names,
                   by = "taxon_name") %>%
  dplyr::filter(is.na(action) | action != "delete") %>%
  dplyr::select(-action) %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) %>%
  dplyr::relocate(taxon_name, .before = taxon_name_original) %>%
  dplyr::relocate(publication, .after = count) %>%
  dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
                                    "[ODD taxon]",
                                    taxon_name)) %>%
  # dplyr::group_by(site_name, taxon_name) %>%
  # dplyr::mutate(count = sum(as.double(count), na.rm = TRUE)) %>%
  # dplyr::ungroup() %>%
  # dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
  #                                   taxon_name_original,
  #                                   taxon_name)) %>%
  # dplyr::distinct(site_name, taxon_name, .keep_all = TRUE) %>%
  dplyr::select(-taxon_name_original, -taxon_name_author)

APD_publications <- APD_old %>%
  dplyr::filter(site_name %in% APD$site_name) %>%
  dplyr::distinct(site_name, publication) %>%
  dplyr::arrange(site_name)

APD_publications %>%
  readr::write_excel_csv("inst/extdata/APD_publications.csv", na = "")

# ------------------------------------------------------------------------------
# |                                APD Paradox                                 |
# ------------------------------------------------------------------------------
apd_paradox_files_txt <- list.files("~/Downloads/SMPDSv2/APD_Paradox",
                                    pattern = ".txt", full.names = TRUE)
apd_paradox_files <- apd_paradox_files_txt %>%
  purrr::map(~.x %>% readr::read_delim(delim = ";", col_names = FALSE) # %>%
               # magrittr::set_attr("name", basename(.x))
             )
# SANDBOX ----
# ------------------------------------------------------------------------------
# |                                  Sandbox                                   |
# ------------------------------------------------------------------------------
apd_taxa_names <- readxl::read_xlsx("~/Downloads/SMPDSv2/smpdsv2-APD-Herzschuh-taxon-names-2021-08-05_SPH.xlsx",
                                    sheet = 2) %>%
  dplyr::slice(-c(1491:1492, 1564:1566)) %>%
  dplyr::mutate(action = ifelse(stringr::str_detect(tolower(clean_name),
                                                    "exclude"),
                                "delete", "update"),
                clean_name = ifelse(stringr::str_detect(tolower(clean_name),
                                                        "exclude"),
                                    NA, clean_name),
                clean_name = clean_name %>%
                  stringr::str_squish()) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::filter(!is.na(taxon_name)) %>%
  dplyr::distinct(taxon_name, clean_name, .keep_all = TRUE)
apd_taxa_names %>%
  readr::write_excel_csv("inst/extdata/apd_taxa.csv", na = "")


aux <- output %>%
  purrr::map_df(~.x) %>%
  dplyr::rename(taxon_name = `Taxon Name [APD]`,
                taxon_name_author = `Taxon Name [Author]`,
                site_name = name,
                depth_in_m = `Depth [m]`,
                age_BP = `Calendar Chronology [yrs BP]`,
                count = Count) %>%
  dplyr::select(-`Radiocarbon Chronology [yrs BP]`) %>%
  dplyr::mutate(taxon_name = taxon_name %>%
                  stringr::str_squish())
apd_odd_taxon <- apd_clean_taxon_names %>%
  dplyr::filter(is.na(action)) %>%
  .$taxon_name
aux2 <- aux %>%
  dplyr::filter(taxon_name %in% apd_odd_taxon) %>%
  dplyr::filter(is.na(age_BP) | age_BP <= 100) %>%
  dplyr::arrange(taxon_name)

APD %>%
  dplyr::filter(is.na(age_BP) | age_BP <= 50) %>%
  dplyr::arrange(site_name, depth_in_m) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/APD-modern-records_all.csv", na = "")

APD %>%
  dplyr::filter(!is.na(age_BP) & (age_BP >= -72 & age_BP <= 50)) %>%
  dplyr::arrange(site_name, depth_in_m) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/APD-modern-records_50_-72.csv", na = "")

APD %>%
  dplyr::filter(!is.na(age_BP) & (age_BP < -72)) %>%
  dplyr::arrange(site_name, depth_in_m) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/APD-modern-records_over_-72.csv")
