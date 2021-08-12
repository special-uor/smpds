## code to prepare `APD` dataset goes here
`%>%` <- magrittr::`%>%`
output <- smpds::process_apd("~/Downloads/SMPDSv2/APD/",
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

apd_clean_taxon_names <- readr::read_csv("inst/extdata/apd_taxon.csv")

APD <- output %>%
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


usethis::use_data(APD, overwrite = TRUE, compress = "xz")

# ------------------------------------------------------------------------------
# |                                APD Paradox                                 |
# ------------------------------------------------------------------------------
apd_paradox_files_txt <- list.files("~/Downloads/SMPDSv2/APD_Paradox",
                                    pattern = ".txt", full.names = TRUE)
apd_paradox_files <- apd_paradox_files_txt %>%
  purrr::map(~.x %>% readr::read_delim(delim = ";", col_names = FALSE) # %>%
               # magrittr::set_attr("name", basename(.x))
             )
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
                                    NA, clean_name)) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::filter(!is.na(taxon_name)) %>%
  dplyr::distinct(taxon_name, clean_name, .keep_all = TRUE)


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
