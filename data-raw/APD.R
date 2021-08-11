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
  dplyr::rename(taxon_name = `Taxon Name [APD]`,
                entity_name = name,
                value = Count) %>%
  dplyr::mutate(taxon_name = taxon_name %>%
                  stringr::str_squish()) %>%
  dplyr::left_join(apd_clean_taxon_names,
                   by = "taxon_name") %>%
  dplyr::filter(action != "delete") %>%
  # dplyr::select(-action) %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) # %>%
  # dplyr::group_by(entity_name, taxon_name) %>%
  # dplyr::mutate(value = sum(as.double(value), na.rm = TRUE)) %>%
  # dplyr::ungroup() %>%
  # dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
  #                                   taxon_name_original,
  #                                   taxon_name)) %>%
  # dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE) %>%
  # dplyr::select(-taxon_name_original)


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


aux2 <- aux %>%
  dplyr::filter(taxon_name %in%
                  apd_clean_taxon_names$taxon_name[is.na(apd_clean_taxon_names$action)]) %>%
  dplyr::arrange(taxon_name)
