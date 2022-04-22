## code to prepare `APD` dataset goes here
## Original ----
`%>%` <- magrittr::`%>%`

apd_clean_taxon_names <- readr::read_csv("inst/extdata/apd_taxa.csv")
apd_publications <- readr::read_csv("inst/extdata/APD_publications.csv")

APD_SPH <- readxl::read_xlsx("~/Downloads/SMPDSv2/APD-modern-records_diagnosed/APD-modern-records_all_SPH_diagnosed.xlsx",
                             sheet = 1) %>%
  dplyr::rename(SPH_comment = ...14) %>%
  dplyr::mutate(ID_APD = seq_along(sigle))
APD_SPH2 <- APD_SPH %>%
  dplyr::filter(!is.na(age_BP))
APD_SPH_unused2 <- APD_SPH %>%
  dplyr::filter(!(ID_APD %in% APD_SPH2$ID_APD)) %>%
  dplyr::arrange(sigle)


APD_all <- APD_SPH2 %>%
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
  dplyr::relocate(publication, DOI, .after = publication_old) %>%
  dplyr::select(-publication_old, -publicationv2)

usethis::use_data(APD, overwrite = TRUE, compress = "xz")

# ------------------------------------------------------------------------------
# |                             Raw data (.ASCII)                              |
# ------------------------------------------------------------------------------
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

# SPH revisions ----
"The entities and their counts were manually inspected by SPH"

`%>%` <- magrittr::`%>%`
## Load data ----
APD_all <-
  "data-raw/GLOBAL/APD_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .after = doi)

### Metadata ----
APD_metadata <-
  APD_all %>%
  dplyr::select(site_name:ID_SAMPLE) %>%
  dplyr::rename(basin_size = `basin_size (km2)`)

### Polen counts ----
APD_counts <-
  APD_all %>%
  dplyr::select(ID_SAMPLE) %>%
  dplyr::bind_cols(
   APD_all %>% # Convert columns with counts to numeric type
     dplyr::select(-c(site_name:ID_SAMPLE)) %>%
     purrr::map_dfc(~.x %>% as.numeric)
  ) %>%
  magrittr::set_names(
    colnames(.) %>%
      stringr::str_replace_all("\\.\\.\\.", "#")
  ) %>%
  tidyr::pivot_longer(-ID_SAMPLE,
                      names_to = "clean",
                      values_to = "taxon_count") %>%
  dplyr::mutate(clean = clean %>%
                  stringr::str_remove_all("\\#[0-9]+$") %>%
                  stringr::str_squish()) %>%
  dplyr::group_by(ID_SAMPLE, clean) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::distinct() %>%
  dplyr::ungroup()

### Amalgamations ----
APD_taxa_amalgamation <-
  "data-raw/GLOBAL/APD_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  dplyr::mutate(clean = clean %>% stringr::str_squish(),
                intermediate = intermediate %>% stringr::str_squish(),
                amalgamated = amalgamated %>% stringr::str_squish())

### Combine counts and amalgamation ----
APD_taxa_counts_amalgamation <-
  APD_counts %>%
  dplyr::left_join(APD_taxa_amalgamation,
                   by = c("clean")) %>%
  dplyr::relocate(taxon_count, .after = amalgamated) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1)

APD_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated))

## Find DOIs ----
APD_metadata_pubs <-
  APD_metadata %>%
  dplyr::distinct(publication) %>%
  dplyr::arrange(publication) %>%
  dplyr::mutate(DOI = publication %>%
                  stringr::str_extract_all("\\[DOI\\s*(.*?)\\s*\\](;|$)") %>%
                  purrr::map_chr(~.x %>%
                                   stringr::str_remove_all("^\\[DOI:|\\]$") %>%
                                   stringr::str_squish() %>%
                                   stringr::str_c(collapse = ";\n"))
  ) %>%
  dplyr::mutate(ID_PUB = seq_along(publication))
# APD_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/APD_modern-references.csv")

### Load cleaned publications list ----
APD_clean_publications <-
  "data-raw/GLOBAL/APD_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI)

## Append clean publications ----
APD_metadata_2 <-
  APD_metadata %>%
  dplyr::left_join(APD_metadata_pubs %>%
                     dplyr::select(-DOI),
                   by = "publication") %>%
  dplyr::left_join(APD_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi, -ID_PUB) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication)

## Extract PNV/BIOME ----
APD_metadata_3 <-
  APD_metadata_2 %>%
  dplyr::select(-dplyr::starts_with("ID_BIOME")) %>%
  smpds::parallel_extract_biome(cpus = 12) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

APD_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * 1.1,
                    ylim = range(.$latitude, na.rm = TRUE) * 1.1)

## Create count tables ----
### Clean ----
APD_clean <-
  APD_taxa_counts_amalgamation %>%
  dplyr::select(-intermediate, -amalgamated) %>%
  dplyr::rename(taxon_name = clean) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     names_sort = TRUE) %>%
  dplyr::arrange(ID_SAMPLE)

### Intermediate ----
APD_intermediate <-
  APD_taxa_counts_amalgamation %>%
  dplyr::select(-clean, -amalgamated) %>%
  dplyr::rename(taxon_name = intermediate) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     names_sort = TRUE) %>%
  dplyr::arrange(ID_SAMPLE)

### Amalgamated ----
APD_amalgamated <-
  APD_taxa_counts_amalgamation %>%
  dplyr::select(-clean, -intermediate) %>%
  dplyr::rename(taxon_name = amalgamated) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     names_sort = TRUE) %>%
  dplyr::arrange(ID_SAMPLE)

# Store subsets ----
APD <-
  APD_metadata_3 %>%
  dplyr::mutate(
    clean = APD_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = APD_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = APD_amalgamated %>%
      dplyr::select(-c(ID_SAMPLE))
  ) %>%
  dplyr::mutate(
    basin_size = basin_size %>%
      stringr::str_replace_all("unknown", "not known"),
    entity_type = entity_type %>%
      stringr::str_replace_all("unknown", "not known"),
    site_type = site_type %>%
      stringr::str_replace_all("unknown", "not known")
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = clean) %>%
  dplyr::mutate(source = "APD", .before = 1)

usethis::use_data(APD, overwrite = TRUE, compress = "xz")

## Inspect enumerates ----
### basin_size -----
APD$basin_size %>%
  unique() %>% sort()

### site_type ----
APD$site_type %>%
  unique() %>% sort()

### entity_type ----
APD$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    APD %>%
                      dplyr::select(source:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    APD %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    APD %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    APD %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/APD_",
                              Sys.Date(),
                              ".xlsx"))
