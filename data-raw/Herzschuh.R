## code to prepare `Herzschuh` dataset goes here
# Source:
# Herzschuh, U., Cao, X., Laepple, T., Dallmeyer, A., Telford, R.J., Ni, J.,
# Chen, F., Kong, Z., Liu, G., Liu, K.B. and Liu, X., 2019. Position and
# orientation of the westerly jet determined Holocene rainfall patterns in China.
# Nature communications, 10(1), pp.1-8.
# https://doi.org/10.1038/s41467-019-09866-8
# Original ----
`%>%` <- magrittr::`%>%`
# "modern" -> when age_BP missing
Herzschuh_file1 <- readr::read_csv("~/Downloads/SMPDSv2/SourceData_China_Herschuh/SourceDataFile1.csv") %>%
  dplyr::rename(ID_HERZSCHUH = ID,
                entity_name = Site.name,
                longitude = Long,
                latitude = Lat,
                elevation = Elev) %>%
  dplyr::select(-Pann) %>%
  dplyr::mutate(age_BP = "modern", .after = elevation) %>%
  dplyr::mutate(entity_name = entity_name %>%
                  stringr::str_replace_all("s00-", "Alashan-00-") %>%
                  stringr::str_replace_all("s01-", "Alashan-01-") %>%
                  stringr::str_replace_all("Alashan-00-2$",
                                           "Alashan-00-02") %>%
                  stringr::str_replace_all("Alashan-00-4$",
                                           "Alashan-00-04") %>%
                  stringr::str_replace_all("Alashan-00-6$",
                                           "Alashan-00-06") %>%
                  stringr::str_replace_all("Alashan-00-7$",
                                           "Alashan-00-07") %>%
                  stringr::str_replace_all("Alashan-00-9$",
                                           "Alashan-00-09") %>%
                  stringr::str_replace_all("Alashan-01-3\\.97$",
                                           "Alashan-01-03.97") %>%
                  stringr::str_replace_all("Alashan-01-5\\.99$",
                                           "Alashan-01-05.99") %>%
                  stringr::str_replace_all("Alashan-01-6$",
                                           "Alashan-01-06") %>%
                  stringr::str_replace_all("Alashan-01-7$",
                                           "Alashan-01-07") %>%
                  stringr::str_replace_all("North-HL3",
                                           "China North-HL03") %>%
                  stringr::str_replace_all("North-JA1",
                                           "China North-JA01") %>%
                  stringr::str_replace_all("North-PH5",
                                           "China North-PH05") %>%
                  stringr::str_replace_all("North-PH7",
                                           "China North-PH07") %>%
                  stringr::str_remove_all("-7$")
                )

Herzschuh_file2 <- readr::read_csv("~/Downloads/SMPDSv2/SourceData_China_Herschuh/SourceDataFile2.csv") %>%
  dplyr::rename(ID_HERZSCHUH = ID,
                country = Country,
                province = Province,
                entity_name = Site,
                latitude = Latitude,
                longitude = Longitude,
                elevation = Altitude,
                age_BP = Cal.yr.BP) %>%
  dplyr::select(-country, -province, -dplyr::starts_with("Pann"))
Herzschuh_file2_modern <- Herzschuh_file2 %>%
  dplyr::filter(is.na(age_BP) | age_BP <= 50) %>%
  smpds::rm_na_taxa(1:6) %>%
  dplyr::mutate(age_BP = as.character(age_BP)) %>%
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(n = length(entity_name),
                entity_name2 = paste0(entity_name, " ", seq_along(entity_name)),
                entity_name = ifelse(n > 1, entity_name2, entity_name)) %>%
  dplyr::select(-entity_name2, -n) %>%
  dplyr::ungroup() #%>%
  # smpds::normalise_taxa(1:6)

aux <- Herzschuh_file2_modern %>%
  dplyr::filter(entity_name %in% Herzschuh_file1$entity_name) %>%
  smpds::rm_zero_taxa(1:6) %>%
  smpds::total_taxa(1:6)
aux_rev <- Herzschuh_file1 %>%
  dplyr::filter(entity_name %in% Herzschuh_file2_modern$entity_name) %>%
  smpds::rm_zero_taxa(1:6) %>%
  smpds::total_taxa(1:6)

aux <- smpds::compare_latlon(Herzschuh_file2_modern,
                             Herzschuh_file1,
                             digits = 2) %>%
  dplyr::distinct()
Herzschuh_file2_modern %>%
  dplyr::filter(entity_name %in% aux$entity_name.x)
Herzschuh_file1 %>%
  dplyr::filter(entity_name %in% aux$entity_name.y)


# con <- file("~/Downloads/SMPDSv2/SourceData_China_Herschuh/SourceDataFile3.dat", "rb")
# readBin(con, what = "raw", 10e6)
# Herzschuh_file3 <- readr::read_delim("~/Downloads/SMPDSv2/SourceData_China_Herschuh/SourceDataFile3.dat", delim = "\n")

## Filter taxon_names
Herzschuh_clean_taxon_names <- readr::read_csv("inst/extdata/herzschuh_taxa.csv")

Herzschuh_file1_long <- Herzschuh_file1 %>%
  tidyr::pivot_longer(-c(1:6), names_to = "taxon_name") %>%
  dplyr::left_join(smpds::clean_taxa(), #Herzschuh_clean_taxon_names,
                   by = "taxon_name") %>%
  dplyr::filter(action != "delete") %>%
  dplyr::select(-action) %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) %>%
  dplyr::group_by(entity_name, taxon_name, age_BP) %>%
  dplyr::mutate(value = sum(as.double(value), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
                                    taxon_name_original,
                                    taxon_name)) %>%
  dplyr::distinct(entity_name, taxon_name, age_BP, .keep_all = TRUE) %>%
  dplyr::select(-taxon_name_original)

Herzschuh_file2_modern_long <- Herzschuh_file2_modern %>%
  tidyr::pivot_longer(-c(1:6), names_to = "taxon_name") %>%
  dplyr::left_join(Herzschuh_clean_taxon_names,
                   by = "taxon_name") %>%
  dplyr::filter(action != "delete") %>%
  dplyr::select(-action) %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) %>%
  dplyr::group_by(entity_name, taxon_name, age_BP) %>%
  dplyr::mutate(value = sum(as.double(value), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
                                    taxon_name_original,
                                    taxon_name)) %>%
  dplyr::distinct(entity_name, taxon_name, age_BP, .keep_all = TRUE) %>%
  dplyr::select(-taxon_name_original)

aux <- compare_latlon(Herzschuh_file2_modern, Herzschuh_file1, digits = 2) %>%
  dplyr::distinct()
Herzschuh_file2_modern_long %>%
  dplyr::filter(entity_name %in% aux$entity_name.x) %>%
  tidyr::pivot_wider(1:6, names_from = "taxon_name") %>%
  smpds::rm_zero_taxa(1:6) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total = dplyr::c_across(-c(1:6)) %>%
                  sum(na.rm = TRUE),
                .after = 6)
Herzschuh_file1_long %>%
  dplyr::filter(entity_name %in% aux$entity_name.y) %>%
  tidyr::pivot_wider(1:6, names_from = "taxon_name") %>%
  smpds::rm_zero_taxa(1:6) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total = dplyr::c_across(-c(1:6)) %>%
                  sum(na.rm = TRUE),
                .after = 6)

Herzschuh <- Herzschuh_file1_long %>%
  dplyr::bind_rows(Herzschuh_file2_modern_long) %>%
  tidyr::pivot_wider(id_cols = 1:6, names_from = "taxon_name") %>%
  smpds::sort_taxa(cols = 1:6) %>% # Sort the taxon_names alphabetically
  dplyr::arrange(entity_name) %>%
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(n = length(entity_name),
                entity_name2 = paste0(entity_name, "_", seq_along(entity_name)),
                entity_name = ifelse(n > 1, entity_name2, entity_name)) %>%
  dplyr::select(-entity_name2, -n, -ID_HERZSCHUH) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(basin_size = NA,
                site_type = NA,
                entity_type = NA,
                ID_BIOME = tibble::tibble(latitude, longitude) %>%
                  smpds::parallel_extract_biome(buffer = 12000, cpus = 6) %>%
                  .$ID_BIOME,
                publication =
                  paste("Herzschuh, U., Cao, X., Laepple, T., Dallmeyer, A., Telford, R.J., Ni, J.,",
                        "Chen, F., Kong, Z., Liu, G., Liu, K.B. and Liu, X., 2019. Position and",
                        "orientation of the westerly jet determined Holocene rainfall patterns in China.",
                        "Nature communications, 10(1), pp.1-8."),
                DOI = "10.1038/s41467-019-09866-8",
                .after = elevation) %>%
  dplyr::mutate(source = "Herzschuh et al., 2019",
                site_name = entity_name %>%
                  stringr::str_remove_all("[-_0-9]*$"),
                .before = 1) %>%
  progressr::with_progress()

not_applicable_biome_pattern <-
  "Marine|marine|Sea|sea|Coastal|coastal|Open Water|Baikel Lake"
Herzschuh2 <- Herzschuh %>%
  dplyr::mutate(
    ID_BIOME = ifelse(entity_type %>%
                        stringr::str_detect(not_applicable_biome_pattern) &
                        is.na(ID_BIOME),
                      -888888,
                      ID_BIOME),
    ID_BIOME = ifelse(site_type %>%
                        stringr::str_detect(not_applicable_biome_pattern) &
                        is.na(ID_BIOME),
                      -888888,
                      ID_BIOME),
    ID_BIOME = ifelse(entity_name %>%
                        stringr::str_detect("Baikel Lake") &
                        is.na(ID_BIOME),
                      -888888,
                      ID_BIOME),
    ID_BIOME = ifelse(is.na(ID_BIOME),
                      -999999,
                      ID_BIOME)
  )

Herzschuh <- Herzschuh2
usethis::use_data(Herzschuh, overwrite = TRUE, compress = "xz")

# ------------------------------------------------------------------------------
# |                          Find matches in the CMPD                          |
# ------------------------------------------------------------------------------
## Match by entity_name
Herzschuh_CMPD_entity_name <- Herzschuh %>%
  dplyr::mutate(entity_name = entity_name %>%
                  stringr::str_replace_all("s00-", "Alashan-00-") %>%
                  stringr::str_replace_all("s01-", "Alashan-01-") %>%
                  stringr::str_replace_all("Alashan-00-2$",
                                           "Alashan-00-02") %>%
                  stringr::str_replace_all("Alashan-00-4$",
                                           "Alashan-00-04") %>%
                  stringr::str_replace_all("Alashan-00-6$",
                                           "Alashan-00-06") %>%
                  stringr::str_replace_all("Alashan-00-7$",
                                           "Alashan-00-07") %>%
                  stringr::str_replace_all("Alashan-00-9$",
                                           "Alashan-00-09") %>%
                  stringr::str_replace_all("Alashan-01-3\\.97$",
                                           "Alashan-01-03.97") %>%
                  stringr::str_replace_all("Alashan-01-5\\.99$",
                                           "Alashan-01-05.99") %>%
                  stringr::str_replace_all("Alashan-01-6$",
                                           "Alashan-01-06") %>%
                  stringr::str_replace_all("Alashan-01-7$",
                                           "Alashan-01-07") %>%
                  stringr::str_replace("North-HL3",
                                       "China North-HL03") %>%
                  stringr::str_replace("North-JA1",
                                       "China North-JA01") %>%
                  stringr::str_replace("North-PH5",
                                       "China North-PH05") %>%
                  stringr::str_replace("North-PH7",
                                       "China North-PH07") %>%
                  stringr::str_replace("North-QU02",
                                       "China North-QU02") %>%
                  stringr::str_replace("North-QU03",
                                       "China North-QU03") %>%
                  stringr::str_replace("North-PO3",
                                       "China North-PO03") %>%
                  stringr::str_remove_all("-7$")
                ) %>%
  dplyr::filter(entity_name %in% CMPD$entity_name) %>%
  tidyr::pivot_longer(-c(1:10)) %>% # Use the pivot to remove empty taxon
  dplyr::filter(!(is.na(value) | value == 0)) %>%
  tidyr::pivot_wider(1:10) %>%
  dplyr::select(1:10, order(colnames(.)[-c(1:10)]) + 10) %>%
  dplyr::arrange(entity_name)

Herzschuh_CMPD_entity_name_rev <- CMPD %>%
  dplyr::filter(
    entity_name %in% Herzschuh_CMPD_entity_name$entity_name
  ) %>%
  tidyr::pivot_longer(-c(1:13)) %>% # Use the pivot to remove empty taxon
  dplyr::filter(!(is.na(value) | value == 0)) %>%
  tidyr::pivot_wider(1:13) %>%
  dplyr::select(1:13, order(colnames(.)[-c(1:13)]) + 13) %>%
  dplyr::arrange(entity_name)

Herzschuh_CMPD_entity_name[1, ] %>% tidyr::pivot_longer(-c(1:10)) %>% dplyr::filter(!is.na(value)) %>% dplyr::select(1:5, 11:12)
Herzschuh_CMPD_entity_name_rev[1, ] %>% tidyr::pivot_longer(-c(1:13)) %>% dplyr::filter(!is.na(value)) %>% dplyr::select(1:7, 14:15)

compare_latlon(CMPD,
               Herzschuh %>%
                 dplyr::filter(!(entity_name %in%
                                   Herzschuh_CMPD_entity_name$entity_name)) ,
               digits = 3)

aux <- compare_latlon(CMPD, Herzschuh, digits = 2)
aux2 <- aux %>%
  dplyr::select(ID_CMPD, ID_HERZSCHUH, dplyr::starts_with("entity"))
Herzschuh %>%
  dplyr::filter(!(entity_name %in% unique(aux2$entity_name.y)))

# ------------------------------------------------------------------------------
# |                         Find matches in the EMPDv2                         |
# ------------------------------------------------------------------------------
## Match by entity_name
Herzschuh_EMPDv2_entity_name <- Herzschuh %>%
  dplyr::mutate(entity_name = entity_name %>%
                  stringr::str_replace_all("Inner Mongolia 0", "Inner Mongolia ") %>%
                  stringr::str_replace_all("Inner Mongolia C0", "Inner Mongolia C")) %>%
  dplyr::filter(entity_name %in% EMPDv2$site_name) %>%
  tidyr::pivot_longer(-c(1:10)) %>% # Use the pivot to remove empty taxon
  dplyr::filter(!(is.na(value) | value == 0)) %>%
  tidyr::pivot_wider(1:10) %>%
  dplyr::select(1:10, order(colnames(.)[-c(1:10)]) + 10) # Sort the taxon_names alphabetically

Herzschuh_EMPDv2_entity_name_rev <- EMPDv2 %>%
  dplyr::filter(site_name %>%
                  stringr::str_detect("Inner Mongolia")) %>%
  tidyr::pivot_longer(-c(1:13)) %>% # Use the pivot to remove empty taxon
  dplyr::filter(!is.na(value)) %>%
  dplyr::group_by(entity_name) %>%
  # dplyr::mutate(total = sum(value, na.rm = TRUE), # Convert pollen counts to %
  #               value = value / total * 100) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(1:13) %>%
  dplyr::select(1:13, order(colnames(.)[-c(1:13)]) + 13) # Sort the taxon_names alphabetically

Herzschuh_EMPDv2_entity_name[1, ] %>% tidyr::pivot_longer(-c(1:10)) %>% dplyr::filter(!is.na(value)) %>% dplyr::select(1:5, 11:12)
Herzschuh_EMPDv2_entity_name_rev[1, ] %>% tidyr::pivot_longer(-c(1:13)) %>% dplyr::filter(!is.na(value)) %>% dplyr::select(1:7, 14:15)


aux <- compare_latlon(EMPDv2, Herzschuh, digits = 3) %>%
  dplyr::distinct(site_name.y, .keep_all = TRUE)
Herzschuh_subset <- Herzschuh %>%
  dplyr::filter(!(site_name %in% aux$site_name.y))
compare_latlon(EMPDv2, Herzschuh_subset, digits = 2)


idx <- CMPD_excluded$entity_name %in% Herzschuh_CMPD_entity_name_rev$entity_name
CMPD_excluded$entity_name[!idx]
# ------------------------------------------------------------------------------
# |                                  Sandbox                                   |
# ------------------------------------------------------------------------------
Herzschuh %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_count = dplyr::c_across(Abies:Zygophyllum) %>%
                  sum(na.rm = TRUE), .before = Abies) #%>%
  # dplyr::arrange(total_count) %>%
  # dplyr::filter(total_count < 99)

Herzschuh_clean_taxon_names <- readxl::read_xlsx("~/Downloads/SMPDSv2/smpdsv2-APD-Herzschuh-taxon-names-2021-08-05_SPH.xlsx",
                                                 sheet = 1) %>%
  dplyr::distinct() %>%
  dplyr::mutate(action = ifelse(stringr::str_detect(clean_name, "delete"), "delete", "update"),
                clean_name = ifelse(stringr::str_detect(clean_name, "delete"), NA, clean_name)) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::filter(!is.na(taxon_name))

Herzschuh_clean_taxon_names2 <- readr::read_csv("~/Downloads/SMPDSv2/Herzschuh_file2_taxa_SPH.csv")%>%
  dplyr::distinct() %>%
  dplyr::mutate(action = ifelse(stringr::str_detect(clean_name, "delete"), "delete", "update"),
                clean_name = ifelse(stringr::str_detect(clean_name, "delete"), NA, clean_name)) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::filter(!is.na(taxon_name))

Herzschuh_clean_taxon_names %>%
  dplyr::bind_rows(Herzschuh_clean_taxon_names2) %>%
  dplyr::distinct() %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::filter(!is.na(taxon_name)) %>%
  readr::write_csv("inst/extdata/herzschuh_taxa.csv", na = "")

tmp30 <- Herzschuh_file1_long %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(n = length(taxon_name),
                unique_count = length(unique(value))) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(entity_name, taxon_name) %>%
  dplyr::filter(n != 1)
tmp31 <- tmp30 %>%
  dplyr::filter(unique_count > 1)
tmp31 %>%
  dplyr::select(1:6, 8, 7) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/Herzschuh-multiple-records-same-taxon-entity.csv", na = "")

# SPH revisions ----
"The entities and their counts were manually inspected by SPH"

`%>%` <- magrittr::`%>%`
## Load data ----
Herzschuh_all <-
  "data-raw/GLOBAL/Herzschuh_clean_no dups_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  dplyr::rename(doi = DOI) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .after = doi) %>%
  dplyr::relocate(age_BP, .before = ID_BIOME) %>%
  dplyr::rename(basin_size = `basin_size (km2)`)

### Metadata ----
Herzschuh_metadata <-
  Herzschuh_all %>%
  dplyr::select(site_name:ID_SAMPLE)

### Polen counts ----
Herzschuh_counts <-
  Herzschuh_all %>%
  dplyr::select(ID_SAMPLE) %>%
  dplyr::bind_cols(
    Herzschuh_all %>% # Convert columns with counts to numeric type
      dplyr::select(-c(source:ID_SAMPLE)) %>%
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
Herzschuh_taxa_amalgamation <-
  "data-raw/GLOBAL/Herzschuh_clean_no dups_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  dplyr::mutate(clean = clean %>% stringr::str_squish(),
                intermediate = intermediate %>% stringr::str_squish(),
                amalgamated = amalgamated %>% stringr::str_squish())

### Combine counts and amalgamation ----
Herzschuh_taxa_counts_amalgamation <-
  Herzschuh_counts %>%
  dplyr::left_join(Herzschuh_taxa_amalgamation,
                   by = c("clean")) %>%
  dplyr::relocate(taxon_count, .after = amalgamated) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1)

Herzschuh_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated)) %>%
  dplyr::distinct(clean, intermediate, amalgamated)

## Find DOIs ----
Herzschuh_metadata_pubs <-
  Herzschuh_metadata %>%
  dplyr::distinct(publication, doi) %>%
  dplyr::arrange(publication) %>%
  dplyr::mutate(DOI = publication %>%
                  stringr::str_extract_all("\\[DOI\\s*(.*?)\\s*\\](;|$)") %>%
                  purrr::map_chr(~.x %>%
                                   stringr::str_remove_all("^\\[DOI:") %>%
                                   stringr::str_remove_all("\\]\\s*;\\s*$") %>%
                                   stringr::str_remove_all("\\]$") %>%
                                   stringr::str_remove_all("doi:") %>%
                                   stringr::str_squish() %>%
                                   stringr::str_c(collapse = ";\n"))
  ) %>%
  dplyr::mutate(ID_PUB = seq_along(publication)) %>%
  dplyr::mutate(updated_publication = NA, .before = publication) %>%
  dplyr::mutate(updated_DOI = NA, .before = DOI)
# Herzschuh_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/Herzschuh_modern-references.csv")

### Load cleaned publications list ----
Herzschuh_clean_publications <-
  "data-raw/GLOBAL/Herzschuh_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI)

## Append clean publications ----
Herzschuh_metadata_2 <-
  Herzschuh_metadata %>%
  dplyr::left_join(Herzschuh_metadata_pubs %>%
                     dplyr::select(-DOI, -doi, -dplyr::contains("updated")),
                   by = "publication") %>%
  dplyr::left_join(Herzschuh_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi, -ID_PUB) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication)

## Extract PNV/BIOME ----
Herzschuh_metadata_3 <-
  Herzschuh_metadata_2 #%>%
# dplyr::select(-dplyr::starts_with("ID_BIOME")) %>%
# smpds::parallel_extract_biome(cpus = 12) %>%
# # smpds::biome_name() %>%
# dplyr::relocate(ID_BIOME, .after = doi) %>%
# smpds::pb()

Herzschuh_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE),
                    ylim = range(.$latitude, na.rm = TRUE))

## Create count tables ----
### Clean ----
Herzschuh_clean <-
  Herzschuh_taxa_counts_amalgamation %>%
  dplyr::select(-intermediate, -amalgamated) %>%
  dplyr::rename(taxon_name = clean) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     names_sort = TRUE)
### Intermediate ----
Herzschuh_intermediate <-
  Herzschuh_taxa_counts_amalgamation %>%
  dplyr::select(-clean, -amalgamated) %>%
  dplyr::rename(taxon_name = intermediate) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     names_sort = TRUE)

### Amalgamated ----
Herzschuh_amalgamated <-
  Herzschuh_taxa_counts_amalgamation %>%
  dplyr::select(-clean, -intermediate) %>%
  dplyr::rename(taxon_name = amalgamated) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     names_sort = TRUE)

# Store subsets ----
Herzschuh <-
  Herzschuh_metadata_3 %>%
  dplyr::mutate(
    clean = Herzschuh_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = Herzschuh_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = Herzschuh_amalgamated %>%
      dplyr::select(-c(ID_SAMPLE))
  ) %>%
  dplyr::mutate(
    basin_size = basin_size %>%
      stringr::str_replace_all("unknown", "not known"),
    entity_type = entity_type %>%
      stringr::str_replace_all("unknown", "not known") %>%
      stringr::str_to_lower(),
    site_type = site_type %>%
      stringr::str_replace_all("estuarine", "coastal, estuarine") %>%
      stringr::str_replace_all("terrestrial, soil", "soil") %>%
      stringr::str_replace_all("unknown", "not known")
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = clean)

usethis::use_data(Herzschuh, overwrite = TRUE, compress = "xz")

## Inspect enumerates ----
### basin_size -----
Herzschuh$basin_size %>%
  unique() %>% sort()

### site_type ----
Herzschuh$site_type %>%
  unique() %>% sort()

### entity_type ----
Herzschuh$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    Herzschuh %>%
                      dplyr::select(site_name:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    Herzschuh %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    Herzschuh %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    Herzschuh %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/Herzschuh_",
                              Sys.Date(),
                              ".xlsx"))

