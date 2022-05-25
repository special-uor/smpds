## code to prepare `Phelps` dataset goes here
# Source:
# Phelps, L.N., Chevalier, M., Shanahan, T.M., Aleman, J.C., Courtney‐Mustaphi,
# C., Kiahtipes, C.A., Broennimann, O., Marchant, R., Shekeine, J., Quick, L.J.
# and Davis, B.A., 2020. Asymmetric response of forest and grassy biomes to
# climate variability across the African Humid Period: influenced by
# anthropogenic disturbance?. Ecography, 43(8), pp.1118-1142.
# doi:10.1111/ecog.04990
# Original ----
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
                entity_name = paste0(sigle, "_", depth),
                # entity_name = ifelse(n > 1,
                #                      paste0(sigle, "_", depth),
                #                      sigle),
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

# apd_taxa <- readr::read_csv("inst/extdata/apd_taxa.csv")
# phelps_taxa <- readr::read_csv("inst/extdata/phelps_taxa.csv")
# all_taxa <- readr::read_csv("inst/extdata/all_taxa.csv")
# ref_taxa <- apd_taxa %>%
#   dplyr::bind_rows(phelps_taxa) %>%
#   dplyr::bind_rows(all_taxa %>%
#                      dplyr::select(1:3)) %>%
#   dplyr::mutate(clean_name = clean_name %>%
#                   stringr::str_squish()) %>%
#   dplyr::filter(!is.na(action)) %>%
#   dplyr::distinct() %>%
#   dplyr::arrange(dplyr::desc(action), taxon_name)
phelps_all2 <- phelps_all %>%
  # dplyr::mutate(taxon_name = taxon_name %>%
  #                 stringr::str_replace_all("undiff\\.|undif", "") %>%
  #                 stringr::str_squish()) %>%
  dplyr::left_join(smpds::clean_taxa(), #ref_taxa,
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
# tibble::tibble(taxon_name = sort(unique(phelps_all$taxon_name))) %>%
#   dplyr::mutate(taxon_name = taxon_name %>%
#                   stringr::str_replace_all("undiff\\.|undif", "") %>%
#                   stringr::str_squish()) %>%
#   dplyr::left_join(ref_taxa,
#                    by = "taxon_name") %>%
#   dplyr::filter(is.na(clean_name), is.na(action)) %>%
#   dplyr::mutate(clean_name = taxon_name) %>%
#   dplyr::distinct() %>%
#   readr::write_excel_csv("~/Downloads/SMPDSv2/phelps-taxon_names_2021-08-25v2.csv", na = "")

phelps_all_sum <- phelps_all2 %>%
  dplyr::group_by(site_name, depth, taxon_name) %>%
  dplyr::mutate(count = sum(count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(site_name, depth, taxon_name, .keep_all = TRUE) %>%
  dplyr::select(-ID_PHELPS)

phelps_all_sum_wide <- phelps_all_sum %>%
  tidyr::pivot_wider(6:14, names_from = "taxon_name", values_from = "count") %>%
  smpds::rm_na_taxa(1:9) %>%
  # smpds::rm_zero_taxa(1:9) %>%
  smpds::sort_taxa(1:9)

Phelps_APD <- phelps_all_sum_wide %>%
  dplyr::filter(entity_name %in% smpds::APD$entity_name)
  # dplyr::filter((stringr::str_remove_all(entity_name, "_[.0-9]*$") %in%
  #                  stringr::str_remove_all(smpds::APD$entity_name, "_[0-9]*$")))

Phelps <- phelps_all_sum_wide %>%
  dplyr::filter(!(entity_name %in% smpds::APD$entity_name))
  # dplyr::filter(!(stringr::str_remove_all(entity_name, "_[.0-9]*$") %in%
  #                   stringr::str_remove_all(smpds::APD$entity_name, "_[0-9]*$")))

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

# SPH revisions ----
"The entities and their counts were manually inspected by SPH"

`%>%` <- magrittr::`%>%`
## Load data ----
Phelps_all <-
  "data-raw/GLOBAL/Phelps_2021-09-24_version 2_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .after = doi)

### Metadata ----
Phelps_metadata <-
  Phelps_all %>%
  dplyr::select(site_name:ID_SAMPLE)

### Pollen counts ----
Phelps_counts <-
  Phelps_all %>%
  dplyr::select(ID_SAMPLE) %>%
  dplyr::bind_cols(
    Phelps_all %>% # Convert columns with counts to numeric type
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
  dplyr::ungroup() %>%
  dplyr::mutate(clean = clean %>% stringr::str_squish())

### Amalgamations ----
Phelps_taxa_amalgamation <-
  "data-raw/GLOBAL/Phelps_2021-09-24_version 2_clean_SPH.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  dplyr::mutate(clean = clean %>% stringr::str_squish(),
                intermediate = intermediate %>% stringr::str_squish(),
                amalgamated = amalgamated %>% stringr::str_squish())

### Combine counts and amalgamation ----
Phelps_taxa_counts_amalgamation <-
  Phelps_counts %>%
  dplyr::left_join(Phelps_taxa_amalgamation,
                   by = c("clean")) %>%
  dplyr::relocate(taxon_count, .after = amalgamated) %>%
  # dplyr::left_join(Phelps_metadata %>%
  #                    dplyr::select(entity_name, ID_SAMPLE),
  #                  by = "entity_name") %>%
  # dplyr::select(-entity_name, -taxon_name) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1)

### Additional taxonomic corrections (SPH - May 20th) ----
taxonomic_corrections <- "data-raw/GLOBAL/taxonomic_corrections.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  purrr::map_df(stringr::str_squish)

Phelps_taxa_counts_amalgamation_rev <-
  Phelps_taxa_counts_amalgamation %>%
  dplyr::mutate(ID_COUNT = seq_along(ID_SAMPLE)) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("clean",  "all")),
                   by = c("clean" =  "original_taxon")) %>%
  dplyr::mutate(clean = dplyr::coalesce(corrected_taxon_name,
                                        clean)) %>%
  dplyr::select(-corrected_taxon_name, -level) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("intermediate", "all")),
                   by = c("clean" =  "original_taxon")) %>%
  dplyr::mutate(intermediate = dplyr::coalesce(corrected_taxon_name,
                                               intermediate)) %>%
  dplyr::select(-corrected_taxon_name, -level) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("amalgamated", "all")),
                   by = c("clean" =  "original_taxon")) %>%
  dplyr::mutate(amalgamated = dplyr::coalesce(corrected_taxon_name,
                                              amalgamated)) %>%
  dplyr::select(-corrected_taxon_name, -level) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("all")),
                   by = c("clean" =  "original_taxon")) %>%
  dplyr::mutate(clean = dplyr::coalesce(corrected_taxon_name,
                                        clean)) %>%
  dplyr::select(-corrected_taxon_name, -level) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("all")),
                   by = c("intermediate" =  "original_taxon")) %>%
  dplyr::mutate(intermediate = dplyr::coalesce(corrected_taxon_name,
                                               intermediate)) %>%
  dplyr::select(-corrected_taxon_name, -level) %>%
  dplyr::left_join(taxonomic_corrections %>%
                     dplyr::filter(level %in% c("all")),
                   by = c("amalgamated" =  "original_taxon")) %>%
  dplyr::mutate(amalgamated = dplyr::coalesce(corrected_taxon_name,
                                              amalgamated)) %>%
  dplyr::select(-corrected_taxon_name, -level)

Phelps_taxa_counts_amalgamation_rev %>%
  dplyr::group_by(ID_COUNT) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::filter(n > 1)

waldo::compare(Phelps_taxa_counts_amalgamation %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               Phelps_taxa_counts_amalgamation_rev %>%
                 dplyr::distinct(clean, intermediate, amalgamated),
               max_diffs = Inf)

Phelps_taxa_counts_amalgamation <- Phelps_taxa_counts_amalgamation_rev %>%
  dplyr::filter(!is.na(taxon_count), taxon_count > 0) %>%
  dplyr::select(-ID_COUNT)

Phelps_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated))

## Find DOIs ----
Phelps_metadata_pubs <-
  Phelps_metadata %>%
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
# Phelps_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/Phelps_modern-references.csv")

### Load cleaned publications list ----
Phelps_clean_publications <-
  "data-raw/GLOBAL/Phelps_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI)

## Append clean publications ----
Phelps_metadata_2 <-
  Phelps_metadata %>%
  dplyr::left_join(Phelps_metadata_pubs %>%
                     dplyr::select(-DOI),
                   by = "publication") %>%
  dplyr::left_join(Phelps_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi, -ID_PUB) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication)

## Extract PNV/BIOME ----
Phelps_metadata_3 <-
  Phelps_metadata_2 %>%
  dplyr::select(-dplyr::starts_with("ID_BIOME")) %>%
  smpds::parallel_extract_biome(cpus = 6) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

Phelps_metadata_2 %>%
  smpds::plot_biome()

## Create count tables ----
### Clean ----
Phelps_clean <-
  Phelps_taxa_counts_amalgamation %>%
  dplyr::select(-intermediate, -amalgamated) %>%
  dplyr::rename(taxon_name = clean) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     values_fill = 0,
                     names_sort = TRUE)
### Intermediate ----
Phelps_intermediate <-
  Phelps_taxa_counts_amalgamation %>%
  dplyr::select(-clean, -amalgamated) %>%
  dplyr::rename(taxon_name = intermediate) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     values_fill = 0,
                     names_sort = TRUE)

### Amalgamated ----
Phelps_amalgamated <-
  Phelps_taxa_counts_amalgamation %>%
  dplyr::select(-clean, -intermediate) %>%
  dplyr::rename(taxon_name = amalgamated) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(ID_SAMPLE,
                     names_from = taxon_name,
                     values_from = taxon_count,
                     values_fill = 0,
                     names_sort = TRUE)

# Store subsets ----
Phelps <-
  Phelps_metadata_3 %>%
  dplyr::mutate(
    clean = Phelps_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = Phelps_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = Phelps_amalgamated %>%
      dplyr::select(-c(ID_SAMPLE))
  ) %>%
  dplyr::mutate(
    basin_size_num = basin_size %>%
      as.numeric() %>%
      round(digits = 6) %>%
      as.character(),
    basin_size = dplyr::coalesce(
      basin_size_num,
      basin_size
    ),
    basin_size = basin_size %>%
      stringr::str_replace_all("unknown", "not known"),
    entity_type = entity_type %>%
      stringr::str_replace_all("unknown", "not known"),
    site_type = site_type %>%
      stringr::str_replace_all("unknown", "not known") %>%
      stringr::str_replace_all("terrestrial, soil", "soil") %>%
      stringr::str_replace_all("Drained/dry lake", "lacustrine, drained lake")
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = clean) %>%
  dplyr::mutate(source = "Phelps et al., 2020", .before = 1) %>%
  dplyr::mutate(age_BP = as.character(age_BP)) %>%
  dplyr::select(-basin_size_num)

usethis::use_data(Phelps, overwrite = TRUE, compress = "xz")


# Inspect enumerates ----
### basin_size -----
Phelps$basin_size %>%
  unique() %>% sort()

### site_type ----
Phelps$site_type %>%
  unique() %>% sort()

### entity_type ----
Phelps$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    Phelps %>%
                      dplyr::select(site_name:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    Phelps %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    Phelps %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    Phelps %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/Phelps_",
                              Sys.Date(),
                              ".xlsx"))


# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/phelps_climate_reconstructions_2022-04-29.csv" %>%
  readr::read_csv()

# Load daily values for precipitation to compute MAP (mean annual precipitation)
climate_reconstructions_pre <-
  "data-raw/reconstructions/phelps_climate_reconstructions_pre_2022-04-29.csv" %>%
  readr::read_csv() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(map = sum(dplyr::c_across(T1:T365), na.rm = TRUE), .before = T1)

climate_reconstructions_2 <- climate_reconstructions %>%
  dplyr::bind_cols(climate_reconstructions_pre %>%
                     dplyr::select(map))

climate_reconstructions_with_counts <-
  Phelps %>%
  # smpds::Phelps %>%
  # dplyr::select(-c(mi:map)) %>%
  dplyr::bind_cols(
    climate_reconstructions_2 %>%
      dplyr::select(sn = site_name,
                    en = entity_name,
                    new_elevation = elevation,
                    mi:map)
  ) %>%
  dplyr::relocate(mi:map, .before = clean) %>%
  dplyr::mutate(elevation = dplyr::coalesce(elevation, new_elevation))
climate_reconstructions_with_counts %>%
  dplyr::filter(site_name != sn | entity_name != en)
waldo::compare(smpds::Phelps,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:map, sn, en, new_elevation))
)

Phelps <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation) %>%
  dplyr::mutate(source = "Phelps et al., 2020", .before = 1)
usethis::use_data(Phelps, overwrite = TRUE, compress = "xz")
waldo::compare(smpds::Phelps,
               Phelps,
               max_diffs = Inf)

climate_reconstructions_2 %>%
  smpds::plot_climate_countour(
    var = "mat",
    xlim = range(.$longitude, na.rm = TRUE),
    ylim = range(.$latitude, na.rm = TRUE)
  )

climate_reconstructions_2 %>%
  smpds::plot_climate(
    var = "map",
    xlim = range(.$longitude, na.rm = TRUE),
    ylim = range(.$latitude, na.rm = TRUE)
  )

rm(climate_reconstructions,
   climate_reconstructions_2,
   climate_reconstructions_pre,
   climate_reconstructions_with_counts)
