## code to prepare `IbMPD` dataset goes here
# Source:
# Harrison, S.P., Shen, Y. and Sweeney, L., 2021. Pollen data and charcoal data
# of the Iberian Peninsula. University of Reading. Dataset.
# http://doi.org/10.17864/1947.294
# Harrison, S.P., Shen, Y. and Sweeney, L., 2022. Pollen data and charcoal data
# of the Iberian Peninsula (version 3). University of Reading. Dataset.
# https://doi.org/10.17864/1947.000369
`%>%` <- magrittr::`%>%`
ibmpd_all <- readr::read_csv("inst/extdata/Iberia_pollen_records_v3.csv",
                             col_types = paste0("ccdddcc",
                                                paste0(rep("d", 213),
                                                       collapse = ""),
                                                collapse = "")) %>%
  magrittr::set_names(colnames(.) %>%
                        stringr::str_replace_all("\\.\\.", " ") %>%
                        stringr::str_remove_all("\\.")) %>%
  dplyr::filter(INTCAL2020_median <= 50) %>%
  # dplyr::filter((!is.na(`IPEage cal`) & `IPEage cal` <= 50) |
  #               (!is.na(INTCAL2020_mean) & INTCAL2020_mean <= 50) |
  #               (!is.na(INTCAL2020_median) & INTCAL2020_median <= 50)) %>%
  # dplyr::group_by(entity_name) %>%
  # dplyr::mutate(entity_name = entity_name %>%
  #                 stringr::str_c("_", seq_along(entity_name))) %>%
  # dplyr::select(-dplyr::starts_with("INTCAL")) %>%
  dplyr::select(-dplyr::contains("INTCAL2020_uncer")) %>%
  dplyr::rename(
    # source = souce,
    avg_depth = `avg_depth cm`,
    # age_BP = `IPEage cal`,
    age_BP = INTCAL2020_median,
    publication = reference
  ) %>%
  dplyr::mutate(avg_depth = avg_depth / 100) %>%
  dplyr::relocate(source, .before = 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-`IPEage cal`, -INTCAL2020_mean)

# ibmpd_all %>%
#   dplyr::filter(age_BP < -72) %>%
#   smpds::rm_zero_taxa(1:9) %>%
#   readr::write_excel_csv("~/Downloads/SMPDSv2/IbMPD_future_records.csv", na = "")

ibmpd_all_2 <- ibmpd_all %>%
  smpds::parallel_extract_biome(buffer = 12000, cpus = 8) %>%
  dplyr::relocate(ID_BIOME, .after = age_BP) %>%
  smpds::pb()

ibmpd_all_2 %>%
  smpds::plot_biome(xlim = range(.$longitude),
                    ylim = range(.$latitude))

ibmpd_sites <- ibmpd_all %>%
  dplyr::distinct(site_name, .keep_all = TRUE) %>%
  dplyr::select(site_name, latitude, longitude) %>%
  dplyr::mutate(ID_BIOME = tibble::tibble(latitude, longitude) %>%
                  smpds::parallel_extract_biome(buffer = 12000, cpus = 4) %>%
                  .$ID_BIOME) %>%
  progressr::with_progress()

# Extract metadata from the RPD

sites_entities_rpd <- rpdata::site %>%
  dplyr::filter(site_name %in% ibmpd_sites$site_name) %>%
  dplyr::arrange(site_name) %>%
  dplyr::left_join(rpdata::entity %>%
                     dplyr::select(-latitude, -longitude, -elevation),
                   by = "ID_SITE") %>%
  dplyr::select(ID_SITE,
                ID_ENTITY,
                site_name,
                entity_name,
                latitude,
                longitude,
                elevation,
                site_type,
                entity_type = TYPE,
                source,
                basin_size_class,
                basin_size_km2) %>%
  dplyr::filter(!(entity_name %in% c("PRD1", "PRD2", "PRD3", "PRD5")))

publications_rpd <- sites_entities_rpd %>%
  rpdata::get_publications() %>%
  dplyr::select(ID_SITE, ID_ENTITY, entity_name, citation, DOI = pub_DOI_URL) %>%
  dplyr::group_by(ID_ENTITY) %>%
  dplyr::mutate(citation = citation %>%
                  stringr::str_c(collapse = ";\n"),
                DOI = DOI %>%
                  # .[!is.na(.)] %>%
                  stringr::str_replace_na("") %>%
                  stringr::str_c(collapse = ";\n")) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::filter(!(entity_name %in% c("PRD1", "PRD2", "PRD3", "PRD5")))

sites_entities_rpd2 <- sites_entities_rpd %>%
  dplyr::left_join(publications_rpd %>%
                     dplyr::select(-entity_name),
                   by = c("ID_SITE", "ID_ENTITY")) %>%
  dplyr::filter(entity_type != "pollen concentration")

ibmpd_all2 <- sites_entities_rpd2 %>%
  dplyr::select(site_name, entity_name, site_type, entity_type, dplyr::starts_with("basin")) %>%
  dplyr::distinct() %>%
  dplyr::right_join(ibmpd_all,
                    by = c("site_name")) %>%
  dplyr::mutate(basin_size = ifelse(!is.na(basin_size_km2) &
                                      basin_size_km2 >= 0,
                                    as.character(basin_size_km2),
                                    basin_size_class)) %>%
  dplyr::select(-basin_size_class, -basin_size_km2)

IbMPD <-
  ibmpd_all_2 %>%
  # ibmpd_all2 %>%
  # dplyr::left_join(ibmpd_sites,
  #                  by = c("site_name", "latitude", "longitude")) %>%
  dplyr::rename(depth = avg_depth) %>%
  dplyr::relocate(ID_BIOME, .after = age_BP) %>%
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(n = length(entity_name),
                entity_name = ifelse(n > 1,
                                     paste0(entity_name, "_", depth),
                                     entity_name)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-n)

dim(IbMPD)
dim(smpds::IbMPD)
# IbMPD$entity_name
# smpds::IbMPD

usethis::use_data(IbMPD, overwrite = TRUE, compress = "xz")

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    IbMPD %>%
                      dplyr::select(site_name:ID_BIOME))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    IbMPD %>%
                      dplyr::select(-c(site_name:ID_BIOME)) #%>%
                    # tidyr::unnest(clean)
)
# openxlsx::addWorksheet(wb, "intermediate")
# openxlsx::writeData(wb, "intermediate",
#                     EMPDv2 %>%
#                       dplyr::select(ID_EMPDv2, intermediate) %>%
#                       tidyr::unnest(intermediate))
# openxlsx::addWorksheet(wb, "amalgamated")
# openxlsx::writeData(wb, "amalgamated",
#                     EMPDv2 %>%
#                       dplyr::select(ID_EMPDv2, amalgamated) %>%
#                       tidyr::unnest(amalgamated))
# openxlsx::addWorksheet(wb, "taxon_list")
# openxlsx::writeData(wb, "taxon_list",
#                     "inst/extdata/all_taxa.csv" %>%
#                       readr::read_csv())
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/IbMPD_",
                              Sys.Date(),
                              ".xlsx"))

# SPH revisions ----
"The entities and their counts were manually inspected by SPH"

`%>%` <- magrittr::`%>%`
## Load data ----
### Metadata ----
IbMPD_metadata <-
  "data-raw/GLOBAL/C_Iberian data_clean.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  dplyr::rename(basin_size = `basin size`) %>%
  dplyr::mutate(
    publication = "Harrison, S.P., Shen, Y. and Sweeney, L., 2022. Pollen data and charcoal data of the Iberian Peninsula (version 3). University of Reading. Dataset.",
    doi = "10.17864/1947.000369"
  ) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .after = doi)

### Pollen counts ----
IbMPD_taxa_counts_amalgamation <-
  "data-raw/GLOBAL/C_Iberian data_clean.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  janitor::clean_names() %>%
  dplyr::left_join(
    IbMPD_metadata %>%
      dplyr::select(entity = entity_name,
                    ID_SAMPLE)
  ) %>%
  dplyr::filter(!is.na(ID_SAMPLE)) %>% # Remove entities without metadata
  dplyr::relocate(ID_SAMPLE, .before = 1) %>%
  dplyr::select(-entity) %>%
  dplyr::mutate(
    clean = clean %>% stringr::str_squish(),
    intermediate = intermediate %>% stringr::str_squish(),
    amalgamated = amalgamated %>% stringr::str_squish()
  )

IbMPD_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated)) %>%
  dplyr::distinct(clean, intermediate, amalgamated)

# ## Find DOIs ----
# IbMPD_metadata_pubs <-
#   IbMPD_metadata %>%
#   dplyr::distinct(publication, doi) %>%
#   dplyr::arrange(publication) %>%
#   dplyr::mutate(DOI = publication %>%
#                   stringr::str_extract_all("\\[DOI\\s*(.*?)\\s*\\](;|$)") %>%
#                   purrr::map_chr(~.x %>%
#                                    stringr::str_remove_all("^\\[DOI:") %>%
#                                    stringr::str_remove_all("\\]\\s*;\\s*$") %>%
#                                    stringr::str_remove_all("\\]$") %>%
#                                    stringr::str_remove_all("doi:") %>%
#                                    stringr::str_squish() %>%
#                                    stringr::str_c(collapse = ";\n"))
#   ) %>%
#   dplyr::mutate(ID_PUB = seq_along(publication)) %>%
#   dplyr::mutate(updated_publication = NA, .before = publication) %>%
#   dplyr::mutate(updated_DOI = NA, .before = DOI)
# # IbMPD_metadata_pubs %>%
# #   readr::write_excel_csv("data-raw/GLOBAL/IbMPD_modern-references.csv")
#
# ### Load cleaned publications list ----
# IbMPD_clean_publications <-
#   "data-raw/GLOBAL/IbMPD_modern-references_clean.csv" %>%
#   readr::read_csv() %>%
#   dplyr::select(-DOI)
#
# ## Append clean publications ----
# IbMPD_metadata_2 <-
#   IbMPD_metadata %>%
#   dplyr::left_join(IbMPD_metadata_pubs %>%
#                      dplyr::select(-DOI, -doi, -dplyr::contains("updated")),
#                    by = "publication") %>%
#   dplyr::left_join(IbMPD_clean_publications,
#                    by = "ID_PUB") %>%
#   dplyr::select(-publication.x, -publication.y, -doi, -ID_PUB) %>%
#   dplyr::rename(doi = updated_DOI,
#                 publication = updated_publication)

## Extract PNV/BIOME ----
IbMPD_metadata_3 <-
  IbMPD_metadata %>%
  # IbMPD_metadata_2 %>%
  dplyr::select(-dplyr::starts_with("ID_BIOME")) %>%
  smpds::parallel_extract_biome(cpus = 10) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

IbMPD_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * c(0.9, 1.1),
                    ylim = range(.$latitude, na.rm = TRUE) * c(0.9, 1.1))

## Create count tables ----
### Clean ----
IbMPD_clean <-
  IbMPD_taxa_counts_amalgamation %>%
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
IbMPD_intermediate <-
  IbMPD_taxa_counts_amalgamation %>%
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
IbMPD_amalgamated <-
  IbMPD_taxa_counts_amalgamation %>%
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
IbMPD <-
  IbMPD_metadata_3 %>%
  dplyr::mutate(
    clean = IbMPD_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = IbMPD_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = IbMPD_amalgamated %>%
      dplyr::select(-c(ID_SAMPLE))
  ) %>%

  dplyr::mutate(
    basin_size_old = basin_size,
    basin_size_num = basin_size %>%
      as.numeric() %>%
      round(digits = 6) %>%
      as.character(),
    basin_size = dplyr::coalesce(
      basin_size_num,
      basin_size
    ),
    basin_size = basin_size %>%
      stringr::str_to_lower() %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("unknown|Unknown", "not known"),
    entity_type = entity_type %>%
      stringr::str_to_lower() %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("unknown|Unknown", "not known") %>%
      stringr::str_to_lower(),
    site_type = site_type %>%
      stringr::str_to_lower() %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("estuarine", "coastal, estuarine") %>%
      stringr::str_replace_all("drained/dry lake", "lacustrine, drained lake") %>%
      stringr::str_replace_all("terrestrial, other sediments", "terrestrial") %>%
      stringr::str_replace_all("terrestrial, soil", "soil") %>%
      stringr::str_replace_all("unknown", "not known")
  ) %>%
  dplyr::relocate(ID_SAMPLE, .before = clean) %>%
  dplyr::select(-basin_size_num, -basin_size_old)

usethis::use_data(IbMPD, overwrite = TRUE, compress = "xz")

# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/IbMPD_climate_reconstructions_2022-04-30.csv" %>%
  readr::read_csv()

climate_reconstructions_with_counts <- smpds::IbMPD %>%
  dplyr::bind_cols(
    climate_reconstructions %>%
      dplyr::select(sn = site_name,
                    en = entity_name,
                    new_elevation = elevation,
                    mi:mtwa)
  ) %>%
  dplyr::relocate(mi:mtwa, .before = clean) %>%
  dplyr::mutate(elevation = dplyr::coalesce(elevation, new_elevation))
climate_reconstructions_with_counts %>%
  dplyr::filter(site_name != sn | entity_name != en)
waldo::compare(smpds::IbMPD,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:mtwa))
)
IbMPD <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation)
usethis::use_data(IbMPD, overwrite = TRUE, compress = "xz")

climate_reconstructions %>%
  smpds::plot_climate_countour(
    var = "mat",
    xlim = range(.$longitude, na.rm = TRUE),
    ylim = range(.$latitude, na.rm = TRUE)
  )

# Inspect enumerates ----
### basin_size -----
IbMPD$basin_size %>%
  unique() %>% sort()

### site_type ----
IbMPD$site_type %>%
  unique() %>% sort()

### entity_type ----
IbMPD$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    IbMPD %>%
                      dplyr::select(source:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    IbMPD %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    IbMPD %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    IbMPD %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/IbMPD_",
                              Sys.Date(),
                              ".xlsx"))
