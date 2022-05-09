## code to prepare `EMPD` dataset goes here
# The Eurasian Modern Pollen Database
# Source:
# Davis, B.A., Chevalier, M., Sommer, P., Carter, V.A., Finsinger, W., Mauri, A.,
# Phelps, L.N., Zanon, M., Abegglen, R., Åkesson, C.M. and Alba-Sánchez, F., 2020.
# The Eurasian Modern Pollen Database (EMPD), version 2. Earth system science data,
# 12(4), pp.2423-2445.
# https://doi.org/10.5194/essd-12-2423-2020
# https://doi.pangaea.de/10.1594/PANGAEA.909130?format=html#download
# https://essd.copernicus.org/articles/12/2423/2020/
# sheets <- c("metadata", "climate", "ecosystems", "counts", "p_vars", "sampleContexts", "sampleTypes", "sampleMethods", "workerRoles", "countries", "ageUncertainties", "locationReliabilities", "groupID")
sheets <- c("metadata", "counts", "p_vars")
`%>%` <- magrittr::`%>%`
empdv2_workbook <- sheets %>%
  purrr::map(function(s) {
    readxl::read_xlsx(path = "inst/extdata/empdv2.xlsx",
                      sheet = s)
  }) %>%
  magrittr::set_names(sheets)

empdv2_str <- empdv2_workbook %>%
  purrr::map(~names(.x))

# ------------------------------------------------------------------------------
# |                               Load metadata                                |
# ------------------------------------------------------------------------------
empdv2_metadata <- empdv2_workbook$metadata %>%
  dplyr::select(-dplyr::starts_with("Worker")) %>%
  dplyr::group_by(SampleName) %>%
  dplyr::mutate(DOI = c(DOI1, DOI2, DOI3, DOI4, DOI5) %>%
                  .[!is.na(.)] %>%
                  stringr::str_c(collapse = ";\n"),
                DOI = ifelse(DOI == "", NA, DOI),
                Publication = c(Publication1, Publication2, Publication3, Publication4, Publication5) %>%
                  .[!is.na(.)] %>%
                  stringr::str_c(collapse = ";\n"),
                Publication = ifelse(Publication == "", NA, Publication)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(DOI1, DOI2, DOI3, DOI4, DOI5),
                -c(Publication1, Publication2, Publication3, Publication4, Publication5),
                -Country,
                -LocationReliability) %>%
  dplyr::rename(entity_name = SampleName,
                original_entity_name = OriginalSampleName,
                site_name = SiteName,
                latitude = Latitude,
                longitude = Longitude,
                elevation = Elevation,
                location_notes = LocationNotes,
                area_of_site = AreaOfSite,
                entity_context = SampleContext,
                site_description = SiteDescription,
                vegetation_description = VegDescription,
                entity_type = SampleType,
                sample_method = SampleMethod,
                age_BP = AgeBP) %>%
  dplyr::mutate(ID_EMPDv2 = seq_along(entity_name), .before = 1)

empdv2_metadata_workers <- empdv2_workbook$metadata %>%
  dplyr::select(dplyr::starts_with("Worker"))

# Construct single table with metadata (based on SMPDSv1)
EMPD <- empdv2_metadata %>%
  dplyr::select(ID_EMPDv2,
                source = EMPD_version,
                site_name,
                entity_name,
                latitude,
                longitude,
                elevation,
                basin_size = area_of_site,
                site_type = site_description,
                entity_type = entity_type,
                age_BP,
                publication = Publication,
                DOI) %>%
  dplyr::mutate(basin_size = basin_size * 0.01) # hectares to km2

# ------------------------------------------------------------------------------
# |                             Extract count data                             |
# ------------------------------------------------------------------------------
empdv2_counts <- empdv2_workbook$counts %>%
  dplyr::rename(entity_name = SampleName,
                taxon_name = original_varname
                # taxon_name = acc_varname
                ) %>%
  dplyr::arrange(entity_name,
                 taxon_name) %>%
  dplyr::mutate(ID_COUNT = seq_along(entity_name), .before = 1)

## Filter taxon_names
empdv2_clean_taxon_names <- readr::read_csv("inst/extdata/empdv2_taxa.csv")

empdv2_counts2 <- empdv2_counts %>%
  dplyr::mutate(taxon_name = taxon_name %>%
                  stringr::str_replace_all("-type|-Typ", " type") %>%
                  stringr::str_replace_all("-TYPE", " tyPE")) %>%
  dplyr::left_join(smpds::clean_taxa(), #empdv2_clean_taxon_names %>%
                     # dplyr::bind_rows(smpds::clean_taxa()) %>%
                     # dplyr::distinct(taxon_name,
                     #                 .keep_all = TRUE),
                   by = "taxon_name") %>%
  dplyr::filter(action != "delete") %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) %>%
  dplyr::select(-action, -acc_varname) %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(count = sum(as.double(count), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
                                    taxon_name_original,
                                    taxon_name)) %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE)

# Create a wide version of the unique counts
empdv2_counts_wide <- empdv2_counts2 %>%
  dplyr::distinct(entity_name, taxon_name, count, .keep_all = TRUE) %>%
  tidyr::pivot_wider(id_cols = entity_name, names_from = taxon_name, values_from = count) %>%
  dplyr::select(1, order(colnames(.)[-1]) + 1) # Sort the taxon_names alphabetically

# Attach counts to metadata

# tictoc::tic("Mutate")
# EMPDv2_all22 <- EMPD %>%
#   dplyr::full_join(empdv2_counts_wide,
#                    by = "entity_name") %>%
#   dplyr::mutate(ID_BIOME = tibble::tibble(latitude, longitude) %>%
#                   smpds::parallel_extract_biome(buffer = 12000, cpus = 6) %>%
#                   .$ID_BIOME,
#                 .before = publication) %>%
#   # smpds::parallel_extract_biome(buffer = 12000, cpus = 6) %>%
#   # dplyr::relocate(ID_BIOME, .before = publication) %>%
#   progressr::with_progress()
# tictoc::toc()
tictoc::tic("Pipe")
EMPDv2_all <- EMPD %>%
  dplyr::full_join(empdv2_counts_wide,
                   by = "entity_name") %>%
  # dplyr::mutate(ID_BIOME = tibble::tibble(latitude, longitude) %>%
  #                 smpds::parallel_extract_biome(buffer = 12000, cpus = 6) %>%
  #                 .$ID_BIOME,
  #               .before = publication) %>%
  smpds::parallel_extract_biome(buffer = 12000, cpus = 6) %>%
  dplyr::relocate(ID_BIOME, .before = publication) %>%
  progressr::with_progress()
tictoc::toc()

  # dplyr::mutate(ID_BIOME = tibble::tibble(latitude, longitude) %>%
  #                 smpds::parallel_extract_biome(buffer = 12000, cpus = 6) %>%
  #                 .$ID_BIOME,
  #               .before = publication)

not_applicable_biome_pattern <-
  "Marine|marine|Sea|sea|Coastal|coastal|Open Water|Amur River|Continental slope|Samsun ridge"
EMPDv2_all2 <- EMPDv2_all %>%
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
    ID_BIOME = ifelse(is.na(ID_BIOME),
                      -999999,
                      ID_BIOME)
    )
EMPDv2_all %>%
  dplyr::filter(is.na(ID_BIOME)) %>%
  dplyr::select(1:14)
EMPDv2_all2 %>%
  dplyr::filter(is.na(ID_BIOME)) %>%
  dplyr::select(1:14)

# EMPDv2_all %>% # Records without ID_BIOME
#   dplyr::filter(is.na(ID_BIOME)) %>%
#   dplyr::select(1:14) %>%
#   dplyr::rename(biome = ID_BIOME) %>%
#   readr::write_excel_csv("~/Downloads/SMPDSv2/EMPDV2-records-without-biome_2021-08-20.csv", na = "")

# ------------------------------------------------------------------------------
# |                           Extract other subsets                            |
# ------------------------------------------------------------------------------
EMPDv2_excluded <- EMPDv2_all2 %>%
  dplyr::filter(
    site_name %>% stringr::str_detect("Inner Mongolia") # Herzschuh
  )

EMPDv2 <- EMPDv2_all2 %>%
  dplyr::filter(!(ID_EMPDv2 %in% EMPDv2_excluded$ID_EMPDv2))
  # dplyr::filter(
  #   site_name %>% stringr::str_detect("Inner Mongolia", negate = TRUE) # Herzschuh
  # )

usethis::use_data(EMPDv2, overwrite = TRUE, compress = "xz")

# ------------------------------------------------------------------------------
# |                                  Sandbox                                   |
# ------------------------------------------------------------------------------
EMPDv2 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_count = dplyr::c_across(Abies:Zygophyllum) %>%
                  sum(na.rm = TRUE), .before = Abies) #%>%
  # dplyr::arrange(total_count) %>%
  # dplyr::filter(total_count < 99)

# Find any matches in the SMPDSv1
# aux <- SMPDSv1_long %>%
#   dplyr::filter(entity_name %in% empdv2_counts$entity_name)
empdv2_counts_subset <- empdv2_counts %>%
  dplyr::filter(entity_name %in% SMPDSv1_long$entity_name,
                taxon_name %in% SMPDSv1_long$taxon_name)
EMPDv2_SMPDSv1 <- EMPDv2 %>%
  dplyr::filter(entity_name %in% SMPDSv1$entity_name) %>%
  purrr:::map_dfc(function(col) { # Delete columns with all NA
    if (all(is.na(col)))
      return(NULL)
    col
  })

SMPDSv1_EMPDv2 <- SMPDSv1 %>%
  dplyr::filter(entity_name %in% EMPDv2$entity_name) %>%
  purrr:::map_dfc(function(col) { # Delete columns with all NA
    if (all(is.na(col)))
      return(NULL)
    col
  })

# Compare the values in each subset: SMPDSv1 and EMPD
# aux <- SMPDSv1_EMPDv2$entity_name %>%
#   purrr::map(function(ent) {
#     cols <- intersect(colnames(EMPDv2_SMPDSv1), colnames(SMPDSv1_EMPDv2))
#     waldo::compare(EMPDv2_SMPDSv1 %>%
#                      dplyr::filter(entity_name == ent) %>%
#                      dplyr::select(!!cols),
#                    SMPDSv1_EMPDv2 %>%
#                      dplyr::filter(entity_name == ent) %>%
#                      dplyr::select(!!cols),
#                    x_arg = "EMPDv2",
#                    y_arg = "SMPDSv1",
#                    max_diffs = Inf)
#   })


# aux <- empdv2_counts %>%
#   dplyr::filter(stringr::str_extract(entity_name, "[a-zA-Z]*") %in% SMPDSv1_long$short_entity_name,
#                 taxon_name %in% SMPDSv1_long$taxon_name)
empdv2_counts_filtered <- empdv2_counts %>%
  dplyr::filter(!(ID_COUNT %in% empdv2_counts_subset$ID_COUNT))

empdv2_counts %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE)
  tidyr::pivot_wider(id_cols = ID_COUNT, names_from = taxon_name, values_from = count)

# Clean ups
# empdv2_clean_taxon_names <- readxl::read_xlsx("~/Downloads/SMPDSv2/smpdsv2-taxon-names-2021-08-05_SPH.xlsx",
#                                               sheet = 1) %>%
#   dplyr::distinct() %>%
#   dplyr::mutate(action = ifelse(stringr::str_detect(clean_name, "delete"), "delete", "update"),
#                 clean_name = ifelse(stringr::str_detect(clean_name, "delete"), NA, clean_name)) %>%
#   dplyr::arrange(dplyr::desc(action), taxon_name) %>%
#   dplyr::filter(!is.na(taxon_name))

## Search duplicated counts
empdv2_counts_unique <- empdv2_counts %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE)
empdv2_counts_dup <- empdv2_counts %>%
  dplyr::filter(!(ID_COUNT %in% empdv2_counts_unique$ID_COUNT))

tmp <- empdv2_counts_dup %>%
  # dplyr::slice(1:10) %>%
  purrr::pmap_df(function(entity_name, taxon_name, ...) {
    ent <- entity_name
    tax <- taxon_name
    empdv2_counts %>%
      dplyr::filter(entity_name == ent,
                    taxon_name == tax)
  })

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    EMPDv2 %>%
                      dplyr::select(ID_EMPDv2:DOI))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    EMPDv2 %>%
                      dplyr::select(-c(source:DOI)) #%>%
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
openxlsx::addWorksheet(wb, "taxon_list")
openxlsx::writeData(wb, "taxon_list",
                    empdv2_clean_taxon_names)
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/EMPDv2_",
                              Sys.Date(),
                              ".xlsx"))


# SPH revisions ----
"The entities and their counts were manually inspected by SPH"

`%>%` <- magrittr::`%>%`
## Load data ----
EMPDv2_all <-
  "data-raw/GLOBAL/EMPDv2_only_SPH_clean_no dups.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  dplyr::rename(doi = DOI) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(entity_name), .after = doi)

### Metadata ----
EMPDv2_metadata <-
  EMPDv2_all %>%
  dplyr::select(source:ID_SAMPLE)

### Polen counts ----
EMPDv2_counts <-
  EMPDv2_all %>%
  dplyr::select(ID_SAMPLE) %>%
  dplyr::bind_cols(
    EMPDv2_all %>% # Convert columns with counts to numeric type
      dplyr::select(-c(Comments:ID_SAMPLE)) %>%
      purrr::map_dfc(~.x %>% as.numeric)
  ) %>%
  magrittr::set_names(
    colnames(.) %>%
      stringr::str_replace_all("\\.\\.\\.", "#")
  ) %>%
  tidyr::pivot_longer(-ID_SAMPLE,
                      names_to = "taxon_name",
                      values_to = "taxon_count") %>%
  dplyr::mutate(taxon_name = taxon_name %>%
                  stringr::str_remove_all("\\#[0-9]+$") %>%
                  stringr::str_squish()) %>%
  dplyr::group_by(ID_SAMPLE, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::distinct() %>%
  dplyr::ungroup()

### Amalgamations ----
EMPDv2_taxa_amalgamation <-
  "data-raw/GLOBAL/EMPDv2_only_SPH_clean_no dups.xlsx" %>%
  readxl::read_excel(sheet = 2) %>%
  magrittr::set_names(c(
    "taxon_name", "clean", "intermediate", "amalgamated"
  )) %>%
  dplyr::distinct() %>%
  purrr::map_df(stringr::str_squish)

### Combine counts and amalgamation ----
EMPDv2_taxa_counts_amalgamation <-
  EMPDv2_counts %>%
  dplyr::left_join(EMPDv2_taxa_amalgamation,
                   by = c("taxon_name")) %>%
  dplyr::relocate(taxon_count, .after = amalgamated) %>%
  dplyr::relocate(ID_SAMPLE, .before = 1) %>%
  dplyr::select(-taxon_name)

EMPDv2_taxa_counts_amalgamation %>%
  dplyr::filter(is.na(clean) | is.na(intermediate) | is.na(amalgamated)) %>%
  dplyr::distinct(clean, intermediate, amalgamated)

## Find DOIs ----
EMPDv2_metadata_pubs <-
  EMPDv2_metadata %>%
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
# EMPDv2_metadata_pubs %>%
#   readr::write_excel_csv("data-raw/GLOBAL/EMPDv2_modern-references.csv")

### Load cleaned publications list ----
EMPDv2_clean_publications <-
  "data-raw/GLOBAL/EMPDv2_modern-references_clean.csv" %>%
  readr::read_csv() %>%
  dplyr::select(-DOI)

## Append clean publications ----
EMPDv2_metadata_2 <-
  EMPDv2_metadata %>%
  dplyr::left_join(EMPDv2_metadata_pubs %>%
                     dplyr::select(-DOI, -doi, -dplyr::contains("updated")),
                   by = "publication") %>%
  dplyr::left_join(EMPDv2_clean_publications,
                   by = "ID_PUB") %>%
  dplyr::select(-publication.x, -publication.y, -doi, -ID_PUB) %>%
  dplyr::rename(doi = updated_DOI,
                publication = updated_publication)

## Extract PNV/BIOME ----
EMPDv2_metadata_3 <-
  EMPDv2_metadata_2 %>%
  dplyr::select(-dplyr::starts_with("ID_BIOME")) %>%
  smpds::parallel_extract_biome(cpus = 10) %>%
  # smpds::biome_name() %>%
  dplyr::relocate(ID_BIOME, .after = doi) %>%
  smpds::pb()

EMPDv2_metadata_3 %>%
  smpds::plot_biome(xlim = range(.$longitude, na.rm = TRUE) * c(0.9, 1.1),
                    ylim = range(.$latitude, na.rm = TRUE) * c(0.9, 1.1))

## Create count tables ----
### Clean ----
EMPDv2_clean <-
  EMPDv2_taxa_counts_amalgamation %>%
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
EMPDv2_intermediate <-
  EMPDv2_taxa_counts_amalgamation %>%
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
EMPDv2_amalgamated <-
  EMPDv2_taxa_counts_amalgamation %>%
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

# Extract missing elevations ----
EMPDv2_metadata_4 <-
  EMPDv2_metadata_3 %>%
  dplyr::rename(elevation_original = elevation) %>%
  smpds:::get_elevation(cpus = 12)

EMPDv2_metadata_4 %>%
  dplyr::select(ID_SAMPLE, elevation, elevation_original) %>%
  dplyr::mutate(within_90p =
                  dplyr::between(elevation,
                                 min(c(0.9, 1.1) * elevation_original),
                                 max(c(0.9, 1.1) * elevation_original)),
                within_95p =
                  dplyr::between(elevation,
                                 min(c(0.95, 1.05) * elevation_original),
                                 max(c(0.95, 1.05) * elevation_original)),
                within_975p =
                  dplyr::between(elevation,
                                 min(c(0.975, 1.025) * elevation_original),
                                 max(c(0.975, 1.025) * elevation_original))
  ) %>%
  dplyr::filter(!within_975p)

# Store subsets ----
EMPDv2 <-
  EMPDv2_metadata_3 %>%
  dplyr::mutate(
    clean = EMPDv2_clean %>%
      dplyr::select(-c(ID_SAMPLE)),
    intermediate = EMPDv2_intermediate %>%
      dplyr::select(-c(ID_SAMPLE)),
    amalgamated = EMPDv2_amalgamated %>%
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

usethis::use_data(EMPDv2, overwrite = TRUE, compress = "xz")

# Load climate reconstructions ----
climate_reconstructions <-
  "data-raw/reconstructions/EMPDv2_climate_reconstructions_2022-04-30.csv" %>%
  readr::read_csv()

climate_reconstructions_with_counts <- smpds::EMPDv2 %>%
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
waldo::compare(smpds::EMPDv2,
               climate_reconstructions_with_counts %>%
                 dplyr::select(-c(mi:mtwa))
)
EMPDv2 <- climate_reconstructions_with_counts %>%
  dplyr::select(-sn, -en, -new_elevation)
usethis::use_data(EMPDv2, overwrite = TRUE, compress = "xz")

climate_reconstructions %>%
  smpds::plot_climate_countour(
    var = "mat",
    xlim = range(.$longitude, na.rm = TRUE),
    ylim = range(.$latitude, na.rm = TRUE)
  )

# Inspect enumerates ----
### basin_size -----
EMPDv2$basin_size %>%
  # stringr::str_replace_all("not applicable", "-888888") %>%
  # stringr::str_replace_all("not known", "-999999") %>%
  # as.numeric() %>%
  # round(digits = 6) %>%
  unique() %>%
  sort()

### site_type ----
EMPDv2$site_type %>%
  unique() %>% sort()

### entity_type ----
EMPDv2$entity_type %>%
  unique() %>% sort()

# Export Excel workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata",
                    EMPDv2 %>%
                      dplyr::select(source:ID_SAMPLE))
openxlsx::addWorksheet(wb, "clean")
openxlsx::writeData(wb, "clean",
                    EMPDv2 %>%
                      dplyr::select(ID_SAMPLE, clean) %>%
                      tidyr::unnest(clean))
openxlsx::addWorksheet(wb, "intermediate")
openxlsx::writeData(wb, "intermediate",
                    EMPDv2 %>%
                      dplyr::select(ID_SAMPLE, intermediate) %>%
                      tidyr::unnest(intermediate))
openxlsx::addWorksheet(wb, "amalgamated")
openxlsx::writeData(wb, "amalgamated",
                    EMPDv2 %>%
                      dplyr::select(ID_SAMPLE, amalgamated) %>%
                      tidyr::unnest(amalgamated))
openxlsx::saveWorkbook(wb,
                       paste0("data-raw/GLOBAL/EMPDv2_",
                              Sys.Date(),
                              ".xlsx"))
