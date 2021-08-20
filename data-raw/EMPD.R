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
sheets <- c("metadata", "climate", "ecosystems", "counts", "p_vars", "sampleContexts", "sampleTypes", "sampleMethods", "workerRoles", "countries", "ageUncertainties", "locationReliabilities", "groupID")
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
  dplyr::left_join(empdv2_clean_taxon_names,
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

# Create wide version of the unique counts
empdv2_counts_wide <- empdv2_counts2 %>%
  dplyr::distinct(entity_name, taxon_name, count, .keep_all = TRUE) %>%
  tidyr::pivot_wider(id_cols = entity_name, names_from = taxon_name, values_from = count) %>%
  dplyr::select(1, order(colnames(.)[-1]) + 1) # Sort the taxon_names alphabetically

# Attach counts to metadata
EMPDv2_all <- EMPD %>%
  dplyr::full_join(empdv2_counts_wide,
                   by = "entity_name") %>%
  dplyr::mutate(ID_BIOME = tibble::tibble(latitude, longitude) %>%
                  smpds::parallel_extract_biome(buffer = 12000, cpus = 6) %>%
                  .$ID_BIOME,
                .before = publication)

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
