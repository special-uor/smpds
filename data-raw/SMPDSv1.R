## code to prepare `SMPDSv1` dataset goes here
# The SPECIAL Modern Pollen Dataset
`%>%` <- magrittr::`%>%`
SMPDSv1 <- readxl::read_xlsx("~/Downloads/SMPDSv2/Sandy_s MPDS_20_October_expanded.xlsx",
                             sheet = 1,
                             col_types = c(rep("guess", 10),
                                           rep("numeric", 1555))) %>%
  dplyr::rename(source = SOURCE,
                site_name = `Site Name`,
                entity_name = `Entity name`,
                latitude = Latitude,
                longitude = Longitude,
                elevation = Elevation,
                basin_size = `Basin size  (Km2)`,
                site_type = `Site Type`,
                entity_type = `Entity Type`,
                age_BP = AgeBP) %>%
  dplyr::mutate(ID_SMPDSv1 = seq_along(entity_name), .before = 1) %>%
  smpds::sort_taxa(1:11) %>%
  dplyr::mutate(ID_BIOME = tibble::tibble(latitude, longitude) %>%
                  smpds::parallel_extract_biome(buffer = 12000, cpus = 6) %>%
                  dplyr::filter(!is.na(ID_BIOME)) %>%
                  dplyr::distinct(ID, .keep_all = TRUE) %>%
                  dplyr::right_join(tibble::tibble(ID = seq_along(latitude)),
                                    by = "ID") %>%
                  .$ID_BIOME,
                publication = NA,
                .after = age_BP) # Sort the taxon_names alphabetically

usethis::use_data(SMPDSv1, overwrite = TRUE, compress = "xz")

# ------------------------------------------------------------------------------
# |                         Find matches in the EMPDv2                         |
# ------------------------------------------------------------------------------
compare_latlon(EMPDv2, SMPDSv1, digits = 2)

# ------------------------------------------------------------------------------
# |                                  Sandbox                                   |
# ------------------------------------------------------------------------------
SMPDSv1 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_count = dplyr::c_across(Abies:Zygophyllum) %>%
                  sum(na.rm = TRUE), .before = Abies) #%>%
# dplyr::arrange(total_count) %>%
# dplyr::filter(total_count < 99)


# Find duplicated entity_name
idx <- duplicated(SMPDSv1$entity_name)
SMPDSv1_dup <- SMPDSv1 %>%
  dplyr::filter(entity_name %in% SMPDSv1$entity_name[idx])

SMPDSv1 <- SMPDSv1 %>%
  dplyr::filter(!(ID_SMPDSv1 %in% SMPDSv1_dup$ID_SMPDSv1))

SMPDSv1_long <- SMPDSv1 %>%
  tidyr::pivot_longer(cols = -c(1:12), names_to = "taxon_name") %>%
  dplyr::filter(!is.na(value))

SMPDSv1_wide <- SMPDSv1_long %>%
  tidyr::pivot_wider(1:12, names_from = "taxon_name")
# duplicated_taxa <- SMPDSv1 %>%
#   dplyr::select(dplyr::contains("..."))
# idx <- rowSums(is.na(duplicated_taxa[, 4:6])) != 3
# duplicated_taxa[idx,]
