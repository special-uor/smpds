## code to prepare `IbMPD` dataset goes here
# Source:
# Harrison, S.P., Shen, Y. and Sweeney, L., 2021. Pollen data and charcoal data
# of the Iberian Peninsula. University of Reading. Dataset.
# http://doi.org/10.17864/1947.294
# Pre-processing ------
# Load the original Iberian data and the records and updated age moodels
ibmpd_all <- readr::read_csv("inst/extdata/iberia_pollen_records.csv",
                             col_types = paste0("ccdddcc",
                                                paste0(rep("d", 213),
                                                       collapse = ""),
                                                collapse = "")) %>%
  magrittr::set_names(colnames(.) %>%
                        stringr::str_replace_all("\\.\\.", " ") %>%
                        stringr::str_remove_all("\\.")) %>%
  dplyr::select(-dplyr::contains("INTCAL2020_uncer")) %>%
  dplyr::rename(avg_depth = `avg_depth cm`,
                publication = reference,
                source = souce)

revised_pollen_age_models <-
  readxl:::read_xlsx("~/Downloads/SMPDSv2/Yicheng/pollen_age_0908.xlsx") %>%
  dplyr::rename(avg_depth = depths,
                publication = reference,
                source = souce) %>%
  dplyr::select(-dplyr::contains("uncer"))
  # magrittr::set_names(colnames(.) %>%
  #                       stringr::str_replace_all())
a <- sort(unique(ibmpd_all$entity_name))
b <- sort(unique(revised_pollen_age_models$entity_name))
a[!a %in% b]
b[!b %in% a]

ibmpd_all_part1 <- ibmpd_all %>%
  dplyr::filter(entity_name %in%
                  sort(unique(revised_pollen_age_models$entity_name)))
ibmpd_all_part2 <- ibmpd_all %>%
  dplyr::filter(!entity_name %in%
                  sort(unique(revised_pollen_age_models$entity_name)))

revised_pollen_age_models_part1 <- revised_pollen_age_models %>%
  dplyr::filter(entity_name %in%
                  sort(unique(ibmpd_all$entity_name)))
revised_pollen_age_models_part2 <- revised_pollen_age_models %>%
  dplyr::filter(!entity_name %in%
                  sort(unique(ibmpd_all$entity_name)))

aux <- revised_pollen_age_models_part1 %>%
  dplyr::select(1:6) %>%
  dplyr::right_join(ibmpd_all_part1,
                    by = c("site_name", "entity_name", "avg_depth")) %>%
  dplyr::bind_rows(ibmpd_all_part2) %>%
  dplyr::relocate(mean, .after = INTCAL2020_mean) %>%
  dplyr::relocate(median, .after = INTCAL2020_median) %>%
  dplyr::mutate(age_BP = dplyr::coalesce(mean,
                                         INTCAL2020_mean,
                                         `IPEage cal`),
                .after = publication)

aux2 <- aux %>%
  dplyr::filter(age_BP >= -72, age_BP <= 50) %>%
  dplyr::select(-dplyr::contains("INTCAL2020_uncer")) %>%
  # dplyr::mutate(avg_depth = avg_depth / 100) %>%
  dplyr::relocate(source, .before = 1) %>%
  dplyr::relocate(avg_depth, .after = publication) %>%
  dplyr::ungroup() %>%
  dplyr::select(-`IPEage cal`,
                -INTCAL2020_mean,
                -INTCAL2020_median,
                -mean,
                -median,
                -sample_ids) %>%
  dplyr::arrange(site_name, entity_name, avg_depth)

aux2 %>%
  readr::write_excel_csv("inst/extdata/iberia_pollen_records_v2.csv", na = "")

# Processing -----
ibmpd_all <- readr::read_csv("inst/extdata/iberia_pollen_records_v2.csv",
                             col_types = paste0("cccdddcdd",
                                                paste0(rep("d", 205),
                                                       collapse = ""),
                                                collapse = "")) #%>%
  # magrittr::set_names(colnames(.) %>%
  #                       stringr::str_replace_all("\\.\\.", " ") %>%
  #                       stringr::str_remove_all("\\.")) %>%
  # dplyr::filter(INTCAL2020_median <= 100) %>%
  # # dplyr::filter((!is.na(`IPEage cal`) & `IPEage cal` <= 50) |
  # #               (!is.na(INTCAL2020_mean) & INTCAL2020_mean <= 50) |
  # #               (!is.na(INTCAL2020_median) & INTCAL2020_median <= 50)) %>%
  # # dplyr::group_by(entity_name) %>%
  # # dplyr::mutate(entity_name = entity_name %>%
  # #                 stringr::str_c("_", seq_along(entity_name))) %>%
  # # dplyr::select(-dplyr::starts_with("INTCAL")) %>%
  # dplyr::select(-dplyr::contains("INTCAL2020_uncer")) %>%
  # dplyr::rename(source = souce,
  #               avg_depth = `avg_depth cm`,
  #               # age_BP = `IPEage cal`,
  #               age_BP = INTCAL2020_median,
  #               publication = reference) %>%
  # dplyr::mutate(avg_depth = avg_depth / 100) %>%
  # dplyr::relocate(source, .before = 1) %>%
  # dplyr::ungroup() %>%
  # dplyr::select(-`IPEage cal`, -INTCAL2020_mean)

# ibmpd_all %>%
#   dplyr::filter(age_BP < -72) %>%
#   smpds::rm_zero_taxa(1:9) %>%
#   readr::write_excel_csv("~/Downloads/SMPDSv2/IbMPD_future_records.csv", na = "")

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
  dplyr::select(site_name, site_type, entity_type, dplyr::starts_with("basin")) %>%
  dplyr::distinct() %>%
  dplyr::right_join(ibmpd_all,
                    by = c("site_name")) %>%
  dplyr::mutate(basin_size = ifelse(!is.na(basin_size_km2) &
                                      basin_size_km2 >= 0,
                                    as.character(basin_size_km2),
                                    basin_size_class)) %>%
  dplyr::select(-basin_size_class, -basin_size_km2)


IbMPD <- ibmpd_all2 %>%
  dplyr::left_join(ibmpd_sites,
                   by = c("site_name", "latitude", "longitude")) %>%
  dplyr::rename(depth = avg_depth) %>%
  dplyr::relocate(ID_BIOME, .after = age_BP) %>%
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(n = length(entity_name),
                entity_name = ifelse(n > 1,
                                     paste0(entity_name, "_", depth / 100),
                                     entity_name)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-n)

# Comparisons -----

dim(IbMPD)
dim(smpds::IbMPD)
new <- IbMPD[!IbMPD$entity_name %in% smpds::IbMPD$entity_name, ]
removed <- smpds::IbMPD[!smpds::IbMPD$entity_name %in% IbMPD$entity_name, ]
updated <- IbMPD[IbMPD$entity_name %in% smpds::IbMPD$entity_name, ]

usethis::use_data(IbMPD, overwrite = TRUE, compress = "xz")
