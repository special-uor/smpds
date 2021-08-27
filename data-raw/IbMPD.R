## code to prepare `IbMPD` dataset goes here
# Source:
# Harrison, S.P., Shen, Y. and Sweeney, L., 2021. Pollen data and charcoal data
# of the Iberian Peninsula. University of Reading. Dataset.
# http://doi.org/10.17864/1947.294
ibmpd_all <- readr::read_csv("inst/extdata/iberia_pollen_records.csv",
                             col_types = paste0("ccdddcc",
                                                paste0(rep("d", 213),
                                                       collapse = ""),
                                                collapse = "")) %>%
  magrittr::set_names(colnames(.) %>%
                        stringr::str_replace_all("\\.\\.", " ") %>%
                        stringr::str_remove_all("\\.")) %>%
  dplyr::filter(INTCAL2020_median <= 100) %>%
  # dplyr::filter((!is.na(`IPEage cal`) & `IPEage cal` <= 50) |
  #               (!is.na(INTCAL2020_mean) & INTCAL2020_mean <= 50) |
  #               (!is.na(INTCAL2020_median) & INTCAL2020_median <= 50)) %>%
  # dplyr::group_by(entity_name) %>%
  # dplyr::mutate(entity_name = entity_name %>%
  #                 stringr::str_c("_", seq_along(entity_name))) %>%
  # dplyr::select(-dplyr::starts_with("INTCAL")) %>%
  dplyr::select(-dplyr::contains("INTCAL2020_uncer")) %>%
  dplyr::rename(source = souce,
                avg_depth = `avg_depth cm`,
                # age_BP = `IPEage cal`,
                age_BP = INTCAL2020_median,
                publication = reference) %>%
  dplyr::mutate(avg_depth = avg_depth / 100) %>%
  dplyr::relocate(source, .before = 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-`IPEage cal`, -INTCAL2020_mean)

ibmpd_all %>%
  dplyr::filter(age_BP < -72) %>%
  smpds::rm_zero_taxa(1:9) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/IbMPD_future_records.csv", na = "")

ibmpd_sites <- ibmpd_all %>%
  dplyr::distinct(site_name, .keep_all = TRUE) %>%
  dplyr::select(site_name, latitude, longitude) %>%
  dplyr::mutate(ID_BIOME = tibble::tibble(latitude, longitude) %>%
                  smpds::parallel_extract_biome(buffer = 12000, cpus = 4) %>%
                  .$ID_BIOME)

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
                                     paste0(entity_name, "_", depth),
                                     entity_name)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-n)

dim(IbMPD)
dim(smpds::IbMPD)
IbMPD$entity_name
smpds::IbMPD

usethis::use_data(IbMPD, overwrite = TRUE, compress = "xz")
