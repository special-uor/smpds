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
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(entity_name = entity_name %>%
                  stringr::str_c("_", seq_along(entity_name))) %>%
  # dplyr::select(-dplyr::starts_with("INTCAL")) %>%
  dplyr::select(-dplyr::contains("INTCAL2020_uncer")) %>%
  dplyr::rename(source = souce,
                avg_depth = `avg_depth cm`,
                age_BP = `IPEage cal`,
                publication = reference) %>%
  dplyr::relocate(source, .before = 1) %>%
  dplyr::ungroup()

ibmpd_sites <- ibmpd_all %>%
  dplyr::distinct(site_name, .keep_all = TRUE) %>%
  dplyr::select(site_name, latitude, longitude) %>%
  dplyr::mutate(ID_BIOME = tibble::tibble(latitude, longitude) %>%
                  smpds::parallel_extract_biome(buffer = 12000, cpus = 4) %>%
                  .$ID_BIOME)
  # dplyr::mutate(ID_BIOME = list(latitude, longitude) %>%
  #                 purrr:::pmap_dbl(function(latitude, longitude) {
  #                   tibble::tibble(latitude,
  #                                  longitude) %>%
  #                     sf::st_as_sf(x = ., coords = c("longitude", "latitude")) %>%
  #                     smpds::extract_biome(buffer = 12000) %>%
  #                     dplyr::filter(!is.na(ID_BIOME)) %>%
  #                     dplyr::slice(1) %>%
  #                     .$ID_BIOME
  #                 }))

# Extract metadata from the RPD
sites_entities_rpd <- rpdata::site %>%
  dplyr::filter(site_name %in% ibmpd_sites$site_name) %>%
  dplyr::arrange(site_name) %>%
  dplyr::left_join(rpdata::entity) %>%
  dplyr::select(ID_SITE, ID_ENTITY, site_name, entity_name, latitude, longitude, elevation, site_type, entity_type = TYPE, source, basin_size_class, basin_size_km2)
publications_rpd <- sites_entities_rpd %>%
  rpdata::get_publications() %>%
  dplyr::select(ID_SITE, ID_ENTITY, citation, DOI = pub_DOI_URL)

ibmpd_all2 <- sites_entities_rpd %>%
  dplyr::select(-entity_name) %>%
  dplyr::right_join(ibmpd_all) %>%
  dplyr::mutate(basin_size = ifelse(!is.na(basin_size_km2) &
                                      basin_size_km2 >= 0,
                                    as.character(basin_size_km2),
                                    basin_size_class),
                .after = source) %>%
  dplyr::select(-basin_size_class, -basin_size_km2)

IbMPD <- ibmpd_all %>%
  dplyr::left_join(ibmpd_sites,
                   by = c("site_name", "latitude", "longitude")) %>%
  dplyr::relocate(ID_BIOME, .after = age_BP)

usethis::use_data(IbMPD, overwrite = TRUE, compress = "xz")
