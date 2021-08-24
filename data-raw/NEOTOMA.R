## code to prepare `NEOTOMA` dataset goes here
neotoma_metadata <- readr::read_csv("inst/extdata/neotoma_metadata.csv")
neotoma_count <- readr::read_csv("inst/extdata/neotoma_count.csv")
NEOTOMA <- neotoma_metadata %>%
  dplyr::left_join(neotoma_count, by = "entity_name") %>%
  smpds::parallel_extract_biome(buffer = 12000, cpus = 1) %>%
  dplyr::relocate(ID_BIOME, .after = age_BP) %>%
  smpds::sort_taxa(1:11)

usethis::use_data(NEOTOMA, overwrite = TRUE)


# Export list of taxon names for clean-up
tibble::tibble(taxon_name = colnames(NEOTOMA)[-c(1:11)],
               clean_name = taxon_name) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/NEOTOMA_taxa_2021-08-24.csv", na = "")

# ------------------------------------------------------------------------------
# |                                  Sandbox                                   |
# ------------------------------------------------------------------------------
neotoma_metadata <-
  readxl::read_xlsx("~/Downloads/SMPDSv2/NEOTOMA/Modern samples_neotoma update.xlsx",
                    sheet = 1) %>%
  magrittr::set_names(c("source",
                        "site_name",
                        "entity_name",
                        "latitude",
                        "longitude",
                        "elevation",
                        "basin_size",
                        "site_type",
                        "entity_type",
                        "age_BP",
                        "publication")) %>%
  dplyr::group_by(site_name) %>%
  dplyr::mutate(publication = publication %>%
                  stringr::str_c(collapse = ";\n")) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(site_name, .keep_all = TRUE) %>%
  dplyr::mutate(elevation2 = list(latitude, longitude) %>%
                  purrr::pmap_dbl(function(latitude, longitude) {
                    rgbif::elevation(latitude = latitude,
                                     longitude = longitude,
                                     username = "villegar",
                                     elevation_model = "srtm1") %>%
                      .$elevation_geonames
                  }))
neotoma_metadata %>%
  dplyr::mutate(elevation = elevation2) %>%
  dplyr::select(-elevation2) %>%
  readr::write_excel_csv("inst/extdata/neotoma_metadata.csv", na = "")


neotoma_count <-
  readxl::read_xlsx("~/Downloads/SMPDSv2/NEOTOMA/Modern samples_neotoma update.xlsx",
                    sheet = 2,
                    col_names = FALSE)
neotoma_count_unused <- neotoma_count[seq(3, nrow(neotoma_count), 3), ]
neotoma_count2 <- neotoma_count %>%
  dplyr::slice(-seq(3, nrow(neotoma_count), 3))

neotoma_count_long <- seq(1, nrow(neotoma_count2), 2) %>%
  purrr::map_dfr(function(i) {
    names <- neotoma_count2[i, ] %>% purrr:::flatten_chr()
    values <- neotoma_count2[i + 1, ] %>% purrr:::flatten_chr()
    tibble::tibble(entity_name = values[1],
                   taxon_name = names[-1],
                   count = values[-1]) %>%
      dplyr::filter(!is.na(count))
  }) %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(count = as.double(count) %>%
                  sum(na.rm = FALSE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE)
neotoma_count_wide <- neotoma_count_long %>%
  tidyr::pivot_wider(entity_name,
                     names_from = "taxon_name",
                     values_from = "count") %>%
  smpds::sort_taxa() %>%
  smpds::rm_na_taxa()

neotoma_count_wide %>%
  readr::write_excel_csv("inst/extdata/neotoma_count.csv", na = "")
