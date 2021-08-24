## code to prepare `NEOTOMA` dataset goes here
neotoma_metadata <- readr::read_csv("inst/extdata/neotoma_metadata.csv")

neotoma_count <-
  readxl::read_xlsx("~/Downloads/SMPDSv2/NEOTOMA/Modern samples_neotoma update.xlsx",
                    sheet = 2,
                    col_names = FALSE)
usethis::use_data(NEOTOMA, overwrite = TRUE)


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
