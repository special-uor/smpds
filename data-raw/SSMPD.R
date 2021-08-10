## code to prepare `SSMPD` dataset goes here
# Southern Sweden Modern Pollen Data (SSMPD)
# Source:
# Gaillard, M.J., Birks, H.J.B., Emanuelsson, U. and Berglund, B.E., 1992. Modern
# pollen/land-use relationships as an aid in the reconstruction of past land-uses
# and cultural landscapes: an example from south Sweden. Vegetation history and
# archaeobotany, 1(1), pp.3-17.
# https://doi.org/10.1007/BF00190697
ssmpds_counts <- readxl::read_xlsx("inst/extdata/ssmpds.xlsx",
                                   sheet = 1,
                                   skip = 3) %>%
  dplyr::rename(ID_SSMPD = 1)

ssmpds_metadata <- readr::read_csv("inst/extdata/ssmpds_metadata.csv")

ssmpds_counts_long <- ssmpds_counts %>%
  tidyr::pivot_longer(-c(1:2), names_to = "taxon_name") %>%
  dplyr::mutate(taxon_name = taxon_name %>%
                  stringr::str_extract("[a-zA-Z\\s]*"),
                ID_SSMPD = seq_along(ID_SSMPD))
ssmpds_counts_long_unique <- ssmpds_counts_long %>%
  dplyr::distinct(sites, taxon_name, .keep_all = TRUE)
ssmpds_counts_long_dup <- ssmpds_counts_long %>%
  dplyr::filter(!(ID_SSMPD %in% ssmpds_counts_long_unique$ID_SSMPD))
ssmpds_counts_long %>%
  dplyr::filter((sites %in% ssmpds_counts_long_dup$sites),
                (taxon_name %in% ssmpds_counts_long_dup$taxon_name)) %>%
  dplyr::arrange(sites, taxon_name) %>%
  dplyr::group_by(sites, taxon_name) %>%
  dplyr::mutate(counts = paste0(sort(unique(value)), collapse = ",") %>%
                  stringr::str_replace_all("0,", "") %>%
                  stringr::str_replace_all("0", "")) %>%
  dplyr::filter(counts != "", stringr::str_detect(counts, ","))  %>%
  dplyr::distinct(sites, taxon_name, .keep_all = TRUE) %>%
  dplyr::select(-ID_SSMPD,
                entity_name = sites,
                -value) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/SSMPD_multiple_counts.csv", na = "")
unique(ssmpds_counts_long_dup$taxon_name)
# .$counts %>%
  # stringr::str_replace_all("0,", "") %>%
  # stringr::str_replace_all("0", "") %>%
  # unique()

  # dplyr::select(dplyr::contains("..."))
# usethis::use_data(, overwrite = TRUE)

# ------------------------------------------------------------------------------
# |                                  Sandbox                                   |
# ------------------------------------------------------------------------------
SSMPDS %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_count = dplyr::c_across(Abies:Zygophyllum) %>%
                  sum(na.rm = TRUE), .before = Abies) #%>%

ssmpds_metadata <- readxl::read_xlsx("inst/extdata/ssmpds.xlsx",
                                     sheet = 2) %>%
  dplyr::rename(ID_SSMPD = 1,
                latitude = Latitude,
                longitude = Longitude,
                elevation = Elevation) %>%
  dplyr::mutate(elevation = list(latitude, longitude) %>%
                  purrr::pmap_dbl(function(latitude, longitude) {
                    rgbif::elevation(latitude = latitude,
                                     longitude = longitude,
                                     username = "villegar",
                                     elevation_model = "srtm1") %>%
                      .$elevation_geonames
                  }))
ssmpds_metadata %>%
  readr::write_csv("inst/extdata/ssmpds_metadata.csv", na = "")

# sf::st_as_sf(x = ., coords = c("longitude", "latitude")) #%>%
