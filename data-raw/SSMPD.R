## code to prepare `SSMPD` dataset goes here
# Southern Sweden Modern Pollen Data (SSMPD)
# Source:
# Gaillard, M.J., Birks, H.J.B., Emanuelsson, U. and Berglund, B.E., 1992. Modern
# pollen/land-use relationships as an aid in the reconstruction of past land-uses
# and cultural landscapes: an example from south Sweden. Vegetation history and
# archaeobotany, 1(1), pp.3-17.
# https://doi.org/10.1007/BF00190697
ssmpd_counts <- readxl::read_xlsx("inst/extdata/ssmpd.xlsx",
                                   sheet = 1,
                                   skip = 3) %>%
  dplyr::rename(ID_SSMPD = 1,
                entity_name = sites)

ssmpd_metadata <- readr::read_csv("inst/extdata/ssmpd_metadata.csv") %>%
  dplyr::rename(entity_name = sites) %>%
  dplyr::mutate(source = "Gaillard et al., 1992", #"SSMPD",
                site_name = entity_name %>%
                  stringr::str_remove("\\s[A-Z]{1,1}[a-z]{0,1}$"),
                .before = entity_name) %>%
  dplyr::mutate(basin_size = NA,
                site_type = NA,
                entity_type = NA,
                age_BP = "modern",
                ID_BIOME = tibble::tibble(latitude, longitude) %>%
                  smpds::parallel_extract_biome(buffer = 12000, cpus = 6) %>%
                  .$ID_BIOME,
                publication =
                  paste("Gaillard, M.J., Birks, H.J.B., Emanuelsson, U. and",
                        "Berglund, B.E., 1992. Modern pollen/land-use",
                        "relationships as an aid in the reconstruction of past",
                        "land-uses and cultural landscapes: an example from",
                        "south Sweden. Vegetation history and archaeobotany,",
                        "1(1), pp.3-17."),
                DOI = "10.1007/BF00190697")

ssmpd_taxon_names_clean <- readr::read_csv("inst/extdata/ssmpd_taxa.csv")

ssmpd_counts_long <- ssmpd_counts %>%
  tidyr::pivot_longer(-c(1:2), names_to = "taxon_name") %>%
  dplyr::mutate(taxon_name = taxon_name %>%
                  stringr::str_extract("[a-zA-Z\\s]*"),
                ID_SSMPD = seq_along(ID_SSMPD)) %>%
  dplyr::left_join(ssmpd_taxon_names_clean,
                   by = "taxon_name") %>%
  dplyr::filter(action != "delete") %>%
  dplyr::select(-action) %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(value = sum(as.double(value), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
                                    taxon_name_original,
                                    taxon_name)) %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE) %>%
  dplyr::select(-taxon_name_original)

ssmpd_counts_wide <- ssmpd_counts_long %>%
  dplyr::select(-ID_SSMPD) %>%
  tidyr::pivot_wider(id_cols = 1, names_from = "taxon_name") %>%
  dplyr::select(1, order(colnames(.)[-c(1)]) + 1) # Sort the taxon_names alphabetically

SSMPD <- ssmpd_metadata %>%
  dplyr::left_join(ssmpd_counts_wide,
                   by = "entity_name") %>%
  dplyr::mutate(ID_SSMPD = seq_along(ID_SSMPD))

usethis::use_data(SSMPD, overwrite = TRUE, compress = "xz")

# ------------------------------------------------------------------------------
# |                                  Sandbox                                   |
# ------------------------------------------------------------------------------
Herzschuh %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_count = dplyr::c_across(Abies:Zygophyllum) %>%
                  sum(na.rm = TRUE), .before = Abies) #%>%
# dplyr::arrange(total_count) %>%
# dplyr::filter(total_count < 99)


# ------------------------------------------------------------------------------
# |                         Find matches in the EMPDv2                         |
# ------------------------------------------------------------------------------
tolerance <- function(x, digits = 0) {
  # round(x, digits = digits)
  trunc(x * 10 ^ digits) / 10 ^ digits
}
DEC <- 1
tmp <- EMPDv2 %>%
  dplyr::filter(tolerance(latitude, DEC) %in% tolerance(ssmpd_metadata$latitude, DEC),
                tolerance(longitude, DEC) %in% tolerance(ssmpd_metadata$longitude, DEC))

ssmpd_metadata %>%
  dplyr::mutate(lat = tolerance(latitude, DEC),
                lon = tolerance(longitude, DEC)) %>%
  dplyr::inner_join(tmp %>%
                     dplyr::mutate(lat = tolerance(latitude, DEC),
                                   lon = tolerance(longitude, DEC)),
                   by = c("lat", "lon")) %>%
  dplyr::select(-lat, -lon) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/ssmps_matching_records_EMPDv2.csv", na = "")
  # dplyr::select(dplyr::starts_with("latitude")) %>%
  # dput()

EMPDv2 %>%
  dplyr::filter(entity_name %>% stringr::str_detect("South Swede")) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/EMPDv2_south_sweden_site.csv", na = "")

# ------------------------------------------------------------------------------
# |                                  Sandbox                                   |
# ------------------------------------------------------------------------------
ssmpd %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_count = dplyr::c_across(Abies:Zygophyllum) %>%
                  sum(na.rm = TRUE), .before = Abies) #%>%


ssmpd_metadata <- readxl::read_xlsx("inst/extdata/ssmpd.xlsx",
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
ssmpd_metadata %>%
  readr::write_csv("inst/extdata/ssmpd_metadata.csv", na = "")

# sf::st_as_sf(x = ., coords = c("longitude", "latitude")) #%>%


ssmpd_taxon_names_clean <- readxl::read_xlsx("~/Downloads/SMPDSv2/ssmpd_clean names.xlsx",
                                             sheet = 1) %>%
  magrittr::set_names(c("taxon_name", "clean_name")) %>%
  dplyr::mutate(action = ifelse(stringr::str_detect(tolower(clean_name),
                                                    "exclude"),
                                "delete", "update"),
                clean_name = ifelse(stringr::str_detect(tolower(clean_name),
                                                        "exclude"),
                                    NA, clean_name),
                action = ifelse(is.na(action), "update", action),
                clean_name = ifelse(is.na(clean_name), taxon_name, clean_name)) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::filter(!is.na(taxon_name)) %>%
  dplyr::distinct(taxon_name, clean_name, .keep_all = TRUE)


ssmpd_counts_long_unique <- ssmpd_counts_long %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE)

ssmpd_counts_long_dup <- ssmpd_counts_long %>%
  dplyr::filter(!(ID_SSMPD %in% ssmpd_counts_long_unique$ID_SSMPD))

ssmpd_counts_long %>%
  dplyr::filter((entity_name %in% ssmpd_counts_long_dup$entity_name),
                (taxon_name %in% ssmpd_counts_long_dup$taxon_name)) %>%
  dplyr::arrange(entity_name, taxon_name) %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(counts = paste0(sort(unique(value)), collapse = ",") %>%
                  stringr::str_replace_all("0,", "") %>%
                  stringr::str_replace_all("0", "")) %>%
  dplyr::filter(counts != "", stringr::str_detect(counts, ","))  %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE) %>%
  dplyr::select(-ID_SSMPD,
                -value) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/SSMPD_multiple_counts.csv", na = "")
unique(ssmpd_counts_long_dup$taxon_name)
# .$counts %>%
# stringr::str_replace_all("0,", "") %>%
# stringr::str_replace_all("0", "") %>%
# unique()

# dplyr::select(dplyr::contains("..."))
