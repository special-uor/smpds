# code to create the `latin_america_pollen` dataset
# Load raw data ----
latin_america_pollen <-
  "data-raw/GLOBAL/Latin America/Latin American modern 2008_SPH_cleaned.xls" %>%
  readxl::read_excel(sheet = 2) %>%
  dplyr::mutate(A135 = A135 %>%
                  stringr::str_replace_all("6.888.765", "6.888"))

## Metadata ----
latin_america_pollen_metadata <- latin_america_pollen %>%
  dplyr::slice(1:3) %>%
  dplyr::select(-c(2:4)) %>%
  dplyr::rowwise() %>%
  purrr::map_dfr(~suppressWarnings({
    .x %>%
      as.numeric() %>%
      t() %>%
      tibble::as_tibble()
  })) %>%
  dplyr::slice(-1) %>%
  magrittr::set_names(c("longitude", "latitude", "elevation")) %>%
  dplyr::filter(!is.na(longitude), !is.na(latitude)) %>%
  dplyr::mutate(site_id =
                  stringr::str_c("A",
                                 ifelse(seq_along(longitude) < 10, "0", ""),
                                 seq_along(longitude)),
                .before = 1) %>%
  dplyr::mutate(longitude = ifelse(abs(longitude) > 180, NA, longitude),
                publication = "Marchant, R., Cleef, A., Harrison, S.P., Hooghiemstra, H., Markgraf, V., Van Boxel, J., Ager, T., Almeida, L., Anderson, R., Baied, C. and Behling, H., 2009. Pollen-based biome reconstructions for Latin America at 0, 6000 and 18 000 radiocarbon years ago. Climate of the Past, 5(4), pp.725-767.",
                doi = "10.5194/cp-5-725-2009")
### Plot ----
latin_america_pollen_metadata %>%
  smpds::plot_climate("elevation",
                      xlim = range(latin_america_pollen_metadata$longitude,
                                   na.rm = TRUE),
                      contour = TRUE)
## Counts ----
latin_america_pollen_counts <- latin_america_pollen %>%
  dplyr::slice(-c(1:4)) %>%
  magrittr::set_names(c(
    "clean", "intermediate", "amalgamated", "smpdsv1",
    colnames(.)[-c(1:4)]
  )) %>%
  dplyr::mutate(A135 = as.numeric(A135)) %>%
  dplyr::select(clean:A219) %>%
  tidyr::pivot_longer(cols = -c(clean:smpdsv1),
                      names_to = "site_id",
                      values_to = "taxon_count")

latin_america_pollen_counts_clean <-
  latin_america_pollen_counts %>%
  dplyr::select(-c(intermediate, amalgamated, smpdsv1)) %>%
  dplyr::rename(taxon_name = clean) %>%
  dplyr::group_by(site_id, taxon_name) %>%
  dplyr::mutate(taxon_count = sum(taxon_count, na.rm = TRUE)) %>%
  dplyr::distinct() %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(site_id,
                     names_from = "taxon_name",
                     values_from = "taxon_count", names_sort = TRUE)

sa_la_matches <- smpds::compare_latlon(south_america_pollen,
                      latin_america_pollen_metadata)
sa_la_matches_comparison <- sa_la_matches %>%
  purrr::pmap_df(function(entity_name,
                          site_id,
                          latitude.x,
                          longitude.x,
                          elevation.x,
                          latitude.y,
                          longitude.y,
                          elevation.y,
                          ...) {
    # browser()
    sa_counts <- south_america_pollen %>%
      dplyr::filter(entity_name == !!entity_name) %>%
      dplyr::select(clean) %>%
      tidyr::unnest(clean) %>%
      dplyr::mutate(ID_SAMPLE_SA = seq_len(nrow(.)), .before = 1) %>%
      tidyr::pivot_longer(-1) %>%
      dplyr::filter(!is.na(value), value != 0) %>%
      dplyr::rename(count_SA = value)
    la_counts <- latin_america_pollen_counts_clean %>%
      dplyr::filter(site_id == !!site_id) %>%
      dplyr::select(-site_id) %>%
      dplyr::mutate(ID_SAMPLE_LA = seq_len(nrow(.)), .before = 1) %>%
      tidyr::pivot_longer(-1) %>%
      dplyr::filter(!is.na(value), value != 0) %>%
      dplyr::rename(count_LA = value)
    aux <- sa_counts %>%
      dplyr::full_join(la_counts,
                       by = "name") %>%
      dplyr::rename(taxon_name = name)
    tibble::tibble(
      sa_name = entity_name,
      la_name = site_id,
      sa_lat = latitude.x,
      sa_lon = longitude.x,
      sa_elv = elevation.x,
      la_lat = latitude.y,
      la_lon = longitude.y,
      la_elv = elevation.y,
      same_num_counts = nrow(sa_counts) == nrow(la_counts),
      counts = aux
    )
  })

sa_la_matches_comparison %>%
  tidyr::unnest(counts) %>%
  magrittr::set_names(colnames(.) %>%
                        stringr::str_replace_all("la_", "LA_") %>%
                        stringr::str_replace_all("sa_", "SA_")) # %>%
  # readr::write_excel_csv("~/Downloads/sa_la_pollen_comparison.csv", na = "0")


south_america_pollen %>%
  dplyr::filter(entity_name == "AJATA") %>%
  dplyr::select(clean) %>%
  tidyr::unnest(clean) %>%
  dplyr::mutate(ID_SAMPLE_SA = seq_len(nrow(.)), .before = 1) %>%
  tidyr::pivot_longer(-1) %>%
  dplyr::filter(!is.na(value), value != 0)

latin_america_pollen_counts_clean %>%
  dplyr::filter(site_id == "A120") %>%
  dplyr::select(-site_id) %>%
  dplyr::mutate(ID_SAMPLE_LA = seq_len(nrow(.)), .before = 1) %>%
  tidyr::pivot_longer(-1) %>%
  dplyr::filter(!is.na(value), value != 0) %>%
  dplyr::rename(count_LA = value)
