## code to prepare `CMPD` dataset goes here
# The Chinese Modern Pollen Data by Ni Jian
cmpd_counts <- readxl::read_xlsx("inst/extdata/cmpd_counts.xlsx",
                                 sheet = 2,
                                 skip = 1) %>%
  dplyr::rename(entity_name = `Site name`,
                longitude = `Longitude (°E)`,
                latitude = `Latitude(°N)`,
                elevation = `Elevation (m)`) %>%
  dplyr::mutate(entity_name = entity_name %>%
                  stringr::str_squish()) %>%
  dplyr::slice(-c(1:2))
cmpd_metadata <- readxl::read_xlsx("inst/extdata/cmpd_metadata.xlsx",
                                   sheet = 2,
                                   skip = 1)  %>%
  dplyr::rename(site_name = `Site`,
                entity_name = `Site name`,
                longitude = `Longitude (°E)`,
                latitude = `Latitude(°N)`,
                elevation = `Elevation (m)`,
                entity_type = `Sample type`,
                data_source = `Data source`,
                data_type = `Data type`,
                publication = 21) %>% # 参考文献 (References)
  dplyr::mutate(ID_CMPD = seq_along(entity_name),
                entity_name = entity_name %>%
                  stringr::str_squish(),
                site_name = list(site_name, entity_name) %>%
                  purrr::pmap_chr(function(site_name, entity_name) {
                    site <- iconv(site_name, "latin1", "ASCII", sub = "") %>%
                      stringr::str_remove_all('\\*') %>%
                      stringr::str_remove_all("\\)") %>%
                      stringr::str_squish()
                    if (site != "")
                      entity_name <- entity_name %>%
                        stringr::str_remove_all(pattern = site)
                    entity_name %>%
                      stringr::str_squish() %>%
                      # stringr::str_remove_all("[-]*$") %>%
                      stringr::str_remove_all("HLJ-") %>%
                      stringr::str_remove_all("[0-9-\\*]*$") %>%
                      stringr::str_remove_all("25.*$") %>%
                      stringr::str_squish()
                  }),
                basin_size = NA,
                age_BP = NA,
                DOI = NA,
                source = "CMPD") %>%
  dplyr::mutate(BiomeID = list(latitude, longitude) %>%
                  purrr:::pmap_dbl(function(latitude, longitude) {
                    BiomeID <- tibble::tibble(latitude,
                                   longitude) %>%
                      sf::st_as_sf(x = ., coords = c("longitude", "latitude")) %>%
                      smpds::extract_biome(buffer = 12000) %>%
                      dplyr::filter(!is.na(BiomeID)) %>%
                      dplyr::slice(1) %>%
                      .$BiomeID
                    if (length(BiomeID) == 0)
                      return(NA)
                    BiomeID
                  }))

# ------------------------------------------------------------------------------
# |                                 Clean data                                 |
# ------------------------------------------------------------------------------
## Load table with taxons
cmpd_taxons <- readr::read_csv("inst/extdata/cmpd_taxon.csv")
### Clean taxon names
cmpd_clean_taxon_names <- cmpd_taxons %>%
  dplyr::filter(action == "update")

### Nonsense. categories, to check numbers before deciding whether to delete
cmpd_clean_taxon_names_nonsense <- cmpd_taxons %>%
  dplyr::filter(action == "inspect")

### Irrelevant taxon (to be deleted)
cmpd_clean_taxon_names_delete <- cmpd_taxons %>%
  dplyr::filter(action == "delete")

## Create long version of the count data
cmpd_counts_long <- cmpd_counts %>%
  tidyr::pivot_longer(cols = -c(1:6), names_to = "taxon_name") %>%
  dplyr::filter(!is.na(value))

## Find entities with >1% of the total assemblage in the
## `cmpd_clean_taxon_names_nonsense` list
aux <- cmpd_counts_long %>%
  dplyr::filter(value > 0) %>% # Filter taxon with count = 0
  dplyr::group_by(entity_name) %>% # Group by entity_name
  dplyr::mutate(nonsense = ifelse(taxon_name %in%
                                    cmpd_clean_taxon_names_nonsense$taxon_name,
                                  value, 0) %>%
                  as.double() %>%
                  sum(na.rm = TRUE), # Find % of nonsense taxon
                total = sum(as.double(value), na.rm = TRUE)) %>%
  dplyr::filter(nonsense > 0) %>% # Filter entities without nonsense taxon
  dplyr::distinct(entity_name, .keep_all = TRUE) %>%
  dplyr::select(-taxon_name, -value)

entities_with_high_perc_of_nonsense_taxon <- aux %>%
  dplyr::filter(nonsense > 1)

entities_with_low_perc_of_nonsense_taxon <- aux %>%
  dplyr::filter(nonsense <= 1)

cmpd_counts_long2 <- cmpd_counts_long %>%
  # Filter entities with >1% of the total assemblage of nonsense taxon
  dplyr::filter(!(entity_name %in%
                    entities_with_high_perc_of_nonsense_taxon$entity_name)) %>%
  # Filter counts with <1% of the total assemblage of nonsense taxon
  dplyr::filter(
    !(entity_name %in%
        entities_with_low_perc_of_nonsense_taxon$entity_name) &
    !(taxon_name %in%
        cmpd_clean_taxon_names_nonsense$taxon_name)) %>%
  # Filter irrelevant taxon
  dplyr::filter(!(taxon_name %in%
                    cmpd_clean_taxon_names_delete$taxon_name)) %>%
  # Update taxon names
  dplyr::left_join(cmpd_clean_taxon_names,
                   by = "taxon_name") %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) %>%
  # Aggregate multiple records of entity_name - taxon_name pairs
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(value = sum(as.double(value), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
                                    taxon_name_original,
                                    taxon_name)) %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE)


cmpd_counts_wide <- cmpd_counts_long2 %>%
  dplyr::select(-taxon_name_original) %>%
  tidyr::pivot_wider(1:6, names_from = "taxon_name") %>%
  dplyr::select(1:6, order(colnames(.)[-c(1:6)]) + 6) # Sort the taxon_names alphabetically

CMPD <- cmpd_metadata %>%
  dplyr::select(ID_CMPD,
                source,
                site_name,
                entity_name,
                latitude,
                longitude,
                elevation,
                basin_size,
                entity_type,
                age_BP,
                publication,
                DOI,
                BiomeID
  ) %>%
  dplyr::right_join(cmpd_counts_wide %>%
                      dplyr::select(-c(1:2, 4:6)),
                    by = "entity_name")

usethis::use_data(CMPD, overwrite = TRUE, compress = "xz")


# ------------------------------------------------------------------------------
# |                                  Sandbox                                   |
# ------------------------------------------------------------------------------
CMPD %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_count = dplyr::c_across(Abelia:Zygophyllum) %>%
                  sum(na.rm = TRUE), .before = Abelia) %>%
  dplyr::arrange(total_count) %>%
  dplyr::filter(total_count < 99)

# Find duplicated records
idx <- duplicated(cmpd_counts$entity_name)
cmpd_counts_dup <- cmpd_counts %>%
  dplyr::filter(entity_name %in% cmpd_counts$entity_name[idx])

# ------------------------------------------------------------------------------
# |                   Export nonsense records for inspection                   |
# ------------------------------------------------------------------------------
cmpd_counts_nonsense <- cmpd_counts %>%
  dplyr::select(1:6, !!cmpd_clean_taxon_names_nonsense$taxon_name) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_count = sum(dplyr::c_across(`Abies+Picea`:`Tilia+Ulmus`) %>%
                                    as.numeric(), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(total_count > 0)

cmpd_counts_nonsense2 <- cmpd_metadata %>%
  dplyr::select(ID_CMPD,
                source,
                site_name,
                entity_name,
                latitude,
                longitude,
                elevation
  ) %>%
  dplyr::right_join(cmpd_counts_nonsense %>%
                     dplyr::select(-c(1:2, 4:6)))
cmpd_counts_nonsense2 %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/CMPD-taxons-for-inspection_2021-08-09.csv", na = "")


# ------------------------------------------------------------------------------
# |          Export duplicated record: entity_name - taxon_name pairs          |
# ------------------------------------------------------------------------------
# Find duplicate entity_name - taxon_name pairs
tmp20 <- cmpd_counts_long2 %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(n = length(taxon_name),
                unique_count = length(unique(value))) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(entity_name, taxon_name) %>%
  dplyr::filter(n != 1)
tmp21 <- tmp20 %>%
  dplyr::filter(unique_count > 1)
tmp21 %>%
  dplyr::select(1:7, 9, 8) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/CMPD-multiple-records-same-taxon-entity.csv", na = "")


cmpd_counts_long <- cmpd_counts %>%
  tidyr::pivot_longer(cols = -c(1:6), names_to = "taxon_name") %>%
  dplyr::filter(!is.na(value))  %>%
  dplyr::filter(!(taxon_name %in% cmpd_clean_taxon_names_nonsense$taxon_name),
                !(taxon_name %in% cmpd_clean_taxon_names_delete$taxon_name)) %>%
  dplyr::left_join(cmpd_clean_taxon_names,
                   by = "taxon_name") %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name)

cmpd_counts2 <- cmpd_counts %>%
  dplyr::select(-!!cmpd_clean_taxon_names_nonsense$taxon_name,
                -!!cmpd_clean_taxon_names_delete$taxon_name) %>%
  magrittr::set_names(c(colnames(.)[c(1:6)],
                        tibble::tibble(taxon_name = colnames(.)[-c(1:6)]) %>%
                          dplyr::left_join(cmpd_clean_taxon_names,
                                           by = "taxon_name") %>%
                          .$clean_name))
