## code to prepare `Herzschuh` dataset goes here
# Source:
# Herzschuh, U., Cao, X., Laepple, T., Dallmeyer, A., Telford, R.J., Ni, J.,
# Chen, F., Kong, Z., Liu, G., Liu, K.B. and Liu, X., 2019. Position and
# orientation of the westerly jet determined Holocene rainfall patterns in China.
# Nature communications, 10(1), pp.1-8.
# https://doi.org/10.1038/s41467-019-09866-8
Herzschuh_file1 <- readr::read_csv("~/Downloads/SMPDSv2/SourceData_China_Herschuh/SourceDataFile1.csv") %>%
  dplyr::rename(ID_HERZSCHUH = ID,
                entity_name = Site.name,
                longitude = Long,
                latitude = Lat,
                elevation = Elev)
Herzschuh_file2 <- readr::read_csv("~/Downloads/SMPDSv2/SourceData_China_Herschuh/SourceDataFile2.csv") %>%
  dplyr::rename(ID_HERZSCHUH = ID,
                country = Country,
                province = Province,
                site_name = Site,
                latitude = Latitude,
                longitude = Longitude,
                elevation = Altitude,
                age_BP = Cal.yr.BP)
# con <- file("~/Downloads/SMPDSv2/SourceData_China_Herschuh/SourceDataFile3.dat", "rb")
# readBin(con, what = "raw", 10e6)
# Herzschuh_file3 <- readr::read_delim("~/Downloads/SMPDSv2/SourceData_China_Herschuh/SourceDataFile3.dat", delim = "\n")

c(colnames(Herzschuh_file1)[-c(1:6)],
  colnames(Herzschuh_file2)[-c(1:8, 235:236)]) %>%
  unique() %>%
  sort()

## Filter taxon_names
Herzschuh_clean_taxon_names <- readxl::read_xlsx("~/Downloads/SMPDSv2/smpdsv2-APD-Herzschuh-taxon-names-2021-08-05_SPH.xlsx",
                                                 sheet = 1) %>%
  dplyr::distinct()

Herzschuh_file1_long <- Herzschuh_file1 %>%
  dplyr::select(-Pann) %>%
  tidyr::pivot_longer(-c(1:5), names_to = "taxon_name") %>%
  dplyr::left_join(Herzschuh_clean_taxon_names,
                   by = "taxon_name") %>%
  dplyr::filter(clean_name != "to delete") %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) # %>%
  # dplyr::select(-taxon_name_original)

tmp30 <- Herzschuh_file1_long %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(n = length(taxon_name),
                unique_count = length(unique(value))) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(entity_name, taxon_name) %>%
  dplyr::filter(n != 1)
tmp31 <- tmp30 %>%
  dplyr::filter(unique_count > 1)
tmp31 %>%
  dplyr::select(1:6, 8, 7) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/Herzschuh-multiple-records-same-taxon-entity.csv", na = "")

Herzschuh <- Herzschuh_file1_2 %>%
  tidyr::pivot_wider(id_cols = 1:5, names_from = "taxon_name")
usethis::use_data(Herzschuh, overwrite = TRUE)
