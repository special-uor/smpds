## code to prepare `Herzschuh` dataset goes here
# Source:
# Herzschuh, U., Cao, X., Laepple, T. et al. Position and orientation of the westerly jet
# determined Holocene rainfall patterns in China. Nat Commun 10, 2376 (2019).
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

Herzschuh <- Herzschuh_file1 %>%
  dplyr::select(-Pann)
usethis::use_data(Herzschuh, overwrite = TRUE)
