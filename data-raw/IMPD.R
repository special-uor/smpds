## code to prepare `IMPD` dataset goes here
# The Italian Modern Pollen Data (IMPD)
# Source:
# Finsinger, W., Heiri, O., Valsecchi, V., Tinner, W. and Lotter, A.F., 2007.
# Modern pollen assemblages as climate indicators in southern Europe. Global
# Ecology and Biogeography, 16(5), pp.567-582.
# https://doi.org/10.1111/j.1466-8238.2007.00313.x
IMPD <- readr::read_tsv("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/Appendix4-Italy_lakes_pollen-abundance.tab",
                        skip = 300)
IMPD_A1 <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
                             sheet = 1) %>%
  dplyr::rename(site_code = abbreviation,
                latitude = lat,
                longitude = lon,
                elevation = elev) %>%
  dplyr::mutate(latitude = latitude / 10e4,
                longitude = longitude / 10e4)
# IMPD_A2 <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
#                              sheet = 2)
# IMPD_A3a <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
#                               sheet = 3)
# IMPD_A3b <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
#                               sheet = 4)
IMPD_A4 <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
                             sheet = 5) %>%
  dplyr::rename(event = Event,
                latitude = Latitude,
                longitude = Longitude,
                date_time = `Date/Time`,
                lake_name = Lake,
                site_code = `Site (Site code)`,
                lake_ID = `ID (Lake ID)`,
                sample_label = `Sample label`,
                depth = `Depth [m]`,
                depth_top = `Depth top [m]`,
                depth_bottom = `Depth bot [m]`
                )
IMPD <- IMPD_A1 %>%
  dplyr::select(site_code, site_name, EMPDv2_name) %>%
  dplyr::left_join(IMPD_A4,
                   by = "site_code")
usethis::use_data(IMPD, overwrite = TRUE)
