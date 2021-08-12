## code to prepare `ItMPD` dataset goes here
# The Italian Modern Pollen Data (ItMPD)
# Source:
# Finsinger, W., Heiri, O., Valsecchi, V., Tinner, W. and Lotter, A.F., 2007.
# Modern pollen assemblages as climate indicators in southern Europe. Global
# Ecology and Biogeography, 16(5), pp.567-582.
# https://doi.org/10.1111/j.1466-8238.2007.00313.x
# ItMPD <- readr::read_tsv("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/Appendix4-Italy_lakes_pollen-abundance.tab",
#                         skip = 300)
ItMPD_A1 <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
                             sheet = 1) %>%
  dplyr::rename(site_code = abbreviation,
                latitude = lat,
                longitude = lon,
                elevation = elev) %>%
  dplyr::mutate(latitude = latitude / 10e4,
                longitude = longitude / 10e4)
# ItMPD_A2 <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
#                              sheet = 2)
# ItMPD_A3a <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
#                               sheet = 3)
# ItMPD_A3b <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
#                               sheet = 4)
ItMPD_A4 <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
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
ItMPD <- ItMPD_A1 %>%
  dplyr::select(site_code, site_name, EMPDv2_name) %>%
  dplyr::left_join(ItMPD_A4,
                   by = "site_code")
usethis::use_data(ItMPD, overwrite = TRUE)

# ------------------------------------------------------------------------------
# |                         Find matches in the EMPDv2                         |
# ------------------------------------------------------------------------------
compare_latlon(EMPDv2, ItMPD, digits = 2) %>%
  dplyr::filter(entity_name %>% stringr::str_starts("Finsinger")) %>%
  dplyr::filter(site_name.x == EMPDv2_name) %>%
  dplyr::distinct(EMPDv2_name)

aux <- compare_latlon(EMPDv2, ItMPD, digits = 3) %>%
  dplyr::distinct(site_name.y, .keep_all = TRUE)
ItMPD_subset <- ItMPD %>%
  dplyr::filter(!(site_name %in% aux$site_name.y))
compare_latlon(EMPDv2, ItMPD_subset, digits = 2)


EMPDv2_Finsinger <- EMPDv2 %>%
  dplyr::filter(entity_name %>% stringr::str_starts("Finsinger"))
ItMPD %>%
  dplyr::filter(!(EMPDv2_name %in% EMPDv2_Finsinger$site_name)) %>%
  dplyr::select(1:10)

EMPDv2_Finsinger %>%
  dplyr::filter(!(site_name %in% ItMPD$EMPDv2_name))

aux2 <-EMPDv2 %>%
  dplyr::filter(site_name %in% ItMPD$EMPDv2_name)
ItMPD %>%
  dplyr::filter(!(EMPDv2_name %in% aux2$site_name)) %>%
  dplyr::select(1:10)

