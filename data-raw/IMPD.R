## code to prepare `IMPD` dataset goes here
# The Italian Modern Pollen Data (IMPD)
IMPD <- readr::read_tsv("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/Appendix4-Italy_lakes_pollen-abundance.tab",
                        skip = 300)
IMPD_A1 <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
                             sheet = 1)
IMPD_A2 <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
                             sheet = 2)
IMPD_A3a <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
                              sheet = 3)
IMPD_A3b <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
                              sheet = 4)
IMPD_A5 <- readxl::read_xlsx("~/Downloads/SMPDSv2/Pollen data from surface-sediment samples from 92 lakes in Italy./Data/appendices.xlsx",
                             sheet = 5)
usethis::use_data(IMPD, overwrite = TRUE)
