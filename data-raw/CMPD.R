## code to prepare `CMPD` dataset goes here
# The Chinese Modern Pollen Data by Ni Jian
china_modern_pollen_data <- readxl::read_xlsx("~/Downloads/SMPDSv2/Re_modern_pollen_data_China_NIJIAN/China modern pollen - data.xlsx",
                                              sheet = 2,
                                              skip = 1)
china_modern_pollen_info <- readxl::read_xlsx("~/Downloads/SMPDSv2/Re_modern_pollen_data_China_NIJIAN/China modern pollen - info.xlsx",
                                              sheet = 2,
                                              skip = 1)
names(china_modern_pollen_data)
names(china_modern_pollen_info)

usethis::use_data(CMPD, overwrite = TRUE)
