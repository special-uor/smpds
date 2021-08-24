path <- "~/OneDrive - University of Reading/UoR/Data/CRU/4.04/"
path <- "/gpfs/scratch/roberto.villegasdiaz/R/CRU"
CPUS <- 1
`%>%` <- magrittr::`%>%`
SMPDSv2_cld <- smpds::SMPDSv2[c(1, 15581), 1:14] %>%
  smpds::gwr(varid = "cld",
             reference = file.path(path, "cru_ts4.04.1901.2019.cld.dat-clim-1961-1990-int.nc"),
             cpus = CPUS)
SMPDSv2_cld %>%
  readr::write_excel_csv(file.path(path, "smpdsv2_climate_reconstructions_cld.csv"), na = "")

SMPDSv2_cld_rt <- readr::read_csv("~/Downloads/SMPDSv2/smpdsv2_climate_reconstructions_cld.csv", n_max = 2)
SMPDSv2_cld %>%
  tidyr::pivot_longer(-c(1:14)) %>%
  dplyr::mutate(time = name %>%
                  stringr::str_remove_all("^T") %>%
                  as.double(),
                location = paste0("(", round(latitude, 3),
                                  ",", round(longitude, 3), ")")) %>%
  ggplot2::ggplot(ggplot2::aes(time, value, colour = location)) +
  ggplot2::geom_line()



SMPDSv2_pre <- smpds::SMPDSv2[c(1, 15581), 1:14] %>%
  smpds::gwr(varid = "pre",
             reference = file.path(path, "cru_ts4.04.1901.2019.pre.dat-new-clim-1961-1990-int.nc"),
             cpus = CPUS)
SMPDSv2_pre %>%
  readr::write_excel_csv(file.path(path, "smpdsv2_climate_reconstructions_pre.csv"), na = "")

SMPDSv2_pre_rt <- readr::read_csv("~/Downloads/SMPDSv2/smpdsv2_climate_reconstructions_pre.csv", n_max = 10)
SMPDSv2_pre %>%
  tidyr::pivot_longer(-c(1:14)) %>%
  dplyr::mutate(time = name %>%
                  stringr::str_remove_all("^T") %>%
                  as.double(),
                location = paste0("(", round(latitude, 3),
                                  ",", round(longitude, 3), ")")) %>%
  ggplot2::ggplot(ggplot2::aes(time, value, colour = location)) +
  ggplot2::geom_line()



SMPDSv2_tmp <- smpds::SMPDSv2[, 1:14] %>%
  smpds::gwr(varid = "tmp",
             reference = file.path(path, "cru_ts4.04-clim-1961-1990-daily.tmp.nc"),
             cpus = CPUS)
SMPDSv2_tmp %>%
  readr::write_excel_csv(file.path(path, "smpdsv2_climate_reconstructions_tmp.csv"), na = "")

# cru_ts4.04.1901.2019.vap.dat-clim-1961-1990-int.nc
