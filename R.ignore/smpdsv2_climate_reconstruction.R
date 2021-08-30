path <- "~/Downloads/SMPDSv2/"
path <- "~/OneDrive - University of Reading/UoR/Data/CRU/4.04/"
CPUS <- 4
N_MAX <- 2000

CPUS <- 20
path <- "/gpfs/scratch/roberto.villegasdiaz/R/CRU"
N_MAX <- nrow(smpds::SMPDSv2)
`%>%` <- magrittr::`%>%`

# ------------------------------------------------------------------------------
# |                          Climate from the CRU TS                           |
# ------------------------------------------------------------------------------
tictoc::tic("CLD")
SMPDSv2_cld <- smpds::SMPDSv2[c(1, 15581), 1:14] %>%
  smpds::gwr(varid = "cld",
             reference = file.path(path, "cru_ts4.04.1901.2019.cld.dat-clim-1961-1990-int.nc"),
             cpus = CPUS)
SMPDSv2_cld %>%
  readr::write_excel_csv(file.path(path, "smpdsv2_climate_reconstructions_cld.csv"), na = "")
tictoc::toc()


tictoc::tic("PRE")
SMPDSv2_pre <- smpds::SMPDSv2[c(1, 15581), 1:14] %>%
  smpds::gwr(varid = "pre",
             reference = file.path(path, "cru_ts4.04.1901.2019.pre.dat-new-clim-1961-1990-int.nc"),
             cpus = CPUS)
SMPDSv2_pre %>%
  readr::write_excel_csv(file.path(path, "smpdsv2_climate_reconstructions_pre.csv"), na = "")
tictoc::toc()


tictoc::tic("TMP")
SMPDSv2_tmp <- smpds::SMPDSv2[, 1:14] %>%
  smpds::gwr(varid = "tmp",
             reference = file.path(path, "cru_ts4.04-clim-1961-1990-daily.tmp.nc"),
             cpus = CPUS)
SMPDSv2_tmp %>%
  readr::write_excel_csv(file.path(path, "smpdsv2_climate_reconstructions_tmp.csv"), na = "")
tictoc::toc()


# cru_ts4.04.1901.2019.vap.dat-clim-1961-1990-int.nc

# ------------------------------------------------------------------------------
# |                              Post-processing                               |
# ------------------------------------------------------------------------------
pivot_data <- function(.data, cols = c(1:14), digits = 6, scale = 1, add = 0, value = "value") {
  .data %>%
    tidyr::pivot_longer(-cols) %>%
    dplyr::mutate(time = name %>%
                    stringr::str_remove_all("^T") %>%
                    as.double(),
                  location = paste0("(", round(latitude, digits = digits),
                                    ",", round(longitude, digits = digits),
                                    ")")) %>%
    dplyr::group_by(entity_name) %>%
    dplyr::mutate(value = list(dplyr::all_of(value) * scale + add)) %>%
    dplyr::ungroup() %>%
    magrittr::set_names(colnames(.) %>%
                          stringr::str_replace_all("value", value)) %>%
    dplyr::distinct(entity_name, .keep_all = TRUE)
}

# SPLASH is driven by daily temperature (tmp), precipitation (pre), cloud coverage (cld), and latitude.
SMPDSv2_cld_rt <-
  readr::read_csv(file.path(path, "smpdsv2_climate_reconstructions_cld.csv"),
                  n_max = N_MAX)
SMPDSv2_cld_rt2 <- SMPDSv2_cld_rt %>% pivot_data(value = "cld")
# Calculate sunshine fraction from cloud cover
SMPDSv2_sf_rt2 <- SMPDSv2_cld_rt %>% pivot_data(scale = -0.01, add = 1, value = "sf")

SMPDSv2_pre_rt <-
  readr::read_csv(file.path(path, "smpdsv2_climate_reconstructions_pre.csv"),
                  n_max = N_MAX)
SMPDSv2_pre_rt2 <- SMPDSv2_pre_rt %>% pivot_data(value = "pre")

SMPDSv2_tmp_rt <-
  readr::read_csv(file.path(path, "smpdsv2_climate_reconstructions_tmp.csv"),
                  n_max = N_MAX)
SMPDSv2_tmp_rt2 <- SMPDSv2_tmp_rt %>% pivot_data(value = "tmp")

SMPDSv2_all_vars <- SMPDSv2_sf_rt2 %>%
  dplyr::left_join(SMPDSv2_pre_rt2) %>%
  dplyr::left_join(SMPDSv2_tmp_rt2) %>%
  dplyr::select(site_name:elevation, age_BP, time, sf, pre, tmp)

orb_params <- SMPDSv2_all_vars$age_BP %>%
  as.double() %>%
  tidyr::replace_na(0) %>%
  purrr::map_df(~palinsol::astro(-.x, degree = TRUE))

SMPDSv2_all_vars2 <- SMPDSv2_all_vars %>%
  dplyr::bind_cols(orb_params)

year <- 1961
oplan <- future::plan(future::multisession, workers = CPUS)
{p <- progressr::progressor(steps = nrow(SMPDSv2_all_vars2))
  SMPDSv2_pet <- seq_len(nrow(SMPDSv2_all_vars2)) %>%
    furrr::future_map(function(k) {
      p()
      purrr::map_dbl(seq_len(365),
                     function(i) {
                       splash::calc_daily_evap(lat = SMPDSv2_all_vars2$latitude[k],
                                               n = i,
                                               elv = SMPDSv2_all_vars2$elevation[k],
                                               y = year,
                                               sf = SMPDSv2_all_vars2$sf[[k]][i],
                                               tc = SMPDSv2_all_vars2$tmp[[k]][i],
                                               ke = SMPDSv2_all_vars2$ecc[k],
                                               keps = SMPDSv2_all_vars2$eps[k],
                                               komega = SMPDSv2_all_vars2$varpi[k])$pet_mm
                     })
    })} %>%
  progressr::with_progress()
future::plan(oplan)

# Calculate MI
SMPDSv2_MI <- seq_len(nrow(SMPDSv2_all_vars2)) %>%
  purrr::map_dbl(~sum(SMPDSv2_all_vars2$pre[[.x]], na.rm = TRUE) / sum(SMPDSv2_pet[[.x]], na.rm = TRUE))

# Calculate GDD0
SMPDSv2_GDD0 <- SMPDSv2_all_vars2 %>%
  purrr::pmap_dbl(function(tmp, ...) {
    tmp[!is.na(tmp) & tmp > 0] %>%
      sum(na.rm = TRUE)
  })

# Calculate MAT
SMPDSv2_MAT <- SMPDSv2_all_vars2 %>%
  purrr::pmap_dbl(function(tmp, ...) {
    tmp %>%
      mean(na.rm = TRUE)
  })

# Calculate MTCO
SMPDSv2_MTCO <- SMPDSv2_all_vars2 %>%
  purrr::pmap_dbl(function(tmp, ...) {
    tibble::tibble(tmp = tmp,
                   date = (seq_along(tmp) - 1) %>% lubridate::as_date(),
                   month = lubridate::month(date)) %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(tmp = mean(tmp,  na.rm = TRUE)) %>%
      .$tmp %>%
      min(na.rm = TRUE)
    # tmp %>%
    #   min(na.rm = TRUE)
  })

# Calculate MTWA
SMPDSv2_MTWA <- SMPDSv2_all_vars2 %>%
  purrr::pmap_dbl(function(tmp, ...) {
    tibble::tibble(tmp = tmp,
                   date = (seq_along(tmp) - 1) %>% lubridate::as_date(),
                   month = lubridate::month(date)) %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(tmp = mean(tmp,  na.rm = TRUE)) %>%
      .$tmp %>%
      max(na.rm = TRUE)
    # tmp %>%
    #   max(na.rm = TRUE)
  })

SMPDSv2_climate <- SMPDSv2_all_vars2 %>%
  dplyr::select(-time) %>%
  dplyr::mutate(mi = SMPDSv2_MI,
                gdd0 = SMPDSv2_GDD0,
                mat = SMPDSv2_MAT,
                mtco = SMPDSv2_MTCO,
                mtwa = SMPDSv2_MTWA,
                .after = age_BP)
plot_climate(SMPDSv2_climate)
p_gdd0 <- smpds::plot_gdd0(SMPDSv2_climate)
p_mat <- smpds::plot_mat(SMPDSv2_climate)
p_mi <- smpds::plot_mi(SMPDSv2_climate)
p_mtco <- smpds::plot_mtco(SMPDSv2_climate)
p_mtwa <- smpds::plot_mtwa(SMPDSv2_climate)

tibble::tibble(x = rep(seq_len(length(SMPDSv2_MTWA)), 3),
               y = c(SMPDSv2_MTWA,
                     SMPDSv2_MTCO,
                     SMPDSv2_MAT),
               z = rep(c("MTWA",
                         "MTCO",
                         "MAT"),
                       each = length(SMPDSv2_MTWA))) %>%
  ggplot2::ggplot(ggplot2::aes(x, y, colour = z)) +
  ggplot2::geom_line()

# ------------------------------------------------------------------------------
# |                                   Plots                                    |
# ------------------------------------------------------------------------------
SMPDSv2_pre %>%
  tidyr::pivot_longer(-c(1:14)) %>%
  dplyr::mutate(time = name %>%
                  stringr::str_remove_all("^T") %>%
                  as.double(),
                location = paste0("(", round(latitude, 3),
                                  ",", round(longitude, 3), ")")) %>%
  ggplot2::ggplot(ggplot2::aes(time, value, colour = location)) +
  ggplot2::geom_line()

SMPDSv2_cld %>%
  tidyr::pivot_longer(-c(1:14)) %>%
  dplyr::mutate(time = name %>%
                  stringr::str_remove_all("^T") %>%
                  as.double(),
                location = paste0("(", round(latitude, 3),
                                  ",", round(longitude, 3), ")")) %>%
  ggplot2::ggplot(ggplot2::aes(time, value, colour = location)) +
  ggplot2::geom_line()
