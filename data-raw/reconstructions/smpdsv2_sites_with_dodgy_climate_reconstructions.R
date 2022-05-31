# Setup
CPUS <- 10
Z_BUFFER <- NA
path <-
  "/storage/shared/research/met/pacmedy/roberto.villegasdiaz/CRU/4.04/climatologies"
output_path <- "/storage/shared/research/met/pacmedy/roberto.villegasdiaz/smpds"
`%>%` <- magrittr::`%>%`

data <- file.path(output_path, "datasets/smpdsv2_sites_with_dodgy_climate_reconstructions_2022-05-25.csv") %>%
  readr::read_csv()
dataset <- "smpdsv2_sites_with_dodgy_climate_reconstructions"

# ------------------------------------------------------------------------------
# |                          Climate from the CRU TS                           |
# ------------------------------------------------------------------------------
reconstruct_climate <- function(.data,
                                dataset,
                                path,
                                output_path = path,
                                z_buffer = NA,
                                CPUS) {
  if (!file.exists(file.path(
    output_path,
    paste0(dataset, "_climate_reconstructions_cld_", Sys.Date(), ".csv")
  ))) {
    tictoc::tic("CLD")
    .data_cld <- .data %>%
      smpds::gwr(
        varid = "cld",
        .ref =
          file.path(path, "cru_ts4.04.1901.2019.cld.dat-clim-1961-1990-int.nc"),
        cpus = CPUS,
        z_buffer = z_buffer) #%>%
    #smpds::pb()
    .data_cld %>%
      readr::write_excel_csv(
        file.path(
          output_path,
          paste0(dataset, "_climate_reconstructions_cld_", Sys.Date(), ".csv")
        ),
        na = "")
    tictoc::toc()
  } else {
    print("Skipping CLD ...")
  }

  if (!file.exists(file.path(
    output_path,
    paste0(dataset, "_climate_reconstructions_pre_", Sys.Date(), ".csv")
  ))) {
    tictoc::tic("PRE")
    .data_pre <- .data %>%
      smpds::gwr(
        varid = "pre",
        .ref =
          file.path(path,
                    "cru_ts4.04.1901.2019.pre.dat-new-clim-1961-1990-int.nc"),
        cpus = CPUS,
        z_buffer = z_buffer) # %>%
    #smpds::pb()
    .data_pre %>%
      readr::write_excel_csv(
        file.path(
          output_path,
          paste0(dataset, "_climate_reconstructions_pre_", Sys.Date(), ".csv")
        ),
        na = "")
    tictoc::toc()
  } else {
    print("Skipping PRE ...")
  }

  if (!file.exists(file.path(
    output_path,
    paste0(dataset, "_climate_reconstructions_tmp_", Sys.Date(), ".csv")
  ))) {
    tictoc::tic("TMP")
    .data_tmp <- .data %>%
      smpds::gwr(
        varid = "tmp",
        .ref = file.path(path, "cru_ts4.04-clim-1961-1990-daily.tmp.nc"),
        cpus = CPUS,
        z_buffer = z_buffer) # %>%
    #smpds::pb()
    .data_tmp %>%
      readr::write_excel_csv(
        file.path(
          output_path,
          paste0(dataset, "_climate_reconstructions_tmp_", Sys.Date(), ".csv")
        ),
        na = "")
    tictoc::toc()
  } else {
    print("Skipping TMP ...")
  }
}

# ------------------------------------------------------------------------------
# |                              Post-processing                               |
# ------------------------------------------------------------------------------
postprocessing <- function(.data,
                           dataset,
                           output_path,
                           CPUS) {
  N_MAX <- nrow(.data)

  # SPLASH is driven by daily temperature (tmp), precipitation (pre),
  # cloud coverage (cld), and latitude.
  suppressMessages({
    .data_cld <-
      readr::read_csv(
        file.path(
          output_path,
          paste0(dataset, "_climate_reconstructions_cld_", Sys.Date(), ".csv")
        ),
        n_max = N_MAX)

    .data_cld2 <-
      .data_cld %>%
      smpds::pivot_data(varname = "cld")

    # Calculate sunshine fraction from cloud cover
    .data_sf2 <- .data_cld %>%
      smpds::pivot_data(scale = -0.01, add = 1, varname = "sf")

    .data_pre <-
      readr::read_csv(
        file.path(
          output_path,
          paste0(dataset, "_climate_reconstructions_pre_", Sys.Date(), ".csv")
        ),
        n_max = N_MAX)
    .data_pre2 <- .data_pre %>%
      smpds::pivot_data(varname = "pre")

    .data_tmp <-
      readr::read_csv(
        file.path(
          output_path,
          paste0(dataset, "_climate_reconstructions_tmp_", Sys.Date(), ".csv")
        ),
        n_max = N_MAX)
    .data_tmp2 <- .data_tmp %>%
      smpds::pivot_data(varname = "tmp")
  })

  .data_all_vars <- .data_sf2 %>%
    dplyr::left_join(.data_pre2) %>%
    dplyr::left_join(.data_tmp2) %>%
    dplyr::select(site_name:elevation, sf, pre, tmp)

  ## Reconstruct climate variables
  .data_all_vars2 <- .data_all_vars %>%
    smpds::mi(cpus = CPUS) %>%
    smpds::gdd() %>%
    smpds::mat() %>%
    smpds::mtco() %>%
    smpds::mtwa()

  # ## Reconstruct potential natural vegetation (PNV)
  # .data_all_vars3 <- .data_all_vars2 %>%
  #   smpds::parallel_extract_biome() %>%
  #   smpds::biome_name()

  .data_all_vars2 %>%
    dplyr::select(-sf, -tmp, -pre) %>%
    readr::write_excel_csv(
      file.path(
        output_path,
        paste0(dataset, "_climate_reconstructions_", Sys.Date(), ".csv")
      ),
      na = ""
    )
}

reconstruct_climate(data,
                    dataset = dataset,
                    path = path,
                    output_path = output_path,
                    CPUS = CPUS,
                    z_buffer = Z_BUFFER)

postprocessing(data,
               dataset,
               output_path = output_path,
               CPUS = CPUS)

