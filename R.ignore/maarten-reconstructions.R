path <- "~/OneDrive - University of Reading/UoR/Data/CRU/4.04/"
CPUS <- 6
`%>%` <- magrittr::`%>%`
# Maarten reconstructions ------------------------------------------------------
maarten <-
  readxl::read_xlsx("~/Downloads/SMPDSv2/Maarten/Coordinates_Maarten.xlsx") %>%
  dplyr::rename(latitude = Lat,
                longitude = Lon,
                elevation = Altituda) %>%
  dplyr::mutate(latitude = as.double(latitude),
                longitude = as.double(longitude))
## Interpolate climate from the CRU TS dataset
maarten_cld <- maarten %>%
  smpds::gwr(varid = "cld",
             .ref = file.path(path, "cru_ts4.04.1901.2019.cld.dat-clim-1961-1990-int.nc"),
             cpus = CPUS) %>%
  progressr::with_progress()

maarten_pre <- maarten %>%
  smpds::gwr(varid = "pre",
             .ref = file.path(path, "cru_ts4.04.1901.2019.pre.dat-new-clim-1961-1990-int.nc"),
             cpus = CPUS)

maarten_tmp <- maarten %>%
  smpds::gwr(varid = "tmp",
             .ref = file.path(path, "cru_ts4.04-clim-1961-1990-daily.tmp.nc"),
             cpus = CPUS)

## Transform climate reconstructions
maarten_cld2 <- maarten_cld %>%
  smpds::pivot_data(varname = "cld")
### Calculate sunshine fraction from cloud cover
maarten_sf <- maarten_cld %>%
  smpds::pivot_data(scale = -0.01, add = 1, varname = "sf")
maarten_pre2 <- maarten_pre %>%
  smpds::pivot_data(varname = "pre")
maarten_tmp2 <- maarten_tmp %>%
  smpds::pivot_data(varname = "tmp")

maartenv2 <- maarten_sf %>%
  dplyr::left_join(maarten_pre2) %>%
  dplyr::left_join(maarten_tmp2)

## Reconstruct climate variables
maartenv3 <- maartenv2 %>%
  smpds::mi(cpus = CPUS) %>%
  smpds::gdd() %>%
  smpds::mat() %>%
  smpds::mtco() %>%
  smpds::mtwa() %>%
  progressr::with_progress()

## Reconstruct potential natural vegetation (PNV)
maartenv4 <- maartenv3 %>%
  smpds::parallel_extract_biome() %>%
  smpds::biome_name() %>%
  progressr::with_progress()

maartenv5 <- maartenv4 %>%
  dplyr::mutate(map = pre %>%
                  purrr::map_dbl(~sum(.x, na.rm = TRUE)),
                .before = ID_BIOME) # Calculate MAP

# maartenv5 %>%
#   dplyr::select(-sf, -tmp, -pre) %>%
#   readr::write_excel_csv("~/Downloads/SMPDSv2/Maarten/maarten_reconstructions_2021-09-27.csv")

maartenv5 <- readr::read_csv("~/Downloads/SMPDSv2/Maarten/maarten_reconstructions_2021-09-27.csv") %>%
  dplyr::distinct(latitude, longitude, .keep_all = TRUE)

# NetCDF ----
lon_dim <- ncdf4::ncdim_def("longitude", "degrees_east", sort(unique(maartenv5$longitude)))
lat_dim <- ncdf4::ncdim_def("latitude", "degrees_north", sort(unique(maartenv5$latitude)))
time_dim <- ncdf4::ncdim_def("time", "day", 1)

elev_var <- ncdf4::ncvar_def("elevation", "m", list(lon_dim, lat_dim))
gdd0_var <- ncdf4::ncvar_def("gdd0", "Celsius", list(lon_dim, lat_dim, time_dim))#, maartenv5$gdd0)
map_var <- ncdf4::ncvar_def("map", "mm/year", list(lon_dim, lat_dim, time_dim))#, maartenv5$map)
mat_var <- ncdf4::ncvar_def("mat", "Celsius", list(lon_dim, lat_dim, time_dim))#, maartenv5$mat)
mi_var <- ncdf4::ncvar_def("mi", "unitless", list(lon_dim, lat_dim, time_dim))#, maartenv5$mi)
mtco_var <- ncdf4::ncvar_def("mtco", "Celsius", list(lon_dim, lat_dim, time_dim))#, maartenv5$mtco)
mtwa_var <- ncdf4::ncvar_def("mtwa", "Celsius", list(lon_dim, lat_dim, time_dim))#, maartenv5$mtwa)
# mat_var <- ncdf4::ncvar_def("mat", "Celsius", list(lon_dim, lat_dim, time_dim), maartenv5$mat)
nc <- ncdf4::nc_create("~/Downloads/SMPDSv2/Maarten/maarten_reconstructions_2021-09-27.nc",
                       vars = list(elev_var, gdd0_var, map_var, mat_var, mi_var, mtco_var, mtwa_var))
lat_lon_mat <- tibble::tibble(
  ID = seq_len(length(unique(maartenv5$longitude)) *
                 length(unique(maartenv5$latitude))),
  longitude = rep(sort(unique(maartenv5$longitude)), length(unique(maartenv5$latitude))),
  latitude = rep(sort(unique(maartenv5$latitude)), each = length(unique(maartenv5$longitude)))
)

maartenv5_lat_lon_mat <- lat_lon_mat %>%
  dplyr::left_join(maartenv5)
idx <- table(maartenv5_lat_lon_mat$ID) > 1
which(idx)

ncdf4::ncvar_put(nc, "elevation", maartenv5_lat_lon_mat$elevation)
ncdf4::ncvar_put(nc, "gdd0", maartenv5_lat_lon_mat$gdd0)
ncdf4::ncvar_put(nc, "map", maartenv5_lat_lon_mat$map)
ncdf4::ncvar_put(nc, "mat", maartenv5_lat_lon_mat$mat)
ncdf4::ncvar_put(nc, "mi", maartenv5_lat_lon_mat$mi)
ncdf4::ncvar_put(nc, "mtco", maartenv5_lat_lon_mat$mtco)
ncdf4::ncvar_put(nc, "mtwa", maartenv5_lat_lon_mat$mtwa)
# nc <- ncdf4::nc_create("~/Downloads/SMPDSv2/Maarten/maarten_reconstructions_2021-09-27.nc",
#                        vars = list(map_var))
ncdf4::nc_close(nc)


# Plots ----
maartenv5 <- readr::read_csv("~/Downloads/SMPDSv2/Maarten/maarten_reconstructions_2021-09-27.csv") %>%
  dplyr::distinct(latitude, longitude, elevation, .keep_all = TRUE)

show_plot <- FALSE
size <- 2.5
stroke <- 0.1
width <- 9.5
xlim <- c(78, 96)
ylim <- c(49, 57)

# maartenv5 %>%
#   dplyr::mutate(gdd0v2 = cut(include.lowest = TRUE,
#                              round(gdd0, 0),
#                              breaks = c(-Inf, 500, 750, 1000, 1250, 1500, 1750, 2000, 2150, 2500, Inf),
#                              labels = c("0-499", "500-749", "750-999", "1000-1249", "1250-1499", "1500-1749", "1750-1999", "2000-2249", "2250-2499",  "2500+")),
#                 .after = gdd0) %>%
#   dplyr::mutate(mapv2 = cut(include.lowest = TRUE,
#                             round(map, 0),
#                             breaks = c(-Inf, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, Inf),
#                             labels = c("0-299", "300-349", "350-399", "400-449", "450-499", "500-549", "550-599", "600-649", "650-699", "700-749", "750-799","800+")),
#                 .after = map) %>%
#   dplyr::mutate(matv2 = cut(include.lowest = TRUE,
#                             round(mat, 0),
#                             breaks = c(-Inf, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, Inf),
#                             labels = c("0-299", "300-349", "350-399", "400-449", "450-499", "500-549", "550-599", "600-649", "650-699", "700-749", "750-799","800+")),
#                 .after = mat) %>%
#   dplyr::mutate(miv2 = cut(include.lowest = TRUE,
#                            mi,
#                            breaks = c(-Inf, 0.5, 1.0, 1.5, Inf),
#                            labels = c("0-0.49", "0.5-0.99", "1.0-1.49", "1.5+")),
#                 .after = mi) %>%
#   dplyr::mutate(mtco2 = cut(include.lowest = TRUE,
#                             round(mtco, 0),
#                             breaks = c(-Inf, -27.5, -25, -22.5, -20, -17.5, Inf),
#                             labels = c("<-27.5", "-27.5,-25.01", "-25,-22.51", "-22.5,-20.01", "-20,-17.51", "-17.5,-15.01","-15>")),
#                 .after = mtco)

# GDD0 -----
p_gdd0 <- maartenv5 %>%
  dplyr::mutate(gdd0v2 = cut(include.lowest = TRUE,
                             round(gdd0, 0),
                             breaks = c(-Inf, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, Inf),
                             labels = c("0-499", "500-749", "750-999", "1000-1249", "1250-1499", "1500-1749", "1750-1999", "2000-2249", "2250-2499",  "2500+")),
                .after = gdd0) %>%
  dplyr::mutate(gdd0 = gdd0v2) %>%
  plot_gdd(size = size,
           stroke = stroke,
           xlim = xlim,
           ylim = ylim,
           show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_gdd0_", Sys.Date(), ".pdf")),
                plot = p_gdd0,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")
# MAP ----
p_map <- maartenv5 %>%
  dplyr::mutate(mapv2 = cut(include.lowest = TRUE,
                            round(map, 0),
                            breaks = c(-Inf, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, Inf),
                            labels = c("0-299", "300-349", "350-399", "400-449", "450-499", "500-549", "550-599", "600-649", "650-699", "700-749", "750-799","800+")),
                .after = map) %>%
  dplyr::mutate(map = mapv2) %>%
  plot_climate(var = "map",
               units = "mm/year",
               size = size,
               stroke = stroke,
               xlim = xlim,
               ylim = ylim,show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_map_", Sys.Date(), ".pdf")),
                plot = p_map,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# MAT ----
p_mat <- maartenv5 %>%
  dplyr::mutate(matv2 = cut(include.lowest = TRUE,
                            round(mat, 0),
                            breaks = c(-Inf, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, Inf),
                            labels = c("<-7", "-7,-6.01", "-6,-5.01", "-5,-4.01", "-4,-3.01", "-3,-2.01", "-2,-1.01", "-1,-0.01", "0,0.99","1,1.99", "2+")),
                .after = mat) %>%
  dplyr::mutate(mat = matv2) %>%
  plot_mat(size = size,
           stroke = stroke,
           xlim = xlim,
           ylim = ylim,show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_mat_", Sys.Date(), ".pdf")),
                plot = p_mat,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# MI ----
p_mi <- maartenv5 %>%
  dplyr::mutate(miv2 = cut(include.lowest = TRUE,
                           round(mi, 3),
                           breaks = c(-Inf, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, Inf),
                           labels = c("0-0.399", "0.4-0.499", "0.5-0.599", "0.6-0.699", "0.7-0.799", "0.8-0.899", "0.9-0.999","1-1.999", "1.1+")),
                .after = mi) %>%
  dplyr::mutate(mi = miv2) %>%
  plot_mi(size = size,
          stroke = stroke,
          xlim = xlim,
          ylim = ylim,
          show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_mi_", Sys.Date(), ".pdf")),
                plot = p_mi,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# MTCO ----
p_mtco <- maartenv5 %>%
  dplyr::mutate(mtcov2 = cut(include.lowest = TRUE,
                            round(mtco, 0),
                            breaks = c(-Inf, -27.5, -25, -22.5, -20, -17.5, -15, Inf),
                            labels = c("<-27.5", "-27.5,-25.01", "-25,-22.51", "-22.5,-20.01", "-20,-17.51", "-17.5,-15.01","-15>")),
                .after = mtco) %>%
  dplyr::mutate(mtco = mtcov2) %>%
  plot_climate(var = "mtco",
               units = "\u00B0C",
               fill_scale =
                 ggplot2::scale_fill_brewer(name = "MTCO",
                                            palette = "Spectral",
                                            direction = -1),
               # plot_mtco(
               size = size,
               stroke = stroke,
               xlim = xlim,
               ylim = ylim,
               show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_mtco_", Sys.Date(), ".pdf")),
                plot = p_mtco,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# MTWA ----
p_mtwa <- maartenv5 %>%
  dplyr::mutate(mtwav2 = cut(include.lowest = TRUE,
                            round(mtwa, 2),
                            breaks = c(-Inf, 10, 12.5, 15, 17.5, 20, Inf),
                            labels = c("7.5-9.99", "10-12.49", "12.5-14.99", "15-17.49", "17.5-19.99", "20+")),
                .after = mtwa) %>%
  dplyr::mutate(mtwa = mtwav2) %>%
  plot_climate(var = "mtwa",
               units = "\u00B0C",
               fill_scale =
                 ggplot2::scale_fill_brewer(name = "MTWA",
                                            palette = "Spectral",
                                            direction = -1),
               size = size,
               stroke = stroke,
               xlim = xlim,
               ylim = ylim,
               show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_mtwa_", Sys.Date(), ".pdf")),
                plot = p_mtwa,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

p_biome <- maartenv5 %>%#%>%
  # dplyr::mutate(ID_BIOME = smpds::pnv_classes()$ID_BIOME %>%
  #                 sample(size = length(ID_BIOME), replace = TRUE)),
  plot_biome(size = size,
             stroke = stroke,
             xlim = xlim,
             ylim = ylim,
             legend.key.width = ggplot2::unit(1.3, "cm"),
             show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_PNV_", Sys.Date(), ".pdf")),
                plot = p_biome,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# Contours ----
# GDD0 -----
p_gdd0_contour <- maartenv5 %>%
  plot_climate_countour(var = "gdd0",
                        units = "\u00B0C days",
                        fill_scale =
                          ggplot2::scale_fill_fermenter(name = "GDD0",
                                                        palette = "Spectral",
                                                        direction = -1,
                                                        breaks = c(750, 1000, 1250, 1500, 1750, 2000, 2250, 2500),
                                                        limits = c(750, 2790)),
                        legend.key.width = ggplot2::unit(2.65, "cm"),
                        size = size,
                        xlim = xlim,
                        ylim = ylim,
                        show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_gdd0_", Sys.Date(), "_contour.pdf")),
                plot = p_gdd0_contour,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# MAP ----
p_map_contour <- maartenv5 %>%
  plot_climate_countour(var = "map",
                        units = "mm/year",
                        fill_scale =
                          ggplot2::scale_fill_fermenter(name = "MAP",
                                                        palette = "Spectral",
                                                        direction = 1,
                                                        breaks = c(350, 400, 450, 500, 550, 700, 750, 800),
                                                        limits = c(300, 810)),
                        legend.key.width = ggplot2::unit(2.5, "cm"),
                        size = size,
                        xlim = xlim,
                        ylim = ylim,
                        show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_map_", Sys.Date(), "_contour.pdf")),
                plot = p_map_contour,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# MAT ----
p_mat_contour <- maartenv5 %>%
  plot_climate_countour(var = "mat",
                        units = "\u00B0C",
                        fill_scale =
                          ggplot2::scale_fill_fermenter(name = "MAT",
                                                        palette = "Spectral",
                                                        direction = -1,
                                                        na.value = "#5E4FA2",
                                                        breaks = c(-7, -6, -5, -4, -3, -2, -1, 0, 1, 2),
                                                        limits = c(-9, 3)),
                        legend.key.width = ggplot2::unit(2.85, "cm"),
                        size = size,
                        xlim = xlim,
                        ylim = ylim,
                        show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_mat_", Sys.Date(), "_contour.pdf")),
                plot = p_mat_contour,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")


# MI ----
p_mi_contour <- maartenv5 %>%
  plot_climate_countour(var = "mi",
                        units = "unitless",
                        fill_scale =
                          ggplot2::scale_fill_fermenter(name = "MI",
                                                        direction = 1,
                                                        palette = "YlGnBu",
                                                        breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1),
                                                        limits = c(0.4, 1.3)),
                        legend.key.width = ggplot2::unit(2.45, "cm"),
                        size = size,
                        xlim = xlim,
                        ylim = ylim,
                        show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_mi_", Sys.Date(), "_contour.pdf")),
                plot = p_mi_contour,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# MTCO ----
p_mtco_contour <- maartenv5 %>%
  plot_climate_countour(var = "mtco",
                        units = "\u00B0C",
                        fill_scale =
                          ggplot2::scale_fill_fermenter(name = "MTCO",
                                                        palette = "Spectral",
                                                        direction = -1,
                                                        breaks = c(-30, -27.5, -25, -22.5, -20, -17.5),
                                                        limits = c(-30, -14)),
                        legend.key.width = ggplot2::unit(2.25, "cm"),
                        size = size,
                        xlim = xlim,
                        ylim = ylim,
                        show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_mtco_", Sys.Date(), "_contour.pdf")),
                plot = p_mtco_contour,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# MTWA ----
p_mtwa_countour <- maartenv5 %>%
  plot_climate_countour(var = "mtwa",
                        units = "\u00B0C",
                        fill_scale =
                          ggplot2::scale_fill_fermenter(name = "MTWA",
                                                        palette = "Spectral",
                                                        direction = -1,
                                                        breaks = c(7.5, 10, 12.5, 15, 17.5, 20),
                                                        limits = c(7.5, 21)),
                        size = size,
                        xlim = xlim,
                        ylim = ylim,
                        show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_mtwa_", Sys.Date(), "_contour.pdf")),
                plot = p_mtwa_countour,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# Contours: smooth ----
# GDD0 -----
p_gdd0_smooth_contour <- maartenv5 %>%
  plot_climate_countour(var = "gdd0",
                        units = "\u00B0C days",
                        fill_scale =
                          ggplot2::scale_fill_distiller(name = "GDD0",
                                                        palette = "Spectral",
                                                        direction = -1,
                                                        breaks = c(750, 1000, 1250, 1500, 1750, 2000, 2250, 2500),
                                                        limits = c(750, 2790)),
                        legend.key.width = ggplot2::unit(2.65, "cm"),
                        size = size,
                        xlim = xlim,
                        ylim = ylim,
                        show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_gdd0_", Sys.Date(), "_smooth_contour.pdf")),
                plot = p_gdd0_smooth_contour,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# MAP ----
p_map_smooth_contour <- maartenv5 %>%
  plot_climate_countour(var = "map",
                        units = "mm/year",
                        fill_scale =
                          ggplot2::scale_fill_distiller(name = "MAP",
                                                        palette = "Spectral",
                                                        direction = 1,
                                                        breaks = c(350, 400, 450, 500, 550, 600, 650, 700, 750, 800),
                                                        limits = c(300, 810)),
                        legend.key.width = ggplot2::unit(2.5, "cm"),
                        size = size,
                        xlim = xlim,
                        ylim = ylim,
                        show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_map_", Sys.Date(), "_smooth_contour.pdf")),
                plot = p_map_smooth_contour,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# MAT ----
p_mat_smooth_contour <- maartenv5 %>%
  plot_climate_countour(var = "mat",
                        units = "\u00B0C",
                        fill_scale =
                          ggplot2::scale_fill_distiller(name = "MAT",
                                                        palette = "Spectral",
                                                        direction = -1,
                                                        na.value = "#5E4FA2",
                                                        breaks = c(-7, -6, -5, -4, -3, -2, -1, 0, 1, 2),
                                                        limits = c(-9, 3)),
                        legend.key.width = ggplot2::unit(2.85, "cm"),
                        size = size,
                        xlim = xlim,
                        ylim = ylim,
                        show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_mat_", Sys.Date(), "_smooth_contour.pdf")),
                plot = p_mat_smooth_contour,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")


# MI ----
p_mi_smooth_contour <- maartenv5 %>%
  plot_climate_countour(var = "mi",
                        units = "unitless",
                        fill_scale =
                          ggplot2::scale_fill_distiller(name = "MI",
                                                        direction = 1,
                                                        palette = "YlGnBu",
                                                        breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1),
                                                        limits = c(0.4, 1.22)),
                        legend.key.width = ggplot2::unit(2.45, "cm"),
                        size = size,
                        xlim = xlim,
                        ylim = ylim,
                        show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_mi_", Sys.Date(), "_smooth_contour.pdf")),
                plot = p_mi_smooth_contour,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# MTCO ----
p_mtco_smooth_contour <- maartenv5 %>%
  plot_climate_countour(var = "mtco",
                        units = "\u00B0C",
                        fill_scale =
                          ggplot2::scale_fill_distiller(name = "MTCO",
                                                        palette = "Spectral",
                                                        direction = -1,
                                                        breaks = c(-27.5, -25, -22.5, -20, -17.5, -15),
                                                        limits = c(-30, -14)),
                        legend.key.width = ggplot2::unit(2.25, "cm"),
                        size = size,
                        xlim = xlim,
                        ylim = ylim,
                        show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_mtco_", Sys.Date(), "_smooth_contour.pdf")),
                plot = p_mtco_smooth_contour,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

# MTWA ----
p_mtwa_countour <- maartenv5 %>%
  plot_climate_countour(var = "mtwa",
                        units = "\u00B0C",
                        fill_scale =
                          ggplot2::scale_fill_distiller(name = "MTWA",
                                                        palette = "Spectral",
                                                        direction = -1,
                                                        breaks = c(7.5, 10, 12.5, 15, 17.5, 20),
                                                        limits = c(7.5, 21)),
                        size = size,
                        xlim = xlim,
                        ylim = ylim,
                        show_plot = show_plot)
ggplot2::ggsave(file.path("~/Downloads/SMPDSv2/Maarten/",
                          paste0("maarten_mtwa_", Sys.Date(), "_smooth_contour.pdf")),
                plot = p_mtwa_countour,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

