reference <- "~/OneDrive - University of Reading/UoR/Data/CRU/4.04/cru_ts4.04-clim-1961-1990-daily.tmp.nc"
data <- tibble::tibble(entity_name = "University of Reading",
                       latitude = 51.44140,
                       longitude = -0.9418,
                       elevation = c(61, 161, 261, 361))
tictoc::tic()
out <- #data %>%
  smpds::EMPDv2[1:20, 1:8] %>%
  smpds::gwr(varid = "tmp",
             # reference = "inst/extdata/cru_ts4.04-clim-1961-1990-daily_tmp_1-5.nc",
             reference = "~/OneDrive - University of Reading/UoR/Data/CRU/4.04/cru_ts4.04-clim-1961-1990-daily.tmp.nc",
             cpus = 4)
tictoc::toc()
tictoc::tic()
out2 <- #data %>%
  smpds::EMPDv2[c(18, 13, 15, 2, 19, 5, 1, 12, 3, 14, 4, 17, 16, 20, 6:11), 1:8] %>%
  smpds::gwr(varid = "tmp",
             # reference = "inst/extdata/cru_ts4.04-clim-1961-1990-daily_tmp_1-5.nc",
             reference = "~/OneDrive - University of Reading/UoR/Data/CRU/4.04/cru_ts4.04-clim-1961-1990-daily.tmp.nc",
             cpus = 4)
tictoc::toc()

ncin <- ncdf4::nc_open(reference)
reference_tbl <- ncdf4::ncvar_get(ncin, "tmp") %>%
  smpds:::mask_nc(mask = smpds:::cru_mask()) %>%
  dplyr::filter(land) %>%
  dplyr::select(-land, -sea)
ncdf4::nc_close(ncin)

res <- 0.5
x <- seq(-180 + res / 2, 180 - res / 2, res)
y <- seq(-90 + res / 2, 90 - res / 2, res)

path <- "~/OneDrive - University of Reading/UoR/Data/CRU/4.04/"
tmp <- codos:::nc_var_get(file.path(path, "cru_ts4.04.1901.2019.daily.tmp.nc"),
                          "tmp")$data

codos:::nc_save(filename = "inst/extdata/cru_ts4.04-clim-1961-1990-daily_tmp_1-5.nc",
                var = list(id = "tmp",
                           longname = ncdf4::ncatt_get(ncin,
                                                       "tmp",
                                                       "long_name")$value,
                           missval = ncdf4::ncatt_get(ncin,
                                                      "tmp",
                                                      "missing_value")$value,
                           prec = "double",
                           units = "degrees Celsius",
                           vals = tmp),
                lat = list(id = "lat", longname = "latitude", units = "degrees_north", vals = y),
                lon = list(id = "lon", longname = "longitude", units = "degrees_east", vals = x),
                time = list(calendar = "gregorian",
                            id = "time",
                            units = "days in a year",
                            vals = 1:5), overwrite = TRUE)



path <- "~/OneDrive - University of Reading/UoR/Data/CRU/4.04/"
tmp <- codos:::nc_var_get(file.path(path, "cru_ts4.04.1901.2019.daily.tmp.nc"),
                          "tmp")$data
idx_y <- which.min(abs(codos::lat$data - 51.44140))
idx_x <- which.min(abs(codos::lon$data - -0.9418))
tmp_cru_ts <- tmp[c(idx_x, idx_x + 1), c(idx_y - 1, idx_y), ]
rownames(tmp_cru_ts) <- codos::lat$data[c(idx_y, idx_y - 1)]
colnames(tmp_cru_ts) <- codos::lon$data[c(idx_x, idx_x + 1)]
tmp_cru_ts <- colMeans(tmp_cru_ts, dims = 2)

tibble::tibble(x = rep(1:365, 2),
               y = c(tmp_cru_ts, output),
               z = rep(c("4 grid mean", "GWR"), each = 365)) %>%
  ggplot2::ggplot(ggplot2::aes(x, y, colour = z)) +
  ggplot2::geom_line() +
  ggplot2::scale_color_brewer(name = "CRU TS 4.04", palette = "Set1") +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(12)) +
  ggplot2::labs(x = "[days]",
                y = "Daily Temperature [°C]",
                title = "UoR (51.44140, -0.9418 @ 61 m)") +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                          colour = NA),
                 panel.border = ggplot2::element_rect(fill = NA,
                                                      colour = "grey20"),
                 panel.grid = ggplot2::element_line(colour = "grey92"),
                 panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.5)),
                 strip.background = ggplot2::element_rect(fill = "grey85",
                                                          colour = "grey20"),
                 legend.key = ggplot2::element_rect(fill = "white",
                                                    colour = NA),
                 complete = TRUE,
                 legend.position = "bottom")

basemap <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill = "white", size = 0.25) +
  ggplot2::coord_sf(expand = FALSE)


p <- basemap +
  ggplot2::geom_point(mapping = ggplot2::aes(x = longitude,
                                             y = latitude,
                                             fill = T100
  ),
  data = climate_grid2,
  size = .5,
  shape = 21,
  stroke = 0)
p
