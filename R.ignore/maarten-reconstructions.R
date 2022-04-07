path <- "~/OneDrive - University of Reading/UoR/Data/CRU/4.04/"
maarten_path <- "~/Downloads/SMPDSv2/Maarten/"
maarten_path <- "~/Downloads/"
CPUS <- 6
`%>%` <- magrittr::`%>%`
# Maarten reconstructions ------------------------------------------------------
maarten <-
  # readxl::read_xlsx("~/Downloads/SMPDSv2/Maarten/Coordinates_Maarten.xlsx") %>%
  readxl::read_xlsx("~/Downloads/Coordinates_Maarten.xlsx") %>%
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
  smpds::biome_name()

maartenv4 %>%
  dplyr::select(-sf, -tmp, -pre) %>%
  readr::write_excel_csv("~/Downloads/maarten_reconstructions.csv")

## Plots
show_plot <- FALSE
size <- 1.5
stroke <- 0.1
width <- 16
xlim <- c(60, 120)
ylim <- c(45, 60)
p_gdd0 <- smpds::plot_gdd(maartenv4,
                          size = size,
                          stroke = stroke,
                          xlim = xlim,
                          ylim = ylim,
                          show_plot = show_plot)
ggplot2::ggsave(file.path(maarten_path,
                          paste0("maarten_gdd0_", Sys.Date(), ".pdf")),
                plot = p_gdd0,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")
p_mat <- smpds::plot_mat(maartenv4,
                  size = size,
                  stroke = stroke,
                  xlim = xlim,
                  ylim = ylim,show_plot = show_plot)
ggplot2::ggsave(file.path(maarten_path,
                          paste0("maarten_mat_", Sys.Date(), ".pdf")),
                plot = p_mat,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")
p_mi <- smpds::plot_mi(maartenv4,
                size = size,
                stroke = stroke,
                xlim = xlim,
                ylim = ylim,
                show_plot = show_plot)
ggplot2::ggsave(file.path(maarten_path,
                          paste0("maarten_mi_", Sys.Date(), ".pdf")),
                plot = p_mi,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")
p_mtco <- smpds::plot_mtco(maartenv4,
                    size = size,
                    stroke = stroke,
                    xlim = xlim,
                    ylim = ylim,
                    show_plot = show_plot)
ggplot2::ggsave(file.path(maarten_path,
                          paste0("maarten_mtco_", Sys.Date(), ".pdf")),
                plot = p_mtco,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")
p_mtwa <- smpds::plot_mtwa(maartenv4,
                    size = size,
                    stroke = stroke,
                    xlim = xlim,
                    ylim = ylim,
                    show_plot = show_plot)
ggplot2::ggsave(file.path(maarten_path,
                          paste0("maarten_mtwa_", Sys.Date(), ".pdf")),
                plot = p_mtwa,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")

p_biome <- smpds::plot_biome(maartenv4, #%>%
                         # dplyr::mutate(ID_BIOME = smpds::pnv_classes()$ID_BIOME %>%
                         #                 sample(size = length(ID_BIOME), replace = TRUE)),
                      size = size,
                      stroke = stroke,
                      xlim = xlim,
                      ylim = ylim,
                      legend.key.width = ggplot2::unit(1.3, "cm"),
                      show_plot = show_plot)
ggplot2::ggsave(file.path(maarten_path,
                          paste0("maarten_PNV_", Sys.Date(), ".pdf")),
                plot = p_biome,
                device = "pdf",
                width = width,
                height = 8,
                units = "in")
