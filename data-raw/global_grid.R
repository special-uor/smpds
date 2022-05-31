`%>%` <- magrittr::`%>%`
global_grid <- smpds:::create_sq_grid(
  tibble::tibble(longitude = c(-179.5, 179.5),
                 latitude = c(89.5, -89.5)),
  resolution = 0.5)

land_borders =
  rnaturalearth::ne_countries(scale = "medium",
                              returnclass = "sf")

global_grid_land <- global_grid %>%
  raster::mask(mask = land_borders)

ggplot2::ggplot(global_grid_land %>%
                  raster::rasterToPoints() %>%
                  tibble::as_tibble(),
                mapping = ggplot2::aes(x, y, fill = Z)) +
  ggplot2::geom_tile()

global_grid_land_tb <- global_grid_land %>%
  raster::rasterToPoints() %>%
  tibble::as_tibble() %>%
  magrittr::set_names(c("longitude", "latitude", "Z")) %>%
  dplyr::mutate(ID = seq_along(longitude), .before = 1)

get_elevation <- function(.data, cpus = 4, missing = -999999) {
  latlon_proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  oplan <- future::plan(future::multisession, workers = cpus)
  on.exit(future::plan(oplan), add = TRUE)
  output <- NULL
  {
    pb <- progressr::progressor(steps = nrow(.data))
    output <-
      .data %>%
      dplyr::mutate(elevation = list(latitude, longitude) %>%
                      furrr::future_pmap_dbl(function(latitude, longitude) {
                        suppressMessages({
                          tryCatch({
                            elv <- tibble::tibble(x = longitude,
                                                  y = latitude) %>%
                              sp::SpatialPoints(proj4string =
                                                  sp::CRS(latlon_proj)) %>%
                              elevatr::get_elev_point(prj = latlon_proj,
                                                      src = "aws")
                            pb()
                            return(elv$elevation[1])
                          }, error = function(e) {
                            print(e)
                            return(missing)
                          })
                        })
                      },
                      .options = furrr::furrr_options(seed = TRUE)))
    } %>%
    smpds::pb()
  return(output)
}

CPUS <- 10
global_grid_land_tb_2_1 <- global_grid_land_tb %>%
  dplyr::slice(1:10000) %>%
  get_elevation(cpus = CPUS)
global_grid_land_tb_2_2 <- global_grid_land_tb %>%
  dplyr::slice(10001:20000) %>%
  get_elevation(cpus = CPUS)
global_grid_land_tb_2_3 <- global_grid_land_tb %>%
  dplyr::slice(20001:30000) %>%
  get_elevation(cpus = CPUS)
global_grid_land_tb_2_4 <- global_grid_land_tb %>%
  dplyr::slice(30001:40000) %>%
  get_elevation(cpus = CPUS)
global_grid_land_tb_2_5 <- global_grid_land_tb %>%
  dplyr::slice(40001:50000) %>%
  get_elevation(cpus = CPUS)
global_grid_land_tb_2_6 <- global_grid_land_tb %>%
  dplyr::slice(50001:60000) %>%
  get_elevation(cpus = CPUS)
global_grid_land_tb_2_7 <- global_grid_land_tb %>%
  dplyr::slice(60001:70000) %>%
  get_elevation(cpus = CPUS)
global_grid_land_tb_2_8 <- global_grid_land_tb %>%
  dplyr::slice(70001:84940) %>%
  get_elevation(cpus = CPUS)

global_grid_land_tb_2_all <-
  dplyr::bind_rows(global_grid_land_tb_2_1,
                   global_grid_land_tb_2_2,
                   global_grid_land_tb_2_3,
                   global_grid_land_tb_2_4,
                   global_grid_land_tb_2_5,
                   global_grid_land_tb_2_6,
                   global_grid_land_tb_2_7,
                   global_grid_land_tb_2_8) %>%
  dplyr::select(-Z)

readr::write_rds(global_grid_land_tb_2_all,
                 "~/Downloads/global_grid_land_tb_2_all.Rds")

global_grid_land_tb_2_all <-
  readr::read_rds("~/Downloads/global_grid_land_tb_2_all.Rds")

global_grid_land_tb_2_all_sf <- global_grid_land_tb_2_all %>%
  dplyr::select(-ID) %>%
  dplyr::rename(x = longitude,
                y = latitude,
                z = elevation) %>%
  raster::rasterFromXYZ(
    res = 0.5,
    crs = 4326
    )

save(global_grid_land_tb_2_all_sf, file = "~/Downloads/global_grid_land.RData")

load("~/Downloads/global_grid_land.RData")
uk_map <- rnaturalearth::ne_countries(country = "Costa Rica",
                                      scale = "large",
                                      returnclass = "sf")
uk_elv_df <- DEM %>%
  raster::mask(uk_map) %>%
  raster::rasterToPoints() %>%
  tibble::as_tibble()

uk_elv_df %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x, y, fill = alwdgg)) +
  ggplot2::geom_tile() +
  ggplot2::geom_sf(data = uk_map,
                   fill = NA,
                   inherit.aes = FALSE)

DEM <- raster::raster("~/Downloads/DEM_geotiff/alwdgg.tif")

uk_map <- rnaturalearth::ne_countries(country = "United Kingdom",
                                      scale = "large",
                                      returnclass = "sf")
uk_elv_df <- DEM %>%
  raster::mask(uk_map) %>%
  raster::rasterToPoints() %>%
  tibble::as_tibble()

uk_elv_df %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x, y, fill = alwdgg)) +
  ggplot2::geom_tile() +
  ggplot2::geom_sf(data = uk_map,
                   fill = NA,
                   inherit.aes = FALSE)

DEM_df <- DEM %>%
  raster::rasterToPoints() %>%
  tibble::as_tibble()

# uk_map %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_sf() +
#   ggplot2::geom_tile(
#     mapping = ggplot2::aes(x, y, fill = z),
#     data = uk_elv_df
#   )
#   # sf::st_as_sf(
#   #   coords = c("x", "y"),
#   #   crs = 4326
#   # ) %>%
#
#   ggplot2::ggplot(mapping = ggplot2::aes(x, y, fill = z)) +
#   ggplot2::geom_tile()

# global_grid_land_tb_2_all %>%
  # smpds::plot_climate_tiles(
  #   "elevation",
  #   land_borders =
  #     rnaturalearth::ne_countries(scale = "medium",
  #                                 returnclass = "sf"),
  #   fill_scale = ggplot2::scale_fill_manual(
  #     name = "elevation [m]",
  #     values = wesanderson::wes_palette("Zissou1", 9, type = "continuous")
  #   ))
# DEM_df %>%
world_map <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
DEM %>%
  raster::mask(world_map) %>%
  raster::rasterToPoints() %>%
  tibble::as_tibble() %>%
  magrittr::set_names(c("longitude", "latitude", "elevation")) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = longitude,
                                         y = latitude,
                                         fill = elevation)) +
  ggplot2::geom_tile() +

  ggplot2::geom_sf(data = world_map,
                   fill = NA,
                   size = 0.5,
                   inherit.aes = FALSE) +
  ggplot2::scale_fill_gradientn(
    name = "elevation [m]",
    colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous")
  )

DEM_df %>%
  dplyr::rename(elevation = alwdgg) %>%
  dplyr::mutate(x_round = round(x, 1))

