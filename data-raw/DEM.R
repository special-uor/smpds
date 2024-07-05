DEM <- raster::raster("~/Downloads/DEM_geotiff/alwdgg.tif")
DEM_df <- DEM %>%
  raster::rasterToPoints() %>%
  tibble::as_tibble() %>%
  magrittr::set_names(c("longitude", "latitude", "elevation"))

land_borders <-
  rnaturalearth::ne_countries(scale = "medium",
                              returnclass = "sf")

.data <- subset1_2 %>%
  dplyr::rename(longitude = lon,
                latitude = lat)
crs_raster_format <-
  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 +no_defs"
.data_interp <- .data %>%
  dplyr::mutate(geometry = NA, .after = longitude) %>%
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs = "+proj=longlat +datum=WGS84 +no_defs"
  )

grid_boundary <- .data_interp %>%
  sf::st_bbox() #%>%
  # sf::st_as_sfc(crs = crs_raster_format)

DEM %>%
  raster::crop(grid_boundary) %>%
  raster::mask(mask = land_borders) %>%
  raster::rasterToPoints() %>%
  tibble::as_tibble() %>%
  magrittr::set_names(c("longitude", "latitude", "elevation")) %>%
  smpds::plot_climate_tiles(var = "elevation")

alt_grd_template_sf <- .data_interp %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_make_grid(
    cellsize = c(resolution, resolution),
    what = "centers"
  ) %>%
  sf::st_as_sf() %>%
  cbind(., sf::st_coordinates(.)) %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(Z = 0) %>%
  magrittr::set_names(c("x", "y", "Z"))

# If land_borders are provided, apply as a mask to the grid
if (!missing(land_borders)) {
  alt_grd_template_sf <- alt_grd_template_sf %>%
    raster::rasterFromXYZ(
      crs = crs_raster_format
    ) %>%
    raster::mask(mask = land_borders) %>%
    raster::rasterToPoints()
}

# Extract values for elevation
.data2 <- alt_grd_template_sf %>%
  tibble::as_tibble() %>%
  dplyr::rename(longitude = x,
                latitude = y) %>%
  get_elevation(cpus = cpus)

# Return new raster
alt_grd_template_sf %>%
  tibble::as_tibble() %>%
  dplyr::mutate(Z = .data2$elevation) %>%
  raster::rasterFromXYZ(
    crs = crs_raster_format
  )
