## code to prepare the Potential Natural Vegetation (PNV) datasets,
## `PNV_1km` (1 km resolution) and `PNV_250m` (250 m resolution).
# Sources:
# 1km: Hengl, Tomislav, 2018, "Global Maps of Potential Natural Vegetation at 1 km resolution", https://doi.org/10.7910/DVN/QQHCIK, Harvard Dataverse, V4
# 250m: Hengl, T. (2019). Potential distribution of biomes (Potential Natural Vegetation) at 250 m spatial resolution. https://doi.org/10.5281/zenodo.3526620
URL_1km <- "https://dvn-cloud.s3.amazonaws.com/10.7910/DVN/QQHCIK/1626825bd4e-e9043d7b645e?response-content-disposition=attachment%3B%20filename%2A%3DUTF-8%27%27pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif&response-content-type=image%2Ftiff&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20240621T122950Z&X-Amz-SignedHeaders=host&X-Amz-Expires=3600&X-Amz-Credential=AKIAIEJ3NV7UYCSRJC7A%2F20240621%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=ba5db145ecbf312e036e0a8ec229a8280302c99c76ca7ca44c504edc598f37fc"
URL_250m <- "https://zenodo.org/records/3526620/files/pnv_biome.type_biome00k_c_250m_s0..0cm_2000..2017_v0.2.tif?download=1"
# PNV_1km <- raster::brick("~/Downloads/pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif") |>
#   raster::readAll()
# PNV_250m <- raster::brick("~/Downloads/pnv_biome.type_biome00k_c_250m_s0..0cm_2000..2017_v0.2.tif") |>
#   raster::readAll()
PNV_1km <- raster::brick(system.file("references/pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif", package = "smpds"))
# PNV_250m <- raster::brick(system.file("extdata/pnv_biome.type_biome00k_c_250m_s0..0cm_2000..2017_v0.2.tif", package = "smpds"))

# Crop the PNV map
# p.region <- as(raster::extent(-25, 155, 25, 90), 'SpatialPolygons')
# raster::crs(p.region) <- "+proj=longlat +datum=WGS84 +no_defs"
# PNV <- raster::crop(pnv, p.region)

usethis::use_data(PNV_1km, overwrite = TRUE, compress = "xz")
# usethis::use_data(PNV_250m, overwrite = TRUE, compress = "xz")
