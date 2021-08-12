## code to prepare `PNV` dataset goes here
# Source:
# Hengl, Tomislav, 2018, "Global Maps of Potential Natural Vegetation at 1 km resolution", https://doi.org/10.7910/DVN/QQHCIK, Harvard Dataverse, V4
URL <- "https://dataverse.harvard.edu/dataset.xhtml;jsessionid=9ffbf112e8caa68abc4dcfc0d1a9?persistentId=doi%3A10.7910%2FDVN%2FQQHCIK&version=&q=&fileTypeGroupFacet=&fileAccess=&fileSortField=size#"
PNV <- raster::brick("./inst/extdata/pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif")
PNV_classes <- readxl::read_xlsx("./inst/extdata/pnv_basic_info.xlsx")
# Amalgamate the tundras
PNV[PNV == 30] <- 28
PNV[PNV == 31] <- 28
PNV[PNV == 32] <- 28

# Crop the PNV map
# p.region <- as(raster::extent(-25, 155, 25, 90), 'SpatialPolygons')
# raster::crs(p.region) <- "+proj=longlat +datum=WGS84 +no_defs"
# PNV <- raster::crop(pnv, p.region)
usethis::use_data(PNV, overwrite = TRUE, compress = "xz")
usethis::use_data(PNV_classes, overwrite = TRUE, compress = "xz", internal = TRUE)
