## code to prepare `PNV` dataset goes here
# Source:
# 1km: Hengl, Tomislav, 2018, "Global Maps of Potential Natural Vegetation at 1 km resolution", https://doi.org/10.7910/DVN/QQHCIK, Harvard Dataverse, V4
# 250m: Hengl, T. (2019). Potential distribution of biomes (Potential Natural Vegetation) at 250 m spatial resolution. https://doi.org/10.5281/zenodo.3526620
URL <- "https://dataverse.harvard.edu/dataset.xhtml;jsessionid=9ffbf112e8caa68abc4dcfc0d1a9?persistentId=doi%3A10.7910%2FDVN%2FQQHCIK&version=&q=&fileTypeGroupFacet=&fileAccess=&fileSortField=size#"
URL_250m <- "https://zenodo.org/records/3526620/files/pnv_biome.type_biome00k_c_250m_s0..0cm_2000..2017_v0.2.tif?download=1"
PNV_1km <- raster::brick("./inst/extdata/pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif")
PNV_250m <- raster::brick("./inst/extdata/pnv_biome.type_biome00k_c_250m_s0..0cm_2000..2017_v0.2.tif")
PNV_classes <- readxl::read_xlsx("./inst/extdata/pnv_basic_info.xlsx") %>%
  magrittr::set_names(c("ID_BIOME", "colour", "description")) %>%
  dplyr::select(1, 3, 2) %>%
  dplyr::mutate(colour = stringr::str_to_upper(colour)) %>%
  dplyr::bind_rows(
    tibble::tibble(ID_BIOME = c(30, 31, 32), # Amalgamate the tundras
                   description = "tundra",
                   colour = "#BFC9CA")
    ) %>%
  dplyr::bind_rows(
    tibble::tribble(
      ~ID_BIOME, ~description, ~colour,
      NA,"not applicable","#CC0033",
      -888888,"not applicable","#CC0033",
      -999999,"not known","#FFFFFF"
    )
  )
# Amalgamate the tundras
# PNV[PNV == 30] <- 28
# PNV[PNV == 31] <- 28
# PNV[PNV == 32] <- 28

# Crop the PNV map
# p.region <- as(raster::extent(-25, 155, 25, 90), 'SpatialPolygons')
# raster::crs(p.region) <- "+proj=longlat +datum=WGS84 +no_defs"
# PNV <- raster::crop(pnv, p.region)

# PNV_old <- smpds::PNV
usethis::use_data(PNV_1km, overwrite = TRUE, compress = "xz")
usethis::use_data(PNV_250m, overwrite = TRUE, compress = "xz")
# usethis::use_data(PNV_classes, overwrite = TRUE, compress = "xz", internal = TRUE)
