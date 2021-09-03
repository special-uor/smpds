## code to prepare `CRU_coords` dataset goes here
# Source
# Mitchell, T.D. and Jones, P.D., 2005. An improved method of constructing a
# database of monthly climate observations and associated high resolution grids.
# RMetS, 25(6): pp. 693-712. doi:10.1002/joc.1181
# https://crudata.uea.ac.uk/~timm/grid/CRU_TS_2_1.html

# Create temporal files
tmp.Z <- tempfile(fileext = ".elv.Z")
tmp.elv <- gsub("\\.Z$", "", tmp.Z)
tmp.nc <- paste0(tmp.elv, ".nc")

# Download and uncompress data
download.file("https://crudata.uea.ac.uk/~timm/grid/halfdeg.elv.Z", tmp.Z)
system(sprintf('uncompress %s', tmp.Z))

# Convert the GRIM file to NetCDF
codos:::grim2nc(tmp.elv, "elv", scale_factor = 1, longname = "elevation")

# Load the data from the NetCDF
nc <- ncdf4::nc_open(tmp.nc)
elev <- ncdf4::ncvar_get(nc, "elv")
lat  <- ncdf4::ncvar_get(nc, "lat")
lon <- ncdf4::ncvar_get(nc, "lon")
ncdf4::nc_close(nc)
image(lon, lat, elev)

# Create long vectors and remove missing elevations
lat_long <- rep(lat, length(lon))
lon_long <- rep(lon, each = length(lat))
elev_long <- matrix(t(elev), ncol = 1)[, 1]

CRU_mask <- !is.na(elev) %>%
  magrittr::set_colnames(lat) %>%
  magrittr::set_rownames(lon)

CRU_coords <- tibble::tibble(latitude = lat_long,
                             longitude = lon_long,
                             elevation = elev_long) %>%
  dplyr::filter(!is.na(elevation))

basemap <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill = "white", size = 0.25) +
  ggplot2::coord_sf(expand = FALSE)
p <- basemap +
  ggplot2::geom_point(mapping = ggplot2::aes(x = longitude,
                                             y = latitude,
                                             fill = elevation
  ),
  data = CRU_coords %>%
    # dplyr::filter(elevation < 2000) %>%
    dplyr::mutate(fct_elevation = cut(elevation,
                                      breaks = 3)),
  size = .5,
  shape = 21,
  stroke = 0) # +
  # ggplot2::scale_fill_manual(values = c("#FFFFFF", "#ABC000", "#000ABC"))
p
# Delete temporal files
unlink(tmp.Z)
unlink(tmp.elv)
unlink(tmp.nc)

# Store the dataset
usethis::use_data(CRU_coords, overwrite = TRUE)
