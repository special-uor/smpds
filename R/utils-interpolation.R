#' @keywords internal
create_sq_grid <- function(.data,
                           resolution = 0.5,
                           get_elevation = get_elevation_zero,
                           cpus = 1,
                           land_borders = NULL,
                           z_ref = NULL,
                           xy_pad = c(xmin = 0, ymin = 0, xmax = 0, ymax = 0),
                           grid_boundary = NULL) {
  # Local bindings
  . <- longitude <- x <- y <- NULL
  crs_raster_format <-
    "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 +no_defs"
  .data_interp <- .data %>%
    dplyr::mutate(geometry = NA, .after = longitude) %>%
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = "+proj=longlat +datum=WGS84 +no_defs"
    )

  # Check if the grid_boundary was given, otherwise calculate it from the
  # observations (`.data`)
  if (missing(grid_boundary) ||
      is.null(grid_boundary) ||
      length(grid_boundary) != 4) {
    grid_boundary <- .data_interp %>%
      sf::st_bbox()
  } else {
    grid_boundary <- grid_boundary %>%
      magrittr::set_class(unique(c("bbox", class(.))))
  }

  # If the `z_ref` is given, then use this for the interpolation grid
  if (!missing(z_ref) && !is.null(z_ref)) {
    if ("character" %in% class(z_ref)) {
      z_ref <- raster::raster(z_ref)
    }

    grid_with_elevation <- z_ref %>%
      raster::crop(grid_boundary + xy_pad)

    # If land_borders are provided, apply as a mask to the grid
    if (!missing(land_borders)) {
      grid_with_elevation <- grid_with_elevation %>%
        raster::mask(mask = land_borders)
    }

    grid_with_elevation <- grid_with_elevation %>%
      raster::rasterToPoints() %>%
      tibble::as_tibble() %>%
      magrittr::set_names(c("x", "y", "Z")) %>%
      raster::rasterFromXYZ(
        crs = crs_raster_format
      )

    return(grid_with_elevation)
  }

  alt_grd_template_sf <-
    (grid_boundary + xy_pad) %>%
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
}

#' Create land-sea mask for the CRU TS dataset
#'
#' @param res Numeric value for the mask resolution. Default: 0.5 degrees.
#' @param coordinates Reference data set with columns for \code{latitude},
#'     \code{longitude} and \code{elevation}. Default:
#'     \code{\link{CRU_coords}}.
#'
#' @return Table with land-sea mask:
#' \itemize{
#'  \item \code{land = TRUE}: Grid cell with data provided by the CRU TS.
#'  \item \code{land = FALSE}: Grid cell where data is not provided by the
#'  CRU TS.
#' }
#'
#' @keywords internal
cru_mask <- function(res = 0.5,
                     coordinates = smpds::CRU_coords) {
  # Local bindings
  elevation <- land <- NULL
  x <- seq(-180 + res / 2, 180 - res / 2, res)
  y <- seq(-90 + res / 2, 90 - res / 2, res)
  if (!all(c("latitude", "longitude", "elevation") %in%
           colnames(coordinates))) {
    stop("The `coordinates` table is expected to have columns called:",
         "\n- `elevation`",
         "\n- `latitude`",
         "\n- `longitude`",
         call. = FALSE)
  }
  tibble::tibble(latitude = rep(y, length(x)),
                 longitude = rep(x, each = length(y)),
                 land = FALSE) %>%
    dplyr::left_join(coordinates,
                     by = c("latitude", "longitude")) %>%
    dplyr::mutate(land = ifelse(!is.na(elevation), TRUE, land),
                  sea = !land) %>%
    dplyr::select(-elevation)
}

#' @keywords internal
get_elevation <- function(.data, cpus = 1, missing = -999999) {
  # Local bindings
  latitude <- longitude <- NULL
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

#' @keywords internal
get_elevation_zero <- function(...) {
  return(tibble::tibble(elevation = 0))
}

#' Geographically Weighted Regression
#'
#' @details The input reference data can be in any of the following formats:
#' \itemize{
#'  \item \code{Matrix}: this should be a 3-dimensional object with spatial
#'  components (latitude and longitude) and a temporal component for
#'  representing each time step to be used for the extraction of the data.
#'  \item \code{String}: this should point to a valid path on disk where the
#'  reference NetCDF file is stored. Note that the parameter called \code{varid}
#'  should be used to indicate the identifier of the main variable inside the
#'  NetCDF file (e.g., \code{"tmp"},  \code{"pre"},  \code{"cld"}, etc.).
#' }
#' @param .ref Reference data from which the data will be interpolated (see the
#'     details section).
#' @param .tar Table with geographical target data, including: \code{latitude},
#'     \code{longitude} and \code{elevation}.
#' @param varid String with the identifier of the main variable inside the
#'     NetCDF file pointed by \code{.ref} (if applicable).
#' @inheritParams cru_mask
#' @param xy_buffer Numeric value to be used as the boundary for the search
#' area in the `x` and `y` axes.
#'     \itemize{
#'      \item \code{latitude} < \code{.tar$latitude + xy_buffer}
#'      \item \code{latitude} > \code{.tar$latitude - xy_buffer}
#'      \item \code{longitude} < \code{.tar$longitude + xy_buffer}
#'      \item \code{longitude} > \code{.tar$longitude - xy_buffer}
#'     }
#' @param z_buffer Numeric value to be used as the boundary for the search area
#' in the `z` axis:
#'     \itemize{
#'      \item \code{elevation} <= \code{.tar$elevation * z_buffer}
#'      \item \code{elevation} >= \code{.tar$elevation / z_buffer}
#'     }
#' @param cpus Number of CPUs to be used in parallel, default = 1.
#' @inheritParams spgwr::gwr
#'
#' @return Table with interpolated values from the \code{.ref} data for each
#'     record/row in \code{.tar}.
#' @export
#'
#' @references
#' Peng, Y., Bloomfield, K.J. and Prentice, I.C., 2020. A theory of plant
#' function helps to explain leaf‐trait and productivity responses to elevation.
#' New Phytologist, 226(5), pp.1274-1284. \doi{10.1111/nph.16447}
#'
#' @source
#' This function was adapted from a code developed by Yunke Peng
#' (\email{yunke.peng@usys.ethz.ch}) - ETH Zürich:
#' \url{https://github.com/yunkepeng/gwr}
#'
#' @examples
#' \dontrun{
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                        latitude = 51.44140,
#'                        longitude = -0.9418,
#'                        elevation = c(61, 161, 261, 361))
#' smpds::gwr(.ref = "/path/to/reference-tmp.nc",
#'            .tar = data,
#'            varid = "tmp")
#'
#' ncin <- ncdf4::nc_open("/path/to/reference-tmp.nc")
#' reference_data <- ncdf4::ncvar_get(ncin, varid)
#' ncdf4::nc_close(ncin)
#' reference_data %>%
#'   smpds::gwr(.tar = data)
#' }
gwr <- function(.ref, ...) {
  UseMethod("gwr", .ref)
}

#' @export
#' @rdname gwr
gwr.character <- function(.ref,
                          .tar,
                          varid = NULL,
                          coordinates = smpds::CRU_coords,
                          res = 0.5,
                          xy_buffer = 1.5,
                          z_buffer = NA,
                          cpus = 1,
                          bandwidth = 1.06) {
  if(is.null(varid))
    stop("When `.ref` is a string/path, `varid` cannot be NULL.",
         call. = FALSE)
  # Local bindings
  land <- sea <- NULL
  # Load reference data from the NetCDF file
  ncin <- ncdf4::nc_open(.ref)
  .ref_data <- ncdf4::ncvar_get(ncin, varid)
  ncdf4::nc_close(ncin) # Close connection to the NetCDF
  .ref_data %>%
    gwr(.tar = .tar,
        coordinates = coordinates,
        res = res,
        xy_buffer = xy_buffer,
        z_buffer = z_buffer,
        cpus = cpus,
        bandwidth = bandwidth)
}

#' @export
#' @rdname gwr
gwr.numeric <- function(.ref,
                        .tar,
                        coordinates = smpds::CRU_coords,
                        res = 0.5,
                        xy_buffer = 1.5,
                        z_buffer = NA,
                        cpus = 1,
                        bandwidth = 1.06) {
  if (length(dim(.ref)) != 3)
    stop("Invalid reference object, `.ref`, expecting a 3-dimensional array.",
         call. = FALSE)
  # Local bindings
  land <- sea <- NULL
  .ref_tbl <- .ref %>%
    mask_nc(mask = cru_mask(res = res, coordinates = coordinates)) %>%
    dplyr::filter(land) %>%
    dplyr::select(-land, -sea)

  # Combine the daily gridded data with coordinates
  climate_grid <- coordinates %>%
    dplyr::right_join(.ref_tbl,
                      by = c("latitude", "longitude"))
  # Start implementing Geographically Weighted Regression
  .tar_coords <- .tar
  sp::coordinates(.tar_coords) <- c("longitude", "latitude")
  # sp::gridded(climate_grid2) <- TRUE

  oplan <- future::plan(future::multisession, workers = cpus)
  on.exit(future::plan(oplan), add = TRUE)
  fm_suffix <- " ~ elevation"
  {
    pb <- progressr::progressor(steps = nrow(.tar))
    output <- seq_len(nrow(.tar)) %>%
      furrr::future_map_dfr(function(i) {
        climate_grid2 <- intermediate_output <- NULL
        climate_grid2 <- tryCatch({
          subset_coords(.data = climate_grid,
                        latitude = .tar$latitude[i],
                        longitude = .tar$longitude[i],
                        elevation = .tar$elevation[i],
                        xy_buffer = xy_buffer,
                        z_buffer = z_buffer)
        }, error = function(e) {
          warning("A valid interpolation zone was not found (row ", i,"), ",
                  "this core is likely to be in an area without obsevations ",
                  "in the reference dataset (e.g. marine core)",
                  call. = FALSE)
          NULL
        })
        if (is.null(climate_grid2)) {
          # Extract the number of days in the reference dataset
          ndays <- climate_grid %>%
            dplyr::select(dplyr::starts_with("T")) %>%
            ncol()
          # create an empty data frame to return as the default
          default_output <- tibble::tibble(
            name = stringr::str_c("T", seq_len(ndays)),
            value = NA
          ) %>%
            tidyr::pivot_wider()
          intermediate_output <- default_output
        } else {
          fms <-  names(climate_grid2) %>%
            stringr::str_subset("^T[0-9]*$") %>%
            stringr::str_c(fm_suffix)
          intermediate_output <- fms %>%
            furrr::future_map_dfc(~spgwr::gwr(formula = .x,
                                       data = climate_grid2,
                                       bandwidth = bandwidth,
                                       fit.points = .tar_coords[i, ],
                                       predictions = TRUE)$SDF$pred %>%
                             list() %>%
                             magrittr::set_names(
                               .x %>% stringr::str_remove(fm_suffix)
                             )
            )
        }
        pb()
        intermediate_output
    },
    .options = furrr::furrr_options(seed = TRUE))
  }

  .tar %>%
    dplyr::bind_cols(output)
}

#' Mask NetCDF
#'
#' Mask NetCDF variable.
#'
#' @param .data 3D matrix with \code{latitude}, \code{longitude} and
#'     \code{time}.
#' @param mask 2D matrix with all combinations of \code{latitude},
#'     \code{longitude} and a third variable called \code{land} with logical
#'     values to indicated whether a grid cell should be used or ignored.
#'     \itemize{
#'      \item \code{land = TRUE}: use this value.
#'      \item \code{land = FALSE}: ignore this value.
#'     }
#'
#' @return 2D version of \code{.data}, including \code{latitute},
#'     \code{longitude}, and variables with the format \code{T#}, for each
#'     time step in \code{.data}: \code{T1}, \code{T2}, ..., \code{Tk}.
#' @keywords internal
mask_nc <- function(.data, mask = cru_mask()) {
  mask %>%
    dplyr::bind_cols(seq_len(dim(.data)[3]) %>%
                       purrr::map_dfc(function(t) {
                         list(matrix(t(.data[, , t]), ncol = 1)[, 1]) %>%
                           magrittr::set_names(paste0("T", t))
                       })
    )
}

#' Pivot data
#'
#' Pivot data obtained with the function \code{\link{gwr}}. Groups all the data
#' points for each entity/row into a single list of elements.
#'
#' @param .data Data frame (\code{tibble} object) obtained with
#'     \code{\link{gwr}}.
#' @param timestep String with the pattern used to name each data point at each
#'     time-step. Default: \code{"^T[0-9]*"}.
#' @param cols Metadata columns (\code{latitude}, \code{longitude}, etc.), to
#'     be excluded from the data pivoting. Default: excluded columns matching
#'     the \code{timestep} pattern, for the default pattern, ignores columns
#'     with the prefix \code{T} and digits as suffix (e.g., \code{T1},
#'     \code{T2}, ..., \code{Tt}, etc.).
#' @param scale Numeric value to scale the data. Default: \code{1}, no scaling.
#' @param add Numeric value to be added/subtracted from the data points.
#'     Default: \code{0}, don't add anything.
#' @param varname Output variable name. Default: \code{"value"}.
#'
#' @return Data frame (\code{tibble} object) with a new column named according
#'     to the string passed with \code{value}, this new column contains a list
#'     of the data points at each time step for each observation/row.
#' @export
#'
#' @examples
#' \dontrun{
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                        latitude = 51.44140,
#'                        longitude = -0.9418,
#'                        elevation = 61)
#' smpds::gwr(.ref = "/path/to/reference-tmp.nc",
#'            .tar = data,
#'            varid = "tmp") %>%
#'   smpds::pivot_data(varname = "tmp")
#' }
pivot_data <- function(.data,
                       timestep = "^T[0-9]*",
                       cols = colnames(.data) %>%
                         stringr::str_detect(timestep, negate = TRUE) %>%
                         which(),
                       scale = 1,
                       add = 0,
                       varname = "value") {
  # Local bindings
  . <- .ID <- name <- value <- NULL

  .data %>%
    dplyr::mutate(.ID = seq_len(nrow(.))) %>% # Create unique ID per row/entity
    tidyr::pivot_longer(c(-dplyr::all_of(cols),
                          -.ID)) %>% # Pivot longer excluding cols and ID
    dplyr::group_by(.ID) %>% # Group by the unique ID assigned to each entity
    dplyr::mutate(value = list(value * scale + add)) %>%
    dplyr::ungroup() %>%
    magrittr::set_names(colnames(.) %>%
                          stringr::str_replace_all("value", varname)) %>%
    dplyr::distinct(.ID, .keep_all = TRUE) %>%
    dplyr::select(-name, -.ID)
}

#' Subset data
#'
#' Subset data using coordinates (\code{latitute}, \code{longitude}) and
#' \code{elevation}.
#'
#' @param .data 2D matrix with columns called \code{latitude} and
#'     \code{longitude}.
#' @param latitude Numeric value for reference \code{latitude}.
#' @param longitude Numeric value for reference \code{longitude}.
#' @param elevation Numeric value for reference \code{elevation}.
#' @inheritParams gwr
#'
#' @return Filtered 2D matrix.
#' @keywords internal
subset_coords <- function(.data,
                          latitude,
                          longitude,
                          elevation,
                          xy_buffer,
                          z_buffer = NA) {
  .data_coords <- .data %>%
    dplyr::filter(latitude > min(!!latitude - xy_buffer),
                  latitude < max(!!latitude + xy_buffer),
                  longitude > min(!!longitude - xy_buffer),
                  longitude < max(!!longitude + xy_buffer))

  if (!missing(z_buffer) & !is.null(z_buffer) & !is.na(z_buffer)) {
   .data_coords <- .data_coords %>%
     dplyr::filter(elevation >= min(!!elevation / z_buffer),
                   elevation <= max(!!elevation * z_buffer))
  }
  sp::coordinates(.data_coords) <- c("longitude", "latitude")
  return(.data_coords)
}

#' Thin plate spline regression
#'
#' @inheritDotParams fields::Tps -x -Y -lon.lat
#' @param .data Data frame with spatial and climate data. The following are
#'     expected:
#'     \itemize{
#'         \item **Latitude**, named: \code{latitude}, \code{lat} or \code{y}.
#'         \item **Longitude**, named: \code{longitude}, \code{long}, \code{lon}
#'         or \code{y}.
#'         \item **Main variable**, named: value of \code{var}.
#'     }
#' @param var String with the name of the climate variable to interpolate.
#' @param resolution Numeric value with the resolution (degrees) to interpolate.
#' @param land_borders Data frame with polygons to represent land borders
#'     (e.g. continents, countries, counties, etc.).
#'     Default: `rnaturalearth::ne_countries`.
#' @param check_data Boolean flag to indicate whether `.data` should be checked
#'     or not (i.e. validate coordinates and main variable).
#'     Default: `TRUE`
#' @param z_var String with the name of the variable containing information for
#'     elevation. If this is given, then the elevation is used for the
#'     interpolation.
#' @param z_mode String with the mode in which the elevation should be used:
#'     \itemize{
#'         \item \code{z_mode = "independent"} (Default), use the elevation as
#'         another independent variable to predict `var`.
#'         \item \code{z_mode = "covariate"}, use the elevation as a linear
#'         covariate to predict `var`.
#'     }
#' @param z_ref Raster object or path to raster object with grid containing
#'     elevation information. For example the ETOPO5 (Earth topography 5 arc
#'     minute) data set.
#' @param cpus Numeric value with the number of CPUs to use in the computation
#'     of the elevations for the interpolation grid.
#' @param xy_pad Numeric vector of length 4. The entries should be named as
#'     follow: `xmin`, `ymin`, `xmax`, `ymax`. The values will be used to
#'     expand the interpolation grid horizontally (longitude) and vertically
#'     (latitude).
#' @param grid_boundary Numeric vector of length 4. The entries should be named
#'     as follow: `xmin`, `ymin`, `xmax`, `ymax`. The values will be used to
#'     manually specify the interpolation area.
#' @param ... Additional parameters for the interpolation.
#'
#' @return `tibble` object with interpolated values.
#'
#' @export
tps <- function(.data,
                var,
                resolution = 0.5,
                land_borders =
                  rnaturalearth::ne_countries(scale = "small",
                                              returnclass = "sf"),
                check_data = TRUE,
                z_var = NULL,
                z_mode = "independent",
                z_ref = NULL,
                cpus = 1,
                xy_pad = c(xmin = 0, ymin = 0, xmax = 0, ymax = 0),
                grid_boundary = NULL,
                ...) {
  # Local bindings
  latitude <- longitude <- Z <- NULL
  # Check coordinates
  .data2 <- .data %>%
    check_coords(var = var, skip = !check_data)

  if (!missing(z_var)) {
    .data2 <- .data2 %>%
      dplyr::rename(Z = !!z_var)

    # Create rectangular grid
    sq_grid <- .data2 %>%
      create_sq_grid(resolution = resolution,
                     get_elevation = get_elevation,
                     cpus = cpus,
                     land_borders = land_borders,
                     z_ref = z_ref,
                     xy_pad = xy_pad,
                     grid_boundary = grid_boundary)

    if (z_mode == "independent") {
      message("Using the elevation as an independent variable...")
      fit_tps <- fields::Tps(x = .data2 %>%
                               dplyr::select(longitude,
                                             latitude,
                                             Z) %>%
                               magrittr::set_names(c(
                                 "x", "y", "Z"
                               )),
                             Y = .data2$var,
                             lon.lat = TRUE,
                             ...)
    } else {
      message("Using the elevation as a linear covariate...")
      fit_tps <- fields::Tps(x = .data2 %>%
                               dplyr::select(longitude, latitude) %>%
                               magrittr::set_names(c(
                                 "x", "y"
                               )),
                             Y = .data2$var,
                             lon.lat = TRUE,
                             Z = .data2$Z,
                             ...)
    }
  } else {
    # Create rectangular grid
    sq_grid <- .data2 %>%
      create_sq_grid(resolution = resolution,
                     land_borders = land_borders,
                     z_ref = z_ref,
                     xy_pad = xy_pad,
                     grid_boundary = grid_boundary)

    fit_tps <- fields::Tps(.data2 %>%
                             dplyr::select(longitude, latitude),
                           .data2 %>%
                             dplyr::select(var),
                           lon.lat = TRUE,
                           ...)
  }

  message("Interpolating the new values...")
  oplan <- future::plan(future::multisession, workers = cpus)
  on.exit(future::plan(oplan), add = TRUE)
  {
    N_STEPS <-
      raster::nrow(sq_grid)
    pb <- progressr::progressor(steps = N_STEPS)
    pred_funct <- function(model, x, ...) {
      raster::predict(model, x[, 1:2], Z = x[, 3], ...)
    }
    interp_tps <- seq_len(N_STEPS) %>%
      furrr::future_map_dfr(function(i) {
      # purrr::map_dfr(function(i) {
        sub_sq_grid <-
          tibble::tibble(
            x = raster::xFromCol(sq_grid, seq_len(raster::ncol(sq_grid))),
            y = raster::yFromRow(sq_grid, i),
            Z = raster::getValues(sq_grid, i)
          ) %>%
          raster::rasterFromXYZ(res = raster::res(sq_grid),
                                crs = raster::crs(sq_grid))
        if (!is.null(z_var)) {
          if (z_mode == "independent") {
            intermediate_output <- raster::interpolate(sub_sq_grid,
                                                       fit_tps,
                                                       xyOnly = FALSE)
          } else {
            intermediate_output <- raster::interpolate(sub_sq_grid,
                                                       fit_tps,
                                                       xyOnly = FALSE,
                                                       fun = pred_funct)
          }
        } else {
          intermediate_output <- raster::interpolate(sub_sq_grid, fit_tps)
        }
        pb()
        intermediate_output %>%
          raster::mask(mask = land_borders) %>%
          raster::rasterToPoints() %>%
          tibble::as_tibble() %>%
          magrittr::set_names(c("longitude", "latitude", var))
      },
      .options = furrr::furrr_options(seed = TRUE,
                                      packages = c("fields", "raster", "sf"))
      )
  }
  message("Done. Bye!")
  interp_tps
}
