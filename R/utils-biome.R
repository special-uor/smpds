#' Biome names
#'
#' Obtain Biome names from the Hengl et al., 2018, using the \code{BiomeID}.
#'
#' @param ID Numeric value linked to a Biome in provided by Hengl et al., 2018.
#'
#' @return Table (\code{tibble} object) with Biome metadata.
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                         latitude = 51.4414,
#'                         longitude = 0.9418) %>%
#'   sf::st_as_sf(x = ., coords = c("longitude", "latitude"))
#' data %>%
#'   extract_biome() %>%
#'   .$BiomeID %>%
#'   biome_name()
biome_name <- function(ID) {
  smpds:::PNV_classes %>%
    dplyr::filter(BiomeID %in% ID)
}

#' Extract biome
#'
#' Extracts biome for a point (latitude, longitude) based on the map created by
#' Hengl et al., 2018.
#'
#' @param data Table containing a geometry column.
#' @param reference Reference map with biomes, default: \code{smpds::PNV}.
#' @inherit raster::extract
#'
#' @return Tibble with matched biomes: dominant, sub-dominant, etc.
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' data <- tibble::tibble(entity_name = "University of Reading",
#'                         latitude = 51.4414,
#'                         longitude = 0.9418) %>%
#'   sf::st_as_sf(x = ., coords = c("longitude", "latitude"))
#' data %>%
#'   extract_biome()
extract_biome <- function(data, reference = smpds::PNV, buffer = 12000) {
  if (!("sf" %in% class(data)))
    stop("The given data object does not contain a geometry column.")
  biomes <- raster::extract(reference, data, buffer = buffer, na.rm = TRUE)
  biomes %>%
    purrr::map_df(function(bio) {
      tibble::as_tibble(bio) %>%
        magrittr::set_names("BiomeID") %>%
        dplyr::group_by(BiomeID) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::arrange(dplyr::desc(n)) # %>%
        # dplyr::slice(which(dplyr::row_number() == 1))
    })
}
