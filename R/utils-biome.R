extract_biome <- function(data, reference = smpds::PNV, buffer = 10000, ...) {
  if (!("sf" %in% class(data)))
    stop("The given data object does not contain a geometry column.")
  biomes <- raster::extract(reference, modern.check, buffer = buffer, ...)
  biomes %>%
    purrr::map_df(function(bio) {
      tibble::as_tibble(bio) %>%
        magrittr::set_names("BiomeID") %>%
        dplyr::group_by(BiomeID) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        dplyr::slice(which(dplyr::row_number() == 1))
    })
}
