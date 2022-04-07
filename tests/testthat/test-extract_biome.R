test_that("extract_biome works", {
  # Data frame (tibble) as input
  test_data_t1 <- tibble::tibble(entity_name = "University of Reading",
                                 latitude = 51.44140,
                                 longitude = -0.9418)
  expected_t1 <- tibble::tribble(
    ~entity_name, ~latitude, ~longitude, ~ID_BIOME,
    "University of Reading",   51.4414,    -0.9418,        13
  )
  expect_equal(extract_biome(test_data_t1), expected_t1)

  # Data frame (tibble) as input with marine location (no ID_BIOME expected)
  test_data_t2 <- tibble::tibble(entity_name = "North Sea",
                                 latitude = 56.525027,
                                 longitude = 3.306268)
  expected_t2 <- tibble::tribble(
    ~entity_name, ~latitude, ~longitude, ~ID_BIOME,
    "North Sea", 56.525027,   3.306268,        NA
  )
  expect_equal(extract_biome(test_data_t2), expected_t2)

  # Simple features object as input
  test_data_t3 <- tibble::tibble(entity_name = "University of Reading",
                                 latitude = 51.44140,
                                 longitude = -0.9418) %>%
    sf::st_as_sf(x = ., coords = c("longitude", "latitude"))
  expected_t3 <-
    structure(
      list(
        entity_name = "University of Reading",
        geometry = structure(
          list(structure(
            c(-0.9418, 51.4414), class = c("XY", "POINT", "sfg")
          )),
          class = c("sfc_POINT", "sfc"),
          precision = 0,
          bbox = structure(
            c(
              xmin = -0.9418,
              ymin = 51.4414,
              xmax = -0.9418,
              ymax = 51.4414
            ),
            class = "bbox"
          ),
          crs = structure(list(input = NA_character_, wkt = NA_character_),
                          class = "crs"),
          classes = "GEOMETRYCOLLECTION",
          n_empty = 0L
        ),
        ID_BIOME = 13
      ),
      row.names = 1L,
      sf_column = "geometry",
      agr = structure(
        c(entity_name = NA_integer_,
          ID_BIOME = NA_integer_),
        class = "factor",
        .Label = c("constant",
                   "aggregate", "identity")
      ),
      class = c("sf", "tbl_df", "tbl", "data.frame")
    )
  expect_equal(extract_biome(test_data_t3), expected_t3)

  # Invalid data frame as input, missing longitude
  test_data_t4 <- tibble::tibble(entity_name = "University of Reading",
                                 latitude = 51.44140)
  expect_error(extract_biome(test_data_t4))

  # Invalid input format, list
  expect_error(extract_biome(list(entity_name = "University of Reading",
                                  latitude = 51.44140,
                                  longitude = -0.9418)))
})
