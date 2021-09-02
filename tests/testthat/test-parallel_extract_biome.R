test_that("parallel_extract_biome works", {
  # Data frame (tibble) as input
  test_data_t1 <- tibble::tibble(entity_name = "University of Reading",
                                 latitude = 51.44140,
                                 longitude = -0.9418)
  expected_t1 <- tibble::tribble(
    ~entity_name, ~latitude, ~longitude, ~ID_BIOME,
    "University of Reading",   51.4414,    -0.9418,        13
  )
  expect_equal(parallel_extract_biome(test_data_t1), expected_t1)

  # Data frame (tibble) as input with marine location (no ID_BIOME expected)
  test_data_t2 <- tibble::tibble(entity_name = "North Sea",
                                 latitude = 56.525027,
                                 longitude = 3.306268)
  expected_t2 <- tibble::tribble(
    ~entity_name, ~latitude, ~longitude, ~ID_BIOME,
    "North Sea", 56.525027,   3.306268,        NA
  )
  expect_equal(parallel_extract_biome(test_data_t2), expected_t2)

  # Invalid data frame as input, missing longitude
  test_data_t3 <- tibble::tibble(entity_name = "University of Reading",
                                 latitude = 51.44140,
                                 longitude = NA)
  expect_equal(parallel_extract_biome(test_data_t3), test_data_t3)

  # Invalid input format, list
  expect_error(parallel_extract_biome(list(entity_name = "University of Reading",
                                  latitude = 51.44140,
                                  longitude = -0.9418)))
})
