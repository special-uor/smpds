test_that("the function biome_name works", {
  # Numeric vector as input
  expected_t1 <- tibble::tribble(
    ~ID_BIOME, ~description,   ~colour,
    1, "tropical evergreen broadleaf forest", "#1C5510"
  )
  expect_equal(object = biome_name(1), expected_t1)

  # Empty numeric vector as input
  expect_error(biome_name())

  # Numeric vector with invalid ID_BIOME
  expect_equal(biome_name(2021) %>% nrow(), 0)

  # Data frame (tibble) as input
  test_data_t2 <- tibble::tribble(
    ~name, ~ID_BIOME,
    "A", 1,
    "B", 2,
    "C", 22,
    "D", NA
  )
  expected_t2 <- tibble::tribble(
    ~name, ~ID_BIOME,                               ~description,   ~colour,
      "A",         1,      "tropical evergreen broadleaf forest", "#1C5510",
      "B",         2, "tropical semi-evergreen broadleaf forest", "#659208",
      "C",        22,                                   "steppe", "#FFBA35",
      "D",        NA,                           "not applicable", "#CC0033"
  )
  expect_equal(biome_name(test_data_t2), expected_t2)

  #  Data frame (tibble) with invalid ID_BIOME
  test_data_t3 <- tibble::tribble(
    ~name, ~ID_BIOME,
    "A", 1,
    "B", 2021,
  )
  expected_t3 <- tibble::tribble(
    ~name, ~ID_BIOME,                             ~description,   ~colour,
    "A",         1,      "tropical evergreen broadleaf forest", "#1C5510",
    "B",      2021,                                         NA,         NA
  )
  expect_equal(biome_name(test_data_t3), expected_t3)

  # Invalid input format, list
  expect_error(biome_name(list(ID_BIOME = 1:2)))
})
