test_that("mtwa works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds()
  # Data frame (tibble) as input
  expected_t1 <- test_data %>%
    dplyr::mutate(mtwa = c(16.8160682301779,
                           17.3784003995849,
                           18.0442810914532,
                           18.9124699622965,
                           20.2009653254078))
  expect_equal(smpds::mtwa(test_data), expected_t1)

  # Numeric vector as input
  expected_t2 <- 16.8160682301779
  expect_equal(smpds::mtwa(test_data$tmp[[1]]), expected_t2)

  # Invalid input, list
  expect_error(smpds::mtwa(test_data$tmp))
})
