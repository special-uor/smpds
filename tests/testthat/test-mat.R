test_that("mat works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds()
  # Data frame (tibble) as input
  expected_t1 <- test_data %>%
    dplyr::mutate(mat = c(6.59541705885427,
                          7.06547456380932,
                          7.65090682816755,
                          8.46421197335878,
                          9.61298704315649))
  expect_equal(smpds::mat(test_data), expected_t1)

  # Numeric vector as input
  expected_t2 <- 6.59541705885427
  expect_equal(smpds::mat(test_data$tmp[[1]]), expected_t2)

  # Invalid input, list
  expect_error(smpds::mat(test_data$tmp))
})
