test_that("mi works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds()
  # Data frame (tibble) as input
  expected_t1 <- test_data %>%
    dplyr::mutate(mi = c(0.8239492808809,
                         0.80238254172581,
                         0.769778479936967,
                         0.725173084451647,
                         0.667900286793687))
  expect_equal(smpds::mi(test_data), expected_t1)

  # Invalid input, list
  expect_error(smpds::mi(test_data$tmp))

  # Missing values for the temperature
  test_data2 <- test_data %>%
    dplyr::slice(1) %>%
    dplyr::mutate(tmp = list(NA_real_))
  expected_t2 <- test_data2 %>%
    dplyr::mutate(mi = Inf)
  expect_equal(smpds::mi(test_data2), expected_t2)
})
