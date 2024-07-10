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

  # NA input (tmp = NA_real_), expected output should be NA_real_
  t3_output <- test_data %>%
    dplyr::mutate(tmp = NA_real_) %>%
    smpds::mi()
  expect_true(all(is.na(t3_output$tmp)))
})
