test_that("mtco works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds()
  # Data frame (tibble) as input
  expected_t1 <- test_data %>%
    dplyr::mutate(mtco = c(-4.05351049060602,
                           -3.75826565038094,
                           -3.36965654287521,
                           -2.6945199437864,
                           -1.85959686701675))
  expect_equal(smpds::mtco(test_data), expected_t1)

  # Numeric vector as input
  expected_t2 <- -4.05351049060602
  expect_equal(smpds::mtco(test_data$tmp[[1]]), expected_t2)

  # Invalid input, list
  expect_error(smpds::mtco(test_data$tmp))

  # Missing values for the temperature
  test_data2 <- test_data %>%
    dplyr::slice(1) %>%
    dplyr::mutate(tmp = list(NA_real_))
  expected_t3 <- test_data2 %>%
    dplyr::mutate(mtco = NA_real_)
  expect_equal(smpds::mtco(test_data2), expected_t3)
})
