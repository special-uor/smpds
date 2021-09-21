test_that("gdd works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds()
  # Data frame (tibble) as input
  expected_t1 <- test_data %>%
    dplyr::mutate(gdd0 = c(2683.14221309047,
                           2820.2702107215,
                           2993.1272555014,
                           3230.6203798728,
                           3587.84837219895))
  expect_equal(smpds::gdd(test_data), expected_t1)

  # Numeric vector as input
  expected_t2 <- 2683.14221309047
  expect_equal(smpds::gdd(test_data$tmp[[1]]), expected_t2)

  # Invalid input, list
  expect_error(smpds::gdd(test_data$tmp))

  # GDD for another baseline temperature, 10
  expected_t3 <- test_data %>%
    dplyr::mutate(gdd10 = c(657.598276898224,
                            737.728664343737,
                            839.533603480822,
                            979.023947104924,
                            1198.84170918803))
  expect_equal(smpds::gdd(test_data, baseline = 10), expected_t3)

  # Multiple GDDs
  expected_t4 <- test_data %>%
    dplyr::mutate(gdd0 = c(2683.14221309047,
                           2820.2702107215,
                           2993.1272555014,
                           3230.6203798728,
                           3587.84837219895),
                  gdd10 = c(657.598276898224,
                            737.728664343737,
                            839.533603480822,
                            979.023947104924,
                            1198.84170918803))
  expect_equal(test_data %>%
                 smpds::gdd(baseline = 0) %>%
                 smpds::gdd(baseline = 10), expected_t4)

  # Missing values for the temperature
  test_data2 <- test_data %>%
    dplyr::slice(1) %>%
    dplyr::mutate(tmp = list(NA_real_))
  expected_t5 <- test_data2 %>%
    dplyr::mutate(gdd0 = NA_real_)
  expect_equal(smpds::gdd(test_data2), expected_t5)
})
