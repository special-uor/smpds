test_that("plot_gdd works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds() %>%
    smpds::gdd()
  test_plot <- smpds::plot_gdd(test_data, show_plot = FALSE)
  expect_equal(test_plot$data %>%
                 dplyr::select(-var),
               test_data %>%
                 dplyr::rename(var = gdd0) %>%
                 dplyr::select(-var)
  )
})
