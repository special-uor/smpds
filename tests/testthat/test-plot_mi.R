test_that("plot_mi works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds() %>%
    smpds::mi()
  test_plot <- smpds::plot_mi(test_data, show_plot = FALSE)
  expect_equal(test_plot$layers[[2]]$data, test_data %>%
                 dplyr::rename(var = mi))
})
