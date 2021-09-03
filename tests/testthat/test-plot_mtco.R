test_that("plot_mtco works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds() %>%
    smpds::mtco()
  test_plot <- smpds::plot_mtco(test_data)
  expect_equal(test_plot$layers[[2]]$data, test_data %>%
                 dplyr::rename(var = mtco))
})
