test_that("plot_mat works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds() %>%
    smpds::mat()
  test_plot <- smpds::plot_mat(test_data)
  expect_equal(test_plot$layers[[2]]$data, test_data %>%
                 dplyr::rename(var = mat))
})
