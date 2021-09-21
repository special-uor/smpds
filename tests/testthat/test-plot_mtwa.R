test_that("plot_mtwa works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds() %>%
    smpds::mtwa()
  test_plot <- smpds::plot_mtwa(test_data, show_plot = FALSE)
  # vdiffr::expect_doppelganger("MTWA", test_plot)
  expect_equal(test_plot$data %>%
                 dplyr::select(-var),
               test_data %>%
                 dplyr::rename(var = mtwa) %>%
                 dplyr::select(-var)
               )
})
