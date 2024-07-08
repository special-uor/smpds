test_that("plot_mtwa works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds() %>%
    smpds::mtwa()
  test_plot <- smpds::plot_mtwa(test_data, show_plot = FALSE)
  # Expected output
  expected_t1 <-
    structure(
      list(
        fill = c("#FEE5D9", "#FCAE91", "#FB6A4A", "#DE2D26",
                 "#A50F15"),
        x = c(
          25.3413888888889,
          25.3275,
          25.3275,
          25.3408333333333,
          25.3372222222222
        ),
        y = c(
          42.7411111111111,
          42.7555555555556,
          42.76,
          42.7205555555556,
          42.7244444444444
        ),
        PANEL = structure(c(1L,
                            1L, 1L, 1L, 1L), levels = "1", class = "factor"),
        group = structure(1:5, n = 5L),
        shape = c(21, 21, 21, 21, 21),
        colour = c("black", "black",
                   "black", "black", "black"),
        size = c(1, 1, 1, 1, 1),
        alpha = c(NA,
                  NA, NA, NA, NA),
        stroke = c(0.1, 0.1, 0.1, 0.1, 0.1)
      ),
      row.names = c(NA,-5L),
      class = "data.frame"
    )
  expect_equal(ggplot2::layer_data(test_plot, 2),
               expected_t1)
})
