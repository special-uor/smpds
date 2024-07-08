test_that("plot_mi works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds() %>%
    smpds::mi()
  test_plot <- smpds::plot_mi(test_data, show_plot = FALSE)
  # Expected output
  expected_t1 <-
    structure(
      list(
        fill = c("#253494", "#2C7FB8", "#41B6C4", "#A1DAB4",
                 "#FFFFCC"),
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
        group = structure(5:1, n = 5L),
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
