test_that("plot_climate works", {
  # Missing longitude
  expect_error(tibble::tibble(latitude = 0, mat = 20) %>%
                 smpds::plot_climate())
})
