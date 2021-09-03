test_that("plot_biome works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds()
  test_plot <- smpds::plot_biome(test_data)
  expected_data <- test_data %>%
    dplyr::bind_cols(tibble::tribble(
      ~description,   ~colour, ~n,
      "temperate deciduous broadleaf forest", "#55EB49", 5L,
      "temperate deciduous broadleaf forest", "#55EB49", 5L,
      "temperate deciduous broadleaf forest", "#55EB49", 5L,
      "temperate deciduous broadleaf forest", "#55EB49", 5L,
      "temperate deciduous broadleaf forest", "#55EB49", 5L
    ))
  expect_equal(test_plot$layers[[2]]$data, expected_data)
})
