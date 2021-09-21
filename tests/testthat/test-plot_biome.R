test_that("plot_biome works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds()
  test_plot <- smpds::plot_biome(test_data, show_plot = FALSE)
  expected_data <- test_data %>%
    dplyr::bind_cols(tibble::tribble(
      ~description,   ~colour, ~n,
      "temperate\ndeciduous\nbroadleaf\nforest", "#55EB49", 5L,
      "temperate\ndeciduous\nbroadleaf\nforest", "#55EB49", 5L,
      "temperate\ndeciduous\nbroadleaf\nforest", "#55EB49", 5L,
      "temperate\ndeciduous\nbroadleaf\nforest", "#55EB49", 5L,
      "temperate\ndeciduous\nbroadleaf\nforest", "#55EB49", 5L
    ))
  expect_equal(test_plot$data, expected_data)

  # Use non-tiled legend
  test_plot2 <- smpds::plot_biome(test_data,
                                  show_plot = FALSE,
                                  tiled_legend = FALSE)
  expect_equal(test_plot2$data, expected_data)
})
