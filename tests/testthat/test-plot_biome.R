test_that("plot_biome works", {
  # Load test data
  test_data <- system.file("testdata/test_data_365days.Rds",
                           package = "smpds") %>%
    readr::read_rds()
  test_plot <- smpds::plot_biome(test_data, show_plot = FALSE)
  expected_data <- test_data %>%
    dplyr::rename(var = ID_BIOME) %>%
    dplyr::mutate(ID_BIOME = var) %>%
    dplyr::bind_cols(tibble::tribble(
      ~description,   ~colour, ~n,
      "temperate deciduous broadleaf forest", "#55EB49", 5L,
      "temperate deciduous broadleaf forest", "#55EB49", 5L,
      "temperate deciduous broadleaf forest", "#55EB49", 5L,
      "temperate deciduous broadleaf forest", "#55EB49", 5L,
      "temperate deciduous broadleaf forest", "#55EB49", 5L
    )) |>
    dplyr::mutate(
      description = stringr::str_replace_all(description, " ", "\n")
    )
  expect_equal(test_plot$data, expected_data)
})
