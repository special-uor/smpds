test_that("pivot_data works", {
  # Load reference climate data
  ref_data <- system.file("testdata/test_data_climate_tmp_ref.Rds",
                          package = "smpds") %>%
    readr::read_rds()
  test_data <- tibble::tibble(entity_name = "University of Reading",
                              latitude = 51.44140,
                              longitude = -0.9418,
                              elevation = c(61, 161, 261, 361))
  output <- smpds::gwr(.ref = ref_data,
                       .tar = test_data)
  output_t1 <- output %>%
    smpds::pivot_data(varname = "tmp_degC")
  expected_t1 <- test_data %>%
    dplyr::select(-dplyr::starts_with("T")) %>%
    dplyr::bind_cols(tibble::tribble(
      ~tmp_degC,
      c(4.16420086770048, 4.13352151316261),
      c(3.22778210851516, 3.2077661599684),
      c(2.29136334932985, 2.2820108067742),
      c(1.35494459014453, 1.35625545357999)
    ))
  expect_equal(output_t1, expected_t1)

  # Convert temperature from Celsius to Fahrenheit
  output_t2 <- output %>%
    smpds::pivot_data(varname = "tmp_degF", scale = 9 / 5, add = 32)
  expected_t2 <- test_data %>%
    dplyr::select(-dplyr::starts_with("T")) %>%
    dplyr::bind_cols(tibble::tribble(
      ~tmp_degF,
      c(39.4955615618609, 39.4403387236927),
      c(37.8100077953273, 37.7739790879431),
      c(36.1244540287937, 36.1076194521936),
      c(34.4389002622601, 34.441259816444)
    ))
  expect_equal(output_t2, expected_t2)
})
