test_that("gwr works", {
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
  expected_t1 <- test_data %>%
    dplyr::bind_cols(tibble::tribble(
      ~T1,              ~T2,
      4.16420086770048, 4.13352151316261,
      3.22778210851516,  3.2077661599684,
      2.29136334932985,  2.2820108067742,
      1.35494459014453, 1.35625545357999
    ))
  expect_equal(output, expected_t1)

  # Call the gwr function with a 2D object as the reference
  expect_error(smpds::gwr(.ref = ref_data[,,1], .tar = test_data))

  # Call the gwr function with an invalid path for the reference data
  expect_error(smpds::gwr(.ref = "./non-existing-file.nc",
                          .tar = test_data,
                          varid = "tmp"))

  # Call the gwr function with an invalid path for the reference data and
  # missing varid
  expect_error(smpds::gwr(.ref = "./non-existing-file.nc",
                          .tar = test_data))
})
