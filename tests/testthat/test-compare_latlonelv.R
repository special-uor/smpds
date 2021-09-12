test_that("compare_latlonelv works", {
  # Load test data
  test_data_A <- system.file("testdata/test_data_metadata_A.Rds",
                             package = "smpds") %>%
    readr::read_rds()
  test_data_B <- system.file("testdata/test_data_metadata_B.Rds",
                             package = "smpds") %>%
    readr::read_rds()
  output <- smpds::compare_latlonelv(x = test_data_A,
                                     y = test_data_B,
                                     digits = 4) %>%
    dplyr::select(1:4) %>%
    magrittr::set_names(colnames(.) %>%
                          stringr::str_remove_all("\\.x"))
  expected <- test_data_A %>%
    dplyr::inner_join(test_data_B,
                      by = c("site_name", "latitude", "longitude", "elevation"))
  expect_equal(output, expected)
})
