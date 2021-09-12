test_that("rm_zero_taxa works", {
  # Load test data
  test_data <- system.file("testdata/test_data_counts.Rds",
                           package = "smpds") %>%
    readr::read_rds()
  output <- test_data %>%
    smpds::rm_zero_taxa(cols = 1:6)
  expected <- test_data %>%
    dplyr::select(site_name:ID_BIOME,
                  Abies,
                  Acer,
                  Amaranthaceae,
                  Apiaceae,
                  Artemisia,
                  Asteroideae,
                  Betula,
                  Alnus)
  expect_equal(output, expected)

  # Incorrect indices for cols
  expect_error(test_data %>%
                 smpds::rm_zero_taxa)
})
