test_that("sort_taxa works", {
  # Load test data
  test_data <- system.file("testdata/test_data_counts.Rds",
                           package = "smpds") %>%
    readr::read_rds()
  set.seed(2021)
  output <- test_data %>%
    dplyr::select(1:6, sample(7:200, size = 194)) %>% # Pseudo-random selection
    smpds::sort_taxa(cols = 1:6)
  expect_equal(output, test_data)

  # Incorrect indices for cols
  expect_error(test_data %>%
                 smpds::sort_taxa)
})
