test_that("normalise_taxa works", {
  # Load test data
  test_data <- system.file("testdata/test_data_counts.Rds",
                           package = "smpds") %>%
    readr::read_rds()
  output <- test_data %>%
    smpds::normalise_taxa(cols = 1:6, scale = 100)

  expect_equal(output %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total = dplyr::c_across(Abies:`Betula pubescens`) %>%
                    sum(na.rm = TRUE)) %>%
    .$total,
    rep(100, nrow(test_data)))

  # Incorrect indices for cols
  expect_error(test_data %>%
                 smpds::normalise_taxa)
})
