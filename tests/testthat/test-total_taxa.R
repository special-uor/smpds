test_that("total_taxa works", {
  # Load test data
  test_data <- system.file("testdata/test_data_counts.Rds",
                           package = "smpds") %>%
    readr::read_rds()
  output <- test_data %>%
    smpds::total_taxa(cols = 1:6)

  expected <- test_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total = dplyr::c_across(Abies:`Betula pubescens`) %>%
                    sum(na.rm = TRUE)) %>%
    .$total
  expect_equal(output$total, expected)

  # Incorrect indices for cols
  expect_error(test_data %>%
                 smpds::total_taxa)
})
