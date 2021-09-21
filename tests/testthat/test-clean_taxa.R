test_that("clean_taxa works", {
  expect_equal(smpds::clean_taxa(),
               smpds:::taxa_clean)
})
