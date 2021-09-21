test_that("amalgamate_taxa works", {
  expect_equal(smpds::amalgamate_taxa(),
               smpds:::taxa_amalgamation)
})
