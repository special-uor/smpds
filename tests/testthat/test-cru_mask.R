test_that("cru_mask works", {
  # Use default parameters
  expected <- structure(
    c(`FALSE` = 191780L, `TRUE` = 67420L),
    .Dim = 2L,
    .Dimnames = structure(list(c("FALSE", "TRUE")), .Names = ""),
    class = "table"
  )
  expect_equal(table(smpds:::cru_mask()$land), expected)

  # Use custom coordinates
  expect_error(smpds:::cru_mask(coordinates = tibble::tibble(latitude = 0)))
})
