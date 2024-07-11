test_that("print and generic functions work", {
  # Create snapshot using the entity name
  snp1 <- smpds::snapshot("juodonys_core")

  expect_equal(smpds:::print.snapshot(snp1), snp1)
  expect_equal(smpds:::print.snapshot(snp1, stats_only = FALSE), snp1)

  expect_equal(smpds:::extract_element(snp1$entity, "entity_name"),
               "juodonys_core")
  expect_equal(
    smpds:::extract_element(snp1$entity, "entity_name", ID_ENTITY = FALSE),
    "juodonys_core"
  )

  expect_null(smpds:::extract_element(snp1$entity, "entity name"))
})
