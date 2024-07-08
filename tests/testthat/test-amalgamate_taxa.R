test_that("amalgamate_taxa works", {
  expected_t1 <- tibble::tribble(
    ~unique_species_amalgamation,  ~non_woody_genus_amalgamation,
    ~compatible_with_smpdsv1,
    "Abies",                        "Abies",                          "Abies",
    "Abies alba",                   "Abies alba",                     "Abies",
    "Abies cilicica",               "Abies cilicica",                 "Abies",
    "Abies fargesii",               "Abies fargesii",                 "Abies",
    "Abies nordmanniana",           "Abies nordmanniana",             "Abies",
    "Abies pinsapo",                "Abies pinsapo",                  "Abies",
    "Abies pinsapo subsp marocana", "Abies pinsapo subsp marocana",   "Abies",
    "Abies sibirica",               "Abies sibirica",                 "Abies"
  )
  expect_equal(
    smpds::amalgamate_taxa() %>%
      dplyr::filter(stringr::str_detect(compatible_with_smpdsv1, "Abies")),
    expected_t1)
})
