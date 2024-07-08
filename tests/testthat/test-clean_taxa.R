test_that("clean_taxa works", {
  expected_t1 <- tibble::tribble(
    ~taxon_name,                     ~clean_name,  ~action,
    "Abies",                         "Abies", "update",
    "ABIES",                         "Abies", "update",
    "Abies alba",                    "Abies alba", "update",
    "Abies cilicica",                "Abies cilicica", "update",
    "Abies fargesii",                "Abies fargesii", "update",
    "Abies marocana", "Abies pinsapo subsp marocana ", "update",
    "Abies maroccana", "Abies pinsapo subsp marocana ", "update",
    "Abies nordmanniana",            "Abies nordmanniana", "update",
    "Abies pinsapo",                 "Abies pinsapo", "update",
    "Abies pinsapo subsp marocana",  "Abies pinsapo subsp marocana", "update",
    "Abies pinsapo subsp marocana ",  "Abies pinsapo subsp marocana", "update",
    "Abies sibirica",                "Abies sibirica", "update",
    "cf  Abies",                    "Abies type", "update"
  )
  expect_equal(
    smpds::clean_taxa() %>%
      dplyr::filter(stringr::str_detect(clean_name, "Abies")),
    expected_t1)
})
