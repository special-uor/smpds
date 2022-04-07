test_that("process_apd works", {
  # Create test file (based on 01DIOGO.ascii
  tmp_dir <- tempdir()
  test_file <- file.path(tmp_dir, "01DIOGO.ascii")
  on.exit(unlink(test_file))
  c("##header
#Sigle=01DIOGO
#SiteName=Diogo 01
#Country=Senegal
#Latitude=15°16'0\" N
#Longitude=16°0'0\" W
#Altitude=8m
#Reference1=Pub1
#Reference2=Pub2
#Reference3=Pub3
##data
#X1;X2;X3;X4;X5;X6
Acacia;Acacia groupe i;1081;10313.826172;10313.826172;1
Acacia;Acacia groupe iii;900;8420.178711;8420.178711;1
Acacia;Acacia groupe i;100;50.464287;50.464287;1
Acacia;Acacia groupe i;350;2666;2666;1
Acanthaceae undiff.;Acanthaceae;1130;10826.47168;10826.47168;1
Adansonia ;Adansonia;652;5825.567383;5825.567383;1
Aerva-type lanata;Aerva lanata-type;1081;10313.826172;10313.826172;1") %>%
    readr::write_lines(file = test_file)
  expected_t1 <- list(
    structure(
      list(
        sigle = c(
          "01DIOGO",
          "01DIOGO",
          "01DIOGO",
          "01DIOGO",
          "01DIOGO",
          "01DIOGO",
          "01DIOGO"
        ),
        sitename = c(
          "Diogo 01",
          "Diogo 01",
          "Diogo 01",
          "Diogo 01",
          "Diogo 01",
          "Diogo 01",
          "Diogo 01"
        ),
        country = c(
          "Senegal",
          "Senegal",
          "Senegal",
          "Senegal",
          "Senegal",
          "Senegal",
          "Senegal"
        ),
        latitude = c(
          15.2666666666667,
          15.2666666666667,
          15.2666666666667,
          15.2666666666667,
          15.2666666666667,
          15.2666666666667,
          15.2666666666667
        ),
        longitude = c(-16,-16,-16,-16,-16,-16,-16),
        elevation = c(8, 8, 8, 8, 8, 8, 8),
        publication = c(
          "Pub1;\nPub2;\nPub3",
          "Pub1;\nPub2;\nPub3",
          "Pub1;\nPub2;\nPub3",
          "Pub1;\nPub2;\nPub3",
          "Pub1;\nPub2;\nPub3",
          "Pub1;\nPub2;\nPub3",
          "Pub1;\nPub2;\nPub3"
        ),
        `Taxon Name [APD]` = c(
          "Acacia",
          "Acacia",
          "Acacia",
          "Acacia",
          "Acanthaceae undiff.",
          "Adansonia ",
          "Aerva-type lanata"
        ),
        `Taxon Name [Author]` = c(
          "Acacia groupe i",
          "Acacia groupe iii",
          "Acacia groupe i",
          "Acacia groupe i",
          "Acanthaceae",
          "Adansonia",
          "Aerva lanata-type"
        ),
        `Depth [m]` = c(1081, 900,
                        100, 350, 1130, 652, 1081),
        `Radiocarbon Chronology [yrs BP]` = c(
          10313.826172,
          8420.178711,
          50.464287,
          2666,
          10826.47168,
          5825.567383,
          10313.826172
        ),
        `Calendar Chronology [yrs BP]` = c(
          10313.826172,
          8420.178711,
          50.464287,
          2666,
          10826.47168,
          5825.567383,
          10313.826172
        ),
        Count = c(1,
                  1, 1, 1, 1, 1, 1)
      ),
      row.names = c(NA,-7L),
      groups = structure(
        list(.rows = structure(
          list(1L, 2L, 3L, 4L, 5L, 6L, 7L),
          ptype = integer(0),
          class = c("vctrs_list_of",
                    "vctrs_vctr", "list")
        )),
        row.names = c(NA,-7L),
        class = c("tbl_df",
                  "tbl", "data.frame")
      ),
      class = c("rowwise_df", "tbl_df", "tbl",
                "data.frame")))
  smpds:::process_apd(path = tmp_dir,
                      col_names = c("Taxon Name [APD]",
                                    "Taxon Name [Author]",
                                    "Depth [m]",
                                    "Radiocarbon Chronology [yrs BP]",
                                    "Calendar Chronology [yrs BP]",
                                    "Count"),
                      col_types = c(readr::col_character(),
                                    readr::col_character(),
                                    readr::col_double(),
                                    readr::col_double(),
                                    readr::col_double(),
                                    readr::col_double())) %>%
    expect_equal(expected = expected_t1)
  unlink(test_file)
})
