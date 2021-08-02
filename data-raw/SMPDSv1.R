## code to prepare `SMPDSv1` dataset goes here
SMPDSv1 <- readr::read_csv("~/Downloads/SMPDSv2/SMPDS_Feb2019.csv")
SMPDSv1_long <- SMPDSv1 %>%
  tidyr::pivot_longer(6:252, names_to = "taxon_name")
SMPDSv1 <- readxl::read_xlsx("~/Downloads/SMPDSv2/Sandy_s MPDS_20_October_expanded.xlsx",
                             sheet = 1,
                             col_types = c(rep("guess", 10),
                                           rep("numeric", 1555)))
# duplicated_taxa <- SMPDSv1 %>%
#   dplyr::select(dplyr::contains("..."))
# idx <- rowSums(is.na(duplicated_taxa[, 4:6])) != 3
# duplicated_taxa[idx,]

usethis::use_data(SMPDSv1, overwrite = TRUE)
