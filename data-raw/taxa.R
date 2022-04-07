## code to prepare `taxa` dataset goes here
`%>%` <- magrittr::`%>%`
taxa_all <-
  readxl::read_xlsx("~/Downloads/SMPDSv2/SMPDSv2-taxa-list_2021-09-08_SPH.xlsx",
                    sheet = 1) %>%
  dplyr::select(-3, -6) %>%
  magrittr::set_names(c("taxon_name",
                        "clean_name",
                        "unique_clean_name",
                        "species_amalgamation",
                        "unique_species_amalgamation",
                        "non_woody_genus_amalgamation",
                        "compatible_with_smpdsv1")) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    action = ifelse(clean_name %>%
                      stringr::str_to_lower() %>%
                      stringr::str_detect("excl"),
                    "delete", "update"),
    clean_name = ifelse(action == "delete", NA, clean_name) %>%
      stringr::str_replace_all("  ", " "),
    .after = clean_name
  ) %>%
  dplyr::mutate(clean_name = clean_name %>%
                  stringr::str_squish(),
                unique_species_amalgamation = unique_species_amalgamation %>%
                  stringr::str_replace_all("  ", " ") %>%
                  stringr::str_squish(),
                non_woody_genus_amalgamation = non_woody_genus_amalgamation %>%
                  stringr::str_replace_all("  ", " ") %>%
                  stringr::str_squish(),
                compatible_with_smpdsv1 = compatible_with_smpdsv1 %>%
                  stringr::str_replace_all("  ", " ") %>%
                  stringr::str_squish()) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name)

taxa_clean <- taxa_all %>%
  dplyr::select(1:3) %>%
  dplyr::distinct() %>%
  dplyr::bind_rows(tibble::tribble(
    ~taxon_name, ~clean_name, ~action,
    "Alisma", NA, "delete",
    "Anthemis arvensis type", "Anthemis arvensis type", "update",
    "Caltha type", NA, "delete",
    "Cannabis", NA, "delete",
    "Charcoal", NA, "delete",
    "Gelasinospora", NA, "delete",
    "Humulus", NA, "delete",
    "Menyanthes", NA, "delete",
    "Nymphaea", NA, "delete",
    "Pediastrum", NA, "delete",
    "Phyteuma-type", "Phyteuma type", "update",
    "Polypodiophyta", NA, "delete",
    "Potamogeton", NA, "delete",
    "Pseudopediastrum boryanum", NA, "delete",
    "Quercus (deciduous)", "Quercus deciduous", "update",
    "Quercus (evergreen)", "Quercus evergreen", "update",
    "Secale type", NA, "delete",
    "Sparganium", NA, "delete",
    "Sphagnum", NA, "delete",
    "Triticum type", NA, "delete",
    "Typha", NA, "delete",
    "Typha latifolia", NA, "delete",
    "Typha/Sparganium", NA, "delete"
    )) %>%
  dplyr::bind_rows(tibble::tribble(
    ~taxon_name,                            ~clean_name,  ~action,
    "Allium p",                             "Allium", "update",
    "Ancistrophyllum secundiflorum",        "Ancistrophyllum secundiflorum", "update",
    "Arecaceae undiff.",                    "Arecaceae", "update",
    "Boragiceae undiff.",                   "Boraginaceae", "update",
    "Combretaceae/Melastomataceae undiff.", NA, "delete",
    "Euphorbiaceae undif",                  "Euphorbiaceae", "update",
    "Plumbagiceae undiff.",                 "Plumbaginaceae", "update",
    "Pontederaceae",                        NA, "delete",
    "Pontederia type",                      NA, "delete",
    "Rhamceae undiff.",                     "Rhamnaceae", "update",
    "Sorbus cf",                            "Sorbus type", "update"
    )) %>%
  dplyr::bind_rows(tibble::tribble(
    ~taxon_name,                            ~clean_name,  ~action,
    "Aristolochiaceae", "Aristolochiaceae", "update", # INSPECT
    "Berberis hispanica", "Berberis hispanica", "update", # INSPECT
    "Carpinusbetulus", "Carpinus betulus", "update", # NEW
    "CarpinusorientalisOstrya", "Carpinus orientalis/Ostrya", "update", # NEW
    "Cytinaceae", "Cytinaceae", "update", # INSPECT
    "Dennstaedtiaceae", "Dennstaedtiaceae", "update", # INSPECT
    "Fabaceae herbs", "Fabaceae (herbs)", "update", # INSPECT
    "Grossulariaceae", "Grossulariaceae", "update", # INSPECT
    "Hymenophyllaceae", "Hymenophyllaceae", "update", # INSPECT
    "Juncaginaceae", "Juncaginaceae", "update", # INSPECT
    "Lavandula", "Lavandula", "update", # NEW
    "Melanthiaceae", "Melanthiaceae", "update", # NEW
    "Montiaceae", "Montiaceae", "update", # INSPECT
    "Nartheciaceae", "Nartheciaceae", "update", # INSPECT
    "OxyriaRumex", "Oxyria/Rumex", "update", # NEW
    "Parrotia", "Parrotia", "update", # INSPECT
    "Quercusdeciduous", "Quercus deciduous", "update", # NEW
    "Quercusevergreen", "Quercus evergreen", "update", # NEW
    "Quercusintermediate", "Quercus intermediate", "update", # INSPECT
    "Retama", "Retama", "update", # INSPECT
    "Sanguisorbagroup", "Sanguisorba type", "update", # INSPECT
    "Sciadopityaceae", "Sciadopityaceae", "update", # INSPECT
    "Transeauina", NA, "delete", # NEW
    "UlmusZelkova", "Ulmus/Zelkova", "update" # NEW
  )) %>%
  # dplyr::bind_rows(tibble::tribble(
  #   ~taxon_name,                            ~clean_name,  ~action,
  #   "Ancistrophyllum secundiflorum",        "Ancistrophyllum secundiflorum", "update",
  #   "Arecaceae undiff.",                    "Arecaceae undiff.", "update",
  #   "Boragiceae undiff.",                   "Boragiceae undiff.", "update",
  #   "Combretaceae/Melastomataceae undiff.", "Combretaceae/Melastomataceae undiff.", "update",
  #   "Euphorbiaceae undif",                  "Euphorbiaceae undif", "update",
  #   "Plumbagiceae undiff.",                 "Plumbagiceae undiff.", "update",
  #   "Rhamceae undiff.",                     "Rhamceae undiff.", "update"
  # ))
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::mutate(clean_name =
                  ifelse(clean_name %>%
                           stringr::str_detect("Caryophyllaceae subfam Silenoideae"),
                         "Caryophyllaceae subfam. Silenoideae",
                         clean_name) %>%
                  stringr::str_replace_all("Mysine melanophloeos",
                                           "Myrsine melanophloeos") %>%
                  stringr::str_replace_all("Juniperus Excelsa",
                                           "Juniperus excelsa"))

taxa_amalgamation <- taxa_all %>%
  dplyr::select(-c(1:5)) %>%
  dplyr::distinct() %>%
  dplyr::filter(compatible_with_smpdsv1 != "Noaea") %>%
  dplyr::mutate(unique_species_amalgamation =
                  unique_species_amalgamation %>%
                  stringr::str_remove_all("type") %>%
                  stringr::str_squish())

idx <- (taxa_clean$clean_name %>%
          stringr::str_remove_all("\\s*type|-type") %>%
          stringr::str_squish()) %in%
  taxa_amalgamation$unique_species_amalgamation
taxa_clean %>%
  dplyr::slice(which(!idx)) %>%
  dplyr::filter(is.na(action) | action == "update") %>%
  dplyr::mutate(comment = ifelse(is.na(action),
                                 "NEW: Species not found in the master sheet.",
                                 "Not found in the Unique Species Amalgamation (G) column.")) %>%
  dplyr::select(-action) #%>%
  # readr::write_csv(paste0("~/Downloads/SMPDSv2/SMPDSv2-missing-taxa-", Sys.Date(), ".csv"),
  #                  na = "")


taxa_clean %>%
  readr::write_excel_csv("inst/extdata/all_taxa.csv", na = "")

taxa_cleanup_files <- list.files("inst/extdata/", "_taxa\\.csv$", full.names = TRUE)
taxa_clean_all <- taxa_cleanup_files %>%
  purrr::map_dfr(~.x %>%
                   readr::read_csv() %>%
                   dplyr::mutate(source = basename(.x), .after = 3)) %>%
  dplyr::select(1:4) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name) %>%
  dplyr::mutate(ID = seq_along(taxon_name)) %>%
  dplyr::mutate(clean_name = ifelse(action == "delete", NA, clean_name))

taxa_clean_all2 <- taxa_clean_all %>%
  dplyr::arrange(action) %>%
  dplyr::distinct(taxon_name, .keep_all = TRUE)
  # dplyr::distinct(taxon_name, clean_name, .keep_all = TRUE)
taxa_clean_all3 <- taxa_clean_all %>%
  dplyr::distinct(taxon_name, .keep_all = TRUE)
taxa_clean_all4 <- taxa_clean_all2 %>%
  dplyr::filter(!(ID %in% taxa_clean_all3$ID))

discrepancies <- taxa_clean_all %>%
  dplyr::filter(taxon_name %in% taxa_clean_all4$taxon_name) %>%
  dplyr::distinct(taxon_name, clean_name, .keep_all = TRUE) %>%
  dplyr::arrange(taxon_name, clean_name)
# discrepancies %>%
#   dplyr::select(-ID) %>%
#   dplyr::mutate(action = ifelse(!is.na(clean_name) & taxon_name == clean_name,
#                                 "keep original name", action)) %>%
#   readr::write_excel_csv("~/Downloads/SMPDSv2/SMPDSv2_taxa_discrepancies_2021-09-02.csv", na = "EXCLUDE")

    # taxa_clean %>%
#   dplyr::distinct(taxon_name, clean_name, .keep_all = TRUE)
#   readr::write_excel_csv("inst/extdata/taxa_clean.csv", na = "")
taxa_clean <- taxa_clean_all2 %>%
  dplyr::select(-source, -ID) %>%
  dplyr::arrange(dplyr::desc(action), taxon_name)
usethis::use_data(taxa_clean, taxa_amalgamation,
                  internal = TRUE,
                  overwrite = TRUE)

taxa_all %>% dplyr::group_by(clean_name) %>% dplyr::mutate(n = length(unique(non_woody_genus_amalgamation))) %>% dplyr::filter(n > 1)
taxa_amalgamation %>% dplyr::group_by(unique_species_amalgamation) %>% dplyr::mutate(n = length(unique(non_woody_genus_amalgamation))) %>% dplyr::filter(n > 1)
