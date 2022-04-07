## code to prepare `EPD` dataset goes here
`%>%` <- magrittr::`%>%`
neotoma_epd_csv <- readr::read_csv("~/Downloads/neotoma_epd.csv")
neotoma_epd_json <- jsonlite::read_json("~/Downloads/neotoma_epd.json")
neotoma_epd_csv[1, ]
future::plan(future::multisession, workers = 10)
progressr::with_progress({
  p <- progressr::progressor(steps = nrow(neotoma_epd_csv))
  neotoma_geochron <- neotoma_epd_csv %>%
    furrr::future_pmap(function(#siteid, sitename, latitude, longitude,
                                datasetid, datasettype, ...) {
      p()
      if(datasettype %in% c("geochronologic")) {
        tryCatch({
          return(neotoma::get_geochron(datasetid))
        }, error = function(e) return(NULL))
      } else {
        return(NULL)
      }
    })
})


neotoma_geochron %>%
  readr::write_rds("data-raw/neotoma_epd_geochron.Rds")

neotoma_geochron <-
  readr::read_rds("data-raw/neotoma_epd_geochron.Rds")

# aux2 <- neotoma_epd_csv %>%
#   dplyr::filter(!(datasettype %in% c("charcoal", "geochronologic", "loss-on-ignition", "pollen"))) %>%
#   dplyr::slice(1:5) %>%
#   purrr::pmap(function(siteid, sitename, latitude, longitude, datasetid, datasettype, ...) {
#     neotoma::get_chroncontrol(datasetid)
#     if(datasettype %in% c("geochronologic")) {
#       neotoma::get_geochron(datasetid)
#     } else if (datasettype %in% c("geochrono")) {
#       neotoma::get_geochron(datasetid)
#     }
#     # else {
#     #   neotoma::get_dataset(datasetid)
#     # }
#   })

future::plan(future::multisession, workers = 10)
progressr::with_progress({
  p <- progressr::progressor(steps = nrow(neotoma_epd_csv))
  neotoma_pollen <- neotoma_epd_csv %>%
    furrr::future_pmap(function(datasetid, ...) {
      p()
      tryCatch(neotoma::get_download(datasetid), error = function(e) NULL)
    })
})

neotoma_pollen %>%
  readr::write_rds("data-raw/neotoma_epd_pollen.Rds")

neotoma_pollen <-
  readr::read_rds("data-raw/neotoma_epd_pollen.Rds")

neotoma_pollen_counts <- neotoma_pollen %>%
  purrr::map(~tryCatch(neotoma::ages(.x), error = function(e) NULL))

future::plan(future::multisession, workers = 15)
progressr::with_progress({
  p <- progressr::progressor(steps = length(neotoma_pollen))
  neotoma_pollen2 <- tibble::tibble(neotoma_pollen = neotoma_pollen,
                                    counts = neotoma_pollen_counts) %>%
    furrr::future_pmap(function(neotoma_pollen, counts) {
      if (is.null(neotoma_pollen) || is.null(neotoma_pollen[[1]]) || is.null(neotoma_pollen[[1]][[1]]))
        return(NULL)
      site <- neotoma_pollen[[1]] %>%
        purrr::pluck("dataset") %>%
        purrr::pluck("site.data") %>%
        tibble::as_tibble()
      dataset <- neotoma_pollen[[1]] %>%
        purrr::pluck("dataset") %>%
        purrr::pluck("dataset.meta") %>%
        tibble::as_tibble()
      tryCatch({
        ages <- neotoma::ages(neotoma_pollen[[1]]) %>%
          tibble::as_tibble()
      },
      error = function(e) {
        ages <- NULL
      })

      tryCatch({
        counts <- neotoma::counts(neotoma_pollen[[1]]) %>%
          tibble::as_tibble()
      },
      error = function(e) {
        counts <- NULL
      })

      tryCatch({
        depths <- neotoma::depths(neotoma_pollen[[1]]) %>%
          tibble::as_tibble()
      },
      error = function(e) {
        depths <- NULL
      })

      tryCatch({
        taxa <- neotoma::taxa(neotoma_pollen[[1]]) %>%
          tibble::as_tibble()
      },
      error = function(e) {
        taxa <- NULL
      })
      dataset.id <- dataset$dataset.id
      pollen_meta <- tryCatch(neotoma::get_chroncontrol(dataset.id, FALSE),
                              error = function(e) NULL)

      chron.control <- pollen_meta %>%
        purrr::pluck("chron.control") %>%
        tibble::as_tibble()
      if (nrow(chron.control) > 0) {
        chron.control$dataset.id <- dataset.id
      }
      meta <- pollen_meta %>%
        purrr::pluck("meta") %>%
        tibble::as_tibble()

      p()
      list(
        ages = ages,
        chron_control = chron.control,
        counts = counts,
        dataset = dataset,
        depths = depths,
        meta = meta,
        pollen_meta = pollen_meta,
        site = site,
        taxa = taxa
      )
    })
})

neotoma_pollen2 %>%
  readr::write_rds("data-raw/neotoma_epd_pollen2.Rds")

neotoma_pollen2_old <-
  readr::read_rds("data-raw/neotoma_epd_pollen2.Rds")

# waldo::compare(neotoma_pollen2_old[2], neotoma_pollen2[2])

neotoma_epd_csv2 <- neotoma_epd_csv %>%
  dplyr::mutate(geochron = neotoma_geochron,
                pollen = neotoma_pollen2) %>%
  dplyr::filter(stringr::str_detect(datasettype, "pollen"))

neotoma_epd_csv2 %>%
  readr::write_rds("data-raw/neotoma_epdv2.Rds")

neotoma_epd_csv2 <-
  readr::read_rds("data-raw/neotoma_epdv2.Rds")

taxa_list <- neotoma_epd_csv2 %>%
  tidyr::unnest_wider(pollen) %>%
  dplyr::group_by(siteid, datasetid) %>%
  purrr::pmap_dfr(function(taxa, ...) {taxa}) %>%
  dplyr::mutate(alias = alias %>%
                  stringr::str_c(collapse = "; "),
                variable.context = variable.context %>%
                  stringr::str_c(collapse = "; ")) %>%
  dplyr::distinct()

# taxa_list %>% dplyr::group_by(taxon.name) %>% dplyr::mutate(n = length(taxon.name)) %>% dplyr::filter(n > 1)

neotoma_epd_csv3 <- neotoma_epd_csv2 %>%
  # dplyr::slice(1:10) %>%
  tidyr::unnest_wider(pollen) %>%
  dplyr::select(-pollen_meta) %>%
  tidyr::unnest(c(dataset, meta, chron_control),
                names_repair = "unique") %>%
  dplyr::group_by(siteid, datasetid) %>%
  dplyr::mutate(pollen = dplyr::bind_cols(ages, counts) %>%
                  list()) %>%
  dplyr::select(-ages, -counts)

neotoma_epd_csv3 %>%
  readr::write_rds("data-raw/neotoma_epdv3.Rds")

neotoma_epd_csv3 <-
  readr::read_rds("data-raw/neotoma_epdv3.Rds")

neotoma_epd_csv4 <- neotoma_epd_csv3 %>%
  dplyr::select(-geochron, -taxa) %>%
  dplyr::select(-pollen) %>%
  tidyr::unnest(site)

neotoma_epd_csv4 %>%
  dplyr::select(-`dataset.id...18`, -`dataset.id...20`, -site.id) %>%
  dplyr::rename(elevation = elev) %>%
  dplyr::relocate(elevation, .after = longitude) %>%
  readr::write_csv(file = "data-raw/neotoma-epd-sites.csv", na = "")
neotoma_epd_csv4 %>%
  readr::write_rds("data-raw/neotoma_epdv4.Rds")

neotoma_epd_csv4 <-
  readr::read_rds("data-raw/neotoma_epdv4.Rds")

neotoma_epd_csv5 <- neotoma_epd_csv2 %>%
  # dplyr::slice(1:10) %>%
  tidyr::unnest_wider(pollen) %>%
  dplyr::select(-pollen_meta, -chron_control, -geochron, -taxa) %>%
  tidyr::unnest(c(dataset, meta, site),
                names_repair = "unique") %>%
  dplyr::group_by(siteid, datasetid) %>%
  dplyr::mutate(pollen = dplyr::bind_cols(ages, counts) %>%
                  list()) %>%
  dplyr::select(-ages, -counts) %>%
  # dplyr::select(-geochron, -taxa) %>%
  # dplyr::select(-pollen) %>%
  dplyr::rename(age_older = age.older,
                age_type = age.type,
                age_younger = age.younger,
                dataset_id = dataset.id) %>%
  tidyr::unnest(pollen)

neotoma_epd_csv5 %>%
  # dplyr::select(-`dataset.id...18`, -`dataset.id...20`, -site.id) %>%
  dplyr::rename(elevation = elev) %>%
  dplyr::relocate(elevation, .after = longitude) %>%
  readr::write_csv(file = "data-raw/neotoma-epd-counts.csv", na = "")
neotoma_epd_csv5 %>%
  readr::write_rds("data-raw/neotoma_epdv5.Rds")

neotoma_epd_csv5 <-
  readr::read_rds("data-raw/neotoma_epdv5.Rds")


aux <- neotoma_epd_csv2 %>%
  dplyr::filter(!siteid %in% neotoma_epd_csv4$siteid)
neotoma_epd_csv2 %>%
  dplyr::filter(siteid == 15930)

aux2 <- neotoma_epd_csv2 %>%
  dplyr::filter(siteid %in% aux$siteid)
# neotoma_pollen <- neotoma_epd_csv %>%
#   purrr::pmap(function(datasetid, ...) {
#     tryCatch(neotoma::get_download(datasetid), error = function(e) NULL)
#   })

future::plan(future::multisession, workers = 10)
progressr::with_progress({
  p <- progressr::progressor(steps = nrow(aux2))
  aux3 <- aux2 %>%
    furrr::future_pmap(function(datasetid, ...) {
      p()
      tryCatch(neotoma::get_dataset(datasetid), error = function(e) NULL)
    })
})

# epd <- neotoma::get_dataset()
usethis::use_data(EPD, overwrite = TRUE)
