## code to prepare a snapshot of the database
`%>%` <- magrittr::`%>%`
conn <- dabr::open_conn_mysql("SMPDSv2",
                              password = rstudioapi::askForPassword())

climate <- dabr::select_all(conn, "climate") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("climate", class(.)))
usethis::use_data(climate, overwrite = TRUE, compress = "xz")

entity <- dabr::select_all(conn, "entity") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("entity", class(.)))
usethis::use_data(entity, overwrite = TRUE, compress = "xz")

pollen_count <- conn %>%
  dabr::select_all("pollen_count") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("pollen_count", class(.)))
usethis::use_data(pollen_count, overwrite = TRUE, compress = "xz")

taxon_name <- conn %>%
  dabr::select_all("taxon_name") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("taxon_name", class(.)))
usethis::use_data(taxon_name, overwrite = TRUE, compress = "xz")

# Close database connection
dabr::close_conn(conn)
