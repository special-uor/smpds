#' Create a snapshot of the data linked to entities
#'
#' Create a snapshot of the data linked to entities. Including metadata,
#' climate & vegetation reconstructions and pollen counts.
#'
#' @param x This object accepts different classes. If the given object is
#'     a database connection, then extracts data from the database using the
#'     `ID_SITE`, `ID_ENTITY` or `entity_name` (these should be provided after
#'     the connection object). Alternatively, if the given object is a vector,
#'     then it will retrieve the records from an internal snapshot of the
#'     database, included in this package.
#' @param ... Optional parameters.
#'
#' @rdname snapshot
#' @export
snapshot <- function(x, ...) {
  if (missing(x))
    return(snapshot.default(x, ...))
  UseMethod("snapshot", x)
}

#' @param ID_ENTITY Optional, if `ID_SITE`, `entity_name` or `site_name` are
#'     provided.
#' @param ID_SITE Optional, if `ID_ENTITY`, `entity_name` or `site_name` are
#'     provided.
#' @param entity_name Optional, if `ID_SITE`, `ID_ENTITY` or `site_name` are
#'     provided.
#' @param site_name Optional, if `ID_SITE`, `ID_ENTITY` or `entity_name` are
#'     provided.
#' @param quiet Boolean flag to indicate if queries should be displayed.
#'
#' @rdname snapshot
#' @return List with the individual tables.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' conn <- dabr::open_conn_mysql(dbname = "SMPDSv2",
#'                               password = rstudioapi::askForPassword())
#' # Using the entity name
#' snp1 <- smpds::snapshot(conn, entity_name = "juodonys_core")
#' snp1
#'
#' # Using the site name
#' snp2 <- smpds::snapshot(conn, site_name = "Petresiunai")
#' snp2
#'
#' # Using the ID_ENTITY
#' snp3 <- smpds::snapshot(conn, ID_ENTITY = 1)
#' snp3
#'
#' # Using the ID_SITE
#' snp4 <- smpds::snapshot(conn, ID_SITE = 2)
#' snp4
#' }
snapshot.MariaDBConnection <- function(x,
                                       ...,
                                       ID_ENTITY,
                                       ID_SITE,
                                       entity_name,
                                       site_name,
                                       quiet = TRUE) {
  if (!missing(ID_ENTITY)) {
    .snapshot_by_entity(x, ID_ENTITY, quiet = quiet)
  } else if (!missing(ID_SITE)) {
    .snapshot_by_site(x, ID_SITE, quiet = quiet)
  } else if (!missing(entity_name)) {
    .snapshot_by_entity_name(x, entity_name, quiet = quiet)
  } else if (!missing(site_name)) {
    .snapshot_by_site_name(x, site_name, quiet = quiet)
  } else {
    message("At least one of the following is required:\n",
            "- ID_SITE\n- ID_ENTITY\n- entity_name\n- site_name")
  }
}

#' @param use_site_name Boolean flag to indicate whether to search using
#'     `entity_name` (default) or `site_name`, using the values in `x`.
#' @rdname snapshot
#' @export
#'
#' @examples
#' # Using the entity name
#' snp1 <- smpds::snapshot("juodonys_core")
#' snp1
#'
#' # Using the site name
#' snp2 <- smpds::snapshot("Petresiunai", use_site_name = TRUE)
#' snp2
snapshot.character <- function(x, ..., use_site_name = FALSE) {
  # Local bindings
  . <- amalgamation_level <- count <- entity_name <- site_name <- NULL
  taxon_name <- ID_ENTITY <- ID_SAMPLE <- ID_TAXON <- NULL

  if (use_site_name) {
    entity_tb <- smpds::entity %>%
      dplyr::filter(site_name %in% x)
  } else {
    entity_tb <- smpds::entity %>%
      dplyr::filter(entity_name %in% x)
  }
  if (nrow(entity_tb) == 0) {
    message("No records were found!")
    return(NULL)
  }

  climate_tb <- smpds::climate %>%
    dplyr::filter(ID_SAMPLE %in% entity_tb$ID_SAMPLE)
  if (nrow(entity_tb) > 0) {
    tryCatch({
      pollen_count_tb <- smpds::pollen_count %>%
        dplyr::filter(ID_SAMPLE %in% entity_tb$ID_SAMPLE) %>%
        dplyr::left_join(smpds::taxon_name, by = "ID_TAXON") %>%
        split(.$amalgamation_level) %>%
        purrr::map(function(counts) {
          counts %>%
            dplyr::select(-ID_TAXON, -amalgamation_level) %>%
            dplyr::filter(!is.na(count)) %>%
            tidyr::pivot_wider(id_cols = c(ID_SAMPLE),
                               names_from = taxon_name,
                               names_sort = TRUE,
                               values_fill = 0,
                               values_from = count) %>%
            dplyr::select(1, order(colnames(.)[-1]) + 1)
        }) %>%
        magrittr::set_names(names(.) %>%
                              stringr::str_replace_all("0", "clean") %>%
                              stringr::str_replace_all("1", "intermediate") %>%
                              stringr::str_replace_all("2", "amalgamated"))
    }, error = function(e) {
      pollen_count_tb <- NULL
    })
  } else {
    pollen_count_tb <- NULL
  }

  list(
    entity = entity_tb,
    climate = climate_tb,
    pollen_count = pollen_count_tb
  ) %>%
    magrittr::set_class(c("snapshot", class(.)))
}

#' @param use_id_site Boolean flag to indicate whether to search using
#'     `ID_ENTITY` (default) or `ID_SITE`, using the values in `x`.
#' @rdname snapshot
#' @export
#'
#' @examples
#' # Using the ID_ENTITY
#' snp1 <- smpds::snapshot(1)
#' snp1
#'
#' # Using the ID_SITE
#' snp2 <- smpds::snapshot(2, use_id_site = TRUE)
#' snp2
snapshot.numeric <- function(x, ..., use_id_site = FALSE) {
  # Local bindings
  ID_ENTITY <- ID_SITE <- NULL
  if (use_id_site) {
    entity_tb <- smpds::entity %>%
      dplyr::filter(ID_SITE %in% x)
  } else {
    entity_tb <- smpds::entity %>%
      dplyr::filter(ID_ENTITY %in% x)
  }
  if (nrow(entity_tb) == 0) {
    message("No records were found!")
    return(NULL)
  }
  return(snapshot(entity_tb$entity_name))
}

#' @rdname snapshot
#' @export
snapshot.tbl_df <- function(x, ...) {
  NextMethod("snapshot")
}

#' @rdname snapshot
#' @export
snapshot.tbl <- function(x, ...) {
  NextMethod("snapshot")
}

#' @rdname snapshot
#' @export
#'
#' @examples
#' # Using the entity table
#' `%>%` <- magrittr::`%>%`
#' snp1 <- smpds::entity %>%
#'   dplyr::slice(1) %>%
#'   smpds::snapshot()
#' snp1
#'
#' # Using a custom data frame (`tibble` object) with site names
#' snp2 <- tibble::tibble(
#'     site_name = c("Aligol lake", "Big Sandy Creek")
#'   ) %>%
#'   smpds::snapshot()
#' snp2
snapshot.data.frame <- function(x, ...) {
  # Extract column's names of the input data frame
  names <- colnames(x)
  if ("ID_ENTITY" %in% names) {
    return(snapshot(x$ID_ENTITY))
  } else if ("ID_SITE" %in% names) {
    return(snapshot(x$ID_SITE, use_id_site = TRUE))
  } else if ("entity_name" %in% names) {
    return(snapshot(x$entity_name))
  } else if ("site_name" %in% names) {
    return(snapshot(x$site_name, use_site_name = TRUE))
  }
}


#' @rdname snapshot
#' @export
snapshot.default <- function(x, ...) {
  smpds::entity$entity_name %>%
    snapshot()
}

#' @keywords internal
.snapshot_by_site <- function(x, ID_SITE, quiet = TRUE) {
  entity_tb <- x %>%
    dabr::select("SELECT ID_ENTITY FROM entity WHERE ID_SITE IN (",
                 paste0(ID_SITE, collapse = ", "),
                 ")",
                 quiet = quiet)
  return(.snapshot_by_entity(x, entity_tb$ID_ENTITY, quiet = quiet))
}

#' @keywords internal
.snapshot_by_entity <- function(x, ID_ENTITY, quiet = TRUE) {
  # Local bindings
  . <- amalgamation_level <- count <- taxon_name <- NULL
  ID_SAMPLE <- ID_SAMPLE <- ID_TAXON <- NULL

  entity_tb <- x %>%
    dabr::select("SELECT * FROM entity WHERE ID_ENTITY IN (",
                 paste0(ID_ENTITY, collapse = ", "),
                 ")",
                 quiet = quiet) %>%
    magrittr::set_class(c("entity", class(.)))
  if (nrow(entity_tb) == 0) {
    message("No records were found!")
    return(NULL)
  }
  climate_tb <- x %>%
    dabr::select("SELECT * FROM climate WHERE ID_SAMPLE IN (",
                 paste0(entity_tb$ID_SAMPLE, collapse = ", "),
                 ")",
                 quiet = quiet) %>%
    magrittr::set_class(c("climate", class(.)))
  if (nrow(entity_tb) > 0) {
    tryCatch({
      taxon_name_tb <- dabr::select_all(x, "taxon_name", quiet = TRUE)
      pollen_count_tb <- x %>%
        dabr::select("SELECT * FROM pollen_count WHERE ID_SAMPLE IN (",
                     paste0(entity_tb$ID_SAMPLE, collapse = ", "),
                     ")",
                     quiet = quiet) %>%
        dplyr::left_join(taxon_name_tb, by = "ID_TAXON") %>%
        split(.$amalgamation_level) %>%
        purrr::map(function(counts) {
          counts %>%
            dplyr::select(-ID_TAXON, -amalgamation_level) %>%
            dplyr::filter(!is.na(count)) %>%
            tidyr::pivot_wider(id_cols = c(ID_SAMPLE),
                               names_from = taxon_name,
                               names_sort = TRUE,
                               values_fill = 0,
                               values_from = count) %>%
            dplyr::select(1, order(colnames(.)[-1]) + 1)
        }) %>%
        magrittr::set_names(names(.) %>%
                              stringr::str_replace_all("0", "clean") %>%
                              stringr::str_replace_all("1", "intermediate") %>%
                              stringr::str_replace_all("2", "amalgamated"))
    }, error = function(e) {
      pollen_count_tb <- NULL
    })
  } else {
    pollen_count_tb <- NULL
  }

  list(
    entity = entity_tb,
    climate = climate_tb,
    pollen_count = pollen_count_tb
  ) %>%
    magrittr::set_class(c("snapshot", class(.)))
}

#' @keywords internal
.snapshot_by_entity_name <- function(x, entity_name, quiet = TRUE) {
  entity_tb <- x %>%
    dabr::select("SELECT ID_ENTITY FROM entity WHERE entity_name IN (",
                 paste0(dabr::quote(entity_name), collapse = ", "),
                 ")",
                 quiet = quiet)
  if (nrow(entity_tb) == 0) {
    message("No records were found!")
    return(NULL)
  }
  return(.snapshot_by_entity(x, entity_tb$ID_ENTITY, quiet = quiet))
}

#' @keywords internal
.snapshot_by_site_name <- function(x, site_name, quiet = TRUE) {
  entity_tb <- x %>%
    dabr::select("SELECT ID_ENTITY FROM entity WHERE site_name IN (",
                 paste0(dabr::quote(site_name), collapse = ", "),
                 ")",
                 quiet = quiet)
  if (nrow(entity_tb) == 0) {
    message("No records were found!")
    return(NULL)
  }
  return(.snapshot_by_entity(x, entity_tb$ID_ENTITY, quiet = quiet))
}

#' Write DB snapshot to disk
#' Write DB snapshot to disk as individual CSV files.
#'
#' @param .data DB snapshot (object of `smpds` class).
#' @param prefix String with a prefix path where the data should be stored.
#'
#' @return Invisibly returns the input DB snapshot.
#' @export
write_csvs <- function(.data, prefix) {
  if (!("snapshot" %in% class(.data)))
    stop("The given object does not look like a valid snapshot from the ",
         "`SMPDSv2 database. Try using the function `snapshot` first.",
         call. = FALSE)
  if (!dir.exists(dirname(prefix)))
    stop("The provided directory, `", dirname(prefix), "`, does not exist.",
         call. = FALSE)
  .data$entity %>%
    dplyr::left_join(.data$climate,
                     by = "ID_SAMPLE") %>%
    readr::write_excel_csv(file = paste0(prefix, "_metadata.csv"), na = "")
  .data$pollen_count$clean %>%
    readr::write_excel_csv(file = paste0(prefix, "_pollen_counts_clean.csv"))
  .data$pollen_count$intermediate %>%
    readr::write_excel_csv(file = paste0(prefix, "_pollen_counts_intermediate.csv"))
  .data$pollen_count$amalgamated %>%
    readr::write_excel_csv(file = paste0(prefix, "_pollen_counts_amalgamated.csv"))
  return(.data)
}
