#' Import Global Historical Climatology Network hourly (GHCNh) Data
#'
#' This function imports hourly data from the GHCNh.
#'
#' @param code The specific station code(s) of interest, identified using
#'   [import_ghcn_stations()].
#' @param year The specific year(s) to import.
#' @param hourly Should hourly means be calculated? The default is `TRUE`. If
#'   `FALSE` then the raw data are returned.
#' @param all GHCN data comes bundled with various extra columns, such as
#'   measurement codes, quality codes, report types, source codes, attributes,
#'   and so on. By default, these are removed. Setting `all = TRUE` will retain
#'   these in the returned table.
#'
#' @author Jack Davison
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @export
import_ghcn_hourly <-
  function(code,
           year,
           hourly = TRUE,
           all = FALSE) {
    urls <-
      purrr::pmap(
        expand.grid(code = code, year = year),
        ~ paste0(
          "https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/access/by-year/",
          ..2,
          "/psv/GHCNh_",
          ..1,
          "_",
          ..2,
          ".psv"
        )
      ) %>%
      purrr::list_c()

    raw <-
      purrr::map(urls, purrr::possibly(
        ~ readr::read_delim(
          .x,
          delim = "|",
          progress = FALSE,
          show_col_types = FALSE,
          name_repair = tolower
        )
      ), .progress = TRUE) %>%
      purrr::list_rbind()

    if (nrow(raw) == 0L) {
      message("No data returned.")
      return(NULL)
    }

    out <-
      raw %>%
      tidyr::unite(date,
        .data$year,
        .data$month,
        .data$day,
        .data$hour,
        .data$minute,
        sep = " "
      ) %>%
      dplyr::mutate(date = lubridate::ymd_hm(.data$date))

    if (!all) {
      id <-
        !grepl(
          "_measurement_code|_quality_code|_report_type|_source_code|_source_station_id",
          names(out)
        )
      out <- out[id]
    }

    if (hourly) {
      out <-
        openair::timeAverage(out,
          avg.time = "hour",
          type = c("station_id", "station_name")
        )
    }

    return(out)
  }

#' Import Global Historical Climatology Network daily (GHCNd) Data
#'
#' This function imports daily data from the GHCNd.
#'
#' @inheritParams import_ghcn_hourly
#'
#' @author Jack Davison
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @export
import_ghcn_daily <- function(code, year, all = FALSE) {
  # import data
  out <-
    purrr::map_vec(
      .x = code,
      .f = ~ paste0(
        "https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/",
        .x,
        ".csv"
      )
    ) %>%
    purrr::map(purrr::possibly(
      ~ readr::read_csv(
        .x,
        show_col_types = FALSE,
        progress = TRUE,
        name_repair = tolower
      )
    )) %>%
    purrr::list_rbind()

  if (nrow(out) == 0L) {
    message("No data returned.")
    return(NULL)
  }

  # select specified years
  id <- as.numeric(format(out$date, "%Y")) %in% year
  out <- out[id, ]

  if (nrow(out) == 0L) {
    message("No data returned.")
    return(NULL)
  }

  # include all?
  if (!all) {
    id <-
      !grepl(
        "_attributes",
        names(out)
      )
    out <- out[id]
  }

  # return
  return(out)
}
