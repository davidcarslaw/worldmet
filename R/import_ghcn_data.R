
#' Import Global Historical Climatology Network hourly (GHCNh) Data
#'
#' This function imports hourly data from the GHCNh.
#'
#' @param code The specific station code(s) of interest, identified using
#'   [import_ghcn_stations()].
#' @param year The specific year(s) to import.
#' @param all GHCNh data comes bundled with various extra columns, such as
#'   measurement codes, quality codes, report types, and source codes. By
#'   default, these are removed. Setting `all = TRUE` will retain these in the
#'   returned table.
#'
#' @author Jack Davison
#'
#' @return a [tibble][tibble::tibble-package]
import_ghcn_hourly <- function(code, year, all = FALSE) {
  psv_addr <-
    "https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/access/by-year/"
  
  urls <-
    purrr::pmap(
      expand.grid(code = code, year = year),
      ~ paste0(psv_addr, ..2, "/psv/GHCNh_", ..1, "_", ..2, ".psv")
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
    tidyr::unite(date, year, month, day, hour, minute, sep = " ") %>%
    dplyr::mutate(date = lubridate::ymd_hm(date))
  
  if (!all) {
    id <-
      !grepl(
        "_measurement_code|_quality_code|_report_type|_source_code|_source_station_id",
        names(out)
      )
    out <- out[id]
  }
  
  return(out)
}
