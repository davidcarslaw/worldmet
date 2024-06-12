#' Import Global Historical Climatology Network hourly (GHCNh) Metadata
#'
#' This function imports a table of metadata outlining the available sites in
#' the GHCNh.
#'
#' @param lat,lng Values representing a decimal latitude and longitude (or other
#'   Y/X coordinate if using a different `crs`). If provided,
#'   `import_ghcnh_stations()` will locate `n` stations near to this global
#'   coordinate.
#' @param crs The coordinate reference system (CRS) of `lat` and `lng`, passed
#'   to `sf::st_crs()`. By default this is [EPSG:4326](https://epsg.io/4326),
#'   the CRS associated with the commonly used latitude and longitude
#'   coordinates. Different coordinate systems can be specified using crs (e.g.,
#'   `crs = 27700` for the [British National Grid](https://epsg.io/27700)).
#' @param n The number of nearest sites to search based on `lat` and `lng`.
#'   Defaults to `10`.
#' @param station A string which which to search and filter station names. For
#'   example, `station = "heathrow"` would return all stations containing the
#'   word "heathrow". The strings can be partial and can be upper or lower case
#'   e.g. `site = "HEATHR"`.
#' @param country,state One or more two-letter 'country' or 'state' codes with
#'   which to filter the metadata. These can be obtained with
#'   [import_ghcng_codes()].
#' @param return The form in which to return the data. One of:
#' - `"map"` (the default), to return an interactive `leaflet` map.
#' - `"table"`, to return a [tibble][tibble::tibble-package]
#' - `"sf"`, to return an [sf spatial table][sf::st_sf()]
#'
#' @return Varies (see the `return` parameter).
#'
#' @author Jack Davison
#'
#' @export
import_ghcnh_stations <- function(lat = NULL,
                                  lng = NULL,
                                  n = 10,
                                  crs = 4326,
                                  station = NULL,
                                  country = NULL,
                                  state = NULL,
                                  return = c("map", "table", "sf")
) {
  # check for valid "return" strings
  return <- match.arg(return, c("map", "table", "sf"))
  
  # import metadata
  meta <-
    readr::read_csv(
      "https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/doc/ghcnh-station-list.csv",
      col_names = c(
        "id",
        "latitude",
        "longitude",
        "elevation",
        "state",
        "name",
        "gsn_flag",
        "hcn_crn_flag",
        "wmo_id"
      ),
      na = c("", "-999.9"),
      show_col_types = FALSE,
      progress = FALSE
    )
  
  2# filter by station, country, and state
  if (!is.null(station)) {
    meta <- meta[grepl(station, meta$name, ignore.case = TRUE),]
  }
  if (!is.null(country)) {
    meta <- meta[substr(meta$id, 1, 2) %in% country,]
  }
  if (!is.null(state)) {
    meta <- meta[meta$state %in% state,]
  }
  
  # convert to spatial dataframe
  meta_sf <-
    sf::st_as_sf(
      meta,
      coords = c("longitude", "latitude"),
      remove = FALSE,
      crs = 4326
    )
  
  # filter by lat/lng/n, if provided
  if (!is.null(lat) & !is.null(lng)) {
    # get target SF object
    target <-
      dplyr::tibble(latitude = lat, longitude = lng) %>%
      sf::st_as_sf(
        coords = c("longitude", "latitude"),
        crs = crs
      ) %>%
      sf::st_transform(crs = 4326)
    
    meta_sf$dist <- as.vector(sf::st_distance(target, meta_sf)) / 1000
    
    meta_sf <- 
      meta_sf %>%
      dplyr::arrange(dist) %>%
      head(n = n)
    
    meta <- 
      dplyr::filter(meta, .data$id %in% meta_sf$id)
  }
  
  # return data
  if (return == "map") {
    map <-
      leaflet::leaflet(meta_sf) %>%
      leaflet::addTiles(group = "Default") %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite") %>%
      leaflet::addMarkers(clusterOptions = leaflet::markerClusterOptions(),
                          popup = paste0("<b>", meta_sf$name, "</b><br>",
                                         meta_sf$id, "<hr>",
                                         "<b>Country:</b> ", substr(meta_sf$id, 1, 2), "<br>",
                                         "<b>State:</b> ", meta_sf$state, "<br>",
                                         "<b>Elevation:</b> ", meta_sf$elevation, "m <br>")) %>%
      leaflet::addLayersControl(
        baseGroups = c("Default", "Satellite"),
        options = leaflet::layersControlOptions(FALSE, TRUE)
      )
    
    return(map)
  } else if (return == "table") {
    return(meta)
  } else if (return == "sf") {
    return(meta_sf)
  }
}

#' Import Country and State Codes used in the Global Historical Climatology
#' Network hourly (GHCNh) Database
#'
#' @param table One of `"countries"` or `"states"`, specifying whether country
#'   or state codes wish to be returned by the function.
#'   
#' @return a [tibble][tibble::tibble-package]
#'
#' @author Jack Davison
#'
#' @export
import_ghcnh_codes <- function(table = c("countries", "states")) {
  table <- match.arg(table, c("countries", "states"))
  
  if (table == "countries") {
    thenames <- c("country_code", "country")
  } else {
    thenames <- c("state_code", "state")
  }
  
  dplyr::tibble(dummy = readr::read_lines(
    paste0(
      "https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/doc/ghcn-",
      table,
      ".txt"
    )
  )) %>%
    tidyr::separate_wider_delim(
      dummy,
      delim = " ",
      too_many = "merge",
      names = thenames
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), trimws))
}
