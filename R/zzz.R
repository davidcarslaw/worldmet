.onLoad <- function(...) {
  
  ##  packageStartupMessage("\ttype citation(\"openair\") for how to cite openair")
  utils::globalVariables(c("AA1", "AW1", "CIG", "DATE", "DEW", "ELEVATION", "GA1", "GA2", "GA3",
                           "LATITUDE", "LONGITUDE", "NAME",
                           "SLP", "STATION", "TMP", "VIS", "WND", "air_temp", "atmos_pres",
                           "ceil_hgt", "cl_1", "cl_1_height",
                           "cl_2", "cl_2_height", "cl_3", "cl_3_height", "description",
                           "dew_point", "latitude",
                           "longitude", "precip", "visibility", "wd", "weatherCodes", "ws"))
}


