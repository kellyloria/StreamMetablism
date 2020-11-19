#' Downloads GLDAS surface air pressure data
#' @description This function downloads GLDAS surface air pressure data
#' (Pa) for a given Latitude and Longitude.
#'
#' @param save_dir The save directory for files to be placed in. For example, "C:/myfolder
#' @param Site_ID The site ID, for example "FL_ICHE2700"
#' @param Lat The site Latitude
#' @param Lon The site Longitude
#' @param startDate The starting date for the download (YYYY-MM-DD)
#' @param endDate The end date for the download (YYYY-MM-DD)
#'
#' @return Returns a time series of GLDAS 3-hourly surface air pressure (Pa)
#' date to the most recent available data
#' @export

#===============================================================================
#Function for downloading GLDAS surface air pressure
#https://disc.gsfc.nasa.gov/information/tools?title=Hydrology%20Data%20Rods
#Created 6/24/2020
#===============================================================================
GLDAS_press_DL <- function(save_dir, Site_ID, Lat, Lon, startDate, endDate){
  #The initial string to build the URL
    http_string <- paste("https://hydro1.gesdisc.eosdis.nasa.gov/daac-bin/access/timeseries.cgi?variable=GLDAS2:GLDAS_NOAH025_3H_v2.1:Psurf_f_inst")

  #Separating the date information
    start_split <- strsplit(startDate, "-")[[1]]
    end_split <- strsplit(endDate, "-")[[1]]

  #Build individual components of the url
    location_string <- paste0("&location=GEOM:POINT(", (Lon + 0.125), ",%20", (Lat + 0.125), ")")
    start_string <- paste0("&startDate=", start_split[1], "-", start_split[2], "-", start_split[3], "T00")
    end_string <- paste0("&endDate=", end_split[1], "-", end_split[2], "-", end_split[3], "T23")
    
  #Generating the URL
    url <- paste0(http_string, start_string, end_string, location_string, "&type=asc2")    
    
  #Downloading the data
    destfile <- paste(save_dir, "/", Site_ID, "_NLDAS.asc", sep = "")

  #Error catch in case the page is inaccessible. A little inelegant at present...
    try_result <- try(download.file(url, destfile, method = "curl"), silent = FALSE)

    if(class(try_result) == "try-error") {file.remove(destfile)}

} #End GLDAS_press_DL function


