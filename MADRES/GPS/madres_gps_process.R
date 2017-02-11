setwd("D:/Temp")

test <- read.csv("X:/MADRES/Project 2/Data/GPS/processed/MadresGpsTracking_MAD3100_1478202324407_decoded.csv", 
                 header = FALSE, stringsAsFactors = FALSE)

ProcessGPS <- function(gpsX) {
  output.name <- c("subjectID", "string_time", "date", "time", "timestamp", 
                   "source", "latitude", "longitude", "accuracy", "altitude", "velocity", 
                   "sat_inUse", "sat_inView", "wifi_on", "wifi_connected", "network_on", "network_connected")
  output <- data.frame(matrix(nrow = nrow(gpsX), ncol = length(output.name), dimnames = list(c(), output.name)))
  output$subjectID <- gpsX$V1
  output$string_time <- gpsX$V2
  output$date <- sapply(strsplit(gpsX$V2, " "), "[[", 1)
  output$time <- sapply(strsplit(gpsX$V2, " "), "[[", 2)
  output$timestamp <- gpsX$V4

  Extract <- function(lineX) {
    segX <- strsplit(lineX, " ")[[1]]
    if (grepl("gps", segX[1])) {
      return(c(gsub("Location\\[", "", segX[1]), strsplit(segX[2], ",")[[1]][1], strsplit(segX[2], ",")[[1]][2], 
                   gsub("acc=", "", segX[3]), gsub("alt=", "", segX[5]), gsub("vel=", "", segX[6])))
    } else {
      return(c(gsub("Location\\[", "", segX[1]), strsplit(segX[2], ",")[[1]][1], strsplit(segX[2], ",")[[1]][2],
                   gsub("acc=", "", segX[3]), "", ""))
    }
  }

  locX = lapply(gpsX$V3, Extract)
  output$source <- sapply(locX, "[[", 1)
  output$latitude <- sapply(locX, "[[", 2)
  output$longitude <- sapply(locX, "[[", 3)
  output$accuracy <- sapply(locX, "[[", 4)
  output$altitude <- sapply(locX, "[[", 5)
  output$velocity <- sapply(locX, "[[", 6)

  output$sat_inUse <- gpsX$V5
  output$sat_inUse[grepl("WiFi", output$sat_inUse)] <- ""
  output$sat_inView <- gpsX$V6
  output$sat_inView[grepl("Network", output$sat_inView)] <- ""
  
  wifi <- gsub("WiFi", "", gpsX$V5)
  wifi <- ifelse(grepl("[0-9]+", wifi), "", wifi)
  output$wifi_on <- ifelse(nchar(wifi) > 0, ifelse(strsplit(wifi, "")[[1]][1] == "+", 1, 0), "")
  output$wifi_connected <- ifelse(nchar(wifi) > 0, ifelse(strsplit(wifi, "")[[1]][2] == "+", 1, 0), "")

  network <- gsub("Network", "", gpsX$V6)
  network <- ifelse(grepl("[0-9]+", network), "", network)
  output$network_on <- ifelse(nchar(network) > 0, ifelse(strsplit(network, "")[[1]][1] == "+", 1, 0), "")
  output$network_connected <- ifelse(nchar(network) > 0, ifelse(strsplit(network, "")[[1]][2] == "+", 1, 0), "") 

  return(output)
}