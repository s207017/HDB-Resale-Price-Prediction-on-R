library(maptools)
library(rvest)
library(dplyr)
library(stringr)
library(janitor)
library(data.table)
library(plyr)
library(readr)
library(dplyr)
library(tidyverse)
library(reshape2)
library(sf)
library(httr)
library(xml2)
library(jsonlite)
library(tibble)
library(XML)
library(geosphere)
library(matrixStats)
library(data.table)

#Set working directory to your folder
#IMPORTANT
#ONLY MODIFY 'directory' variable to the directory where the 'Final' folder is located, 'directory_constant' variable should remain unchanged
directory <- "/Users/seunghwanchoi/Documents/y2s1/BC2406"
##########################################################
directory_constant <- "/Final/5. Data Cleaning for amenities data"
finaldirectory <- paste(directory, directory_constant, sep = "")
setwd(finaldirectory)

#Import csvs
hawker <- read.csv("CLEANED_COLLECTED_hawker_and_markets.csv")
hdb <- read.csv("CLEANED_COLLECTED_hdb_data.csv")
hospital <- read.csv("CLEANED_COLLECTED_hospital_data.csv")
lrt <- read.csv("CLEANED_COLLECTED_lrt_data.csv")
supmarket <- read.csv("CLEANED_COLLECTED_supermarket.csv")
mrt <- read.csv("CLEANED_COLLECTED_mrt_data.csv")
parks <- read.csv("CLEANED_COLLECTED_park.csv")
polyclinic <- read.csv("CLEANED_COLLECTED_polyclinic.csv")
school <- read.csv("CLEANED_COLLECTED_school.csv")
shopmall <- read.csv("CLEANED_COLLECTED_shoppingmall.csv")

###IMPORTANT DO NOT MODIFY THE FOLLOWING CODES
directory_constant <- "/Final/2. Data Cleaning"
finaldirectory <- paste(directory, directory_constant, sep = "")
setwd(finaldirectory)
hdb <- read.csv("CLEANED_COLLECTED_hdb_data.csv")


#Setting new working directory for export
###IMPORTANT: DO NOT MODIFY THE FOLLOWING CODES###
directory_constant <- "/Final/6. FindDistance"
finaldirectory <- paste(directory, directory_constant, sep = "")
setwd(finaldirectory)
################################################################

findDistance <- function(home, amenity){
  tempFrame <- data.table(matrix(nrow = nrow(amenity), ncol = 1))
  tempFrame[, V1 := as.numeric(V1)]
  tempFrameCount = 0
  output <- data.table(matrix(ncol = 3, nrow = nrow(home)))
  output[, `:=`(V1 = as.character(V1), V2 = as.numeric(V2), V3 = as.integer(V3))]
  output[, V3 := 0]
  for (i in 1:nrow(home)){ 
    output[i, V1 := home[i,1]] 
    homePoint <- c(home[i,3],home[i,2])
    for (j in 1:nrow(amenity)){
      amenityPoint <- c(amenity[j,3],amenity[j,2])
      point_mat <- matrix(c(homePoint, amenityPoint), ncol = 2, byrow = TRUE)
      tempNum = distHaversine(point_mat)/1000
      tempFrame[j, V1:=tempNum]
      if(tempNum <= 2){
        temp = output[i, V3]
        output[i, V3 := temp + 1]
      }
      tempFrameCount = tempFrameCount + 1
    }
    output[i, V2 := tempFrame[, min(V1)]]
    print(i)
  }
  .GlobalEnv$temp <- output
}

hdbdistinct <- hdb[!duplicated(hdb$address), ][c("ADDRESS", "LATITUDE", "LONGTITUDE")]

#hawker
findDistance(hdbdistinct, hawker)
hdbnew <- temp
colnames(hdbnew) <- c("ADDRESS", "Nearest_hawker", "Number_of_hawker_in_2km")
hdbfinal <- merge(hdb, hdbnew, by = "ADDRESS")

#hospital
findDistance(hdbdistinct, hospital)
hospnew <- temp
colnames(hospnew) <- c("ADDRESS", "Nearest_hospital", "Number_of_hospital_in_2km")
hdbfinal <- merge(hdbfinal, hospnew, by = "ADDRESS")

#mrt and lrt
#combining mrt and lrt
mrtlrt <- rbind(mrt[c("ADDRESS", "LATITUDE", "LONGTITUDE")], lrt[c("ADDRESS", "LATITUDE", "LONGTITUDE")])
findDistance(hdbdistinct, mrtlrt)
mrtlrtnew <- temp
colnames(mrtlrtnew) <-c("ADDRESS", "Nearest_station", "Number_of_stations_in_2km")
mrtlrtnew
hdbfinal <- merge(hdbfinal, mrtlrtnew, by = "ADDRESS")
hdbfinal

#supermarket
findDistance(hdbdistinct, supmarket)
supmarketnew <- temp
colnames(supmarketnew) <- c("ADDRESS", "Nearest_supmarket", "Number_of_supmarkets_in_2km")
hdbfinal <- merge(hdbfinal, supmarketnew, by = "ADDRESS")


#parks
parks <- parks[c("address", "LONGTITUDE", "LATITUDE")]
findDistance(hdbdistinct, parks)
parksnew <- temp
colnames(parksnew) <- c("ADDRESS", "Nearest_park", "Number_of_parks_in_2km")
hdbfinal <- merge(hdbfinal, parksnew, by = "ADDRESS")

#polyclinics
findDistance(hdbdistinct, polyclinic)
polynew <- temp
colnames(polynew) <- c("ADDRESS", "Nearest_polyclinic", "Number_of_polyclinic_in_2km")
hdbfinal <- merge(hdbfinal, polynew, by = "ADDRESS")

#school
findDistance(hdbdistinct, school)
schoolnew <- temp
colnames(schoolnew) <- c("ADDRESS", "Nearest_school", "Number_of_schools_in_2km")
hdbfinal <- merge(hdbfinal, schoolnew , by = "ADDRESS")


#shopping mall
findDistance(hdbdistinct, shopmall)
shopmallnew <- temp
colnames(shopmallnew) <- c("ADDRESS", "Nearest_mall", "Number_of_shopmalls_in_2km")
hdbfinal <- merge(hdbfinal, shopmallnew, by = "ADDRESS")
hdbfinal

write_csv(hdbfinal, "HDB_FINAL_DATA.csv")

