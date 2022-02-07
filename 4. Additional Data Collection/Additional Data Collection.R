#Data Collection
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

#set working directory to your folder
#IMPORTANT
#ONLY MODIFY 'directory' variable to the directory where the 'Final' folder is located, 'directory_constant' variable
#should remain unchanged
directory <- "/Users/seunghwanchoi/Documents/y2s1/BC2406"
##########################################################
directory_constant <- "/Final/4. Additional Data Collection"
finaldirectory <- paste(directory, directory_constant, sep = "")
setwd(finaldirectory)


#API trial
url <- "https://developers.onemap.sg/commonapi/search?"
res <- GET(url, query = list(returnGeom = 'Y', getAddrDetails = 'Y', pageNum = 1, searchVal="540 HOUGANG AVE 8"))
res <- content(res)
trial <- res[[4]][[1]][c("ADDRESS","LATITUDE","LONGTITUDE")]
rm(trial)


# Append coordinates function ====
append_coordinates <- function(input) {
  output <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(output) <- c('ADDRESS' ,'LATITUDE' ,'LONGTITUDE' , 'address')
  counter <- 0
  url <- "https://developers.onemap.sg/commonapi/search?"
  for (i in input){
    counter <- counter + 1
    print (paste(counter , i))
    res <- GET(url, query = list(returnGeom = 'Y', getAddrDetails = 'Y', pageNum = 1, searchVal= i))
    res <- content(res)
    if (res[[1]] >= 1){
      x <- as.data.frame(c(res[[4]][[1]][c("ADDRESS","LATITUDE","LONGTITUDE")],list(address=i)))
      output <- rbind(output,x)}
  }
  .GlobalEnv$temp <- output
}



# Schools(done) ====
# data downloaded from "https://data.gov.sg/dataset/school-directory-and-information"
school.df <- read.csv('general-information-of-schools.csv')
school_name <- school.df$school_name
append_coordinates(school_name)
school_data <- temp
school_data
write.csv(school_data, "COLLECTED_school_data.csv", row.names = FALSE)


# Polyclinics(done) ====
polyclinic_data <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(polyclinic_data) <- c('ADDRESS' ,'LATITUDE' ,'LONGTITUDE' , 'address')

# NUH Polyclinics (done)
url <- 'https://www.nup.com.sg/Pages/Our%20Clinics/nup.aspx'
webpage <- read_html(url)
healthcare_html <- html_nodes(webpage,'.leftsidenav a')
healthcare_data <- html_text(healthcare_html)
append_coordinates(healthcare_data[-1])
polyclinic_data <- rbind(polyclinic_data, temp)

# Singhealth Polyclinics (done)
url <- 'https://polyclinic.singhealth.com.sg/patient-care/our-polyclinics'
webpage <- read_html(url)
healthcare_html <- html_nodes(webpage,'p:nth-child(3)')
healthcare_data <- html_text(healthcare_html)
healthcare_data <- strsplit(healthcare_data, 'SHP-')
healthcare_data <- (healthcare_data[[1]][-1])
for (i in (1:length(healthcare_data))){
  healthcare_data[i] <- paste(healthcare_data[i]," polyclinic")}
healthcare_data
append_coordinates(healthcare_data)
polyclinic_data <- rbind(polyclinic_data, temp)

# National Healthcare Polyclinics (done)
url <- 'https://www.nhgp.com.sg/Find_A_Polyclinic_Near_You/'
webpage <- read_html(url)
healthcare_html <- html_nodes(webpage,'#ctl00_ctl00_bodyContent_bodyContent_ourClinics td :nth-child(1) :nth-child(1)')
healthcare_data <- html_text(healthcare_html)
healthcare_data <- str_trim(healthcare_data)
for (i in (1:length(healthcare_data))){
  healthcare_data[i] <- paste(healthcare_data[i],"polyclinic")}
append_coordinates(healthcare_data)
polyclinic_data <- rbind(polyclinic_data, temp)
View(polyclinic_data)
write.csv(polyclinic_data, "COLLECTED_polyclinic_data.csv", row.names = FALSE)


# Hospital(done) ====

hospital_data <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(hospital_data) <- c('ADDRESS' ,'LATITUDE' ,'LONGTITUDE' , 'address')

# NUH Hospitals(done)
url <- 'https://en.wikipedia.org/wiki/National_University_Health_System'
webpage <- read_html(url)
healthcare_html <- html_nodes(webpage,'.navigation-not-searchable:nth-child(24) a , .navigation-not-searchable:nth-child(20) a , style+ .navigation-not-searchable a')
healthcare_data <- html_text(healthcare_html)
healthcare_data
append_coordinates(healthcare_data)
hospital_data <- rbind(hospital_data, temp)

# Singhealth Hospitals (done)
url <- 'https://www.singhealth.com.sg/patient-care/specialties-services'
webpage <- read_html(url)
healthcare_html <- html_nodes(webpage,'#accordion .text')
healthcare_data <- html_text(healthcare_html)
healthcare_data
append_coordinates(healthcare_data)
hospital_data <- rbind(hospital_data, temp)

# National healthcare Hospitals(done)
url <- 'https://www.moe.gov.sg/sgis/sponsoring-organisations/industries/healthcare-administrators/national-healthcare-group'
webpage <- read_html(url)
healthcare_html <- html_nodes(webpage,'.moe-typeset li a')
healthcare_data <- html_text(healthcare_html)
healthcare_data <- grep('Polyclinics', healthcare_data, invert = TRUE, value = TRUE)
append_coordinates(healthcare_data)
hospital_data <- rbind(hospital_data, temp)
View(hospital_data)
write.csv(hospital_data, "COLLECTED_hospital_data.csv", row.names = FALSE)


# MRT(done) ====
url <- 'https://en.wikipedia.org/wiki/List_of_Singapore_MRT_stations'
webpage <- read_html(url)
mrt_html <- html_nodes(webpage,'td:nth-child(2) a')
mrt_data <- html_text(mrt_html)
for (i in (1:length(mrt_data))){
  mrt_data[i] <- paste(mrt_data[i], "MRT")
}
str(mrt_data)
append_coordinates(mrt_data)
temp <- distinct(temp)
mrt_data <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(mrt_data) <- c('ADDRESS' ,'LATITUDE' ,'LONGTITUDE' , 'address')
mrt_data <- rbind(mrt_data, temp)
View(mrt_data)
write.csv(mrt_data, "COLLECTED_mrt_data.csv", row.names = FALSE)

# LRT(done) ====
url <- 'https://en.wikipedia.org/wiki/List_of_Singapore_LRT_stations'
webpage <- read_html(url)
lrt_html <- html_nodes(webpage,'.wikitable td:nth-child(2) a , td:nth-child(3) a')
lrt_data <- html_text(lrt_html)
lrt_data <- lrt_data[c(-44,-45)]
for (i in (1:length(lrt_data))){
  lrt_data[i] <- paste(lrt_data[i], "LRT")
}
append_coordinates(lrt_data)
temp <- distinct(temp)
lrt_data <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(lrt_data) <- c('ADDRESS' ,'LATITUDE' ,'LONGTITUDE' , 'address')
lrt_data <- rbind(lrt_data, temp)
View(lrt_data)
write.csv(lrt_data, "COLLECTED_lrt_data.csv", row.names = FALSE)


#SuperMarket
result <- stream_in(file("https://data.gov.sg/api/action/datastore_search?resource_id=3561a136-4ee4-4029-a5cd-ddf591cce643"))
result <- flatten(result)
result_table <- as_tibble(result)
totalCount <- result_table$result.total
iterNo <- ifelse(totalCount%%100 == 0, totalCount/100, ceiling(totalCount/100)) 
counter = 1
postlist <- data.frame(matrix(nrow = 0, ncol = 1))
for (i in 1:iterNo){
  postalCode <- as.data.frame(result_table$result.records[[1]][["postal_code"]])
  for (j in 1:nrow(postalCode)){
    postlist[counter,1] = postalCode[j,1]
    counter <- counter+1
  }
  nexturl <- paste("https://data.gov.sg", result_table$result._links.next, sep = "")
  result_table <- as_tibble(flatten(stream_in(file(nexturl))))
}
superlist <- dplyr::pull(postlist, 1)
superlist
append_coordinates(superlist)
superData <- temp
write.csv(superData, "COLLECTED_supermarket.csv", row.names = FALSE)



#Hawkers
#data from https://data.gov.sg/dataset/list-of-government-markets-hawker-centres
hawkerData <- read.csv("list-of-government-markets-hawker-centres.csv")
hawkerData <- hawkerData$name_of_centre
hawkerData
append_coordinates(hawkerData)
hawkerlist <- temp
hawkerlist
write.csv(hawkerlist, "COLLECTED_hawker_and_markets.csv", row.names = FALSE)

#Shopping Malls
#shoppingmalls
url <- "https://en.wikipedia.org/wiki/List_of_shopping_malls_in_Singapore"
read <- rvest::read_html('https://en.wikipedia.org/wiki/List_of_shopping_malls_in_Singapore') 
read1 <- html_nodes(read, "li") %>%
  html_text()
shoppData <- read1[9:179]
append_coordinates(shoppData)
shopData <- temp
write.csv(shopData, "COLLECTED_shoppingmall.csv", row.names = FALSE)

#Parks
#data from https://data.gov.sg/dataset/parks
coords <- getKMLcoordinates(kmlfile = "parks-kml.kml", ignoreAltitude = TRUE)
coords
parkcoords <- do.call(rbind.data.frame, coords)
colnames(parkcoords) <- c("lat","lon")
new <- cbind(number = c(1:nrow(parkcoords)), parkcoords)
write.csv(new, "COLLECTED_park.csv", row.names = FALSE)