library(data.table)
library(plyr)
library(readr)
library(dplyr)
library(tidyverse)
library(reshape2)

#Set working directory to your folder
#IMPORTANT
#ONLY MODIFY 'directory' variable to the directory where the 'Final' folder is located, 'directory_constant' variable should remain unchanged
directory <- "/Users/seunghwanchoi/Documents/y2s1/BC2406"
##########################################################
directory_constant <- "/Final/4. Additional Data Collection"
finaldirectory <- paste(directory, directory_constant, sep = "")
setwd(finaldirectory)

##Import csv
sch_xy <- fread("COLLECTED_school_data.csv")
hawker_xy <- fread("COLLECTED_hawker_and_markets.csv")
shop_xy<-fread("COLLECTED_shoppingmall.csv")
mkt_xy<-fread("COLLECTED_supermarket.csv")
mrt_xy<-fread("COLLECTED_mrt_data.csv")
lrt_xy<-fread("COLLECTED_lrt_data.csv")
hosp_xy<-fread("COLLECTED_hospital_data.csv")
polyclinic_xy<-fread("COLLECTED_polyclinic_data.csv")
parks_xy<-fread("COLLECTED_park.csv")


#Reset working directory for csv exports
###IMPORTANT: DO NOT MODIFY THE FOLLOWING CODES###
directory_constant <- "/Final/5. Data Cleaning for amenities data"
finaldirectory <- paste(directory, directory_constant, sep = "")
setwd(finaldirectory)



##AMENITIES CLEANING
#combine coordinates of all relevant amenities
###############################################################################3
#School
sch_xy$amenitiestype <- "school"
write.csv(sch_xy, "CLEANED_COLLECTED_school.csv", row.names = FALSE)

#Hawker and markets
hawker_xy$amenitiestype <- "hawker"
write.csv(hawker_xy, "CLEANED_COLLECTED_hawker_and_markets.csv", row.names = FALSE)

#Shopping malls
shop_xy$amenitiestype <- "shopping mall"
write.csv(shop_xy, "CLEANED_COLLECTED_shoppingmall.csv", row.names = FALSE)

#Supermarkets
names(mkt_xy)[names(mkt_xy) == 'SEARCHVAL'] <- 'address'
write.csv(mkt_xy, "CLEANED_COLLECTED_supermarket.csv", row.names = FALSE)

#MRT
mrt_xy$amenitiestype <- "mrt"
names(mrt_xy)[names(mrt_xy) == 'STN_NAME'] <- 'address'
names(mrt_xy)[names(mrt_xy) == 'Latitude'] <- 'LATITUDE'
names(mrt_xy)[names(mrt_xy) == 'Longitude'] <- 'LONGITUDE'
write.csv(mrt_xy, "CLEANED_COLLECTED_mrt_data.csv", row.names = FALSE)

#LRT
lrt_xy$amenitiestype <- "lrt"
names(lrt_xy)[names(lrt_xy) == 'STN_NAME'] <- 'address'
names(lrt_xy)[names(lrt_xy) == 'Latitude'] <- 'LATITUDE'
names(lrt_xy)[names(lrt_xy) == 'Longitude'] <- 'LONGITUDE'
write.csv(lrt_xy, "CLEANED_COLLECTED_lrt_data.csv", row.names = FALSE)

#Hospitals
hosp_xy$amenitiestype <- "hospital"
names(hosp_xy)[names(hosp_xy) == 'SEARCHVAL'] <- 'address'
write.csv(hosp_xy, "CLEANED_COLLECTED_hospital_data.csv", row.names = FALSE)

#Polyclinincs
polyclinic_xy$amenitiestype <- "polyclinic"
names(polyclinic_xy)[names(polyclinic_xy) == 'SEARCHVAL'] <- 'address'
write.csv(polyclinic_xy, "CLEANED_COLLECTED_polyclinic.csv", row.names = FALSE)

#Parks
parks_xy$amenitiestype <- "parks"
names(parks_xy)[names(parks_xy) == 'number'] <- 'address'
names(parks_xy)[names(parks_xy) == 'lon'] <- 'LONGTITUDE'
names(parks_xy)[names(parks_xy) == 'lat'] <- 'LATITUDE'
write.csv(parks_xy, "CLEANED_COLLECTED_park.csv", row.names = FALSE)