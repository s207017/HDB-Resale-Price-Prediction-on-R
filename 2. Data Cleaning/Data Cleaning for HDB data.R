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
directory_constant <- "/Final/1. Initial Data Collection"
finaldirectory <- paste(directory, directory_constant, sep = "")
setwd(finaldirectory)

##HDB DATA CLEANING
hdb_data <- fread("COLLECTED_hdb_data.csv")
str(hdb_data)
# Convert selected columns to factor variable
cols <- c("town", "flat_type", "storey_range","flat_model")
hdb_data[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
str(hdb_data)

# Recategorize flat model
oldfm <- c("Type S1", "Type S2","Premium Apartment Loft","Model A-Maisonette",'Improved-Maisonette','Premium Maisonette')
newfm <- c("Type S1S2", "Type S1S2","Premium Apartment","Maisonette",'Executive Maisonette','Executive Maisonette')
hdb_data$flat_model <- mapvalues(hdb_data$flat_model,oldfm,newfm)
flatmodel <- data.frame(unique(hdb_data$flat_model))
str(hdb_data)


# Separate month column into respective year and month
names(hdb_data)[names(hdb_data) == 'month'] <- 'yearmonth'
hdb_data <- cbind(hdb_data, colsplit(hdb_data$yearmonth, "-", c("year", "month")))
hdb_data <- cbind(hdb_data, colsplit(hdb_data$remaining_lease, " ", c("year_lease","yearna", "month_lease","monthna")))

# Convert remaining lease to years  
hdb_data$remaining_lease <- ifelse( is.na(hdb_data$year_lease + (hdb_data$month_lease / 12)), 
                                    hdb_data$year_lease,
                                    hdb_data$year_lease + (hdb_data$month_lease / 12)  )
hdb_data <-  subset(hdb_data, select = -c(yearmonth,year_lease,yearna,month_lease,monthna) )

#Add remaining_lease for the NAs
hdb_data[is.na(remaining_lease)] <- hdb_data %>% filter(is.na(remaining_lease)) %>%
  mutate(remaining_lease = lease_commence_date + 100 - (year + (month/12)))
str(hdb_data)

#Get distinct flat model
hdb_data %>% distinct(flat_model)

#Get distinct flat_type
hdb_data %>% distinct(flat_type)


#Get distinct storey_range
hdb_data %>% distinct(storey_range)

#Changing working directory for export
###IMPORTANT: DO NOT MODIFY THE FOLLOWING CODES###
directory_constant <- "/Final/2. Data Cleaning"
finaldirectory <- paste(directory, directory_constant, sep = "")
setwd(finaldirectory)

#Export csv file
write.csv(hdb_data,"CLEANED_COLLECTED_hdb_data.csv", row.names = FALSE)






