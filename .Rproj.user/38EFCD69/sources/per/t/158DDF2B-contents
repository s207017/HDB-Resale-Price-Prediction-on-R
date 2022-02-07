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
directory_constant <- "/Final/1. Initial Data Collection"
finaldirectory <- paste(directory, directory_constant, sep = "")
setwd(finaldirectory)


#Load Flat Data
`15-16.df` <- read.csv("resale-flat-prices-based-on-registration-date-from-jan-2015-to-dec-2016.csv")

`17-20.df` <- read.csv("resale-flat-prices-based-on-registration-date-from-jan-2017-onwards.csv")

`12-14.df` <- read.csv("resale-flat-prices-based-on-registration-date-from-mar-2012-to-dec-2014.csv")

#Combining the three csvs
totallist<-list(`12-14.df`,`15-16.df`,`17-20.df`)
total.df<-rbindlist(totallist,fill = TRUE)

total.df

# Separate month column into respective year and month
#names(total.df)[names(total.df) == 'month'] <- 'yearmonth'
#total.df <- cbind(total.df, colsplit(total.df$yearmonth, "-", c("year", "month")))
#total.df <- cbind(total.df, colsplit(total.df$remaining_lease, " ", c("year_lease","yearna", "month_lease","monthna")))

# Convert remaining lease to years  
#total.df$remaining_lease <- ifelse( is.na(total.df$year_lease + (total.df$month_lease / 12)), 
                    #                total.df$year_lease,
                     #             total.df$year_lease + (total.df$month_lease / 12)  )
#total.df <-  subset(total.df, select = -c(yearmonth,year_lease,yearna,month_lease,monthna) )

#Add remaining_lease for the NAs
#total.df[is.na(remaining_lease)] <- total.df %>% filter(is.na(remaining_lease)) %>%
 # mutate(remaining_lease = lease_commence_date + 100 - (year + (month/12)))

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


#Find Flat coordinates
# HDB coordinates (done) ====
total.df$address <- paste(total.df$block," ",total.df$street_name)
all_addresses <- factor(total.df$address)
str(total.df)
str(all_addresses)
add <- levels(all_addresses)
str(add)
append_coordinates (add)
hdb_data <- merge(total.df,temp,by="address")
write.csv(hdb_data, "COLLECTED_hdb_data.csv", row.names = FALSE )

str(hdb_data)



