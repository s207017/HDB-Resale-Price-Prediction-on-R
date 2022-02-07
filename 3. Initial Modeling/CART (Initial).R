#Load relevant packages
library(data.table)
library(plyr)
library(readr)
library(dplyr)
library(tidyverse)
library(reshape2)
library(caret)
library(car)
library(caTools)
library(rpart)
library(rpart.plot)
library(MLmetrics)
library(ggplot2)
library(vip)
setwd("~/NTU Year 4/01 School Work/BC2406 Analytics 1 Visual & Predictive Techniques/AY21 Team Project/Kaggle Singapore HDB Flat Resale Prices/21_10_3PM")
hdb_data <- fread("xx.csv")
#summary(hdb_data)
cols <- c("town", "flat_type", "storey_range","flat_model")
hdb_data[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
#str(hdb_data)

#================================================================== 
#CART
# Continuous Y: Set method = 'anova'

rfpreg3 <- hdb_data
#Cleaning
rfpreg3 <- distinct(rfpreg3[!grepl("199|200|2010|2011", rfpreg3$year),])
rfpreg3 <- subset(rfpreg3, select = -c(address, ADDRESS, block, street_name))

set.seed(2004)
# 70% trainset. 
train <- sample.split(Y = rfpreg3$resale_price, SplitRatio = 0.7)
hdb_trainset <- subset(rfpreg3, train == T)
hdb_testset <- subset(rfpreg3, train == F)

cart1 <- rpart(resale_price ~ town + flat_type + storey_range + floor_area_sqm + flat_model + remaining_lease +
                  Nearest_hawker + Number_of_hawker_in_2km + Nearest_hospital + Number_of_hospital_in_2km + Nearest_station +
                  Number_of_stations_in_2km + Nearest_supmarket + Number_of_supmarkets_in_2km + Nearest_park +
                  Number_of_parks_in_2km + Nearest_polyclinic + Number_of_polyclinic_in_2km + Nearest_school +
                  Number_of_schools_in_2km + Nearest_mall + Number_of_shopmalls_in_2km ,
                data = hdb_trainset, method = 'anova', control = rpart.control(minsplit = 100, cp = 0))

printcp(cart1)
plotcp(cart1)
print(cart1)
summary(cart1)

# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)

# Prune the max tree using a particular CP value
cart2 <- prune(cart1, cp = cp.opt)
printcp(cart2, digits = 3)
print(cart2)
rpart.plot(cart2, nn = T, main = "Optimal Tree")
rpart.rules(cart2)
## The number inside each node represent the mean value of Y.
VI2 <- data.table(cart2$variable.importance)
cart2$variable.importance
vipplot2 <- vip(cart2, width = 0.5,mapping = aes_string(fill = "Variable"), aesthetics = list(color = "grey35", size = 0.8))
plot(vipplot2)
## floor_area_sqm has the highest importance followed by flat_type
summary(cart2)

# Make predictions on the test data
predictions2 <- cart2 %>% predict(hdb_testset)
actual_predict2 <- data.frame(hdb_testset$resale_price,predictions2)
actual_predict2 %>% rename(predicted_price = predictions2,actual_price = hdb_testset.resale_price)
p2 <- ggplot(data=actual_predict2,aes(x=hdb_testset.resale_price,y=predictions2)) + 
  labs(x='Actual Price',y='Predicted Price',title = 'Predicted vs Actual Resale Price') +
  geom_point(size = 0.01,aes(alpha = 0.1)) + geom_abline(slope = 1,color="red",size = 1)

# head(predictions)
# Compute the prediction error RMSE
RMSE2 <- RMSE(predictions2, hdb_testset$resale_price)
MAE2 <- MAE(predictions2, hdb_testset$resale_price)
MAPE2 <- MAPE(predictions2, hdb_testset$resale_price)
RMSE2
MAE2
MAPE2
cp1=1e-03
cart3 <- prune(cart1, cp = cp1)
printcp(cart3, digits = 3)
print(cart3)
#rpart.plot(cart3, nn = T, main = "Pruned Tree, CP = 1e-03",cex=0.15)
rpart.rules(cart3)
## The number inside each node represent the mean value of Y.
VI3 <- data.table(cart3$variable.importance)
vipplot3 <- vip(cart3, width = 0.5,mapping = aes_string(fill = "Variable"), aesthetics = list(color = "grey35", size = 0.8))
plot(vipplot3)
## floor_area_sqm has the highest importance followed by flat_type
summary(cart3)

split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width = 15), collapse = "\n")
  }
  labs
}

prp(cart3,main = "Pruned Tree, CP = 1e-03", split.fun = split.fun)

# Make predictions on the test data
predictions3 <- cart3 %>% predict(hdb_testset)
actual_predict3 <- data.frame(hdb_testset$resale_price,predictions3)
actual_predict3 %>% rename(predicted_price = predictions3,actual_price = hdb_testset.resale_price)
p3 <- ggplot(data=actual_predict3,aes(x=hdb_testset.resale_price,y=predictions3)) + 
  labs(x='Actual Price',y='Predicted Price',title = 'Predicted vs Actual Resale Price') +
  geom_point(size = 0.01,aes(alpha = 0.1)) + geom_abline(slope = 1,color="red",size = 1)
# head(predictions)
# Compute the prediction error RMSE
RMSE3 <- RMSE(predictions3, hdb_testset$resale_price)
MAE3 <- MAE(predictions3, hdb_testset$resale_price)
MAPE3 <- MAPE(predictions3, hdb_testset$resale_price)

cp2=5e-03
cart4 <- prune(cart1, cp = cp2)
printcp(cart4, digits = 3)
print(cart4)
#rpart.plot(cart3, nn = T, main = "Pruned Tree, CP = 1e-03",cex=0.15)
rpart.rules(cart4)
## The number inside each node represent the mean value of Y.
VI4 <- data.table(cart4$variable.importance)
vipplot4 <- vip(cart4, width = 0.5,mapping = aes_string(fill = "Variable"), aesthetics = list(color = "grey35", size = 0.8))
plot(vipplot4)
## floor_area_sqm has the highest importance followed by flat_type
summary(cart4)

#prp(cart2, split.fun = split.fun)
prp(cart4,main = "Pruned Tree, CP = 5e-03", split.fun = split.fun)

# Make predictions on the test data
predictions4 <- cart4 %>% predict(hdb_testset)
actual_predict4 <- data.frame(hdb_testset$resale_price,predictions4)
actual_predict4 %>% rename(predicted_price = predictions4,actual_price = hdb_testset.resale_price)
p4 <- ggplot(data=actual_predict4,aes(x=hdb_testset.resale_price,y=predictions3)) + 
  labs(x='Actual Price',y='Predicted Price',title = 'Predicted vs Actual Resale Price') +
  geom_point(size = 0.01,aes(alpha = 0.1)) + geom_abline(slope = 1,color="red",size = 1)
# head(predictions)
# Compute the prediction error RMSE
RMSE4 <- RMSE(predictions4, hdb_testset$resale_price)
MAE4 <- MAE(predictions4, hdb_testset$resale_price)
MAPE4 <- MAPE(predictions4, hdb_testset$resale_price)

grid.arrange(vipplot2,vipplot3,vipplot4, nrow=1)
grid.arrange(p2,p3,p4, ncol=1)
