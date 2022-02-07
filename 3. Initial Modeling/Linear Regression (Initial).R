library(caTools)


#Set working directory to your folder
#IMPORTANT
#ONLY MODIFY 'directory' variable to the directory where the 'Final' folder is located, 'directory_constant' variable should remain unchanged
directory <- "/Users/seunghwanchoi/Documents/y2s1/BC2406"
##########################################################
directory_constant <- "/Final/2. Data Cleaning"
finaldirectory <- paste(directory, directory_constant, sep = "")
setwd(finaldirectory)


# Linear Regression =====
hdb_data.df <- read.csv('CLEANED_COLLECTED_hdb_data.csv')

set.seed(2004)

# Train Test Split
train <- sample.split(Y = hdb_data.df$resale_price, SplitRatio = 0.7)
trainset <- subset(hdb_data.df, train == T)
testset <- subset(hdb_data.df, train == F)
str(trainset)

# Modelling
trial_model <- lm( resale_price ~ ., data = trainset)
summary(test)

# RSME
price_pred_trial <- predict(trial_model, testset)
actual_pred_trial <- data.frame(cbind(actuals=testset$resale_price, predicted = price_pred_trial))
sqrt( sum( (actual_pred_trial$actuals - actual_pred_trial$predicted)^2 , na.rm = TRUE ) / nrow(actual_pred_trial) )

# Mean Absolute Percentage Error(MAPE)
mean(abs((actual_pred_trial$actuals-actual_pred_trial$predicted)/actual_pred_trial$actuals)) * 100

