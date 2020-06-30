## HotSpots Starter R Script
# This script should help you get started with the Hotspots data. In it we cover:
  
# Loading the data
# Simple EDA and an example of feature enginnering
# Suggestions for validation split
# Creating a simple model
# Making a submission
# Some tips for improving your score

# Install Packages
library(Metrics)
library(h2o)
library(dplyr)
library(ggplot2)
library(readr)

# Load the Data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

train = read.csv("hot_spots_drc_train.csv")
test  = read.csv("hot_spots_drc_test.csv")

head(train)
head(test)

#############################################
##EDA + Feature Engineering

# Look at correlation with target

cordb = cor(train[,!names(train) %in% c("X","date","ID")], train[,"burn_area"])
cordb <- as.data.frame((cordb))
names(cordb) <- "burn_area"
cordb$index = c(rownames(cordb))

cordb <- cordb[order(cordb$burn_area),]
barplot(cordb[,"burn_area"],names.arg=cordb[,"index"],col="blue", las=2)

# Look at some scatter plots 

ggplot(train, aes(x=climate_vap, y=burn_area))+
  geom_point(size=3,col="blue", alpha=I(0.3))+theme_classic()+theme(legend.position="top")

ggplot(train, aes(x=climate_tmmx, y=burn_area))+
  geom_point(size=3,col="blue", alpha=I(0.3))+theme_classic()+theme(legend.position="top")

#############################################
##Adding date features

# Date variables
train$date <- as.Date(train$date)
train$month <- as.numeric(format(train$date, "%m"))
train$year  <- as.numeric(format(train$date, "%Y"))

# Plotting mean burn_area for each month - very strong mid-year peak (dry season)
aggr_m <- aggregate(train[,c("burn_area")], by = list(month = train$month), FUN = mean)
aggr_m <- aggr_m[order(aggr_m$month),]
ggplot(data=aggr_m, aes(x=month, y=x, group=1)) +
    geom_bar(stat="identity", fill="blue")+theme_classic()

# Plot trend year-on-year
aggr_y <- aggregate(train[,c("burn_area")], by = list(year = train$year), FUN = mean)
aggr_y <- aggr_y[order(aggr_y$year),]
ggplot(data=aggr_y, aes(x=year, y=x, group=1)) +
    geom_line()+geom_point()+theme_classic()

# Let's plot precipitation and burn area on the same plot - note the inverse relationship, and the strong periodic component to both.
aggr_d <- aggregate(train[,c("burn_area","precipitation")], by = list(date = train$date), FUN = mean)
aggr_d <- aggr_d[order(aggr_d$date),]jk
ggplot(aggr_d, aes(date)) + 
  geom_line(aes(y = burn_area, colour = "burn_area")) + 
  geom_line(aes(y = precipitation, colour = "precipitation")) +theme_classic()+labs(y="")

## Adding more features - some ideas
# Read the list of climate variables and what they mean. See if you can combine them in interesting ways - perhaps a 'hot_and_dry' metric...
# Fire depends on some processes that take a long time - for example, there may be more fuel if the previous growing season was a good one. Consider some lagged variables to give the model some inputs for what came before the current month.
# Make some categorical features - 'dominant_land_type' or 'is_peak_rainfall'.

#############################################
####Data Split for Validation

# We don't want to just split randomly. Instead, let's use the last 3 years of the dataset for validation to more closely match the test configuration.
train_all <- train
trainmodel <- train_all[train_all$date <= '2011-01-01',]
validmodel <- train_all[train_all$date > '2011-01-01',]

dim(trainmodel)
dim(validmodel)

#############################################
######Simple Model
# Define input and output columns
in_cols <- names(train[,-c(1:7)])
target_col <- "burn_area"
in_cols

# Get our X and y training and validation sets ready
X_train <- trainmodel[,c(target_col,in_cols)]

X_valid <- validmodel[,in_cols]
Y_valid <- validmodel[,target_col]

# Create and fit the model
model <- lm(burn_area ~ ., X_train)

# Make predictions
preds <- predict(model, X_valid)
preds <- as.data.frame(preds)

# Score
sqrt( mean( (preds$preds-Y_valid)^2 , na.rm = TRUE ) ) # RMSE lower is better

# Exercise. Try a RandomForestRegressor model. Use n_estimators=10 if the default takes too long to run, and experiment with the max_depth parameter.
# With some tweaking, you should be able to get scores ~0.042 or lower.

#############################################
######Making a Submission
# Once you've got some features and a model you're happy with, it's time to submit!
# Look at the sample submission file
ss = read.csv('hot_spots_drc_ss.csv')
head(ss)

# And the test data
head(test)

# So we need to predict the burn area for each row in test.
# Add the same features to test as we did to train
test$date <- as.Date(test$date)
test$month <- as.numeric(format(test$date, "%m"))
test$year  <- as.numeric(format(test$date, "%Y"))

# Get predictions
pred.test <- predict(model, test)

# Create submission df
ss$Prediction <- pred.test

# Save submission file in csv format
write_csv(ss,"mysubmission.csv")
