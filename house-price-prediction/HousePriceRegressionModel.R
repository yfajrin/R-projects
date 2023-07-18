#Load necessary library
#If the packages is not yet installed, uncomment these installation code
#install.packages("plyr")
#install.packages("pacman")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("MASS")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("SmartEDA")
#install.packages("tidyverse")
#install.packages("data.table")
library(plyr)
library(pacman)
library(ggplot2)
library(corrplot)
library(MASS)
library(rpart)
library(rpart.plot)
library(SmartEDA)
library(data.table)
#Load the dataset.
#Create a folder C:/HousePriceData and put the dataset file house_price_dataset_cleaned.csv there

setwd("C:/HousePriceData") #Set the working directory
house <- read.csv(file = "house_price_dataset_cleaned.csv") #Load the csv file and put it into variable named house

#Preliminary data overview & data pre-processing

str(house)	#Look at the data structure	
sapply(house, function(x) sum(is.na(x))) #Look at missing values in the data set
#there is no missing value

#rename variables for simplicity
house <- plyr::rename(house, replace=c(
                land_size_sqm = "lss", 
                house_size_sqm = "hss", 
                no_of_rooms = "nor",
                no_of_bathrooms = "nob",
                large_living_room = "llr",
                parking_space = "psc",
                front_garden = "frg",
                swimming_pool = "swp",
                distance_to_school = "dts",
                wall_fence = "wfe",
                house_age = "age",
                water_front = "wfr",
                distance_to_supermarket_km = "dtsm",
                crime_rate_index = "cri",
                room_size = "rsz",
                property_value = "price"))

#convert categorical variables as factor
house$llr <- as.factor(house$llr)
house$psc <- as.factor(house$psc)
house$frg <- as.factor(house$frg)
house$swp <- as.factor(house$swp)
house$wfe <- as.factor(house$wfe)
house$wfr <- as.factor(house$wfr)
house$rsz <- as.factor(house$rsz)

house$price <- as.numeric(house$price)
#add other variable priceK which shows house price in thousand dollars
house$priceK <- house$price/1000 

str(house) #Last check at the data structure

#Exploratory Data Analysis
summary(house) #See the descriptive statistics of every variables

#Graph a scatterplot for numerical variables vs house price and the smooth lines
  #house price vs land size sqm
  ggplot(data=house, aes(x=lss, y=priceK)) + 
    geom_point(color='blue') + 
    labs(title="House price vs land size sqm", 
         x = "Land size (sqm)", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #house price vs house size sqm
  ggplot(data=house, aes(x=hss, y=priceK)) + 
    geom_point(color='blue') + 
    labs(title="House price vs house size sqm", 
         x = "House size (sqm)", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #house price vs number of rooms
  ggplot(data=house, aes(x=nor, y=priceK)) + 
    geom_point(color='blue') + 
    labs(title="House price vs number of rooms", 
         x = "Number of rooms", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #house price vs number of bathrooms
  ggplot(data=house, aes(x=nob, y=priceK)) + 
    geom_point(color='blue') + 
    labs(title="House price vs number of bathrooms", 
         x = "Number of bathrooms", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #house price vs distance to school
  ggplot(data=house, aes(x=dts, y=priceK)) + 
    geom_point(color='blue') + 
    labs(title="House price vs distance to school", 
         x = "Distance to school (km)", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #house price vs distance to supermarket
  ggplot(data=house, aes(x=dtsm, y=priceK)) + 
    geom_point(color='blue') + 
    labs(title="House price vs distance to supermarket", 
         x = "Distance to supermarket (km)", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #house price vs house age
  ggplot(data=house, aes(x=age, y=priceK)) + 
    geom_point(color='blue') + 
    labs(title="House price vs house age", 
         x = "House age (years)", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #house price vs house age
  ggplot(data=house, aes(x=cri, y=priceK)) + 
    geom_point(color='blue') + 
    labs(title="House price vs crime rate", 
         x = "Crime rate index", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  
#Graph a boxplot for categorical independent variables vs house price and its smooth lines 
  #price vs large living room
  ggplot(data=house, aes(x=llr, y=priceK)) + 
    geom_boxplot(color='blue') + 
    labs(title="House price by large living room", 
         x = "Large living room (1 = yes, 0 = no)", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  t.llr <- aggregate(house$priceK, list(house$llr), 
                     function(x) format(summary(x), scientific = FALSE))
  t.llr
  #price vs parking space availability
  ggplot(data=house, aes(x=psc, y=priceK)) + 
    geom_boxplot(color='blue') + 
    labs(title="House price by parking space availability", 
         x = "Parking space availability (1 = yes, 0 = no)", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  t.psc <- aggregate(house$priceK, list(house$psc), 
                     function(x) format(summary(x), scientific = FALSE))
  t.psc
  #price vs front garden availability
  ggplot(data=house, aes(x=frg, y=priceK)) + 
    geom_boxplot(color='blue') + 
    labs(title="House price by front garden availability", 
         x = "Front garden availability (1 = yes, 0 = no)", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  t.frg <- aggregate(house$priceK, list(house$frg), 
                     function(x) format(summary(x), scientific = FALSE))
  t.frg
  #price vs swimming pool availability
  ggplot(data=house, aes(x=swp, y=priceK)) + 
    geom_boxplot(color='blue') + 
    labs(title="House price by swimming pool availability", 
         x = "Swimming pool availability (1 = yes, 0 = no)", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  t.swp <- aggregate(house$priceK, list(house$swp), 
                     function(x) format(summary(x), scientific = FALSE))
  t.swp
  #price vs wall fences availability
  ggplot(data=house, aes(x=wfe, y=priceK)) + 
    geom_boxplot(color='blue') + 
    labs(title="House price by wall fences availability", 
         x = "Wall fences availability (1 = yes, 0 = no)", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  t.wfe <- aggregate(house$priceK, list(house$wfe), 
                     function(x) format(summary(x), scientific = FALSE))
  t.wfe
  #price vs water front availability
  ggplot(data=house, aes(x=wfr, y=priceK)) + 
    geom_boxplot(color='blue') + 
    labs(title="House price by water front availability", 
         x = "Water front availability (1 = yes, 0 = no)", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  t.wfr <- aggregate(house$priceK, list(house$wfr), 
                     function(x) format(summary(x), scientific = FALSE))
  t.wfr
  #price vs room size category
  ggplot(data=house, aes(x=rsz, y=priceK)) + 
    geom_boxplot(color='blue') + 
    labs(title="House price by room size category", 
         x = "Room size category (0 = small, ..., 3 = large)", 
         y= "House price (in thousand dollars)") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  t.rsz <- aggregate(house$priceK, list(house$rsz), 
                     function(x) format(summary(x), scientific = FALSE))
  t.rsz
  
#Create correlation matrix to show Pearson correlation value for numerical variables
numeric.var <- sapply(house, is.numeric)
corr.matrix <- cor(house[, numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, diag=FALSE)

#Data partition for Multiple Linear Regression Model
set.seed(12345)
n <- dim(house) [1]
train_ind <- runif(n) < 0.7

training<- house[train_ind,]
testing<- house[!train_ind,]

#Multiple Linear Regression Model
  #Model 0: use all variables
  model0 <- lm(priceK ~ .-price , data = training)
  summary(model0)
  #Model 1: use only highly correlated numeric variables and all categorical variables
  model1 <- lm(priceK ~ lss + hss + nor + nob + llr + psc + frg + swp + wfe + wfr + rsz , data = training)
  summary(model1)
  #Model 2: dropping nor, nob, and wfr from model 1
  model2 <- lm(priceK ~ lss + llr + psc + frg + swp + wfe, data = training)
  summary(model2)
  
#Anova analysis for two models as well as random model
anova(model0, model1, test="Chisq")
anova(model1, model2, test="Chisq")

#Evaluating the predictive ability of the second model using testing dataset
model2.pred <- predict(model2, newdata = testing)
rmse <- sqrt(sum((model2.pred - testing$priceK)^2)/length(testing$priceK))
c(RMSE = rmse, R2=summary(model2)$r.squared)
par(mfrow=c(1,1))
ggplot(data=testing, aes(x = testing$priceK, y = model2.pred)) + 
  geom_point(color='blue') + 
  labs(title="Real price vs Predicted price by Model 2", 
       x = "Real house price (in thousand dollars)", 
       y= "Predicted price by Model 2 (in thousand dollars)")
