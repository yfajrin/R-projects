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
library(dplyr)
#Load the dataset.
#Check the data
head(property_info)
head(reserve_2016Q3_train)
head(PropertyID_test)
summary(property_info)
#Add variables into training dataset
train <- reserve_2016Q3_train %>%
  left_join(property_info, by="PropertyID")
test <- data.frame(PropertyID_test)
test$PropertyID <- test$PropertyID_test
test <- subset(test, select = -(PropertyID_test))
head(test)
test <- test  %>%
  left_join(property_info, by="PropertyID")

#check data structure
#Drop variables that is not the determinant of number of reserves: PropertyID, HostID, ListingTitle, Createddate, Zipcode, Neighborhood, MetropolitanStatisticalArea, Latitude, Longitude
train <- subset(train, select = -c(PropertyID, HostID, ListingTitle, Country, State, City, CreatedDate, Zipcode, Neighborhood, MetropolitanStatisticalArea, Latitude, Longitude))
test <- subset(test, select = -c(PropertyID, HostID, ListingTitle, Country, State, City, CreatedDate, Zipcode, Neighborhood, MetropolitanStatisticalArea, Latitude, Longitude))

#Convert char datatype into factors
str(train)
train$PropertyType <- as.factor(train$PropertyType)
train$ListingType <- as.factor(train$ListingType)
train$RequirePhoneVerification <- as.factor(train$RequirePhoneVerification)
train$RequireProfilePic <- as.factor(train$RequireProfilePic)
train$InstantbookEnabled <- as.factor(train$InstantbookEnabled)
train$CancellationPolicy <- as.factor(train$CancellationPolicy)
train$Superhost <- as.factor(train$Superhost)
train$HostProfilePic <- as.factor(train$HostProfilePic)
train$HostVerified  <- as.factor(train$HostVerified)

test$PropertyType <- as.factor(test$PropertyType)
test$ListingType <- as.factor(test$ListingType)
test$RequirePhoneVerification <- as.factor(test$RequirePhoneVerification)
test$RequireProfilePic <- as.factor(test$RequireProfilePic)
test$InstantbookEnabled <- as.factor(test$InstantbookEnabled)
test$CancellationPolicy <- as.factor(test$CancellationPolicy)
test$Superhost <- as.factor(test$Superhost)
test$HostProfilePic <- as.factor(test$HostProfilePic)
test$HostVerified  <- as.factor(test$HostVerified)

levels(train$PropertyType) <- c(levels(train$PropertyType),"Cabin", "Guesthouse")
levels(train$PropertyType)
levels(test$PropertyType)
train <- subset(train, select = -c(PropertyType))
test <- subset(test, select = -c(PropertyType))
str(train)
str(test)

#look at missing values
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

#Dealing with missing values
#Drop variables with missing values more than 2000 = ResponseRate, ResponseTimemin, SecurityDeposit, Cleaningfee, HostListings, ExtraPeopleFee
train <- subset(train, select = -c(ResponseRate, ResponseTimemin, SecurityDeposit, CleaningFee, HostListings, ExtraPeopleFee))
test <- subset(test, select = -c(ResponseRate, ResponseTimemin, SecurityDeposit, CleaningFee, HostListings, ExtraPeopleFee))

#Replace missing values with median of the variable
for(i in 1:ncol(train)){
  train[is.na(train[,i]), i] <- median(train[,i], na.rm = TRUE)
}
for(i in 1:ncol(train)){
  train[is.na(train[,i]), i] <- mean(train[,i], na.rm = TRUE)
}
str(test)
for(i in 1:ncol(test)){
  test[is.na(test[,i]), i] <- median(test[,i], na.rm = TRUE)
}
for(i in 1:ncol(test)){
  test[is.na(test[,i]), i] <- mean(test[,i], na.rm = TRUE)
}
#last check of missing values
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))


#Exploratory Data Analysis
#See correlation matrix
numeric.var <- sapply(train, is.numeric)
corr.matrix <- cor(train[, numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method = 'circle')
#The corrplot shows that the most high correalated variables are NumberofReviews, CleanRating, Superhost, MaxGuest, HostVerified, NumberofPhotos

#See plot of each variable related to number of reservation days
summary(train) #See the descriptive statistics of every variables

#Graph a scatterplot for numerical variables vs number of bookings and the smooth lines
#Number of bookings vs Number of reviews
  ggplot(data=train, aes(x=NumberofReviews, y=NumReserveDays2016Q3)) + 
  geom_point(color='blue') + 
  labs(title="NumberofReviews vs NumReserveDays2016Q3", 
       x = "NumberofReviews", 
       y= "NumReserveDays2016Q3") + 
  geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')

#Number of bookings vs Overall rating
  ggplot(data=train, aes(x=OverallRating, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="OverallRating vs NumReserveDays2016Q3", 
         x = "OverallRating", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #Overall rating plot shows bad counterintuitive result
  
#Number of bookings vs Accuracy rating
  ggplot(data=train, aes(x=AccuracyRating, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="AccuracyRating vs NumReserveDays2016Q3", 
         x = "AccuracyRating", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #Accuracy rating plot shows bad counterintuitive result
  
#Number of bookings vs Clean rating
  ggplot(data=train, aes(x=CleanRating, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="CleanRating vs NumReserveDays2016Q3", 
         x = "CleanRating", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #Clean rating plot shows slightly good result
  
#Number of bookings vs Checkin rating
  ggplot(data=train, aes(x=CheckinRating, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="CheckinRating vs NumReserveDays2016Q3", 
         x = "CheckinRating", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #Checkin rating plot shows almost no difference between high and low rating
  
#Number of bookings vs Communication rating
  ggplot(data=train, aes(x=CommunicationRating, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="CommunicationRating vs NumReserveDays2016Q3", 
         x = "CommunicationRating", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #Communication Rating plot shows bad counterintuitive result 
  
#Number of bookings vs Location rating
  ggplot(data=train, aes(x=LocationRating, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="LocationRating vs NumReserveDays2016Q3", 
         x = "LocationRating", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #Location Rating plot shows counterintuitive result, but it may has significant influence to number of bookings

#Number of bookings vs Value rating
  ggplot(data=train, aes(x=ValueRating, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="Value Rating vs NumReserveDays2016Q3", 
         x = "ValueRating", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #Value Rating plot shows slight better result

#Number of bookings vs Bedrooms
  ggplot(data=train, aes(x=Bedrooms, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="Bedrooms vs NumReserveDays2016Q3", 
         x = "Bedrooms", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #Value Rating plot shows slight better result  

#Number of bookings vs Bathrooms
  ggplot(data=train, aes(x=Bathrooms, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="Bathrooms vs NumReserveDays2016Q3", 
         x = "Bathrooms", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #Bathrooms plot shows no difference
  
#Number of bookings vs MaxGuests
  ggplot(data=train, aes(x=MaxGuests, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="MaxGuests vs NumReserveDays2016Q3", 
         x = "MaxGuests", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #MaxGuests plot shows strong positive relationship
      
#Number of bookings vs PublishedNightlyRate
  ggplot(data=train, aes(x=PublishedNightlyRate, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="PublishedNightlyRate vs NumReserveDays2016Q3", 
         x = "PublishedNightlyRate", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #PublishedNightlyRate plot shows no difference between high and low rate  
  
#Number of bookings vs PublishedMonthlyRate
  ggplot(data=train, aes(x=PublishedMonthlyRate, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="PublishedMonthlyRate vs NumReserveDays2016Q3", 
         x = "PublishedMonthlyRate", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #PublishedMonthlyRate plot shows negative relationship 
  
#Number of bookings vs PublishedWeeklyRate     
  ggplot(data=train, aes(x=PublishedWeeklyRate, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="PublishedWeeklyRate vs NumReserveDays2016Q3", 
         x = "PublishedWeeklyRate", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #PublishedWeeklyRate plot shows negative relationship 

#Number of bookings vs MinimumStay     
  ggplot(data=train, aes(x=MinimumStay, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="MinimumStay vs NumReserveDays2016Q3", 
         x = "MinimumStay", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #MinimumStay plot shows negative relationship  
  
#Number of bookings vs NumberofPhotos     
  ggplot(data=train, aes(x=NumberofPhotos, y=NumReserveDays2016Q3)) + 
    geom_point(color='blue') + 
    labs(title="NumberofPhotos vs NumReserveDays2016Q3", 
         x = "NumberofPhotos", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #MinimumStay plot shows strong positive relationship
  
#Graph a boxplot for categorical independent variables vs number of bookings and its smooth lines 
    #Number of bookings vs ListingType
  ggplot(data=train, aes(x=ListingType, y=NumReserveDays2016Q3)) + 
    geom_boxplot(color='blue') + 
    labs(title="NumReserveDays2016Q3 by ListingType", 
         x = "ListingType", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #ListingType plot shows no relationship
  
  #Number of bookings vs Superhost
  ggplot(data=train, aes(x=Superhost, y=NumReserveDays2016Q3)) + 
    geom_boxplot(color='blue') + 
    labs(title="NumReserveDays2016Q3 by Superhost", 
         x = "Superhost", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #Superhost plot shows positive relationship
  
  #Number of bookings vs HostProfilePic
  ggplot(data=train, aes(x=HostProfilePic, y=NumReserveDays2016Q3)) + 
    geom_boxplot(color='blue') + 
    labs(title="NumReserveDays2016Q3 by HostProfilePic", 
         x = "HostProfilePic", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #HostProfilePic plot shows positive relationship but bad data
  
  #Number of bookings vs HostVerified
  ggplot(data=train, aes(x=HostVerified, y=NumReserveDays2016Q3)) + 
    geom_boxplot(color='blue') + 
    labs(title="NumReserveDays2016Q3 by HostVerified", 
         x = "HostVerified", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #HostVerified plot shows slight positive relationship
  
  #Number of bookings vs CancellationPolicy
  ggplot(data=train, aes(x=CancellationPolicy, y=NumReserveDays2016Q3)) + 
    geom_boxplot(color='blue') + 
    labs(title="NumReserveDays2016Q3 by CancellationPolicy", 
         x = "CancellationPolicy", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #CancellationPolicy plot shows slight positive relationship but bad data
  
  #Number of bookings vs BusinessReady
  ggplot(data=train, aes(x=BusinessReady, y=NumReserveDays2016Q3)) + 
    geom_boxplot(color='blue') + 
    labs(title="NumReserveDays2016Q3 by BusinessReady", 
         x = "BusinessReady", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #BusinessReady plot shows positive relationship
  
  #Number of bookings vs InstantbookEnabled
  ggplot(data=train, aes(x=InstantbookEnabled, y=NumReserveDays2016Q3)) + 
    geom_boxplot(color='blue') + 
    labs(title="NumReserveDays2016Q3 by InstantbookEnabled", 
         x = "InstantbookEnabled", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #InstantbookEnabled plot shows positive relationship
  
  #Number of bookings vs RequireProfilePic
  ggplot(data=train, aes(x=RequireProfilePic, y=NumReserveDays2016Q3)) + 
    geom_boxplot(color='blue') + 
    labs(title="NumReserveDays2016Q3 by RequireProfilePic", 
         x = "RequireProfilePic", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #RequireProfilePic plot shows positive relationship
  
  #Number of bookings vs RequirePhoneVerification
  ggplot(data=train, aes(x=RequirePhoneVerification, y=NumReserveDays2016Q3)) + 
    geom_boxplot(color='blue') + 
    labs(title="NumReserveDays2016Q3 by RequirePhoneVerification", 
         x = "RequirePhoneVerification", 
         y= "NumReserveDays2016Q3") + 
    geom_smooth(method=lm, formula = 'y ~ x', aes(group=1), color = 'red')
  #RequirePhoneVerification plot shows positive relationship
  
#Multiple Linear Regression Model
#Model 0: Use all variables to determine the number of bookings
  model0 <- lm(NumReserveDays2016Q3 ~ . , data = train)
  summary(model0)
  #R-squared low
  
#Model 1: Drop insignificant variable from model 0
  model1 <- lm(NumReserveDays2016Q3 ~ . -(ListingType + AccuracyRating + Bathrooms + HostProfilePic + HostVerified + PublishedWeeklyRate + RequireProfilePic), data = train)
  summary(model1)
  #R-squared low
  
#Model 2: Drop other categorical variables that is insignificant
  model2 <- lm(NumReserveDays2016Q3 ~ . -(ListingType + AccuracyRating + Bathrooms + HostProfilePic + HostVerified + PublishedWeeklyRate + RequireProfilePic + CancellationPolicy), data = train)
  summary(model2)
  #Worse than model 1
  
#Model 3: Use all only highly correlated variables
  model3 <- lm(NumReserveDays2016Q3 ~ NumberofReviews + CleanRating + Superhost + MaxGuests + HostVerified + NumberofPhotos , data = train)
  summary(model3)
  #Worse than model 2

#Model 4: Use all only highly correlated variables and good plot result
  model4 <- lm(NumReserveDays2016Q3 ~ NumberofReviews + CleanRating + LocationRating + Bedrooms + ValueRating + Superhost + MaxGuests + HostVerified + NumberofPhotos + PublishedMonthlyRate + PublishedWeeklyRate + MinimumStay + HostVerified + BusinessReady + InstantbookEnabled + RequireProfilePic + RequirePhoneVerification, data = train)
  summary(model4)
  #still not good enough 

#Model 5: Drop other categorical variables that is insignificant
  model5 <- lm(NumReserveDays2016Q3 ~ NumberofReviews, data = train)
  summary(model5)
  #Worse than model 1 
  
str(train)
  
#Anova analysis for two models
anova(model0, model1, test="Chisq") #result shows no significant difference between model0 and model1
anova(model1, model2, test="Chisq") #result shows significant difference between model1 and model2
anova(model2, model3, test="Chisq") #result shows significant difference between model2 and model3
anova(model3, model4, test="Chisq") #result shows significant difference between model3 and model4
anova(model0, model5, test="Chisq") #result shows significant difference between model3 and model4
anova(model1, model5, test="Chisq") #result shows significant difference between model3 and model4

#Decision: For prediction use model1
pred <- predict(model1, newdata = test)
is.vector(pred)
train <- subset(train, select = -c(CancellationPolicy))
#Revision
#Removing outliers
summary(train)

# create detect outlier function
train2 <- data.frame(train %>%  filter(MinimumStay < 7))
train2 <- data.frame(train2 %>%  filter(NumberofReviews < 45))
train2 <- data.frame(train2 %>%  filter(Bedrooms < 7))
train2 <- data.frame(train2 %>%  filter(PublishedNightlyRate < 500))
train2 <- data.frame(train2 %>%  filter(PublishedMonthlyRate < 9000))
train2 <- data.frame(train2 %>%  filter(PublishedWeeklyRate < 2000))
train2 <- data.frame(train2 %>%  filter(NumberofPhotos < 35))
train2 <- data.frame(train2 %>%  filter(MaxGuests < 9))
summary(train2)

numeric.var2 <- sapply(train2, is.numeric)
corr.matrix2 <- cor(train2[, numeric.var])
corrplot(corr.matrix2, main="\n\nCorrelation Plot for Numerical Variables", method = 'circle')

#Model 5: Use all variables to determine the number of bookings
model5 <- lm(NumReserveDays2016Q3 ~ .  , data = train2)
summary(model5)

model6 <- lm(NumReserveDays2016Q3 ~ . -(RequireProfilePic + BusinessReady + MinimumStay + HostVerified + HostProfilePic + Bathrooms + LocationRating + CommunicationRating + CheckinRating + PublishedMonthlyRate + AccuracyRating + ListingType + ValueRating) , data = train2)
summary(model6)

pred <- predict(model5, newdata = test)
is.vector(pred)
