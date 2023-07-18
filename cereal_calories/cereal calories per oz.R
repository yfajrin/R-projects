#Set the working directory
setwd("C:/Cereal") 

#Load the csv file and put it into variable named cereal, make sure the csv file is in the working directory
cereal <- read.csv(file = "cereal.csv") 

#look at the data type of each variable in the dataframe
str(cereal) 

#create a new variable calPerOz or calories per oz as the variables of interest
cereal$calPerOz = cereal$calories / cereal$weight

#here we have one variable sample data of calories per oz of each cereal product
str(cereal$calPerOz)

#we get minimum, low quartile, median, mean, upper quartile, and maximum
summary(cereal$calPerOz) 

#calculate std deviation
sd <- sd(cereal$calPerOz) 

#calculate standard error
se <- sd/sqrt(length(cereal$calPerOz))

#calculate #95% confidence intervals of the mean
ci <- c(mean(cereal$calPerOz)-2*se,mean(cereal$calPerOz)+2*se)
ci

#create a histogram of the data
hist(cereal$calPerOz,
     main = "Calories per Oz of Cereal Products",
     xlab = "Calories per Oz (kcal)",
     ylab = "Frequency")

#create a boxplot of the data
boxplot(cereal$calPerOz, 
        main = "Calories per Oz of Cereal Products", 
        ylab = "Calories per Oz (kcal)")

