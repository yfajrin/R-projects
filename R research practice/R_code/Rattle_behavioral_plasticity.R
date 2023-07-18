#clear the environment
rm(list=ls())

#set the library that will be used
library(tidyr)
library(ggplot2)
library(dplyr)

#read the relevant data in the working directory and put it into each variable
#make sure that the file is already in the working directory
probability <- read.csv('rattle_probability.csv')
duration <- read.csv('rattle_duration.csv')
rate <- read.csv('rattle_rate.csv')
probability
duration
rate

#renaming the column with more manageable names: Distance, Probability, Duration, Rate
colnames(probability) <- c("Species", "Distance","Probability")
colnames(duration) <- c("Species", "Distance","Duration")
colnames(rate) <- c("Species", "Distance","Rate")

#remove rows with NA using slice() function
probability_clean <- slice(probability, which((Distance!=NA)| !is.na(Distance)))
probability_clean
duration_clean <- slice(duration, which((Distance!=NA)| !is.na(Distance)))
duration_clean
rate_clean <- slice(rate, which((Distance!=NA)| !is.na(Distance)))
rate_clean

#check whether the first data of the initial and the cleaned is the same
#since the first start at the second row, then we use slice(dataframe, 2) to target it
slice(probability, 2) == slice_head(probability_clean) #return TRUE
slice(duration, 2) == slice_head(duration_clean) #return TRUE
slice(rate, 2) == slice_head(rate_clean) #return TRUE
#check whether the last data of the initial and the cleaned is the same
slice_tail(probability) == slice_tail(probability_clean) #return TRUE
slice_tail(duration) == slice_tail(duration_clean) #return TRUE
slice_tail(rate) == slice_tail(rate_clean) #return TRUE

#join the three tibbles
data1 <- left_join(probability_clean, duration_clean, by=c('Species'='Species', 'Distance'='Distance'))
data_all <- left_join(data1, rate_clean, by=c('Species'='Species', 'Distance'='Distance'))

#inspect joined data_all
data_all
sum(is.na(data_all$Probability))
sum(is.na(data_all$Duration))
sum(is.na(data_all$Rate))
summary(data_all)
#there are 14 NA in Duration and Rate column

#explaining the pattern of missing data:
#the cleaned probability data has 55 records showing 55 different species, 
#while the cleaned duration and rate data has only 41 records of species.
#when the three tibbles are merged, there must be 14 missing data in Duration and Rate column,
#because the data only consist for 41 species compared to 55 species in probability data

#convert the data into long format
data_long <- gather(data_all, Variables, Value, Probability:Rate, factor_key=TRUE)
data_long

#plot the relationship between Distance and each variable, save it to distance_vs_variable
distance_vs_variable <- ggplot(data_long, aes(x = Distance, y = Value, color = Variables,
                 shape = Variables)) +
                    geom_point(size = 2) + 
                    facet_grid(rows=vars(Variables), scales="free") +
                    geom_smooth(method = "lm") +
                    ggtitle("Relationship between Distance and each variable") +
                    xlab("Distance") + ylab("Value")
distance_vs_variable

#pattern observation: there is negative relationship between distance and each variable,
#as distance get larger, the value of each variable becomes smaller
#as distance get closer, the value of each variable becomes higher

#using the wide data format, plot the rate as a function of duration, and color it by distance, save it to duration_vs_rate 
rate_vs_duration <- ggplot(data_all, aes(x = Duration, y = Rate, color = Distance)) +
                    geom_point(size = 2) + 
                    geom_smooth(method = "lm") +
                    ggtitle("Relationship between Rate and Duration based on Distance") +
                    xlab("Duration") + ylab("Rate")
rate_vs_duration

#summary:
#we hypothesized that the higher rates of tail shaking would be associated with shorter duration of tail shaking 
#to limit overall energy expenditure, because right rates might be more energetically demanding.
#however, according to the plot, the higher rates, also followed by higher duration.
#so, the hypothesis is not true.
#in this case, distance has an important role.
#rate and duration will be higher when the distance is very close.
#even though high rate of tail shaking for long duration consumes energy,
#the snake will still behave that way when the distance is close

#the species-level data is independent since it does not affected by other data or variable
#however, there may be more insight if the data contains also in higher level of taxonomy 
#for instance, the snake in the same family may behave similar to each other compared to those in different family
#to improve the analysis, the author should provide the data about higher taxonomy level of the snakes,
#so that the analysis can be made on the basis of genus or family or even higher level

