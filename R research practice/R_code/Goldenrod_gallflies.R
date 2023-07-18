#clear the environment
rm(list=ls())

#set the library that will be used, install the packages if it is not yet installed
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(knitr)
library(cowplot)

#read the data. make sure that the file is already in the working directory
gallfly <- read_excel('gallfly_mating.xlsx', na='NA')

#examine the dataset
  #examine the data type of each column
str(gallfly)
  #examine the number of NA in the data
sum(is.na(gallfly))
  #examine the data
gallfly
  #check whether there are duplicated fly_id, since it should all be unique
duplicated(gallfly$fly_id)

#examined error:
#1. The data type for plot column is char while it needs to be numeric. There are several char records in the column which needs to be converted to numeric.
#2. There are 7 missing values NA in plant_choice column
#3. There are differences in writing Alt and Gig in plant_choice column, some use ALT or GIG, or even gigantia

#fix the first error
gallfly$plot[gallfly$plot == 'plot 1'] <- 1
gallfly$plot[gallfly$plot == 'plot 2'] <- 2
gallfly$plot[gallfly$plot == 'plot 3'] <- 3
gallfly$plot <- as.numeric(gallfly$plot)
str(gallfly)

#fix the second error, remove NA by only selecting the complete cases
gallfly <- gallfly[complete.cases(gallfly), ] 

#fix the third error
gallfly$plant_choice[gallfly$plant_choice == 'ALT'] <- 'Alt'
gallfly$plant_choice[gallfly$plant_choice == 'GIG'] <- 'Gig'
gallfly$plant_choice[gallfly$plant_choice == 'Gigantia'] <- 'Gig'
  #convert the column data type to factor
gallfly$plant_choice <- as.factor(gallfly$plant_choice)
gallfly$host_race <- as.factor(gallfly$host_race)
str(gallfly)

#write the cleaned data to gallfly_cleaned.csv and put it into the working directory
write.csv(gallfly, "gallfly_cleaned.csv", row.names=FALSE)

#count the altissima fly which oviposited in altissima, same host_race and plant_choice
same_alt <- nrow(gallfly[gallfly$host_race == 'Alt' & gallfly$plant_choice == 'Alt',])
#count the gigantia fly which oviposited in gigantia, same host_race and plant_choice
same_gig <- nrow(gallfly[gallfly$host_race == 'Gig' & gallfly$plant_choice == 'Gig',])
#sum the result which will shows the number of flies with host_race equal to plant_choice
same <- same_alt + same_gig
same 
#result
#there are 30 flies from altissima host who oviposited in altissima
#there are 31 flies from gigantia host who oviposited in gigantia
#there are 61 flies who oviposited in the same host_race

#count the altissima fly which oviposited in gigantia, different host_race and plant_choice
dif_alt <- nrow(gallfly[gallfly$host_race == 'Alt' & gallfly$plant_choice == 'Gig',])
#count the gigantia fly which oviposited in altissima, different host_race and plant_choice
dif_gig <- nrow(gallfly[gallfly$host_race == 'Gig' & gallfly$plant_choice == 'Alt',])
#sum the result which will shows the number of flies with host_race different to plant_choice
dif <- dif_alt + dif_gig
#result
#there are 3 flies from altissima host who oviposited in gigantia
#there are 1 flies from gigantia host who oviposited in altissima
#there are 4 flies who oviposited in the different host_race

#create a tibble called gall_counts to show the result above
gall_counts <- data.frame(host_race = c('Alt', 'Alt', 'Gig', 'Gig'),
                          plant_choice = c('Alt', 'Gig', 'Gig', 'Alt'),
                          count = c(same_alt, dif_alt, same_gig, dif_gig))
gall_counts <- as_tibble(gall_counts)
knitr::kable(gall_counts)

#create another column to show the proportion of the count
gall_counts$proportion <- gall_counts$count/colSums(gall_counts[3])
knitr::kable(gall_counts)

#summary: the flies from a host_race are most likely to choose the same species in the plant_choice
#About 46.15% of Alt choose also Alt and about 47.7% of Gig also choose Gig
#About 93.85% of the flies oviposited on their own host
#Only about 6.15% of the flies oviposited on the different host

#calculate all Alt host race
N_alt <- gall_counts %>% 
        filter(host_race == "Alt") %>% 
        summarize(sum(count)) %>% 
        pull()
N_alt
#N_alt = 33, or there are 33 observation of flies with altissima host_race

#create a tibble of the expected number of altissima flies oviposited in altissima if it chooses randomly
#and it is calculated on 1000 occasions
altissima_random <- rbinom(1000, N_alt, 0.5)
altissima_random <- as_tibble(altissima_random)
altissima_random

#create a histogram to show the result above, save it to altissima_plot_random
altissima_plot_random <- ggplot(altissima_random) +
                      aes(x = value) +
                      geom_histogram(fill = 'blue') +
                      geom_vline(aes(xintercept = dif_alt), color = 'red')
                      ggtitle("Histogram of the number of Flies from Altissima which oviposited in Altissima") +
                      xlab("Number of Flies") + ylab("Count")
altissima_plot_random

#calculate all Gig host race
N_gig <- gall_counts %>% 
        filter(host_race == "Gig") %>% 
        summarize(sum(count)) %>% 
        pull()
N_gig
#N_gig = 32, or there are 32 observation of flies with altissima host_race

#create a tibble of the expected number of gigantia flies oviposited in gigantia if it chooses randomly
#and it is calculated on 1000 occasions
gigantia_random <- rbinom(1000, N_gig, 0.5)
gigantia_random <- as_tibble(gigantia_random)
gigantia_random

#create a histogram to show the result above, save it to gigantia_plot_random
gigantia_plot_random <- ggplot(gigantia_random) +
                      aes(x = value) +
                      geom_histogram(fill = 'blue') +
                      geom_vline(aes(xintercept = dif_gig), color = 'red') +
                      ggtitle("Histogram of the number of Flies from Gigantia which oviposited in Gigantia") +
                      xlab("Number of Flies") + ylab("Count")
gigantia_plot_random

#create a plot grid to compare two histogram plots above
plot_grid(altissima_plot_random, gigantia_plot_random, ncol = 1, align = "v")

#calculate the probability of  observing exactly the number of choices of the alternate host plant if the flies are actually choosing randomly
  #for altissima
probability_dif_alt <- dbinom(dif_alt, size = N_alt, prob=0.5)
probability_dif_alt
  #for gigantia
probability_dif_gig <- dbinom(dif_gig, size = N_gig, prob=0.5)
probability_dif_gig
  #for both
probability_dif <- probability_dif_alt + probability_dif_gig
probability_dif

#calculate the probability of observing the number of choices of the alternate host plant or any count lower than observed, if the flies are actually choosing randomly
  #for altissima
probability_dif_alt_cumulative <- pbinom(dif_alt, size = N_alt, prob=0.5)
probability_dif_alt_cumulative
  #for gigantia
probability_dif_gig_cumulative <- dbinom(dif_gig, size = N_gig, prob=0.5)
probability_dif_gig_cumulative
  #for both
probability_dif_cumulative <- probability_dif_alt_cumulative + probability_dif_gig_cumulative
probability_dif_cumulative

#interpretation
#1. It is very unlikely to observe only 3 altissima flies (dif_alt) that choose alternate host plant if the flies are actually choosing randomly
#2. It is very unlikely to observe only 1 gigantia flies (dif_gig) that choose alternate host plant if the flies are actually choosing randomly
#3. It is very unlikely to observe 4 or less flies that choose alternate host plant if the flies are actually choosing randomly
  #the probability is very low
#4. It can be concluded that the initial result is not random
#5. It can be concluded that the gall flies from a specific host_race will prefer to choose the same plant to reproduce