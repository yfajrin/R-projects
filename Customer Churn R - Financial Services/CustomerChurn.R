#Load necessary library
#If the packages is not yet installed, uncomment and run these installation code
#install.packages("plyr")
#install.packages("pacman")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("MASS")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("scales")
library(plyr)
library(pacman)
library(ggplot2)
library(corrplot)
library(MASS)
library(rpart)
library(rpart.plot)
library(scales)

#Load the dataset.
#get the path to the working directory
  getwd()

#Put the .csv dataset file into the working directory
#Load the csv file and put it into variable churn_data
  churn_data <- read.csv(file = "Churn_data.csv") 

#Preliminary data overview, data cleaning, and pre-processing
  #Look at missing values in the data set
    sapply(churn_data, function(x) sum(is.na(x))) 
    #there is no missing data  
  
  #Examine the data structure
    str(churn_data)
    #several variables need to be converted into factors

  #Change data structure for char variables to factors
  #For variable HsCrCard, IsActiveMember, and Exited, also change 0 and 1 to No and Yes
    churn_data$Geography <- as.factor(churn_data$Geography)
    
    churn_data$Gender <- as.factor(churn_data$Gender)
    
    churn_data[churn_data$HasCrCard == 0,]$HasCrCard <- "no"
    churn_data[churn_data$HasCrCard == 1,]$HasCrCard <- "yes"
    churn_data$HasCrCard <- as.factor(churn_data$HasCrCard)
    
    churn_data[churn_data$IsActiveMember == 0,]$IsActiveMember <- "no"
    churn_data[churn_data$IsActiveMember == 1,]$IsActiveMember <- "yes"
    churn_data$IsActiveMember <- as.factor(churn_data$IsActiveMember)
    
    churn_data$NumOfProducts <- as.factor(churn_data$NumOfProducts)
    
    churn_data[churn_data$Exited == 0,]$Exited <- "no"
    churn_data[churn_data$Exited == 1,]$Exited <- "yes"
    churn_data$Exited <- as.factor(churn_data$Exited)
    
  #Remove CustomersID as it will not be used to predict churn
    churn_data$CustomerId <- NULL

  #Check the data structure after pre-processing
    str(churn_data)
    summary(churn_data)
    
#Exploratory Data Analysis
  #Create bar graphs for each variable with response churn overview 
  #Create its normalized bar graphs 
  #Create the contingency table
  #Show the table (repeat this process for each categorical variable)
    ggplot(churn_data, aes(Geography)) + geom_bar(aes(fill = Exited)) + coord_flip()
    ggplot(churn_data, aes(Geography)) + geom_bar(aes(fill = Exited), position = "fill") + coord_flip()
    t.Geography <- addmargins(A = table(churn_data$Exited, churn_data$Geography), FUN = list(total = sum), quiet = TRUE)
    t.Geography
    
    ggplot(churn_data, aes(Gender)) + geom_bar(aes(fill = Exited)) + coord_flip()
    ggplot(churn_data, aes(Gender)) + geom_bar(aes(fill = Exited), position = "fill") + coord_flip()
    t.Gender <- addmargins(A = table(churn_data$Exited, churn_data$Gender), FUN = list(total = sum), quiet = TRUE)
    t.Gender
    
    ggplot(churn_data, aes(NumOfProducts)) + geom_bar(aes(fill = Exited)) + coord_flip()
    ggplot(churn_data, aes(NumOfProducts)) + geom_bar(aes(fill = Exited), position = "fill") + coord_flip()
    t.NumOfProducts <- addmargins(A = table(churn_data$Exited, churn_data$NumOfProducts), FUN = list(total = sum), quiet = TRUE)
    t.NumOfProducts
    
    ggplot(churn_data, aes(HasCrCard)) + geom_bar(aes(fill = Exited)) + coord_flip()
    ggplot(churn_data, aes(HasCrCard)) + geom_bar(aes(fill = Exited), position = "fill") + coord_flip()
    t.HasCrCard <- addmargins(A = table(churn_data$Exited, churn_data$HasCrCard), FUN = list(total = sum), quiet = TRUE)
    t.HasCrCard
    
    ggplot(churn_data, aes(IsActiveMember)) + geom_bar(aes(fill = Exited)) + coord_flip()
    ggplot(churn_data, aes(IsActiveMember)) + geom_bar(aes(fill = Exited), position = "fill") + coord_flip()
    t.IsActiveMember <- addmargins(A = table(churn_data$Exited, churn_data$IsActiveMember), FUN = list(total = sum), quiet = TRUE)
    t.IsActiveMember
    
  #Create histogram for numerical variables and its churn overlay
  #Create its normalized histogram
  #Get a summary of numerical variables and make sure there are no outliers
    ggplot(churn_data, aes(CreditScore)) + geom_histogram(aes(fill = Exited), color="black")
    ggplot(churn_data, aes(CreditScore)) + geom_histogram(aes(fill = Exited), color="black", position = "fill")
    summary(churn_data$CreditScore)
    ggplot(churn_data, aes(CreditScore)) + geom_boxplot()
       #there are outliers: customers with very low credit score
  #remove outliers  
    CreditScore_quartiles <- quantile(churn_data$CreditScore, probs=c(.25, .75), na.rm = FALSE)
    CreditScore_IQR <- IQR(churn_data$CreditScore)
    CreditScore_Lower <- CreditScore_quartiles[1] - 1.5*CreditScore_IQR
    CreditScore_Upper <- CreditScore_quartiles[2] + 1.5*CreditScore_IQR 
    churn_data_no_outlier <- subset(churn_data, churn_data$CreditScore > CreditScore_Lower & churn_data$CreditScore < CreditScore_Upper)
    ggplot(churn_data_no_outlier, aes(CreditScore)) + geom_boxplot()
    
    ggplot(churn_data, aes(Age)) + geom_histogram(aes(fill = Exited), color="black")
    ggplot(churn_data, aes(Age)) + geom_histogram(aes(fill = Exited), color="black", position = "fill")
    summary(churn_data$Age)
    ggplot(churn_data, aes(Age)) + geom_boxplot()
      #there are outliers: very old customers higher than 60 years old
  #remove outliers  
    Age_quartiles <- quantile(churn_data$Age, probs=c(.25, .75), na.rm = FALSE)
    Age_IQR <- IQR(churn_data$Age)
    Age_Lower <- Age_quartiles[1] - 1.5*Age_IQR
    Age_Upper <- Age_quartiles[2] + 1.5*Age_IQR 
    churn_data_no_outlier <- subset(churn_data_no_outlier, churn_data_no_outlier$Age > Age_Lower & churn_data_no_outlier$Age < Age_Upper)
    ggplot(churn_data_no_outlier, aes(Age)) + geom_boxplot()
    
    ggplot(churn_data, aes(Tenure)) + geom_histogram(aes(fill = Exited), color="black")
    ggplot(churn_data, aes(Tenure)) + geom_histogram(aes(fill = Exited), color="black", position = "fill")
    summary(churn_data$Tenure)	
    ggplot(churn_data, aes(Tenure)) + geom_boxplot()
      #there are no outliers: tenure is in between 0 to 10 years maximum
    
    ggplot(churn_data, aes(Balance)) + geom_histogram(aes(fill = Exited), color="black") + scale_x_continuous(labels = comma)
    ggplot(churn_data, aes(Balance)) + geom_histogram(aes(fill = Exited), color="black", position = "fill") + scale_x_continuous(labels = comma)
    summary(churn_data$Balance)	
    ggplot(churn_data, aes(Balance)) + geom_boxplot() + scale_x_continuous(labels = comma)
      #there are no outliers
    
    ggplot(churn_data, aes(EstimatedSalary)) + geom_histogram(aes(fill = Exited), color="black") + scale_x_continuous(labels = comma)
    ggplot(churn_data, aes(EstimatedSalary)) + geom_histogram(aes(fill = Exited), color="black", position = "fill") + scale_x_continuous(labels = comma)
    summary(churn_data$EstimatedSalary)	
    ggplot(churn_data, aes(EstimatedSalary)) + geom_boxplot() + scale_x_continuous(labels = comma)
      #there are no outliers
    
  #Correlation between numerical variables
    numeric.var <- sapply(churn_data_no_outlier, is.numeric)
    corr.matrix <- cor(churn_data_no_outlier[,numeric.var])
    corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")
    
#Logistic regression
  #read the testing data and convert the corresponding variables to factors
  testing_churn <- read.csv(file = "Churn_score_data.csv")
  result_churn <- read.csv(file = "Churn_score_true_data.csv")
  
  head(testing_churn)
  head(result_churn)
  
  testing_churn$Geography <- as.factor(testing_churn$Geography)
  
  testing_churn$Gender <- as.factor(testing_churn$Gender)
  
  testing_churn[testing_churn$HasCrCard == 0,]$HasCrCard <- "no"
  testing_churn[testing_churn$HasCrCard == 1,]$HasCrCard <- "yes"
  testing_churn$HasCrCard <- as.factor(testing_churn$HasCrCard)
  
  testing_churn[testing_churn$IsActiveMember == 0,]$IsActiveMember <- "no"
  testing_churn[testing_churn$IsActiveMember == 1,]$IsActiveMember <- "yes"
  testing_churn$IsActiveMember <- as.factor(testing_churn$IsActiveMember)
  
  testing_churn$NumOfProducts <- as.factor(testing_churn$NumOfProducts)
  
  result_churn[result_churn$Exited == 0,]$Exited <- "no"
  result_churn[result_churn$Exited == 1,]$Exited <- "yes"
  result_churn$Exited <- as.factor(result_churn$Exited)
  
  str(testing_churn)
  
  #Fitting the logistic regression model
    Model1 <- glm(Exited ~ .,family=binomial(link="logit"),data=churn_data_no_outlier)
    print(summary(Model1))
    
  #Create another fitting, but only use variables that has higher significance
    Model2 <- glm(Exited ~ Geography+Gender+Age+NumOfProducts+IsActiveMember,family=binomial(link="logit"),data=churn_data_no_outlier)
    print(summary(Model2))
  
  #Anova analysis for two models as well as random model
    anova(Model1, Model2, test="Chisq")
    #random model
    Model0 <- glm(Exited ~ 1,family=binomial(link=logit),data=churn_data_no_outlier)
    anova(Model1, Model0, test="Chisq")
    anova(Model2, Model0, test="Chisq")    

  #Evaluating the predictive ability of the first model using testing
    Model1.pred <- predict(Model1, testing_churn, type='response')
    Model1.pred <- as.factor(ifelse(Model1.pred > 0.5,"yes","no"))
    misClasificError <- mean(Model1.pred != result_churn$Exited)
    print(paste('Logistic Regression Accuracy',1-misClasificError))
    
    print("Confusion Matrix for Logistic Regression"); 
    table(result_churn$Exited, Model1.pred)
    
  #Evaluating the predictive ability of the second model using testing dataset
    Model2.pred <- predict(Model2, testing_churn, type='response')
    Model2.pred <- as.factor(ifelse(Model2.pred > 0.5,"yes","no"))
    misClasificError2 <- mean(Model1.pred != result_churn$Exited)
    print(paste('Logistic Regression Accuracy',1-misClasificError2))
    
    print("Confusion Matrix for Logistic Regression"); 
    table(result_churn$Exited, Model2.pred)
    
  #Odds Ratio
    exp(cbind(OR=coef(Model1), confint(Model1)))
    exp(cbind(OR=coef(Model2), confint(Model2)))
    
#Decision Tree
  #Create decision tree using all variables in training dataset
    tree <- rpart(Exited ~ . , churn_data_no_outlier)
    rpart.plot(tree, extra=3)
    summary(tree)
    
    #Create prediction tree using testing dataset
    pred_tree <- predict(tree, testing_churn, type="class")
    print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = result_churn$Exited)
    
    #Evaluating the Decision Tree accuracy
    p1 <- predict(tree, churn_data_no_outlier, type="class")
    tab1 <- table(Predicted = p1, Actual = churn_data_no_outlier$Exited)
    tab2 <- table(Predicted = pred_tree, Actual = result_churn$Exited)
    print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))  
    