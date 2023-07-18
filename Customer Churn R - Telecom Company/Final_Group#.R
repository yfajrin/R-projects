
#Load necessary library
#If the packages is not yet installed, uncomment and run these installation codes
#install.packages("plyr")
#install.packages("pacman")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("MASS")
#install.packages("rpart")
#install.packages("rpart.plot")
library(plyr)
library(pacman)
library(ggplot2)
library(corrplot)
library(MASS)
library(rpart)
library(rpart.plot)

#Load the dataset.
#Create a folder C:/CustomerChurnData and put the CustomerChurn.csv file there

setwd("C:/CustomerChurnData") #Set the working directory
custchurn <- read.csv(file = "CustomerChurn.csv") #Load the csv file and put it into variable custchurn

#Preliminary data overview & data pre-processing
#Look at the data structure
str(custchurn)		
#Look for missing values in the data set
sapply(custchurn, function(x) sum(is.na(x))) 
#Choose only complete cases
custchurn <- custchurn[complete.cases(custchurn), ]

#Change data structure for char variables to factors
#For variable SeniorCitizen, also change 0 and 1 to No and Yes
custchurn$Churn <- as.factor(custchurn$Churn)
custchurn$gender <- as.factor(custchurn$gender)
custchurn[custchurn$SeniorCitizen == 0,]$SeniorCitizen <- "no"
custchurn[custchurn$SeniorCitizen == 1,]$SeniorCitizen <- "yes"
custchurn$SeniorCitizen <- as.factor(custchurn$SeniorCitizen)
custchurn$Partner <- as.factor(custchurn$Partner)
custchurn$Dependents <- as.factor(custchurn$Dependents)
custchurn$PaperlessBilling <- as.factor(custchurn$PaperlessBilling)
custchurn$PhoneService <- as.factor(custchurn$PhoneService)
custchurn$InternetService <- as.factor(custchurn$InternetService)
custchurn$Contract <- as.factor(custchurn$Contract)
custchurn$PaymentMethod <- as.factor(custchurn$PaymentMethod)    

#Remove unnecessary column: CustomersID  
custchurn$customerID <- NULL

str(custchurn) #Check the data structure after pre-processing

#Exploratory Data Analysis
#Create bar graphs for each variable with response churn overview 
#Create its normalized bar graphs 
#Create the contingency table
#Show the graph and table (repeat this process for each categorical variable)
genderplot <- ggplot(custchurn, aes(gender)) + geom_bar(aes(fill = Churn)) + coord_flip()
genderplotNorm <- ggplot(custchurn, aes(gender)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.gender <- addmargins(A = table(custchurn$Churn, custchurn$gender), FUN = list(total = sum), quiet = TRUE)
genderplot
genderplotNorm
t.gender

SeniorCitizenplot <- ggplot(custchurn, aes(SeniorCitizen)) + geom_bar(aes(fill = Churn)) + coord_flip()
SeniorCitizenplotNorm <- ggplot(custchurn, aes(SeniorCitizen)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.SeniorCitizen <- addmargins(A = table(custchurn$Churn, custchurn$SeniorCitizen), FUN = list(total = sum), quiet = TRUE)
SeniorCitizenplot
SeniorCitizenplotNorm
t.SeniorCitizen

Contractplot <- ggplot(custchurn, aes(Contract)) + geom_bar(aes(fill = Churn)) + coord_flip()
ContractplotNorm <- ggplot(custchurn, aes(Contract)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.Contract <- addmargins(A = table(custchurn$Churn, custchurn$Contract), FUN = list(total = sum), quiet = TRUE)
Contractplot
ContractplotNorm
t.Contract

Dependentsplot <- ggplot(custchurn, aes(Dependents)) + geom_bar(aes(fill = Churn)) + coord_flip()
DependentsplotNorm <- ggplot(custchurn, aes(Dependents)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.Dependents <- addmargins(A = table(custchurn$Churn, custchurn$Dependents), FUN = list(total = sum), quiet = TRUE)
Dependentsplot
DependentsplotNorm
t.Dependents

PhoneServiceplot <- ggplot(custchurn, aes(PhoneService)) + geom_bar(aes(fill = Churn)) + coord_flip()
PhoneServiceplotNorm <- ggplot(custchurn, aes(PhoneService)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.PhoneService <- addmargins(A = table(custchurn$Churn, custchurn$PhoneService), FUN = list(total = sum), quiet = TRUE)
PhoneServiceplot
PhoneServiceplotNorm
t.PhoneService

InternetServiceplot <- ggplot(custchurn, aes(InternetService)) + geom_bar(aes(fill = Churn)) + coord_flip()
InternetServiceplotNorm <- ggplot(custchurn, aes(InternetService)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.InternetService <- addmargins(A = table(custchurn$Churn, custchurn$InternetService), FUN = list(total = sum), quiet = TRUE)
InternetServiceplot
InternetServiceplotNorm
t.InternetService

Partnerplot <- ggplot(custchurn, aes(Partner)) + geom_bar(aes(fill = Churn)) + coord_flip()
PartnerplotNorm <- ggplot(custchurn, aes(Partner)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.Partner <- addmargins(A = table(custchurn$Churn, custchurn$Partner), FUN = list(total = sum), quiet = TRUE)
Partnerplot
PartnerplotNorm
t.Partner

PaperlessBillingplot <- ggplot(custchurn, aes(PaperlessBilling)) + geom_bar(aes(fill = Churn)) + coord_flip()
PaperlessBillingplotNorm <- ggplot(custchurn, aes(PaperlessBilling)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.PaperlessBilling <- addmargins(A = table(custchurn$Churn, custchurn$PaperlessBilling), FUN = list(total = sum), quiet = TRUE)
PaperlessBillingplot
PaperlessBillingplotNorm
t.PaperlessBilling

PaymentMethodplot <- ggplot(custchurn, aes(PaymentMethod)) + geom_bar(aes(fill = Churn)) + coord_flip()
PaymenMethodplotNorm <- ggplot(custchurn, aes(PaymentMethod)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.PaymentMethod <- addmargins(A = table(custchurn$Churn, custchurn$PaymentMethod), FUN = list(total = sum), quiet = TRUE)
PaymentMethodplot
PaymenMethodplotNorm
t.PaymentMethod

#Create histogram for numerical variables and its churn overlay
#Create its normalized histogram
#Show the histogram and get a summary of numerical variables and make sure there are no outliers
tenureplot <- ggplot(custchurn, aes(tenure)) + geom_histogram(aes(fill = Churn), color="black")
tenureplotNorm <- ggplot(custchurn, aes(tenure)) + geom_histogram(aes(fill = Churn), color="black", position = "fill")
tenureplot
tenureplotNorm
summary(custchurn$tenure)

MonthlyChargesplot <- ggplot(custchurn, aes(MonthlyCharges)) + geom_histogram(aes(fill = Churn), color="black")
MonthlyChargesplotNorm <- ggplot(custchurn, aes(MonthlyCharges)) + geom_histogram(aes(fill = Churn), color="black", position = "fill")
MonthlyChargesplot
MonthlyChargesplotNorm
summary(custchurn$MonthlyCharges)

TotalChargesplot <- ggplot(custchurn, aes(TotalCharges)) + geom_histogram(aes(fill = Churn), color="black")
TotalChargesplotNorm <- ggplot(custchurn, aes(TotalCharges)) + geom_histogram(aes(fill = Churn), color="black", position = "fill")
TotalChargesplot
TotalChargesplotNorm
summary(custchurn$TotalCharges)	

#Correlation between numerical variables
numeric.var <- sapply(custchurn, is.numeric)
corr.matrix <- cor(custchurn[,numeric.var])
corr.matrixplot <- corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

#Data partition for Logistic Regression
set.seed(1027) #Using birthday date 10/27
n <- dim(custchurn) [1]
train_ind <- runif(n) < 0.7

training<- custchurn[train_ind,]
testing<- custchurn[!train_ind,]

#Logistic regression
#Fitting the logistic regression model
LogRegModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogRegModel))

#Create another fitting, but only use variables that has higher significance
LogRegModel2 <- glm(Churn ~ tenure+InternetService+Contract+PaymentMethod+TotalCharges,family=binomial(link="logit"),data=training)
print(summary(LogRegModel2))

#Anova analysis for two models as well as random model
anova(LogRegModel, LogRegModel2, test="Chisq")

LogRegModel0 <- glm(Churn ~ 1,family=binomial(link=logit),data=training)
anova(LogRegModel, LogRegModel0, test="Chisq")
anova(LogRegModel2, LogRegModel0, test="Chisq")

#Evaluating the predictive ability of the first model using testing dataset
LogRegModel.pred <- predict(LogRegModel, testing, type='response')
LogRegModel.pred <- ifelse(LogRegModel.pred > 0.5,"Yes","No")
misClasificError <- mean(LogRegModel.pred != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

confmatrix1 <- table(testing$Churn, LogRegModel.pred)
confmatrix1

#Evaluating the predictive ability of the second model using testing dataset
LogRegModel2.pred <- predict(LogRegModel2, testing, type='response')
LogRegModel2.pred <- ifelse(LogRegModel2.pred > 0.5, "Yes","No")
misClasificError <- mean(LogRegModel2.pred != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

confmatrix2 <- table(testing$Churn, LogRegModel2.pred)
confmatrix2

#Odds Ratio
exp(cbind(OR=coef(LogRegModel), confint(LogRegModel)))
exp(cbind(OR=coef(LogRegModel2), confint(LogRegModel2)))

#Decision Tree
#Create decision tree using all variables in training dataset
tree <- rpart(Churn ~ . , training)
treeplot <- rpart.plot(tree, extra=3)
summary(tree)

#Create prediction tree using testing dataset
pred_tree <- predict(tree, testing, type="class")
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)

#Evaluating the Decision Tree accuracy
p1 <- predict(tree, training, type="class")
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))

#End
