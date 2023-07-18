library(plyr)
install.packages(pacman)
library(pacman)
pacman::p_load(pacman,dplyr,plyr,ggplot,ggplot2,ggthemes,ggvis)
install.packages(ggplot)
install.packages("ggplot2") 
library(ggplot2)
library(grid)
install.packages('corrplot')
library(corrplot)
library(MASS)

setwd("C:/CustomerChurnData")
getwd()
read.csv(file = "CustomerChurn.csv")
custchurn <- read.csv(file = "CustomerChurn.csv")
str(custchurn)
sapply(custchurn, function(x) sum(is.na(x)))
custchurn <- custchurn[complete.cases(custchurn), ]

custchurn[custchurn$SeniorCitizen == 0,]$SeniorCitizen <- "no"
custchurn[custchurn$SeniorCitizen == 1,]$SeniorCitizen <- "yes"
custchurn$Churn <- as.factor(custchurn$Churn)
custchurn$gender <- as.factor(custchurn$gender)
custchurn$SeniorCitizen <- as.factor(custchurn$SeniorCitizen)
custchurn$Partner <- as.factor(custchurn$Partner)
custchurn$Dependents <- as.factor(custchurn$Dependents)
custchurn$PaperlessBilling <- as.factor(custchurn$PaperlessBilling)
custchurn$PhoneService <- as.factor(custchurn$PhoneService)
custchurn$InternetService <- as.factor(custchurn$InternetService)
custchurn$Contract <- as.factor(custchurn$Contract)
custchurn$PaymentMethod <- as.factor(custchurn$PaymentMethod)
custchurn$customerID <- NULL
str(custchurn)

ggplot(custchurn, aes(gender)) + geom_bar(aes(fill = Churn)) + coord_flip()
ggplot(custchurn, aes(gender)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.gender <- addmargins(A = table(custchurn$Churn, custchurn$gender), FUN = list(total = sum), quiet = TRUE)
t.gender

ggplot(custchurn, aes(SeniorCitizen)) + geom_bar(aes(fill = Churn)) + coord_flip()
ggplot(custchurn, aes(SeniorCitizen)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.SeniorCitizen <- addmargins(A = table(custchurn$Churn, custchurn$SeniorCitizen), FUN = list(total = sum), quiet = TRUE)
t.SeniorCitizen

ggplot(custchurn, aes(Contract)) + geom_bar(aes(fill = Churn)) + coord_flip()
ggplot(custchurn, aes(Contract)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.Contract <- addmargins(A = table(custchurn$Churn, custchurn$Contract), FUN = list(total = sum), quiet = TRUE)
t.Contract

ggplot(custchurn, aes(Dependents)) + geom_bar(aes(fill = Churn)) + coord_flip()
ggplot(custchurn, aes(Dependents)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.Dependents <- addmargins(A = table(custchurn$Churn, custchurn$Dependents), FUN = list(total = sum), quiet = TRUE)
t.Dependents

ggplot(custchurn, aes(PhoneService)) + geom_bar(aes(fill = Churn)) + coord_flip()
ggplot(custchurn, aes(PhoneService)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.PhoneService <- addmargins(A = table(custchurn$Churn, custchurn$PhoneService), FUN = list(total = sum), quiet = TRUE)
t.PhoneService

ggplot(custchurn, aes(InternetService)) + geom_bar(aes(fill = Churn)) + coord_flip()
ggplot(custchurn, aes(InternetService)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.InternetService <- addmargins(A = table(custchurn$Churn, custchurn$InternetService), FUN = list(total = sum), quiet = TRUE)
t.InternetService

ggplot(custchurn, aes(Partner)) + geom_bar(aes(fill = Churn)) + coord_flip()
ggplot(custchurn, aes(Partner)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.Partner <- addmargins(A = table(custchurn$Churn, custchurn$Partner), FUN = list(total = sum), quiet = TRUE)
t.Partner

ggplot(custchurn, aes(PaperlessBilling)) + geom_bar(aes(fill = Churn)) + coord_flip()
ggplot(custchurn, aes(PaperlessBilling)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.PaperlessBilling <- addmargins(A = table(custchurn$Churn, custchurn$PaperlessBilling), FUN = list(total = sum), quiet = TRUE)
t.PaperlessBilling

ggplot(custchurn, aes(PaymentMethod)) + geom_bar(aes(fill = Churn)) + coord_flip()
ggplot(custchurn, aes(PaymentMethod)) + geom_bar(aes(fill = Churn), position = "fill") + coord_flip()
t.PaymentMethod <- addmargins(A = table(custchurn$Churn, custchurn$PaymentMethod), FUN = list(total = sum), quiet = TRUE)
t.PaymentMethod

ggplot(custchurn, aes(tenure)) + geom_histogram(aes(fill = Churn), color="black")
ggplot(custchurn, aes(tenure)) + geom_histogram(aes(fill = Churn), color="black", position = "fill")
summary(custchurn$tenure)

ggplot(custchurn, aes(MonthlyCharges)) + geom_histogram(aes(fill = Churn), color="black")
ggplot(custchurn, aes(MonthlyCharges)) + geom_histogram(aes(fill = Churn), color="black", position = "fill")
summary(custchurn$MonthlyCharges)

ggplot(custchurn, aes(TotalCharges)) + geom_histogram(aes(fill = Churn), color="black")
ggplot(custchurn, aes(TotalCharges)) + geom_histogram(aes(fill = Churn), color="black", position = "fill")
summary(custchurn$TotalCharges)

numeric.var <- sapply(custchurn, is.numeric)
corr.matrix <- cor(custchurn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

#First, we split the data into training and testing sets:
set.seed(1027)
n <- dim(custchurn) [1]
train_ind <- runif(n) < 0.7

training<- custchurn[train_ind,]
testing<- custchurn[!train_ind,]

#Fitting the Logistic Regression Model:
LogRegModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogRegModel))

confint(LogRegModel, level = .95)

LogRegModel2 <- glm(Churn ~ tenure+InternetService+Contract+PaymentMethod+TotalCharges,family=binomial(link="logit"),data=training)
print(summary(LogRegModel2))


#Feature Analysis
anova(LogRegModel, LogRegModel2, test="Chisq")

#Assessing the predictive ability of the Logistic Regression model
#testing$Churn <- as.character(testing$Churn)
#testing$Churn[testing$Churn=="No"] <- "0"
#testing$Churn[testing$Churn=="Yes"] <- "1"
str(testing)
LogRegModel.pred <- predict(LogRegModel, testing, type='response')
LogRegModel.pred <- ifelse(LogRegModel.pred > 0.5,"Yes","No")
misClasificError <- mean(LogRegModel.pred != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

print("Confusion Matrix for Logistic Regression"); 
table(testing$Churn, LogRegModel.pred)
testing$Churn
#Assessing the predictive ability of the Logistic Regression model
#testing$Churn <- as.character(testing$Churn)
#testing$Churn[testing$Churn=="No"] <- "0"
#testing$Churn[testing$Churn=="Yes"] <- "1"
LogRegModel2.pred <- predict(LogRegModel2, testing, type='response')
LogRegModel2.pred <- ifelse(LogRegModel2.pred > 0.5, "Yes","No")
misClasificError <- mean(LogRegModel2.pred != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

print("Confusion Matrix for Logistic Regression"); 
table(testing$Churn, LogRegModel2.pred)


#Odds Ratio

exp(cbind(OR=coef(LogRegModel), confint(LogRegModel)))
exp(cbind(OR=coef(LogRegModel2), confint(LogRegModel2)))

install.packages('caret')
library(caret)
install.packages("C50")
library(C50)
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

tree <- rpart(Churn ~ . , training)
rpart.plot(tree, extra=3)
summary(tree)

pred_tree <- predict(tree, testing, type="class")
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)
str(testing)
str(tree)

p1 <- predict(tree, training, type="class")
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))

# CLEAN UP #################################################

# Clear packages
detach(, unload = TRUE)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)