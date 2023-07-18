library(tidyverse)
library(dplyr)
library(gplots)
library(ggplot2)

insurance <- read.csv('./insurance.csv')
head(insurance)

## PART ONE
# 1. Mean and Standard Deviation for variable age and charges

mean(insurance$age)
sd(insurance$age)

mean(insurance$charges)
sd(insurance$charges)

# 2. Correlation between age and charges
cor(insurance$age, insurance$charges)

# 3. Simple linear regression model
simple_model <- lm(charges ~ age, data = insurance)
simple_model
round(summary(simple_model)$coefficients, 3)

# 4. Sample predictions for any two data cases and the residuals
# First sample
# Generate random sample index 
sample_1 <- sample(1:nrow(insurance),1)
sample_1
# Obtain age of the random sample
sample_1_age <- data.frame(age = insurance$age[sample_1])
sample_1_age
# Calculate the predicted charges using the random sample age
predicted_value_1 <- predict(simple_model, newdata = sample_1_age)
predicted_value_1
# The actual value for the random sample
actual_value_1 <- insurance$charges[sample_1]
actual_value_1
# Residual is the difference between the actual value and the predicted value 
residual_1 <- actual_value_1 - predicted_value_1
residual_1

# Second sample
# Generate random sample index 
sample_2 <- sample(1:nrow(insurance),1)
sample_2
# Obtain age of the random sample
sample_2_age <- data.frame(age = insurance$age[sample_2])
sample_2_age
# Calculate the predicted charges using the random sample age
predicted_value_2 <- predict(simple_model, newdata = sample_2_age)
predicted_value_2
# The actual value for the random sample
actual_value_2 <- insurance$charges[sample_2]
actual_value_2
# Residual is the difference between the actual value and the predicted value 
residual_2 <- actual_value_2 - predicted_value_2
residual_2

# 5. Scatterplot with regression line
insurance %>%
  ggplot(aes(x=age,y=charges)) +
  geom_point(alpha=0.5) +
  labs(x= "Age", y="Insurance Charge")+
  geom_smooth(method=lm)

#	6. 7. Test whether the slope is significantly different from zero & R-Squared
summary(simple_model)
round(summary(simple_model)$coefficients, 3)

# 8. computation of confidence interval for mean and prediction interval for individual values
age_sample <- 58
sample_size <- nrow(insurance)

# Compute the standard error of the mean
RSE <- sqrt(sum(simple_model$residuals^2) / (nrow(simple_model$model) - 2))
SE_mean <- RSE / sqrt(sample_size)

# Compute the confidence interval for the mean
confidence_level <- 0.95  # Set the desired confidence level (e.g., 95%)
margin_of_error <- qt((1 - confidence_level) / 2, df = sample_size - 1) * SE_mean
confidence_interval <- c("fit : ", predicted_value_1, ", lower :", predicted_value_1 - margin_of_error, ", upper :", predicted_value_1 + margin_of_error)

# Compute the prediction interval for an individual value
SE_prediction <- sqrt(RSE^2 + (SE_mean^2 * (1 + 1/sample_size)))
margin_of_error_prediction <- qt((1 - confidence_level) / 2, df = sample_size - 1) * SE_prediction
prediction_interval <- c("fit : ", predicted_value_1, ", lower :", predicted_value_1 - margin_of_error_prediction, ", upper :", predicted_value_1 + margin_of_error_prediction)

# Print the confidence interval for the mean and the prediction interval
cat("Confidence Interval for the Mean:", "\n", confidence_interval, "\n")
cat("Prediction Interval for Individual Values:", "\n", prediction_interval)

# 9. residual plot and test the assumptions of the regression model
residuals <- residuals(simple_model)
residual_data <- data.frame(Age = insurance$age, Residuals = residuals)

# Create the residual plot using ggplot2
ggplot(residual_data, aes(x = Age, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", fill = "lightblue") +
  geom_hline(yintercept = 0, color = "red") +
  xlab("Age") +
  ylab("Residuals") +
  ggtitle("Residual Plot")

# Create a data frame with the residuals
residual_data_2 <- data.frame(Residuals = residuals)

# Create the histogram plot using ggplot2
ggplot(residual_data_2, aes(x = Residuals)) +
  geom_histogram(fill = "lightblue", color = "black") +
  xlab("Residuals") +
  ylab("Frequency") +
  ggtitle("Histogram of Residuals")

# Create the Q-Q plot using ggplot2
ggplot(residual_data_2, aes(sample = Residuals)) +
  geom_qq() +
  stat_qq_line(distribution = qnorm, color = "red", linetype = "dashed") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  ggtitle("Q-Q Plot of Residuals")

# Perform Shapiro-Wilk test
shapiro.test(residuals)


##PART TWO
# 1. Descriptive statistics
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)
summary(insurance)

# 2. Correlation among all variables
round(with(insurance, cor(cbind(age, sex, bmi, children, smoker, region, charges))),4)

# 3. Multiple linear regression using all predictor variables
multiple_model <- lm(charges ~ ., data=insurance)
summary(multiple_model)

# 4. Model experimentation
#model experimentation 2: drop sex variable
multiple_model_2 <- lm(charges ~ . -sex, data=insurance)
summary(multiple_model_2)

#model experimentation 3: drop sex and region variable
multiple_model_3 <- lm(charges ~ age + bmi + children + smoker, data=insurance)
summary(multiple_model_3)

# 5. Prediction using random sample
# Third sample
# Generate random sample index 
sample_3 <- sample(1:nrow(insurance),1)
sample_3
# Obtain age of the random sample
sample_3_df <- data.frame(age=insurance$age[sample_3], bmi=insurance$bmi[sample_3], children=insurance$children[sample_3], smoker=insurance$smoker[sample_3])
sample_3_df
# Calculate the predicted charges using the random sample age
predicted_value_3 <- predict(multiple_model_3, newdata = sample_3_df)
predicted_value_3
# The actual value for the random sample
actual_value_3 <- insurance$charges[sample_3]
actual_value_3
# Residual is the difference between the actual value and the predicted value 
residual_3 <- actual_value_3 - predicted_value_3
residual_3

#Fourth sample
# Generate random sample index 
sample_4 <- sample(1:nrow(insurance),1)
sample_4
# Obtain age of the random sample
sample_4_df <- data.frame(age=insurance$age[sample_4], bmi=insurance$bmi[sample_4], children=insurance$children[sample_4], smoker=insurance$smoker[sample_4])

# Calculate the predicted charges using the random sample age
predicted_value_4 <- predict(multiple_model_3, newdata = sample_4_df)
predicted_value_4
# The actual value for the random sample
actual_value_4 <- insurance$charges[sample_4]
actual_value_4
# Residual is the difference between the actual value and the predicted value 
residual_4 <- actual_value_4 - predicted_value_4
residual_4

# 9. residual plot and test the assumptions of the regression model
residuals_3 <- residuals(multiple_model_3)
residual_data_3 <- data.frame(Charge = insurance$charges, Residuals = residuals)

# Create the residual plot using ggplot2
ggplot(residual_data_3, aes(x = Charge, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", fill = "lightblue") +
  geom_hline(yintercept = 0, color = "red") +
  xlab("Charge") +
  ylab("Residuals") +
  ggtitle("Residual Plot")

# Create a data frame with the residuals
residual_data_4 <- data.frame(Residuals = residuals)

# Create the histogram plot using ggplot2
ggplot(residual_data_4, aes(x = Residuals)) +
  geom_histogram(fill = "lightblue", color = "black") +
  xlab("Residuals") +
  ylab("Frequency") +
  ggtitle("Histogram of Residuals")

# Create the Q-Q plot using ggplot2
ggplot(residual_data_4, aes(sample = Residuals)) +
  geom_qq() +
  stat_qq_line(distribution = qnorm, color = "red", linetype = "dashed") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  ggtitle("Q-Q Plot of Residuals")

# Perform Shapiro-Wilk test
shapiro.test(residuals_3)

