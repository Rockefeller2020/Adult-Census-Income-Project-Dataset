# HarvardX Data Science Capstone
# Adult Census Income Project Code
# Chase2020

### Install required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(randomForest)

### Import the dataset
# Dataset was downloaded originally from https://www.kaggle.com/uciml/adult-census-income and then saved to a Github repository
dataset <- read.csv("https://github.com/Rockefeller2020/Adult-Census-Income-Project-Dataset/raw/master/adult.csv")

### Modify the dataset to include an income_level item that can be used to analyze the data
dataset_v2 <- dataset %>% mutate(income_level=as.numeric(income))

### Create the training and validation datasets. The validation dataset will be 10% of the adult dataset.
set.seed(1)
test_index <- createDataPartition(y = dataset_v2$income, times=1, p=0.1, list = FALSE)
training <- dataset_v2[-test_index,]
validation <- dataset_v2[test_index,]

### Methods and Data Analysis
### General analysis of the dataset
# Summary of the data
summary(training)

# Display the first six rows of the dataset
head(training)

### Graphs showing average income levels for the dataset by variable
# Plot average income level by age
age_averages <- training %>% 
  group_by(age) %>% summarize(average_income_level=mean(income_level))

ggplot(age_averages, aes(x = age, y = average_income_level)) +
  geom_area(stat = "identity", fill = "grey25") +
  ylim(0,2) +
  stat_smooth(geom = 'area', method = 'loess', span = 1/3,
              alpha = 1/2, fill = "green") +
  labs(title = "Average Income Level By Age")

# Plot average income level by working class
workclass_averages <- training %>% 
  group_by(workclass) %>% summarize(average_income_level=mean(income_level))

ggplot(workclass_averages, aes(x = workclass, y = average_income_level)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = abbreviate) +
  ylim(0,2) +
  labs(title = "Average Income Level By Working Class")

# Plot average income level by education
education_averages <- training %>% 
  group_by(education) %>% summarize(average_income_level=mean(income_level))

ggplot(education_averages, aes(x = education, y = average_income_level)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = abbreviate) +
  ylim(0,2) +
  labs(title = "Average Income Level By Education")

# Plot average income level by education number
education.num_averages <- training %>% 
  group_by(education.num) %>% summarize(average_income_level=mean(income_level))

ggplot(education.num_averages, aes(x = education.num, y = average_income_level)) +
  geom_area(stat = "identity", fill = "grey25") +
  ylim(0,2) +
  labs(title = "Average Income Level By Education Number")

# Plot average income level by marital status
marital.status_averages <- training %>% 
  group_by(marital.status) %>% summarize(average_income_level=mean(income_level))

ggplot(marital.status_averages, aes(x = marital.status, y = average_income_level)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = abbreviate) +
  ylim(0,2) +
  labs(title = "Average Income Level By Marital Status")

# Plot average income level by occupation
occupation_averages <- training %>% 
  group_by(occupation) %>% summarize(average_income_level=mean(income_level))

ggplot(occupation_averages, aes(x = occupation, y = average_income_level)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = abbreviate) +
  ylim(0,2) +
  labs(title = "Average Income Level By Occupation")

# Plot average income level by relationship
relationship_averages <- training %>% 
  group_by(relationship) %>% summarize(average_income_level=mean(income_level))

ggplot(relationship_averages, aes(x = relationship, y = average_income_level)) +
  geom_bar(stat = "identity") +
  ylim(0,2) +
  labs(title = "Average Income Level By Relationship")

# Plot average income level by race
race_averages <- training %>% 
  group_by(race) %>% summarize(average_income_level=mean(income_level))

ggplot(race_averages, aes(x = race, y = average_income_level)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = abbreviate) +
  ylim(0,2) +
  labs(title = "Average Income Level By Race")

# Plot average income level by sex
sex_averages <- training %>% 
  group_by(sex) %>% summarize(average_income_level=mean(income_level))

ggplot(sex_averages, aes(x = sex, y = average_income_level)) +
  geom_bar(stat = "identity") +
  ylim(0,2) +
  labs(title = "Average Income Level By Sex")

# Plot average income level by capital gain reported
training %>%
  ggplot(aes(capital.gain)) + 
  geom_histogram(bins = 15, color = "black") +
  labs(title = "Number of People by Capital Gain Reported",
       x = "Capital_Gain_Reported",
       y = "Number_of_People")

# Determine number of people reporting a captial gain of $0
training %>%
  count(capital.gain == 0)

# Plot average income level by capital loss reported
training %>%
  ggplot(aes(capital.loss)) + 
  geom_histogram(bins = 15, color = "black") +
  labs(title = "Number of People by Capital Loss Reported",
       x = "Capital_Loss_Reported",
       y = "Number_of_People")

# Determine number of people reporting a captial loss of $0
training %>%
  count(capital.loss == 0)

# Plot average income level by hours worked per week
hours.per.week_averages <- training %>% 
  group_by(hours.per.week) %>% summarize(average_income_level=mean(income_level))

ggplot(hours.per.week_averages, aes(x = hours.per.week, y = average_income_level)) +
  geom_area(stat = "identity", fill = "grey25") +
  ylim(0,2) +
  stat_smooth(geom = 'area', method = 'loess', span = 1/3,
              alpha = 1/2, fill = "green") +
  labs(title = "Average Income Level By Hours Worked Per Week")

# Plot average income level by native country
native.country_averages <- training %>% 
  group_by(native.country) %>% summarize(average_income_level=mean(income_level))

ggplot(native.country_averages, aes(x = native.country, y = average_income_level)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = abbreviate) +
  ylim(0,2) +
  labs(title = "Average Income Level By Native Country")

### Models
# Remove variables not needed by models in both training and validation datasets
training_v2 <- within(training, rm(fnlwgt, capital.gain, capital.loss, education, income_level))
summary(training_v2)

validation_v2 <- within(validation, rm(fnlwgt, capital.gain, capital.loss, education, income_level))
summary(validation_v2)

# Naïve Bayes Model
# Separate the income variable being predicted from the other variables in the dataset
training_attributes <- training_v2[,-11]
training_income <- training_v2$income
# Build the naïve bayes model
naïve_bayes_model <- train(training_attributes, training_income, 'nb', trControl = trainControl(method = 'cv', number = 10))
# Use the naïve bayes model to predict the income variable for the validation_v2 dataset
naïve_bayes_prediction <- predict(naïve_bayes_model, validation_v2)
# Display confusion matrix results for the naïve bayes model
naive_bayes_cm <- confusionMatrix(data = naïve_bayes_prediction, reference = validation_v2$income, positive = '>50K')
naive_bayes_cm

# Random Forest Model
# Build the random forest model
random_forest_model <- randomForest(income ~ ., data = training_v2, ntree = 2500)
# Use the random forest model to predict the income variable for the validation_v2 dataset
random_forest_prediction <- predict(random_forest_model, validation_v2)
# Display confusion matrix results for the random forest model
random_forest_cm <- confusionMatrix(data = random_forest_prediction, reference = validation_v2$income, positive = '>50K')
random_forest_cm

### Results
# Display confusion matrix results for both models
naive_bayes_cm
random_forest_cm