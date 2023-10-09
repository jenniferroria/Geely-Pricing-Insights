# Load necessary libraries
library(tidyverse)
library(readr)
library(caret)
library(performance)
library(car)

# Load the data
car_data <- read_csv("CarPrice_Assignment.csv")
summary(car_data)

# Split the dataset into training and test dataset
set.seed(123)
train_index <- createDataPartition(car_data$price, p = 0.8, list = FALSE)
train_data <- car_data[train_index, ]
test_data <- car_data[-train_index, ]

# Create visualizations to evaluate the dataset (ScatterPlot, BoxPlot, Histogram/DensityPlot)
# Scatterplots
ggplot(train_data, aes(x = enginesize, y = price)) + geom_point()
ggplot(train_data, aes(x = horsepower, y = price)) + geom_point()
ggplot(train_data, aes(x = carwidth, y = price)) + geom_point()
ggplot(train_data, aes(x = citympg, y = price)) + geom_point()
ggplot(train_data, aes(x = highwaympg, y = price)) + geom_point()

# Boxplots
ggplot(train_data, aes(x = fueltype, y = price)) + geom_boxplot()
ggplot(train_data, aes(x = enginesize, y = price, group = 1)) + geom_boxplot()
ggplot(train_data, aes(x = horsepower, y = price, group = 1)) + geom_boxplot()
ggplot(train_data, aes(x = carwidth, y = price, group = 1)) + geom_boxplot()
ggplot(train_data, aes(x = citympg, y = price, group = 1)) + geom_boxplot()
ggplot(train_data, aes(x = highwaympg, y = price, group = 1)) + geom_boxplot()

# Histogram/DensityPlots
ggplot(train_data, aes(x = price)) + geom_histogram(binwidth = 3000) + geom_density(aes(y = ..count..), color = "red")
ggplot(train_data, aes(x = enginesize)) + geom_histogram(binwidth = 10) + geom_density(aes(y = ..count..), color = "red")
ggplot(train_data, aes(x = horsepower)) + geom_histogram(binwidth = 5) + geom_density(aes(y = ..count..), color = "red")
ggplot(train_data, aes(x = carwidth)) + geom_histogram(binwidth = 0.5) + geom_density(aes(y = ..count..), color = "red")
ggplot(train_data, aes(x = citympg)) + geom_histogram(binwidth = 3) + geom_density(aes(y = ..count..), color = "red")
ggplot(train_data, aes(x = highwaympg)) + geom_histogram(binwidth = 5) + geom_density(aes(y = ..count..), color = "red")

# Implement multiple linear regression
linear_model <- lm(price ~ enginesize + horsepower + carwidth + citympg + highwaympg, data = train_data)
summary(linear_model)

# Interaction model
interaction_model <- lm(price ~ enginesize * horsepower * carwidth * citympg * highwaympg, data = train_data)
summary(interaction_model)

# Residuals vs Fitted
plot(linear_model, which = 1)

# Normal Q-Q
plot(linear_model, which = 2)

# Scale-Location
plot(linear_model, which = 3)

# Cook's distance
plot(linear_model, which = 4)

# Predict test dataset
predictions <- predict(linear_model, newdata = test_data)

# Get performance
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

rmse(test_data$price, predictions)
