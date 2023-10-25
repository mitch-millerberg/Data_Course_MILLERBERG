# # Mitchell Millerberg
# # Assignment 8
library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)
library(ggplot2)
library(GGally)

# install.packages("fitdistrplus")

# Hypothesis testing / Model fitting (with linear models and ANOVA)
# Model comparisons and selection
# Model interpretation 
# Model predictions

# Write a script that:
# ~ load the “/Data/mushroom_growth.csv” data set
# ~ creates several plots exploring relationships between the response and predictors
# ~ defines at least 4 models that explain the dependent variable “GrowthRate”
# ~ calculates the mean sq. error of each model
# ~ selects the best model you tried
# ~ adds predictions based on new hypothetical values for the independent variables used in your model
# ~ plots these predictions alongside the real data

# load the “/Data/mushroom_growth.csv” data set
data <- read.csv("../../Data/mushroom_growth.csv")

#review the data 
summary(data)
names(data)
str(data)
ggpairs(data, columns = c("Light", "Nitrogen", "Temperature", "GrowthRate"))

# several plots exploring relationships between the response and predictors
# Box plot for GrowthRate by Species
boxplot(GrowthRate ~ Species, data = data,
        xlab = "Species", ylab = "GrowthRate",
        main = "GrowthRate by Species")
# Box plot for GrowthRate by Light level
boxplot(GrowthRate ~ Light, data = data,
        xlab = "Light", ylab = "GrowthRate",
        main = "GrowthRate by Light")
# Box plot for GrowthRate by Nitrogen level
boxplot(GrowthRate ~ Nitrogen, data = data,
        xlab = "Nitrogen", ylab = "GrowthRate",
        main = "GrowthRate by Nitrogen")
# Box plot for the relationship between GrowthRate and Humidity
boxplot(GrowthRate ~ Humidity, data = data,
        xlab = "Humidity", ylab = "GrowthRate",
        main = "GrowthRate vs. Humidity")

# Bar plot for GrowthRate by Humidity
ggplot(data, aes(x = Humidity, y = GrowthRate, fill = Humidity)) +
  geom_bar(stat = "identity") +
  xlab("Humidity") +
  ylab("GrowthRate") +
  ggtitle("GrowthRate by Humidity")

# Scatterplot between GrowthRate and numeric predictors
plot(data$Light, data$GrowthRate, xlab = "Light", ylab = "GrowthRate",
     main = "Scatterplot: Light vs. GrowthRate")
plot(data$Nitrogen, data$GrowthRate, xlab = "Nitrogen", ylab = "GrowthRate",
     main = "Scatterplot: Nitrogen vs. GrowthRate")
plot(data$Temperature, data$GrowthRate, xlab = "Temperature", ylab = "GrowthRate",
     main = "Scatterplot: Temperature vs. GrowthRate")

#scatter plot for light by Growthrate
ggplot(data, aes(x = Light, y = GrowthRate)) +
  geom_point() + 
  geom_smooth(method = "lm")
#scatter plot for Nitrogen by Growthrate
ggplot(data, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm")
#scatter plot for Humidity by Growthrate
ggplot(data, aes(x = Humidity, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm")
#scatter plot for Temperature by Growthrate
ggplot(data, aes(x = Temperature, y = GrowthRate)) +
  geom_point() + 
  geom_smooth(method = "lm")

# cor(data$GrowthRate, data$Light)        # Correlation between GrowthRate and Light
# cor(data$GrowthRate, data$Nitrogen)     # Correlation between GrowthRate and Nitrogen
# cor(data$GrowthRate, data$Temperature)  # Correlation between GrowthRate and Temperature


# # 4 models that explain the dependent variable “GrowthRate”
# Model 1: Linear Regression with Light and Nitrogen
mod1 <- lm(GrowthRate ~ Light + Nitrogen, data = data)

# Model 2: Multiple Linear Regression with Light, Nitrogen, and Temperature
mod2 <- lm(GrowthRate ~ Light + Nitrogen + Temperature, data = data)
 
# Model 3: Linear Regression with Light and a Quadratic Term for Temperature
mod3 <- lm(GrowthRate ~ Light + I(Temperature^2), data = data)

# Model 4: Linear Regression with Interaction between Light and Nitrogen
mod4 <- lm(GrowthRate ~ Light * Nitrogen, data = data)

# Calculate Mean Squared Error for each model
mse1 <- mean(mod1$residuals^2)
mse2 <- mean(mod2$residuals^2)
mse3 <- mean(mod3$residuals^2)
mse4 <- mean(mod4$residuals^2)

# Identify the best model based on the lowest MSE
# The model with the lowest RMSE or the highest R-squared value is generally considered the best.
# Create a function to calculate RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))}

mod1 <- lm(GrowthRate ~ Light + Nitrogen, data = data)
mod2 <- lm(GrowthRate ~ Light + Nitrogen + Temperature, data = data)
mod3 <- lm(GrowthRate ~ Light + I(Temperature^2), data = data)
mod4 <- lm(GrowthRate ~ Light * Nitrogen, data = data)
# Calculate RMSE for each model
rmse1 <- rmse(data$GrowthRate, predict(mod1))
rmse2 <- rmse(data$GrowthRate, predict(mod2))
rmse3 <- rmse(data$GrowthRate, predict(mod3))
rmse4 <- rmse(data$GrowthRate, predict(mod4))

# Print RMSE values
cat("Root Mean Square Error for mod1:", rmse1, "\n")
cat("Root Mean Square Error for mod2:", rmse2, "\n")
cat("Root Mean Square Error for mod3:", rmse3, "\n")
cat("Root Mean Square Error for mod4:", rmse4, "\n")

# Store the RMSE values in a vector
rmse_values <- c(rmse1, rmse2, rmse3, rmse4)

# Find the index of the best model (lowest RMSE)
best_model_index <- which.min(rmse_values)

# The best model
cat("The best model is mod", best_model_index, "\n")

best_model <- mod2
# Generate new hypothetical values
new_data <- expand.grid(
  Light = c(30, 60, 90),  # hypothetical values for Light
  Nitrogen = c(0.1, 0.3, 0.5),  # hypothetical values for Nitrogen
  Humidity = c("Low","High"),  # hypothetical values for Humidity
  Temperature = c(20, 25, 35))  # hypothetical values for Temperature
report(best_model)
# Combine new data and predictions
new_data$PredictedGrowthRate <- predict(best_model, newdata = new_data)

# Plot predictions alongside the real data
ggplot(data, aes(x = Light, y = GrowthRate)) +
  geom_point() +
  geom_point(data = new_data, aes(y = PredictedGrowthRate, color = "Predicted")) +
  labs(x = "Light", y = "GrowthRate") +
  scale_color_manual(values = c("Predicted" = "purple")) +
  ggtitle("Real Data vs. Predicted Data")

