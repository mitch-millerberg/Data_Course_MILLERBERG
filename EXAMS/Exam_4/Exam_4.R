
#Mitchell Millerberg

# # YOUR TASKS:
# 1. Read in the unicef data (10 pts) 
# 2. Get it into tidy format (10 pts) 
# 3. Plot each country’s U5MR over time (20 points)
#
# - Create a line plot (not a smooth trend line) for each country
# - Facet by continent
# 
# 4. Save this plot as LASTNAME_Plot_1.png (5 pts) 
# 5. Create another plot that shows the mean U5MR for all the countries within a given continent at each year (20 pts)
# 
# - Another line plot (not smooth trendline)
# - Colored by continent
# 6. Save that plot as LASTNAME_Plot_2.png (5 pts) 7. Create three models of U5MR (20 pts)
# 
# - mod1 should account for only Year
# - mod2 should account for Year and Continent
# - mod3 should account for Year, Continent, and their interaction term
# 8. Compare the three models with respect to their performance
# 
# - Your code should do the comparing
# - Include a comment line explaining which of these three models you think is best
# 9. Plot the 3 models’ predictions like so: (10 pts)
# Define the file path
# 10. BONUS - Using your preferred model, predict what the U5MR would be for Ecuador in the year 2020. The real value for Ecuador for 2020 was 13 under-5 deaths per 1000 live births. How far off was your model prediction???
#   
#   Your code should predict the value using the model and calculate the difference between the model prediction and the real value (13).
# 
# Source: https://data.unicef.org/country/ecu/
#   
#   Create any model of your choosing that improves upon this “Ecuadorian measure of model correctness.” By transforming the data, I was able to find a model that predicted Ecuador would have a U5MR of 16.61…not too far off from reality
library(tidyverse)
library(tidyr)
library(readxl)
library(janitor)
library(lubridate)
library(gganimate)
library(transformr)
library(ggplot2)
library(stringr)
library(gganimate)
library(gifski)
library(dplyr)
library(sf)
library(gplots)
library(scales)
library(easystats)


file_path <- "unicef-u5mr.csv"

# wrong # file_path <- "./unicef-u5mr.csv"
# file_path <- "././unicef-u5mr.csv"
# Read the CSV file into a data frame
data <- read.csv(file_path)
summary(data)
names(data)

#clean the data pivot longer
tidy_data <- data %>%
  pivot_longer(cols = starts_with("U5MR."),  # Select columns starting with "U5MR."
               names_to = "year",            # Create a new column "year"
               values_to = "U5MR")           # Name the values column "U5MR"

#mutate the data to get rid of the U5MR
tidy_data <- tidy_data %>%
  mutate(year = str_remove(year, "U5MR."))
tidy_data$year <- as.integer(tidy_data$year) #change year into an integer 

# Plot each country’s U5MR over time (20 points)
# - Create a line plot (not a smooth trend line) for each country
# - Facet by continent
ggplot(tidy_data, aes(x = year, y = U5MR, color = CountryName))+
  geom_line() +
  facet_wrap(~ Continent) +
  labs(x = "Year", y = "U5MR") +
  ggtitle("U5MR Over Time by Country and Continent") +
  theme_minimal() +
  guides(color = FALSE)

 
##
# Create another plot that shows the mean U5MR for all the countries within a given continent at each year.
# - Another line plot 
# Calculate the mean U5MR for each year and continent
mean_data <- tidy_data %>%
  group_by(year, Continent) %>%
  summarise(Mean_U5MR = mean(U5MR, na.rm = TRUE))

# Create the line plot
ggplot(mean_data, aes(x = year, y = Mean_U5MR, color = Continent)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Mean U5MR") +
  ggtitle("Mean U5MR Over Time by Continent") +
  theme_minimal() +
  scale_color_manual(values = c("Asia" = "green",
                                "Europe" = "skyblue",
                                "Africa" = "red",
                                "Americas" = "#81a637",
                                "Oceania" = "purple"))

# Create three models of U5MR
# 
# - mod1 should account for only Year
# - mod2 should account for Year and Continent
# - mod3 should account for Year, Continent, and their interaction term

# - Compare the three models with respect to their performance
# - Your code should do the comparing
# - Include a comment lines explaining which of these three models you think is best.
# Plot the 3 models’ predictions
# Generate prediction data
# Get unique years and continents from your dataset

# Create three models of U5MR using tidy_data

# Model 1: Year
mod1 <- lm(U5MR ~ year, data = tidy_data)

# Model 2: Year and Continent
mod2 <- lm(U5MR ~ year + Continent, data = tidy_data)

# Model 3: Year, Continent, and Interaction
mod3 <- lm(U5MR ~ year * Continent, data = tidy_data)


# - Compare the three models with respect to their performance
# Assess model performance
summary(mod1)
summary(mod2)
summary(mod3)

# Compare AIC (Akaike Information Criterion) or BIC (Bayesian Information Criterion)
compare_performance(mod1,mod2,mod3) %>% plot
compare_models(mod1,mod2,mod3)
# Compare models using ANOVA
anova(mod1, mod2, mod3)

#compare AIC and BIC
AIC(mod1, mod2, mod3) 
BIC(mod1, mod2, mod3) 


# concision comment explaining which of these three models is the best.
# mod3 is the best model out of  the three: mod1, mod2, and mod3.
# Here's why:
# AIC and BIC: Both AIC and BIC are lower for mod3 compared to the other models.
# Lower AIC and BIC values indicate a better model fit. 
# R-squared (R2): mod3 has the highest R-squared value (0.640) R-squared measures 
# the proportion of the variance in the dependent variable that's explained by the 
# independent variables.
# It is important to consider he best model selection also depends on the specific context and the goals of your analysis. 


# Plot the 3 models’ predictions
# Generate prediction data
# Get unique years and continents from your dataset
unique_years <- unique(tidy_data$year)
unique_continents <- unique(tidy_data$Continent)

# Create a grid of all combinations of years and continents
new_data <- expand.grid(year = unique_years, Continent = unique_continents)


# Predict values from the models
predictions1 <- predict(mod1, newdata = new_data)
predictions2 <- predict(mod2, newdata = new_data)
predictions3 <- predict(mod3, newdata = new_data)

# Combine the predictions into a new data frame with the new_data
predictions_data <- data.frame(new_data, mod1 = predictions1, mod2 = predictions2, mod3 = predictions3)

# Pivot the data into tidy format for ggplot
predictions_data_long <- pivot_longer(predictions_data, cols = c(mod1, mod2, mod3), names_to = "Model")

# Create the line plot
# Create the line plot with facet by Model
ggplot(predictions_data_long, aes(x = year, y = value, color = Continent)) + # color by Continent
  geom_line(linewidth = 1) +
  facet_wrap(~ Model) +  #  facet by Model
  labs(x = "Year", y = "U5MR") +
  ggtitle("Model Predictions by Year and Continent")

# 10. BONUS - Using your preferred model, predict what the U5MR would be for Ecuador in the year 2020. 
# The real value for Ecuador for 2020 was 13 under-5 deaths per 1000 live births. How far off was your model prediction???
# Your code should predict the value using the model and calculate the difference between the model prediction and the real value (13).
# 
# Source: https://data.unicef.org/country/ecu/
#   
# Create any model of your choosing that improves upon this “Ecuadorian measure of model correctness.”
# By transforming the data, I was able to find a model that predicted Ecuador would have a U5MR of 16.61…not too far off from reality

# Create a data frame with the year 2020 and Continent for Ecuador
new_data_ecuador <- data.frame(year = 2020, Continent = "Americas")

# Predict U5MR for Ecuador in 2020 using mod3
predicted_U5MR_ecuador <- predict(mod3, new_data_ecuador)

# The prediction
cat("Predicted U5MR for Ecuador in 2020:", predicted_U5MR_ecuador, "\n")
# How far off was your model prediction???
cat("Difference from Real Value:", difference, "\n")
#-10.58018  was NOT close to 12.5 DEATHS PER 1,000 LIVE BIRTHS, the difference from real value is 27.21058

# A "better" linear regression model
better_model <- lm(U5MR ~ poly(year, 3) * Continent, data = tidy_data)

#compare "better" model and mod3
compare_performance(mod3,better_model) %>% plot
AIC(mod3,better_model) 
BIC(mod3,better_model) 

# Predict U5MR for Ecuador in 2020 using the new model
predicted_U5MR_ecuador_new <- predict(better_model, newdata = new_data_ecuador)

# Print the new prediction and the difference
cat("Predicted U5MR for Ecuador in 2020 (New Model):", predicted_U5MR_ecuador_new, "\n")
cat("Difference from Real Value:", difference, "\n")