
#Mitchell Millerberg
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

