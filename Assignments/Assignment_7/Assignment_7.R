#Assignment 7
#Mitchell Millerberg
# In this assignment, you will use R (within R-Studio) to:
# 1) Take a real life data set and wrangle it into shape
# 2) Make some exploratory analyses on that cleaned data
# 3) Explore the cleaned data set with a series of figures 
# (I want to see you exploring the data set)

# 4) Address the questions:
    #“Does population of a county correlate with the proportion of any specific 
    #  religious group in that county?”

    #“Does proportion of any specific religion in a given county 
    #  correlate with the proportion of non-religious people?”

# 5) Stick to figures and maybe correlation indices
# 6) Add comment lines that show your thought processes

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



# Load the data and create the Location column
data <- read.csv("../../Data/Utah_Religions_by_County.csv") %>% 
clean_names()
names(data)

#more cleaning, I want the type of religion to be a feature that is filterable 
data_gather <- data %>%
  gather(key = "Religion", value = "Proportion", -county, -pop_2010, -religious, -non_religious)

#“Does population of a county correlate with the proportion of any specific religious group
#in that county?”

#Subset the data
subset_data <- data %>%
  select(county, pop_2010, muslim)  # Replace 'religion' with the specific religious group of interest

#Calculate the proportion of the religious group
subset_data <- subset_data %>%
  mutate(religious_proportion = muslim / pop_2010)  # Replace 'religion' with the specific religious group column

#Create a scatterplot with regression line
ggplot(subset_data, aes(x = pop_2010, y = religious_proportion)) +
  geom_point() +  # Scatterplot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Regression line
  labs(
    x = "Population (2010)",
    y = "Proportion of Religious Group",
    title = "Correlation Between Population and Proportion of Religious Group Muslim"
  )

#Does proportion of any specific religion in a given county correlate with the proportion
# of non-religious people?
# Subset the data
subset2 <- data %>%
  select(county, religious, non_religious)  # Replace with specific columns for the religion and non-religious population

# Calculate the proportions
subset2 <- subset2 %>%
  mutate(
    religious_proportion = religious / (religious + non_religious),
    non_religious_proportion = non_religious / (religious + non_religious)
  )

# Create a scatter plot
ggplot(subset2, aes(x = religious_proportion, y = non_religious_proportion)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Regression line
  labs(
    x = "Proportion of Religious Group",
    y = "Proportion of Non-Religious",
    title = "Correlation Between Religious and Non-Religious Proportions"
  )


# Mapping religion data on a map of utah
# mydata <-  map_data("world")
# view(mydata)
#I Was trying to plot the religions on a GEO map. I ran out of 
# time for this more complex of a task

# Here are some plots using the gather data set.
names(data_gather)

# plot based on population and county
ggplot(data_gather, aes(x = county, y = pop_2010)) +
  geom_point() +
  labs(title = "Population and County",
       x = "County",
       y = "Population_Year_2010") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
 # facet_wrap(~ Religion, scales = "free") +
  
# plot based on Non Religious Percentage and county
ggplot(data_gather, aes(x = county, y = non_religious)) +
  geom_point() +
  labs(title = "Non Religious Percentage and County",
       x = "County",
       y = "Non Religious Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#facet_wrap(~ Religion, scales = "free") +

#violin plot
ggplot(data_gather, aes(x = Religion, y = Proportion)) +
  geom_violin(fill = "blue") +
  labs(x = "Religion", y = "Proportion") +
  ggtitle("Proportions of Religious Groups in Utah Counties") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# plot based on Religious Percentage and county
ggplot(data_gather, aes(x = county, y = religious)) +
  geom_point() +
  labs(title = "Religious Percentage and County",
       x = "County",
       y = "Religious Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# A bar plot by proportion and county 
ggplot(data_gather, aes(x = county, y = Proportion, fill = Religion)) +
  geom_bar(stat = "identity") +
  labs(x = "County", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Proportion of Religious Groups in Utah Counties")

# A boxplot
ggplot(data_gather, aes(x = Religion, y = Proportion, fill = Religion)) +
  geom_boxplot() +
  labs(x = "Religion", y = "Proportion") +
  ggtitle("Proportion of Religious Groups in Utah Counties (Boxplot)")+
theme(axis.text.x = element_text(angle = 45, hjust = 1))


# this heat map analysis didnt work out the way I thought it would.
#heatmap 
# data_heatmap_numeric <- data_heatmap[, -1]
# 
# # Convert the remaining data to a numeric matrix
# heatmap_data <- as.matrix(data_heatmap_numeric)
# 
# # Create the heatmap using the numeric matrix
# heatmap.2(heatmap_data, 
#           trace = "none",
#           Colv = "religion",
#           scale = "column",
#           dendrogram = "none",
#           key = TRUE,
#           keysize = 1.5,
#           cexCol = 0.8,
#           cexRow = 0.8,
#           margins = c(5, 10)
# )


# Calculate the proportion of the specific religious group within each county,
# dividing the count of that religious group by the population.
# Subset the data
subset_lds <- data %>%
  select(county, pop_2010, lds)

# Calculate the proportion of LDS adherents
subset_lds <- subset_lds %>%
  mutate(lds_proportion = lds / pop_2010)

# Calculate the correlation coefficient
correlation <- cor(subset_lds$pop_2010, subset_lds$lds_proportion)

# Print the correlation coefficient
cat("Correlation between Pop_2010 and LDS proportion:", correlation, "\n")

