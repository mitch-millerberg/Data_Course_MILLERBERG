# Assignment 9
# In this assignment, you will use R (within R-Studio) to:
#   
# Load and clean a real data set
# Conduct exploratory analyses, including informative figures
# Build and test appropriate models
# Draw conclusions about your data
# Combine all of the above into a well-documented R-Markdown report and export (knit) it into an HTML file


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
library(skimr)
library(GGally)
library(jtools)
install.packages("jtools")

# Load the data 
GradData <- read.csv("../../Data/GradSchool_Admissions.csv")
names(GradData)
skim(GradData)
summary(GradData)

# Create a heatmap
heatmap <- ggplot(GradData, aes(x = gre, y = gpa, fill = admit)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = "Heatmap of GRE vs. GPA by Acceptance",
       x = "GRE Score",
       y = "GPA",
       fill = "Acceptance") +
  theme_minimal()

# Print the heatmap
print(heatmap)

#plot histograms for the numerical predictors
hist(GradData$gre, col = "blue", xlab = "GRE Score", main = "Histogram of GRE Scores")
hist(GradData$gpa, col = "green", xlab = "GPA", main = "Histogram of GPAs")

#plot Bar chart for "admitance which is a binary outcome
barplot(table(GradData$admit), col = c("red", "green"),
        xlab = "Admissions Status", names.arg = c("Not Admitted", "Admitted"))

#plot scatterplot matrix to see potential realationshps between variables 
plot(GradData[,-1],pch= 20, col = GradData$admit)

GradData %>% 
  select(admit,gpa,gre) %>% 
  ggpairs()

# Box plot to visualize the relationship between GRE scores and admission
plot1 <- ggplot(GradData, aes(x = admit, y = gre, fill = factor(admit))) +
  geom_boxplot() +
  labs(title = "GRE Scores vs. Admission",
       x = "Admission Status",
       y = "GRE Score") +
  scale_fill_manual(values = c("red", "green")) +
  theme(legend.key.size = unit(1.5, "cm"))

print(plot1)
  

# Box plot to visualize the relationship between GPA scores and admission
plot2 <- boxplot(GradData$gpa ~ GradData$admit, col = c("red", "green"),
        names = c("Not Admitted", "Admitted"),
        main = "GPA Scores vs. Admission",
        ylab = "GPA Score",
        xlab = "Admission Status")

# combine plot 1 and 2
# # Arrange the two plots side by side
# grid.arrange(plot1, plot2, ncol = 2)

#scatter plot all rankings
scatter_plot <- ggplot(GradData, aes(x = gre, y = gpa, color = factor(rank))) +
  geom_point() +
  labs(title = "Scatter Plot of GRE vs. GPA by Rank",
       x = "GRE Score",
       y = "GPA",
       color = "Rank") +
  scale_color_manual(values = c("black", "black", "black", "black")) +
  theme_minimal()
print(scatter_plot)

# 
# rank_filter
selected_rank <- 3

# Scatter plot filtered by rank
scatter_plot <- ggplot(GradData, aes(x = gre, y = gpa, color = factor(rank))) +
  geom_point(data = subset(GradData, rank == selected_rank)) +  # Filter the data
  labs(title = paste("Scatter Plot of GRE vs. GPA for Rank", selected_rank),
       x = "GRE Score",
       y = "GPA",
       color = "Rank") +
  scale_color_manual(values = c("green")) +
  theme_minimal()

# Print the scatter plot
print(scatter_plot)

# Create the logistic regression model
model <- glm(admit ~ gre + gpa + rank, data = GradData, family = binomial)

# Add the model's predicted probabilities to the scatter plot
scatter_plot_with_model <- scatter_plot +
  geom_smooth(method = "glm", formula = y ~ x, data = GradData,
              method.args = list(family = binomial), color = "limegreen", linetype = "dashed")

scatter_plot_with_model


# Explore and model the predictors of graduate school admission
# Scatter plot of GRE vs. Admit
plot(GradData$gre, GradData$admit, main = "GRE vs. Admission Status",
     xlab = "GRE Score", ylab = "Admission Status")

# Scatter plot of GPA vs. Admit
plot(GradData$gpa, GradData$admit, main = "GPA vs. Admission Status",
     xlab = "GPA", ylab = "Admission Status")


# Fit a logistic regression model
model <- glm(admit ~ gre + gpa + rank, data = GradData, family = binomial)

model1 <- glm(admit ~ gpa, data = GradData, family = binomial)
model2 <- lm(gre ~ gpa, data = GradData)
print(model)
summary(model1)
summary(model2)

# Conclusions:
# You can conclude the following based on the exploratory plots and the logistic regression model:
#   
# GRE scores and GPA appear to be positively associated with admission.
# A higher rank (lower number) of the undergraduate institution indicates a higher probability of admission.
# The logistic regression model will provide insights into the significance of these variables and how they collectively affect the likelihood of admission.