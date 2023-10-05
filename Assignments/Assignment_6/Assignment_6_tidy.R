#Assignment 6
#mitchell Millerberg
#In this assignment, you will use R (within R-Studio) to:
# 1) Load an untidy data set
# 2) Tidy it using tidyr and dplyr verbs
# 3) Plot it with ggplot
# 4) Cleans this data into tidy (long) form
# 5) Creates a new column specifying whether a sample is from soil or water
# 6) Generates 2 plots: 1) facet_wrap and 2) an animated plot

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

# Load the data and create the Location column
data <- read.csv("../../Data/BioLog_Plate_Data.csv")
#names(data)

#i want create a new column based on the information that is in column 1
# look at column 1 and if the value is "Clear_Creek",or "Waste_Water" then create a value "Water", everything else "Soil"
#I want to use pivotlonger to combine named "Hr_24", "Hr_48" and "Hr_144"
data <- data %>%
  mutate(Type = ifelse(Sample.ID %in% c("Clear_Creek", "Waste_Water"), "Water", "Soil"))

data_long <- data %>%
  pivot_longer(
    cols = c("Hr_24", "Hr_48", "Hr_144"),
    names_to = "Time",
    values_to = "Absorbance"
  )
# want to plot the data using ggplot. Use line ggplot that is facet wrapped on substrate and time. also filter  the
# Dilution == 0.1)
data_time <- data_long  %>%  mutate(Time =  as.numeric(str_remove_all(Time, "\\D+"))) 

data_time %>% 
 filter(Dilution == 0.1) %>% ggplot( aes(x = Time, y = Absorbance, color = Type)) +
  
geom_smooth(se = 0) +
   labs(x = "Time", y = "Absorbance") +
  facet_wrap(~ Substrate, scales = "free") +
  ggtitle("Dilution = 0.1") +
  scale_color_manual(values = c("Water" = "blue", "Soil" = "Red"),
                     labels = c("Soil", "Water")) +
  theme(legend.position = "right") +
  xlim(0, 150)



# Create the ggplot and animate it but only for the substrate “Itaconic Acid” 
#also facet_wrap it by Hr_24

plot <- data_time %>% 
filter(Substrate == "Itaconic Acid") %>% 
group_by(Sample.ID,Time,Dilution,Type) %>% summarise(mean_Absorbance=mean(Absorbance)) %>% 

ggplot(aes(x = Time, y = mean_Absorbance, color = Sample.ID)) +
  geom_line() +
  labs(x = "Time", y = "Mean_Absorbance") +
  facet_wrap(~ Dilution) +
transition_reveal(Time) 

animate(plot,nframes = 25, renderer = gifski_renderer("mean_absorbance_itaconic_animation.gif"))


   
