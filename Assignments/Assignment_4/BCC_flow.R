# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(hms)
str(BCC_time)
# Read the data from the CSV file

BCC_data <- read.csv("./pems_output_BCC.csv",  header = TRUE, sep = ",")
BCC_data <- BCC_data %>% select(Time,Minimum,Mean,Maximum)

BCC_time <- BCC_data %>% mutate(Time=as.POSIXct(strptime(Time,format= "%H:%M")),
                                Mean=as.numeric(gsub(",","",Mean))) %>% 
  mutate(Time=as_hms(Time))

BCC_time

# Create a ggplot
  ggplot(BCC_time, aes(x = Time, y = Mean)) +
  geom_line() +
 
labs(
    title = "Flow of Vehicles between 01/01/2023 To 03/31/2023)",
    x = "Time",
    y = "Mean vehicles"
  ) +
  theme_minimal()
  