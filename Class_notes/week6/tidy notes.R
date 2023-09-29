#r notes  cleaing excel data 
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

#read in file from excel and then clean it 

df1 <- read_xlsx("popquiz data.xlsx") 

%>% 

clean_names() #helps clean the names of the column

df1$'week 1'

#fix names col1
names(df1)[2] <- "site"
df1

dates <- janitor::excel_numeric_to_date(df1$site[1:3]))
class(dates)
#pull the month name abbreevation into charter
part1 <- lubridate::month(dates,label= TRUE,abbr = TRUE) %>% 
  str_to_upper()
#pull the site numbers from day of month
part2 <- lubridate::day(dates)

#paste them together
paste(part1,part2,sep="-")
#add them back to the data frame
df1site[1:3] <- finalproduct
df1 %>% 
  seperate(site, into = c("location","site")) %>% 
  pivot_longer(starts_with("week"), #pivot longer
               names_to = "week",
               values_to = "rel_abund",
               names_prefix = "week_", names_transform = as.numeric) #chops off 


##### df2

read_xlsx("~/Downloads/organized dataset.xlsx") %>% 
  clean_names()

df2$site <- 
dft$site %>%
  str_replace(" Pool","Pool")

df2 %>% 
  separate(site, into = c("location","site")) %>% 
  pivot_longer(starts_with("week"), #pivot longer
               names_to = "week",
               values_to = "rel_abund",
               names_prefix = "week_", names_transform = as.numeric) #chops off 
identical(df1,df2)

df2 
mutate(new_location = case_when(loacation == "SewagePool" ~ "SEP",
                                location == "Hatchery" ~ "HAT")) # change the new location to location when you know it worsk

