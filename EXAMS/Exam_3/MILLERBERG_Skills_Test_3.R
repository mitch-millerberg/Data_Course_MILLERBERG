#exam 3 
#Mitchell Millerberg

#EXAM_3: Complete the tasks below in a markdown script called LASTNAME_Skills_Test_3.Rmd
# 1. Load and clean FacultySalaries_1995.csv
# 2. Build an ANOVA model and display the summary output in your report.
# 3. The rest of the test uses another data set. 
# 4. The “Juniper_Oils.csv” data. 
#    Get it loaded and take a look. Then tidy it! (show the code used for tidying in your report)
# 5. Use a generalized linear model to find which chemicals show concentrations that are 
#    significant, as in P < 0.05) affected by “Years Since Burn”.
# 5.5 . Produce a data frame showing only statistically significant chemicals and models
#    include coefficient estimates, standard error, statistic and p value
#
library(skimr)
library(janitor)
library(tidyr)
library(broom)
library(tidyverse)
library(readxl)
library(lubridate)
library(gganimate)
library(transformr)
library(stringr)
library(gganimate)
library(gifski)
library(dplyr)
library(sf)
library(gplots)
library(scales)
library(easystats)
library(ggplot2)


# Load FacultySalaries_1995.csv
#data1 <- read_csv("./FacultySalaries_1995.csv")

file_path <- "FacultySalaries_1995.csv"

data1 <- read.csv(file_path)
# clean FacultySalaries_1995.csv
# used pivot_longer to combine salary, compensation and number columns
# then changed names in Rank variable
data1 <- data1 %>% 
  pivot_longer(cols = c("AvgFullProfSalary", "AvgAssocProfSalary", "AvgAssistProfSalary"),
               names_to = "Rank",
               values_to = "Salary") %>% 
  pivot_longer(cols = c("AvgFullProfComp", "AvgAssocProfComp", "AvgAssistProfComp"),
               names_to = "Rank_Comp",
               values_to = "Compensation") %>% 
  pivot_longer(cols = c("NumFullProfs", "NumAssocProfs", "NumAssistProfs", "NumInstructors"),
               names_to = "Rank_Num",
               values_to = "Staff_Number") %>% 
  mutate(Rank = case_when(Rank == "AvgFullProfSalary" ~ "Full",
                          Rank == "AvgAssocProfSalary" ~ "Assoc",
                          Rank == "AvgAssistProfSalary" ~ "Assist"))

# removed VIIB schools
data1 <- data1[!(data1$Tier=="VIIB"), ]

# re-created fig1.png
data1 %>% 
  ggplot(aes(x=Rank,y=Salary)) +
  geom_boxplot(aes(fill=Rank)) +
  facet_wrap(~Tier) +
  theme_minimal() +
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 19, color = "black"),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.x = element_text(angle = 65, size = 13, color = "black"),
        axis.text.y = element_text(size = 13, color = "black"),
        axis.title.x = element_text(size = 19, color = "black"),
        axis.title.y = element_text(size = 19, color = "black"),
        strip.text.x = element_text(size = 13, color = "black"))

# exported as millerberg_Fig_1.jpg
ggsave("./millerberg_Fig_1.jpg")

# 2. Build an ANOVA model and display the summary output in your report.
# create a model showing how Rank,state, and tier impact the salary.
model1 <- glm(data = data1, 
            formula = Salary ~ Rank + State + Tier )
summary(model1)

# ANOVA of the formula
anova(model1)
summary(model1)

# export ANOVA table as Salary_ANOVA_Summary.txt
sink("./Salary_ANOVA_Summary.txt") #divert the output to a file instead of the console. 
anova(model1)
sink(NULL) #This line of code stops redirecting the output to the file.

# 3. clean and tidy the next data set “Juniper_Oils.csv” data.
# Load and tidy Juniper_Oils.csv
data2 <- read_csv("./Juniper_Oils.csv")

# combined chemicals into concentration and ID
data2 <- data2 %>% 
  pivot_longer(cols = c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene",
                        "alpha-cedrene","beta-cedrene","cis-thujopsene","alpha-himachalene",
                        "beta-chamigrene","cuparene","compound 1","alpha-chamigrene",
                        "widdrol","cedrol","beta-acorenol","alpha-acorenol","gamma-eudesmol",
                        "beta-eudesmol","alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol",
                        "compound 2","thujopsenal"),
               names_to = "ChemicalID",
               values_to = "Concentration")


# 4. Make another graph:
data2 %>% 
  ggplot(aes(x=YearsSinceBurn, y=Concentration)) +
  facet_wrap(~ChemicalID, scales = "free_y") +
  geom_smooth(method = loess) +
  theme_minimal() +
  labs(x = "Years Since Burn",
       title = "Concentration by Chemical") +
  theme(plot.title = element_text(hjust = 0.5))

# export to millerberg_Fig_2.jpg
ggsave("./millerberg_Fig_2.jpg")

# 5. Use a generalized linear model to find which chemicals show concentrations that are 
# significant, as in P < 0.05) affected by “Years Since Burn”.
# create glm to show statistical significance of time and chemical on concentration
# Produce a data frame showing only statistically significant chemicals and models
# include coefficient estimates, standard error, statistic and p value
# tidy tibble
model2 <- glm(data = data2, formula = Concentration ~ ChemicalID * YearsSinceBurn)

model2_sum <- summary(model2)

tidy <- tidy(model2)

#tidy tibble subsetted to show only statistically significant ChemicalID's
ttss <- tidy[c(tidy$p.value <= 0.05), ]
print(ttss)
