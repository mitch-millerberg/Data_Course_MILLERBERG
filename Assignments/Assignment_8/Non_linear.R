#Assignment_8

# 3) Write the code you would use to model the data found in “/Data/non_linear_relationship.csv” 
# with a linear model (there are a few ways of doing this)

# load the “/Data/mushroom_growth.csv” data set
Non_linear_df <- read.csv("../../Data/non_linear_relationship.csv")
head(Non_linear_df)

# A linear model & Summary 
linear_model <- lm(response ~ predictor, data = Non_linear_df)
summary(linear_model)

# ggplot Plot 
ggplot(linear_model, aes(y =predictor, x = response)) +
  geom_point(color="black")+
  labs(x= "response",y= "predictor")+
  ggtitle("Non_linear_relationship")
