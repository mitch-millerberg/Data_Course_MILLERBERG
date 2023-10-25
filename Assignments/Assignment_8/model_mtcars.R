# # Mitchell Millerberg
# # modeling mtcars
library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)
install.packages("fitdistrplus")

# Hypothesis testing / Model fitting (with linear models and ANOVA)
# Model comparisons and selection
# Model interpretation
# Model predictions

data("mtcars")
glimpse(mtcars)
mod1 <- lm(mpg~disp,data=mtcars)
summary(mod1)
#plot(mod1)
ggplot(mtcars,aes(x=disp,y=mpg))+
  geom_point()+
  geom_smooth(method ="lm")+
  theme_minimal()

mod2 <- lm(mpg ~ qsec, data = mtcars)
summary(mod2)
ggplot(mtcars, aes(x=disp,y=qsec))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_minimal()
mean(mod1$residuals^2)
mean(mod2$residuals^2)

df <- mtcars %>% 
  add_predictions(mod1)
df %>% dplyr::select("mpg","pred")

# Make a new dataframe with the predictor values we want to assess
# mod1 only has "disp" as a predictor so that's what we want to add here
# anything specified in the model needs to be here with exact matching column names
newdf <-  data.frame(disp = c(500,600,700,800,900)) 
# making predictions
pred <-  predict(mod1, newdata = newdf)

# combining hypothetical input data with hypothetical predictions into one new data frame
hyp_preds <- data.frame(disp = newdf$disp,
                        pred = pred)
# Add new column showing whether a data point is real or hypothetical
df$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"

# joining our real data and hypothetical data (with model predictions)
fullpreds <- full_join(df,hyp_preds)
#plot those predictions on our original graph
ggplot(fullpreds,aes(x=disp,y=pred,color=PredictionType))+
  geom_point()+
  geom_point(aes(y=mpg),color="Black")+
  theme_minimal()

# Define a 3rd model
mod3 <- glm(data=mtcars,
            formula = mpg ~ hp + disp + factor(am) + qsec)

# put all models into a list
mods <- list(mod1=mod1,mod2=mod2,mod3=mod3)
# apply "performance" function on all in the list and combine 
map(mods,performance) %>% reduce(full_join)

# gather residuals from all 3 models
mtcars %>% 
  gather_residuals(mod1,mod2,mod3) %>% 
  ggplot(aes(x=model,y=resid,fill=model)) +
  geom_boxplot(alpha=.5) +
  geom_point() + 
  theme_minimal()
# gather predictions from all 3 models
mtcars %>% 
  gather_predictions(mod1,mod2,mod3) %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point(size=3) +
  geom_point(aes(y=pred,color=model)) +
  geom_smooth(aes(y=pred,color=model)) +
  theme_minimal() +
  annotate("text",x=250,y=32,label=mod1$call) +
  annotate("text",x=250,y=30,label=mod2$call) +
  annotate("text",x=250,y=28,label=mod3$call)
report(mod3)
