setwd("~/Documents/KHUSHBU/MS/py_scripts_main/GitHub/Modeling-of-COVID-19-GovMeasures-deaths")

library(dplyr)
library (ggplot2)
library(caret)
library(nlstools)
library(pscl)

df0 <- read.csv(file="HS614_COVID_dataset.csv", header=TRUE)
head(df0)

df0$date <- as.Date(df0$date, "%m/%d/%y")

df1 <- select(df0, c(country, tagged_day, new_confirmed, new_deaths, partial_lockdown, flights_china, school_closure))
head(df1)

summary(df1)

df1_italy <- subset(df1, country =="Italy")
df1_korea <- subset(df1, country == "Korea, South")

plot(df1_italy$tagged_day, df1_italy$new_deaths)
plot(df1_korea$tagged_day, df1_korea$new_deaths)

# correlation
cor(df1[, 2:7], use = "pairwise.complete.obs", method = "pearson")

##############################################################################################################
# trying polynomial linear regression for italy - didnt give good predictions

fit_poly <- lm(new_deaths ~ tagged_day + new_confirmed + flights_china +
                 I(tagged_day^2)+ I(new_confirmed^2) + I(flights_china^2),data = df1_italy[1:60, ])
summary(fit_poly)

#removing all SIP variable : flights_china
fit_poly2 <- lm(new_deaths ~ tagged_day + new_confirmed +
                  I(tagged_day^2)+ I(new_confirmed^2),data = df1_italy[1:60, ])
summary(fit_poly2)

#removing new_confirmed
fit_poly3 <- lm(new_deaths ~ tagged_day + I(tagged_day^2)+ I(new_confirmed^2),data = df1_italy[1:60, ])
summary(fit_poly3)

#removing tagged day^2
fit_poly4 <- lm(new_deaths ~ tagged_day + I(new_confirmed^2),data = df1_italy[1:60, ])
summary(fit_poly4)

plot(fit_poly4, 1)
plot(fit_poly4, 3)
plot(fit_poly4, 5)

# predictions
predictions <- fit_poly4 %>% predict(df1_italy[61:75, ])
actual60_predAfter61 <- c(df1_italy$new_deaths[1:60], predictions)
actual60_predAfter61 <- ceiling(actual60_predAfter61)
actual60_predAfter61

cbind(df1_italy$new_deaths, actual60_predAfter61)
# Model evaluation:
pR2(fit_poly4)
MAE(df1_italy$new_deaths[61:75], predictions) #235.0996

ggplot() + geom_point(aes(x =df1_italy$tagged_day,y = actual60_predAfter61,colour= "predicted")) + 
  geom_smooth(aes(x =df1_italy$tagged_day, y = actual60_predAfter61,colour = "predicted")) + 
  geom_point(aes(x =df1_italy$tagged_day,y = df1_italy$new_deaths, colour = "actual")) + 
  geom_smooth(aes(x =df1_italy$tagged_day,y = df1_italy$new_deaths,colour = "actual")) + 
  xlab("Count of #days since 20th confirmed case") + ylab("Daily new deaths") + 
  ggtitle("Predicted vs Actual new deaths/day for Italy")

######################################################################################################
# trying polynomial linear regression for korea

fit_poly <- lm(new_deaths ~ tagged_day + new_confirmed + flights_china +
                 I(tagged_day^2)+ I(new_confirmed^2) + I(flights_china^2),data = df1_korea[1:60, ])
summary(fit_poly)

#removing all SIP variable : flights_china
fit_poly2 <- lm(new_deaths ~ tagged_day + new_confirmed +
                  I(tagged_day^2)+ I(new_confirmed^2),data = df1_korea[1:60, ])
summary(fit_poly2)

#removing tagged day^2
fit_poly3 <- lm(new_deaths ~ tagged_day +  new_confirmed + I(new_confirmed^2),data = df1_korea[1:60, ])
summary(fit_poly3)

plot(fit_poly3, 1)
plot(fit_poly3, 3)
plot(fit_poly3, 5)

# predictions
predictions <- fit_poly3 %>% predict(df1_korea[61:90, ])
actual60_predAfter61 <- c(df1_korea$new_deaths[1:60], predictions)
actual60_predAfter61 <- ceiling(actual60_predAfter61)
actual60_predAfter61

cbind(df1_korea$new_deaths, actual60_predAfter61)

# Model evaluation:
pR2(fit_poly3)
MAE(df1_korea$new_deaths[61:90], predictions) #4.409849

ggplot() +   geom_point(aes(x =df1_korea$tagged_day,y = actual60_predAfter61,colour= "predicted")) + 
  geom_smooth(aes(x =df1_korea$tagged_day,y = actual60_predAfter61,colour = "predicted")) + 
  geom_point(aes(x =df1_korea$tagged_day,y = df1_korea$new_deaths, colour = "actual")) + 
  geom_smooth(aes(x =df1_korea$tagged_day,y = df1_korea$new_deaths,colour = "actual")) + 
  xlab("tagged days") + ylab("New_deaths") + ggtitle("Predicted vs Actual new_deaths for Korea")

##############################################################################################################
# trying negative binomial regression for italy- didn't give good predictions

library(MASS)
library(rgl)
library(pwr)
library(car)

fit_nbr <- glm.nb(new_deaths ~ tagged_day + new_confirmed + flights_china, data=df1_italy[1:60, ])
summary(fit_nbr)

#removing flight_china
fit_nbr2 <- glm.nb(new_deaths ~ tagged_day + new_confirmed, data=df1_italy[1:60, ])
summary(fit_nbr2)

fit_nbr2$theta
# anova(fit_nbr)

# predictions
predictions <- fit_nbr2 %>% predict(df1_italy[61:75, ])
actual60_predAfter61 <- c(df1_italy$new_deaths[1:60], predictions)
actual60_predAfter61 <- ceiling(actual60_predAfter61)
actual60_predAfter61

cbind(df1_italy$new_deaths, actual60_predAfter61)

# Model evaluation:
pR2(fit_nbr2)
MAE(df1_italy$new_deaths[61:75], predictions) #340.466

ggplot() + geom_point(aes(x =df1_italy$tagged_day, y = actual60_predAfter61,colour= "predicted")) + 
  geom_smooth(aes(x =df1_italy$tagged_day,y = actual60_predAfter61,colour = "predicted")) +
  geom_point(aes(x =df1_italy$tagged_day,y = df1_italy$new_deaths, colour = "actual")) + 
  geom_smooth(aes(x =df1_italy$tagged_day,y = df1_italy$new_deaths, colour = "actual")) +
  xlab("tagged days") + ylab("New_deaths") + ggtitle("Predicted vs Actual new_deaths for Italy")

##############################################################################################################
# trying negative binomial regression for korea- didn't give good predictions

fit_nbr <- glm.nb(new_deaths ~ tagged_day + new_confirmed + flights_china, data=df1_korea[1:60, ])
summary(fit_nbr)

#removing flight_china
fit_nbr2 <- glm.nb(new_deaths ~ tagged_day + new_confirmed, data=df1_korea[1:60, ])
summary(fit_nbr2)

fit_nbr2$theta
# anova(fit_nbr)

# predictions
predictions <- fit_nbr2 %>% predict(df1_korea[61:90, ])
actual60_predAfter61 <- c(df1_korea$new_deaths[1:60], predictions)
actual60_predAfter61 <- ceiling(actual60_predAfter61)
actual60_predAfter61

cbind(df1_korea$new_deaths, actual60_predAfter61)

# Model evaluation:
pR2(fit_nbr2)
MAE(df1_korea$new_deaths[61:90], predictions) #1.864649

ggplot() + geom_point(aes(x =df1_korea$tagged_day, y = actual60_predAfter61,colour= "predicted")) + 
  geom_smooth(aes(x =df1_korea$tagged_day,y = actual60_predAfter61,colour = "predicted")) +
  geom_point(aes(x =df1_korea$tagged_day,y = df1_korea$new_deaths, colour = "actual")) + 
  geom_smooth(aes(x =df1_korea$tagged_day,y = df1_korea$new_deaths, colour = "actual")) +
  xlab("tagged days") + ylab("New_deaths") + ggtitle("Predicted vs Actual new_deaths for South Korea")

##############################################################################################################
#trying lm for italy

fit_lm <- lm(new_deaths ~ tagged_day + new_confirmed + flights_china,data=df1_italy[1:60, ])
summary(fit_lm)

#removing flights_china
fit_lm2 <- lm(new_deaths ~ tagged_day + new_confirmed ,data=df1_italy[1:60, ])
summary(fit_lm2)

# predictions
predictions <- fit_lm2 %>% predict(df1_italy[61:75, ])
actual60_predAfter61 <- c(df1_italy$new_deaths[1:60], predictions)
actual60_predAfter61 <- ceiling(actual60_predAfter61)
actual60_predAfter61

cbind(df1_italy$new_deaths, actual60_predAfter61)

# Model evaluation:
pR2(fit_lm2)
MAE(df1_italy$new_deaths[61:75], predictions) #161.6996

ggplot() + geom_point(aes(x =df1_italy$tagged_day, y = actual60_predAfter61,colour= "predicted")) + 
  geom_smooth(aes(x =df1_italy$tagged_day,y = actual60_predAfter61,colour = "predicted")) +
  geom_point(aes(x =df1_italy$tagged_day,y = df1_italy$new_deaths, colour = "actual")) + 
  geom_smooth(aes(x =df1_italy$tagged_day,y = df1_italy$new_deaths, colour = "actual")) +
  xlab("Count of #days since 20th confirmed case") + ylab("Daily new deaths") + ggtitle("Predicted vs Actual new deaths/day for Italy")
#################################################################################
#trying lm for korea

fit_lm <- lm(new_deaths ~ tagged_day + new_confirmed + flights_china,data=df1_korea[1:60, ])
summary(fit_lm)

#removing flights_china
fit_lm2 <- lm(new_deaths ~ tagged_day + new_confirmed ,data=df1_korea[1:60, ])
summary(fit_lm2)

#removing new_confirmed
fit_lm3 <- lm(new_deaths ~ tagged_day ,data=df1_korea[1:60, ])
summary(fit_lm3)

# predictions
predictions <- fit_lm3 %>% predict(df1_korea[61:90, ])
actual60_predAfter61 <- c(df1_korea$new_deaths[1:60], predictions)
actual60_predAfter61 <- ceiling(actual60_predAfter61)
actual60_predAfter61

cbind(df1_korea$new_deaths, actual60_predAfter61)

# Model evaluation:
pR2(fit_lm3)
MAE(df1_korea$new_deaths[61:90], predictions) #5.798899

ggplot() + geom_point(aes(x =df1_korea$tagged_day, y = actual60_predAfter61,colour= "predicted")) + 
  geom_smooth(aes(x =df1_korea$tagged_day,y = actual60_predAfter61,colour = "predicted")) +
  geom_point(aes(x =df1_korea$tagged_day,y = df1_korea$new_deaths, colour = "actual")) + 
  geom_smooth(aes(x =df1_korea$tagged_day,y = df1_korea$new_deaths, colour = "actual")) +
  xlab("Count of #days since 20th confirmed case") + ylab("Daily new deaths") +
  ggtitle("Predicted vs Actual new deaths/day for Korea")



##############################################################################################################
# Fitting Nonlinear Models with Scale Mixture of Skew-Normal Distributions - lib(‘nlsmsn’)

library(nlsmsn)

y <- df1_italy$new_deaths[1:60]
x <- df1_italy$tagged_day[1:60]

##Define non linear function
# population ~ theta1/(1 + exp(-(theta2 + theta3*year)))

nlf<-function(x,betas){
  resp<- betas[1]/(1 +betas[2]*exp(-betas[3]*x))
  return(resp)
}

# get initital values
betas1.0 <- x[which.max(y)]
# betas2.0 <- 

#########################################################################################################




