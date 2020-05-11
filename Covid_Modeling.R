setwd("~/Documents/KHUSHBU/MS/py_scripts_main/GitHub/Modeling-of-COVID-19-GovMeasures-deaths")

library(dplyr)
library (ggplot2)
library(caret)
library(nlstools)

# set.seed(1000)

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

# #Determine the nonlinear (weighted) least-squares estimates of
#  the parameters of a nonlinear model: Gaussian
#https://stats.stackexchange.com/questions/70153/linear-regression-best-polynomial-or-better-approach-to-use/70184#70184

y <- df1_italy$new_deaths
x <- df1_italy$tagged_day

# y_korea <- 
# x_korea <- 

# Define a Gaussian function (of four parameters)
f <- function(x, theta)  { 
  m <- theta[1]; s <- theta[2]; a <- theta[3]; b <- theta[4];
  a*exp(-0.5*((x-m)/s)^2) + b
}

# Difficult part: Estimate some starting values: crude
# m is x values at which peak occurs
m.0 <- x[which.max(y)] # argmax
# s>0 quantifies the rate at which the curve tapers off
s.0 <- (max(x)-min(x))/6
#s.0 <- (max(x)-min(x))/4 # too fast? Make more guassian
# b is a baseline (min y)
b.0 <- min(y)
# a>0 reflects the overall magnitudes of the relative y values (range)
a.0 <- (max(y)-min(y))

c(m=m.0, s=s.0, a=a.0, b=b.0)

# Do the fit. 
fit <- nls(y ~ f(x,c(m,s,a,b)), data.frame(x,y), start=list(m=m.0, s=s.0, a=a.0, b=b.0))
# a*exp(-0.5*((x-m)/s)^2) + b
summary(fit)

plot(nlsResiduals(fit), which=0)

#850.5028*exp(-0.5*((x-44.4252)/19.4156)^2)  -124.2956
#Predict day 76: May 06, 2020 - should be #369
850.5028*exp(-0.5*((76-44.4252)/19.4156)^2)  -124.2956  # 102

# Display the estimated location of the peak and its SE.
summary(fit)$parameters["m", 1:2]
fit_peak_loc <-ceiling(summary(fit)$parameters["m", 1])
df1_italy$new_deaths[fit_peak_loc]

# future days predictions for the curve
future_tagged_day <- c(76:78)

italy_pred <- 850.5028*exp(-0.5*((future_tagged_day-44.4252)/19.4156)^2)  -124.2956
italy_pred

# from day 76-78
italy_actual_future <- c(369, 274, 243)

pred_actual <- data.frame(future_tagged_day, italy_pred,italy_actual_future)
pred_actual

plot(future_tagged_day, italy_pred)

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

library(pscl)
pR2(fit_poly4)

# predictions
predictions <- fit_poly4 %>% predict(df1_italy[61:75, ])
cbind(df1_italy$new_deaths[61:75], predictions)

ggplot() + geom_point(aes(x =df1_italy$tagged_day[61:75], 
                          y = df1_italy$new_deaths[61:75], colour = "actual")) + 
  geom_smooth(aes(x =df1_italy$tagged_day[61:75], 
                  y = df1_italy$new_deaths[61:75],colour = "actual")) +
  geom_point(aes(x =df1_italy$tagged_day[61:75], 
                 y = predictions,colour= "predicted")) + 
  geom_smooth(aes(x =df1_italy$tagged_day[61:75], 
                  y = predictions,colour = "predicted")) + xlab("tagged days") +
  ylab("New_deaths") + ggtitle("Predicted vs Actual new_deaths for Italy")

###################################################
# trying polynomial linear regression for italy

fit_poly <- lm(new_deaths ~ tagged_day + new_confirmed + flights_china +
                 I(tagged_day^2)+ I(new_confirmed^2) + I(flights_china^2),data = df1_korea[1:60, ])
summary(fit_poly)

#removing all SIP variable : flights_china
fit_poly2 <- lm(new_deaths ~ tagged_day + new_confirmed +
                  I(tagged_day^2)+ I(new_confirmed^2),data = df1_korea[1:60, ])
summary(fit_poly2)

#removing tagged day^2
fit_poly3 <- lm(new_deaths ~ tagged_day +  new_confirmed+ I(new_confirmed^2),data = df1_korea[1:60, ])
summary(fit_poly3)

plot(fit_poly3, 1)
plot(fit_poly3, 3)
plot(fit_poly3, 5)

library(pscl)
pR2(fit_poly3)

# predictions
predictions <- fit_poly3 %>% predict(df1_korea[61:90, ])
cbind(df1_korea$tagged_days[61:90], df1_korea$new_deaths[61:90], predictions)

ggplot() + geom_point(aes(x =df1_korea$tagged_day[61:90], 
                          y = df1_korea$new_deaths[61:90], colour = "actual")) + 
  geom_smooth(aes(x =df1_korea$tagged_day[61:90], 
                  y = df1_korea$new_deaths[61:90],colour = "actual")) +
  geom_point(aes(x =df1_korea$tagged_day[61:90], 
                 y = predictions,colour= "predicted")) + 
  geom_smooth(aes(x =df1_korea$tagged_day[61:90], 
                  y = predictions,colour = "predicted")) + xlab("tagged days") +
  ylab("New_deaths") + ggtitle("Predicted vs Actual new_deaths for Korea")

##############################################################################################################
# trying negative binomial regression - didn't give good predictions

library(MASS)
library(rgl)
library(corrplot)
library(pwr)
library(pscl)
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
cbind(df1_italy$new_deaths[61:75], predictions)

ggplot() + geom_point(aes(x =df1_italy$tagged_day[61:75], 
                          y = df1_italy$new_deaths[61:75], colour = "actual")) + 
  geom_smooth(aes(x =df1_italy$tagged_day[61:75], 
                  y = df1_italy$new_deaths[61:75],colour = "actual")) +
  geom_point(aes(x =df1_italy$tagged_day[61:75], 
                 y = predictions,colour= "predicted")) + 
  geom_smooth(aes(x =df1_italy$tagged_day[61:75], 
                  y = predictions,colour = "predicted")) + xlab("tagged days") +
  ylab("New_deaths") + ggtitle("Predicted vs Actual new_deaths for Italy")

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

