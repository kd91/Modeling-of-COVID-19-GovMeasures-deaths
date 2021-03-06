setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library (ggplot2)
library(caret)
library(nlstools)

df0 <- read.csv(file="data/acaps_johnhopkins_combined_dataset.csv", header=TRUE)
head(df0)

df0$date <- as.Date(df0$date, "%m/%d/%y")

df1 <- dplyr::select(df0, c(country, tagged_day, new_confirmed, new_deaths, partial_lockdown, flights_china, school_closure))
head(df1)

summary(df1)

df1_italy <- subset(df1, country =="Italy")
df1_korea <- subset(df1, country == "Korea, South")

plot(df1_italy$tagged_day, df1_italy$new_deaths)
plot(df1_korea$tagged_day, df1_korea$new_deaths)

# correlation
cor(df1_korea[, 2:7], use = "pairwise.complete.obs", method = "pearson")

####################################################################################
# #Determine the nonlinear (weighted) least-squares estimates of
#  the parameters of a nonlinear model: Gaussian
#https://stats.stackexchange.com/questions/70153/linear-regression-best-polynomial-or-better-approach-to-use/70184#70184

#  ## for korea tagged day & new deaths
y_korea <- df1_korea$new_deaths[1:60]
x_korea <- df1_korea$tagged_day[1:60]

# Define a Gaussian function (of four parameters)
f <- function(x_korea, theta)  {
  m <- theta[1]; s <- theta[2]; a <- theta[3]; b <- theta[4];
  a*exp(-0.5*((x_korea-m)/s)^2) + b
}

# Difficult part: Estimate some starting values: crude
# m is x values at which peak occurs
m.0 <- x_korea[which.max(y_korea)] # argmax
# s>0 quantifies the rate at which the curve tapers off
s.0 <- (max(x_korea)-min(x_korea))/6
#s.0 <- (max(x)-min(x))/4 # too fast? Make more guassian
# b is a baseline (min y)
b.0 <- min(y_korea)
# a>0 reflects the overall magnitudes of the relative y values (range)
a.0 <- (max(y_korea)-min(y_korea))

c(m=m.0, s=s.0, a=a.0, b=b.0)

# Do the fit.
fit <- nls(y_korea ~ f(x_korea,c(m,s,a,b)), data.frame(x_korea,y_korea), start=list(m=m.0, s=s.0, a=a.0, b=b.0))
# a*exp(-0.5*((x-m)/s)^2) + b
summary(fit)

#Interpretation:
#Predict day 61: April 06, 2020 - should be #3
6.510*exp(-0.5*((61-49.366)/23.059)^2)  -1.141  #4.59

#Predict day 91: May 06, 2020 - should be #1
6.510*exp(-0.5*((91-49.366)/23.059)^2)  -1.141 #0.135

# Display the estimated location of the peak and its SE.
summary(fit)$parameters["m", 1:2]
fit_peak_loc <-ceiling(summary(fit)$parameters["m", 1])
df1_korea$new_deaths[fit_peak_loc] #5

# future days predictions for the curve
korea_pred <- 6.510*exp(-0.5*((df1_korea$tagged_day[61:90]-49.366)/23.059)^2) -1.141
korea_pred <- ceiling(korea_pred)
korea_pred

# model evaluation fro residuals and MAE
plot(nlsResiduals(fit), which=0)
MAE(korea_pred, df1_korea$new_deaths[61:90]) # 1

# vector with 1-60 days actual deaths data & 61-75days predicted deaths data
actual1.60_pred61.75 <- c(df1_korea$new_deaths[1:60], korea_pred)

PredVsActual <- data.frame(df1_korea$tagged_day,df1_korea$new_deaths, actual1.60_pred61.75)
PredVsActual

ggplot() +geom_smooth(aes(x =PredVsActual$df1_korea.tagged_day,
                   y = PredVsActual$actual1.60_pred61.75,colour = "predicted")) + 
  geom_point(aes(x =PredVsActual$df1_korea.tagged_day,
                 y = PredVsActual$actual1.60_pred61.75,colour = "predicted")) +
  geom_smooth(aes(x =PredVsActual$df1_korea.tagged_day,
                  y = PredVsActual$df1_korea.new_deaths,colour = "actual")) +
  geom_point(aes(x =PredVsActual$df1_korea.tagged_day,
                 y = PredVsActual$df1_korea.new_deaths,colour = "actual")) +
  xlab("Count of days (since the 20th confirmed case)") + ylab("Daily new deaths") + 
  ggtitle("Predicted vs Actual new deaths/day for South Korea")

###############################################################
# KOREA: for flights_china vs new_deaths

## for korea flights in/out of China & new deaths
y_korea <- df1_korea$new_deaths[1:60]
x_korea <- df1_korea$flights_china[1:60]

# Define a Gaussian function (of four parameters)
f <- function(x_korea, theta)  { 
  m <- theta[1]; s <- theta[2]; a <- theta[3]; b <- theta[4];
  a*exp(-0.5*((x_korea-m)/s)^2) + b
}

# Difficult part: Estimate some starting values: crude
# m is x values at which peak occurs
m.0 <- x_korea[which.max(y_korea)] # argmax
# s>0 quantifies the rate at which the curve tapers off
s.0 <- (max(x_korea)-min(x_korea))/6
#s.0 <- (max(x)-min(x))/4 # too fast? Make more guassian
# b is a baseline (min y)
b.0 <- min(y_korea)
# a>0 reflects the overall magnitudes of the relative y values (range)
a.0 <- (max(y_korea)-min(y_korea))

c(m=m.0, s=s.0, a=a.0, b=b.0)

# Do the fit. 
fit <- nls(y_korea ~ f(x_korea,c(m,s,a,b)), data.frame(x_korea,y_korea), start=list(m=m.0, s=s.0, a=a.0, b=b.0))
# a*exp(-0.5*((x-m)/s)^2) + b
summary(fit)

#Interpretation:
#Predict day 61: April 06, 2020 - should be #6 
6.510*exp(-0.5*((61-15.366)/23.059)^2)  -1.141  #-0.22

#Predict day 91: May 06, 2020 - should be #1
6.510*exp(-0.5*((91-15.366)/23.059)^2)  -1.141 #-1.11

# Display the estimated location of the peak and its SE.
summary(fit)$parameters["m", 1:2]
fit_peak_loc <-ceiling(summary(fit)$parameters["m", 1])
df1_korea$new_deaths[fit_peak_loc] #1

# future days predictions for the curve
korea_pred <- 6.510*exp(-0.5*((df1_korea$flights_china[61:90]-15.366)/23.059)^2) -1.141
korea_pred <- ceiling(korea_pred)
korea_pred

# model evaluation fro residuals and MAE
plot(nlsResiduals(fit), which=0)
MAE(korea_pred, df1_korea$new_deaths[61:90]) #1

# vector with 1-60 days actual deaths data & 61-75days predicted deaths data
actual1.60_pred61.75 <- c(df1_korea$new_deaths[1:60], korea_pred)

PredVsActual <- data.frame(df1_korea$flights_china,df1_korea$new_deaths, actual1.60_pred61.75)
PredVsActual

ggplot() + geom_point(aes(x =PredVsActual$df1_korea.flights_china, 
                 y = PredVsActual$actual1.60_pred61.75,colour = "predicted")) + 
  geom_smooth(aes(x =PredVsActual$df1_korea.flights_china, 
                  y = PredVsActual$actual1.60_pred61.75,colour = "predicted")) +
  geom_point(aes(x =PredVsActual$df1_korea.flights_china, 
                 y = PredVsActual$df1_korea.new_deaths,colour = "actual")) + 
  geom_smooth(aes(x =PredVsActual$df1_korea.flights_china, 
                  y = PredVsActual$df1_korea.new_deaths,colour = "actual")) + 
  xlab("# of Days since start of flight restrictions to/from china") +
  ylab("Daily New Deaths") + ggtitle("Predicted vs Actual new deaths/day for South Korea")

###############################################################
# KOREA: for new_confirmed vs new_deaths

## for korea new confirmed cases & new deaths
y_korea <- df1_korea$new_deaths[1:60]
x_korea <- df1_korea$new_confirmed[1:60]

# Define a Gaussian function (of four parameters)
f <- function(x_korea, theta)  { 
  m <- theta[1]; s <- theta[2]; a <- theta[3]; b <- theta[4];
  a*exp(-0.5*((x_korea-m)/s)^2) + b
}

# Difficult part: Estimate some starting values: crude
# m is x values at which peak occurs
m.0 <- x_korea[which.max(y_korea)] # argmax
# s>0 quantifies the rate at which the curve tapers off
s.0 <- (max(x_korea)-min(x_korea))/6
#s.0 <- (max(x)-min(x))/4 # too fast? Make more guassian
# b is a baseline (min y)
b.0 <- min(y_korea)
# a>0 reflects the overall magnitudes of the relative y values (range)
a.0 <- (max(y_korea)-min(y_korea))

c(m=m.0, s=s.0, a=a.0, b=b.0)

# Do the fit. 
fit <- nls(y_korea ~ f(x_korea,c(m,s,a,b)), data.frame(x_korea,y_korea), start=list(m=m.0, s=s.0, a=a.0, b=b.0))
# a*exp(-0.5*((x-m)/s)^2) + b
summary(fit)

#Interpretation:
#Predict day 61: April 06, 2020 - should be #3 
6.510*exp(-0.5*((61-15.366)/23.059)^2)  -1.141  #-0.22

#Predict day 91: May 06, 2020 - should be #1
6.510*exp(-0.5*((91-15.366)/23.059)^2)  -1.141 #-1.11

# Display the estimated location of the peak and its SE.
summary(fit)$parameters["m", 1:2]
fit_peak_loc <-ceiling(summary(fit)$parameters["m", 1])
df1_korea$new_deaths[fit_peak_loc] #1

# future days predictions for the curve

korea_pred <- 6.510*exp(-0.5*((df1_korea$new_confirmed[61:90]-15.366)/23.059)^2) -1.141
korea_pred
korea_pred <- ceiling(korea_pred)
korea_pred

# model evaluation fro residuals and MAE
plot(nlsResiduals(fit), which=0)
MAE(korea_pred, df1_korea$new_deaths[61:90])

# vector with 1-60 days actual deaths data & 61-75days predicted deaths data
actual1.60_pred61.75 <- c(df1_korea$new_deaths[1:60], korea_pred)

PredVsActual <- data.frame(df1_korea$new_confirmed,df1_korea$new_deaths, actual1.60_pred61.75)
PredVsActual

ggplot() + geom_point(aes(x =PredVsActual$df1_korea.new_confirmed, 
                 y = PredVsActual$actual1.60_pred61.75,colour = "predicted")) + 
  geom_smooth(aes(x =PredVsActual$df1_korea.new_confirmed, 
                  y = PredVsActual$actual1.60_pred61.75,colour = "predicted")) + 
  geom_point(aes(x =PredVsActual$df1_korea.new_confirmed, 
                 y = PredVsActual$df1_korea.new_deaths,colour = "actual")) + 
  geom_smooth(aes(x =PredVsActual$df1_korea.new_confirmed, 
                  y = PredVsActual$df1_korea.new_deaths,colour = "actual")) + 
  xlab("Daily new confirmed cases") + ylab("Daily New Deaths") + 
  ggtitle("Predicted vs Actual new deaths/day for South Korea")
