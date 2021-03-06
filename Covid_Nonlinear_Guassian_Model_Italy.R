setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library (ggplot2)
library(caret)
library(nlstools)

df0 <- read.csv(file="data/acaps_johnhopkins_combined_dataset.csv", header=TRUE)
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
# choosing to use flights_china for nls model from corr of SIP measures with new_deaths

# #Determine the nonlinear (weighted) least-squares estimates of
#  the parameters of a nonlinear model: Gaussian
#https://stats.stackexchange.com/questions/70153/linear-regression-best-polynomial-or-better-approach-to-use/70184#70184

##############################################################################################################
# tagged day vs new_deaths

y <- df1_italy$new_deaths[1:60]
x <- df1_italy$tagged_day[1:60]

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
summary(fit)

#interpretation:
# a*exp(-0.5*((x-m)/s)^2) + b
#850.5028*exp(-0.5*((x-44.4252)/19.4156)^2)  -124.2956
#Predict day 61 - should be #534
850.5028*exp(-0.5*((61-44.4252)/19.4156)^2)  -124.2956  # 466

# Display the estimated location of the peak and its SE.
summary(fit)$parameters["m", 1:2]
fit_peak_loc <-ceiling(summary(fit)$parameters["m", 1])
df1_italy$new_deaths[fit_peak_loc]

# future days predictions for the curve
italy_pred <- 850.5028*exp(-0.5*((df1_italy$tagged_day[61:75]-44.4252)/19.4156)^2)  -124.2956
italy_pred
italy_pred <- ceiling(italy_pred)

# model evaluation fro residuals and MAE
rls <- nlsResiduals(fit)
plot(rls, which=0)
#it's not a very good fit. 
# points are dispersed equally above & below the line
MAE(italy_pred, df1_italy$new_deaths[61:75])   #69.66667
# test.nlsResiduals(rls)

# vector with 1-60 days actual deaths data & 61-75days predicted deaths data
actual1.60_pred61.75 <- c(df1_italy$new_deaths[1:60], italy_pred)

PredVsActual <- data.frame(df1_italy$tagged_day,df1_italy$new_deaths, actual1.60_pred61.75)
PredVsActual

ggplot() +geom_point(aes(x =PredVsActual$df1_italy.tagged_day, 
                 y = PredVsActual$actual1.60_pred61.75,colour = "predicted")) + 
          geom_smooth(aes(x =PredVsActual$df1_italy.tagged_day, 
                  y = PredVsActual$actual1.60_pred61.75,colour = "predicted")) +
  geom_point(aes(x =PredVsActual$df1_italy.tagged_day, 
                 y = PredVsActual$df1_italy.new_deaths,colour = "actual")) + 
  geom_smooth(aes(x =PredVsActual$df1_italy.tagged_day, 
                  y = PredVsActual$df1_italy.new_deaths,colour = "actual")) + 
  xlab("Count of days (since the 20th confirmed case)") +
            ylab("Daily new deaths") + ggtitle("Predicted vs Actual new deaths/day for Italy")

##############################################################################################################
# for flights_china vs new_deaths

y <- df1_italy$new_deaths[1:60]
x <- df1_italy$flights_china[1:60]

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
summary(fit)

#interpretation:
# a*exp(-0.5*((x-m)/s)^2) + b
# 839.5095*exp(-0.5*((x-61.5959)/14.9453)^2)  -66.7616
# Predict: tagged day 61 - flights to china=81, new deaths should be #534
839.5095*exp(-0.5*((81-61.5959)/14.9453)^2)  -66.7616  #294

# Display the estimated location of the peak and its SE.
summary(fit)$parameters["m", 1:2]
fit_peak_loc <-ceiling(summary(fit)$parameters["m", 1])
df1_italy$new_deaths[fit_peak_loc]

# future days predictions for the curve
italy_pred <- 839.5095*exp(-0.5*((df1_italy$flights_china[61:75]-61.5959)/14.9453)^2)  -66.7616
italy_pred
italy_pred <- ceiling(italy_pred)

# model evaluation for residuals and MAE
plot(nlsResiduals(fit), which=0)
#it's not a very good fit. points are not uniformly dispersed
MAE(italy_pred, df1_italy$new_deaths[61:75]) #221.5333

# vector with 1-60 days actual deaths data & 61-75days predicted deaths data
actual1.60_pred61.75 <- c(df1_italy$new_deaths[1:60], italy_pred)

PredVsActual <- data.frame(df1_italy$flights_china,df1_italy$new_deaths, actual1.60_pred61.75)
PredVsActual

ggplot() + geom_point(aes(x =PredVsActual$df1_italy.flights_china, 
                          y = PredVsActual$actual1.60_pred61.75,colour = "predicted")) + 
  geom_smooth(aes(x =PredVsActual$df1_italy.flights_china, 
                  y = PredVsActual$actual1.60_pred61.75,colour = "predicted")) + 
  geom_point(aes(x =PredVsActual$df1_italy.flights_china, 
                          y = PredVsActual$df1_italy.new_deaths,colour = "actual")) + 
  geom_smooth(aes(x =PredVsActual$df1_italy.flights_china, 
                  y = PredVsActual$df1_italy.new_deaths,colour = "actual")) +
   xlab("# of Days since start of flight restrictions to/from china") +
  ylab("Daily New Deaths") + ggtitle("Predicted vs Actual new deaths/day for Italy")

##############################################################################################################
# for new_confirmed vs new_deaths

y <- df1_italy$new_deaths[1:60]
x <- df1_italy$new_confirmed[1:60]

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
summary(fit)

#interpretation:
# a*exp(-0.5*((x-m)/s)^2) + b
# 1215.1*exp(-0.5*((x-6652.5)/4645.6)^2)-464.2
# Predict: tagged day 61 - new_confirmed=2729, new deaths should be #534
1215.1*exp(-0.5*((2729-6652.5)/4645.6)^2) - 464.2  #386.3972

# Display the estimated location of the peak and its SE.
summary(fit)$parameters["m", 1:2]
fit_peak_loc <-ceiling(summary(fit)$parameters["m", 1])
df1_italy$new_deaths[fit_peak_loc]

# future days predictions for the curve
italy_pred <- 1215.1*exp(-0.5*((df1_italy$new_confirmed[61:75]-6652.5)/4645.6)^2) - 464.2
italy_pred
italy_pred <- ceiling(italy_pred)

# model evaluation for residuals and MAE
plot(nlsResiduals(fit), which=0)
#it's not a very good fit. points are not uniformly dispersed
MAE(italy_pred, df1_italy$new_deaths[61:75])

# vector with 1-60 days actual deaths data & 61-75days predicted deaths data
actual1.60_pred61.75 <- c(df1_italy$new_deaths[1:60], italy_pred)

PredVsActual <- data.frame(df1_italy$new_confirmed,df1_italy$new_deaths, actual1.60_pred61.75)
PredVsActual

ggplot() + geom_point(aes(x =PredVsActual$df1_italy.new_confirmed, 
                 y = PredVsActual$actual1.60_pred61.75,colour = "predicted")) + 
  geom_smooth(aes(x =PredVsActual$df1_italy.new_confirmed, 
                  y = PredVsActual$actual1.60_pred61.75,colour = "predicted")) +
  geom_point(aes(x =PredVsActual$df1_italy.new_confirmed, 
                 y = PredVsActual$df1_italy.new_deaths,colour = "actual")) + 
  geom_smooth(aes(x =PredVsActual$df1_italy.new_confirmed, 
                  y = PredVsActual$df1_italy.new_deaths,colour = "actual")) +
  xlab("Daily new confirmed cases") + ylab("Daily New Deaths") +
  ggtitle("Predicted vs Actual new deaths/day for Italy")
