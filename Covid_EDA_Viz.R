setwd("~/Documents/KHUSHBU/MS/py_scripts_main/GitHub/Modeling-of-COVID-19-GovMeasures-deaths")

library(dplyr)
library (ggplot2)
library(caret)

df0 <- read.csv(file="HS614_COVID_dataset.csv", header=TRUE)
head(df0)

df0$date <- as.Date(df0$date, "%m/%d/%y")

df1 <- select(df0, c(country, tagged_day, new_confirmed, new_deaths, partial_lockdown, flights_china, school_closure))
head(df1)
