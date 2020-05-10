setwd("/Users/tiffanytoor1/desktop/MSHI/614/COVID_614")

library(dplyr)
library (ggplot2)
library(caret)

df0 <- read.csv(file="HS614_COVID_dataset.csv", header=TRUE)
head(df0)

df0$date <- as.Date(df0$date, "%m/%d/%y")

df1 <- select(df0, c(country, tagged_day, new_confirmed, new_deaths, partial_lockdown, flights_china, school_closure))
head(df1)

df1_italy <- subset(df1, country =="Italy")
df1_skorea <- subset(df1, country == "Korea, South")

#univariate plots for country & new_deaths/new_confirmed
ggplot(df1, aes(x=country, y= new_deaths)) + geom_path()
ggplot(df1, aes(x=country, y = new_confirmed)) + geom_path()

#not considered bivariate, right? Using subset by countries. 
ggplot(df1_italy, aes(x=tagged_day, y=new_deaths)) + geom_path()
ggplot(df1_skorea, aes(x=tagged_day, y=new_deaths)) + geom_path()


