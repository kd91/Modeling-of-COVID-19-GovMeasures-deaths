# Modeling-of-COVID-19-GovMeasures-deaths

## Exploration and Modeling of Covid-19 Data

In this project, our team seeks to address the following problem: is it possible to predict country-wide mortality rates from the COVID-19 virus, based on the respective timeline of governments’ implementation of various mitigation measures? From the general understanding of the COVID virus, as well as viruses in general, it is common knowledge that mitigation measures (e.g., travel bans, partial and full lockdowns, the closure of schools/restaurants/bars) are necessary in order to prevent the rapid spread of infection. Less known to researchers and to the public, is which particular measures are better mitigation techniques than others. This project aims to compare countries that implemented different mitigation measures at different times, in order to help understand which measures proved to be more successful in reducing the mortality rates. We hope that this information can be useful in the case of future pandemics and/or useful in determining which measures to retract before others, as countries begin to open back up following the current pandemic. 

For this project, we plan to design a model by comparing two countries (most likely, Italy and South Korea). The countries will be compared using the following predictor variables: 1) the number of confirmed cases, 2) the various government mitigation measures taken by the respective countries, 3) the date of implementation of each of this mitigation measures (as well as how many days it has been since this date). Together, these variables will be used to predict the number of deaths for a country using a regression model. 

# Data: 
The dataset we will use to identify mitigation measures and their implementation date can be found here: https://www.acaps.org/covid19-government-measures-dataset (ACAPS dataset). 
The dataset we will use to identify the number of confirmed cases and deaths can be found here: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE) - https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series. Modified tagged day column for above John Hopkins data used from repo available on https://github.com/secregister01/COVID19-N20/tree/master/DateWorld

# Models used for predicting daily new deaths :

- Best Model : Non linear nonlinear (weighted) least-squares estimates of the parameters of a nonlinear model using the Gaussian curve fit

- Other models that were not good predictors ( filename: Covid_modeling_Italy_Skorea.R )
    - polynomial linear regression
    - negative binomial regression

