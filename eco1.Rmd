---
title: "Groupwork Assignment Submission 1"
author: "Group 3A"
date: "6/23/2020"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Name of Group Members
- Abimbola Olawale Victor (abimbolaolawale41@gmail.com)
- Ogbekile Chukwudi Samuel (Ogbekilechukwudi@gmail.com)
- Aboubacar SAÏDOU BOUREIMA (aboubacarsaidouboureima145@gmail.com)
- Michael Segun Olanipekun (molanipekun10@gmail.com)
- Abiodun Jimoh (suliman.jumah@gmail.com)

# Introduction
This submission document the Linear regression analysis of JP Morgan, S&P 500 stock prices and the ARIMA/ARMA forecast for the Case-Shiller Index from 1987 till date. The JP Morgan was first evaluated with the basic statistics computation in terms of returns and annualized volatility. The document also cover the linear regression analysis of JP Morgan and S&P 500 with the former as the explained variable and the latter the explanatory variable. The ARIMA forecast of the Case-Shiller are also explain along within the codeblocks.

# 1.0 Univariate Basic Statistics
### Importing the JP Morgan data downloaded 
```{r}
library(readr) 
library(tseries)
JPM_sheet <- read_csv("JPM(1).csv",col_types = cols(Date = col_date(format = "%m/%d/%Y")))
```
### The average value of the JP Morgan Stock
```{r}
average_stock <- mean(JPM_sheet$`Adj Close`)
average_stock
```
### Get the Adjusted Close Price
```{r}
price<-JPM_sheet$`Adj Close`
n<- length(price)

```

### Calculate the daily return of the JPM Adjusted Closed Price
```{r}
daily_return_log<-log(price[-n]/price[-1])
daily_return_log
```

### Calculate the Volatility of Stock
```{r}
volatility<- sd(daily_return_log)*sqrt(252)*100
volatility
```

# 2.0 Linear Regression 
### Import the JPM dataset
```{r}
JPM_sheet <- read_csv("JPM(1).csv", 
                      col_types = cols(Date = col_date(format = "%m/%d/%Y")))
```

### Import the S & P dataset
```{r}
SandP500_sheet <- read_csv("^GSPC(1).csv")
```

### Extracting the explained and explanatory variable from the two different datasets.
```{r}
JPM_price<-JPM_sheet$`Adj Close`
SandP500_price <-SandP500_sheet$`Adj Close`
```

### Applying linear regression model
```{r}
linear_regression = lm(JPM_price~SandP500_price)
linear_regression
```

### Summary of Linear Regression
```{r}
summary(linear_regression)
```
Based on the Linear regression, we have:

JPM_Price = 13.415509 + 0.033234SandP500_price. 
A unit increase in SandP500_price will cause about <2e-16S in JPM_Price. 
Also the Pvalue ofthe explanatory variable is signiificant which means that it is contributing significantly to the model.
The Mutiple R-Squared is about 57.88% while the Adjusted R-Squared is about 57.68%.
The “Estimate” represent the coefficient of the explanatory variable S&P price and the value of the intercept (constant). Thus, y= mx + c
The Std. Error represent the standard errors of the estimated coefficient. The standard error of the estimated coefficient is much less than the residual standard error, thus, the hypothesis at the true values will not be rejected. Only the intercept has a standard error greater than the standard error but can be overlook as its not one of the explain variable.
The t-values for the individual significances define the ratio of the estimated values to the Std. Error of the coefficients. 
F-Statistics evaluates the joint statistics significance of all explanatory variables. The F-Statistics is similar to the t-value and in this model the same values as with the explanatory variable t-value (i.e there is only one explanatory variable used).
 The p-value of the F-statistics is approximately zero and thus the hypothesis can be accepted.

Based on the result from the linear regression it can be deduced that we can explain only about 57% of the variation among the explanatory variables.

# 3.0 Univariate Time Series
### Importing the Data
```{r}
chdata <- read.csv("CSUSHPISA.csv")
```
Noticed the date data is not well formated.
```{r}
chdata$DATE= as.Date(chdata$DATE)
```

```{r, tseries}
data.ts<-ts(chdata$CSUSHPISA,frequency=12, start=c(1987,1,1))
```

### Fitting and Forcasting ARMA model
```{r}
library(ggfortify)
autoplot(data.ts) +
  xlab("Year") + ylab("Case-Shiller Index series.")
```
### Plotting the ACF plot of the series
```{r}
data.acf <- acf(data.ts, lag.max = 50)
```

Looking at the ACF plot it is obvious that the data is finding it difficult to decay to zero which indicate that the data is not stationary. So we decided to difference the data. 

```{r}
diff_data <- diff(data.ts, differences = 2)
```

```{r}
acf(diff_data)
```

```{r}
pacf(diff_data)
```

Based on the ACF and PACF plot used, we strongly suggest to fit an ARMA model of order (p,q) = (15,1)

```{r}
library(tseries)
adf.test(diff_data)
```

In other to confirm the stationarity level of the differennced data we used Augumented Dicky fuller test which the result is as above.

### Fitting the ARMA model
```{r}
library(forecast)
ARMAmodel<- arima(diff_data, order=c(15,0,1)) 
```

### Model Summary
```{r}
ARMAmodel
```
### Predicting ARMA Model
```{r}
forcast <- forecast(ARMAmodel)
forcast
```

### Plot Forcast
```{r}
autoplot(forecast(ARMAmodel))
```


### Augumented Dicky Fuller (ADF) Test
```{r}
adf.test(data.ts)
```

### Interpretation of ADF
Based on the result of the ADF test, it was obvious that the pvalue of the test is 0.1207 which is less than the 0.05. 
According tp the selection Criterion we accepted the null hyputhesis.

### Decision Rule
The Cash Shriller Index is not Stationary and it will be of our best interest to find a way to stationarize it before fitting the model.  

## Implimenting Arima Model
### Implimenting Box Jenkins for model order selection 

```{r}
acf(diff_data)
```

```{r}
pacf(diff_data)
```

We used the ACF and PACF plot to determine the order of the arima model.
Note that the order of differencing of the model i.e "d" is 2. 
this was based on the fact that we differenced the data twice to make it stationary before fitting the model. 

The ACF plot shows after the first spike the model started to decay to zero. 
This shows the MA of order 1. 

Also, we observed that there was no spike that crosses the line until the 3rd lag while other decays to zero.Therefore we decided to select an AR of order 3.

The order to be used for this model is (3,2,1).

```{r}
library(forecast)
ARIMAmodel<-arima(diff_data, order=c(3,2,1))
```

```{r}
ARIMAmodel
```
```{r}
cast <- forecast(ARIMAmodel)
cast
```


### Checking for the model Adequacy
```{r}
ggtsdiag(ARIMAmodel)
```


### Forcasting the Arima Model
```{r}
arima_fut <-autoplot(forecast(ARIMAmodel))
arima_fut
```


### Predicting ARMA model for the next 3 years i.e future evolution 
```{r}
pred_fut <-autoplot(forecast(ARMAmodel))
pred_fut
```
