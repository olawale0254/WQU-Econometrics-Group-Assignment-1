#(20/06) MScFE 610 Econometrics (C20-S2) Timezone Group 3 - A Submission (Basic Statistics)

#Importing the JP Morgan data downloaded 
library(readr) #Import the library to read file downloaded
library(tseries)

#import the JPM dataset
JPM_sheet <- read_csv("WorldQuant MScFE/MScFE 610 Econometrics/JPM sheet.csv", 
                      col_types = cols(Date = col_date(format = "%m/%d/%Y")))

#import the S & P dataset
SandP500_sheet <- read_csv("WorldQuant MScFE/MScFE 610 Econometrics/^GSPC.csv")
View(SandP500_sheet)

#Extracting the explained and explanatory variable from the two different datasets.
JPM_price<-JPM_sheet$`Adj Close`
SandP500_price <-SandP500_sheet$`Adj Close`

#Applying linear regression model
linear_regression = lm(JPM_price~SandP500_price)
linear_regression

summary(linear_regression)
