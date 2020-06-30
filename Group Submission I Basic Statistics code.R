
#(20/06) MScFE 610 Econometrics (C20-S2) Timezone Group 3 - A Submission (Basic Statistics)
#The calculation of JP Morgan average stock value, stock volatility and return.


#Importing the JP Morgan data downloaded 
library(readr) #Import the library to read file downloaded
library(tseries)
JPM_sheet <- read_csv("JPM.csv", 
                      col_types = cols(Date = col_date(format = "%m/%d/%Y")))
View(JPM_sheet)


#The average value of the JP Morgan Stock
average_stock <- mean(JPM_sheet$`Adj Close`)
average_stock

#Get the Adjusted Close Price
price<-JPM_sheet$`Adj Close`
n<- length(price)

#Calculate the daily return of the JPM Adjusted Closed Price
daily_return_log<-log(price[-n]/price[-1])
daily_return_log

# JPM_sheet$`daily return`<-log(price[-n]/price[-1])
# View(JPM_sheet$`daily return`)

#Calculate the Volatility of Stock
volatility<- sd(daily_return_log)*sqrt(252)*100
volatility
