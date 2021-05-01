library(rjags)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(quantmod)


## Getting the data ##
data <- NULL
data <- cbind(data,
              getSymbols.yahoo("AAPL", 
                               from = "2017-01-01", 
                               to = '2020-08-01',
                               periodicity = "monthly",
                               auto.assign=FALSE)[,6])

head(data)
class(data)

aapl.returns <- na.omit(diff(log(data)))
plot(aapl.returns)
aapl.returns

returns <- as.vector(aapl.returns$AAPL.Adjusted)
class(returns)
length(returns)
returns

mean(returns)
StdDev(returns)
var(returns)
