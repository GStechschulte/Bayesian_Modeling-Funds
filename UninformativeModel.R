library(rjags)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(quantmod)


## Getting the data ##
data <- NULL
data <- cbind(data,
              getSymbols.yahoo("AAPL", from="2017-01-01", periodicity = "monthly",
                               auto.assign=FALSE)[,6])

head(data)

aapl.returns <- na.omit(diff(log(data)))
plot(aapl.returns)

returns <- as.vector(aapl.returns$AAPL.Adjusted)
class(returns)
length(returns)


uninform_model <- 'model{
    
    for (i in length(returns) {
         returns[i] ~ dnorm(mu, std)
    })
    # Likelihood function
    
}
'






















