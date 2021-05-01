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

# --------------------------------

## Model ## - tau (precision) = inverse of variance
uninform_model <- "model{

# Likelihood function for each observation in the returns vector 
for (i in 1:length(Y)) {
Y[i] ~ dnorm(mu, tau)
}

# Prior(s) - expert intuition / knowledge
mu ~ dnorm(-0.025, 10000)
tau ~ dgamma(15, 0.001)
}"

# --------------------------------

## Compiling Model ##
uninform_jags <- jags.model(
  textConnection(uninform_model),
  data = list(Y = returns),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989)
)
# inits argument is for reproducability

## Simulating 10,000 samples from the posterior of the model using MCMC ##
returnSim <- coda.samples(model = uninform_jags,
                          variable.names = c("mu", "tau"),
                          n.iter = 1000)

## Plotting posterior of parameters ##
plot(returnSim, trace = FALSE)

returnChains <- data.frame(returnSim[[1]], iter = 1: 10000)

ggplot(returnChains, aes(x = iter, y = mu)) + 
  geom_line()

ggplot(returnChains, aes(x = mu)) + 
  geom_density() +
  geom_vline(xintercept = mean(returnChains$mu), 
             color = 'red')

# Average return - Uninformative prior has "learned" the appropriate parameters
ggplot(returnChains, aes(x = mu)) + 
  geom_density() +
  geom_vline(xintercept = mean(returns), 
                 color = 'red') +
  geom_vline(xintercept = mean(returnChains$mu),
                 color = 'blue')
