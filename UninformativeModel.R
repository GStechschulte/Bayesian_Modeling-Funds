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
                               to = '2020-'
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
    mu ~ dnorm(0.0, 0.001)
    tau ~ dgamma(0.001, 0.001)
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

# --------------------------------

## Markov Chains ##
head(returnSim, 20)
# these are results from the markov chain, NOT a random sample from the posterior
# rjags uses markov chains to approximate posteriors 
# markov chain is dependent on the previous values and time steps
# examine the trace plot of all 10,000 which can also be visualized as a distribution
# this provides a approximation of the parameters

returnChains <- data.frame(returnSim[[1]], iter = 1: 10000)

ggplot(returnChains, aes(x = iter, y = mu)) + 
  geom_line()

ggplot(returnChains, aes(x = mu)) + 
  geom_density() +
  geom_vline(xintercept = mean(returnChains$mu), 
             color = 'red')

# --------------------------------

## Diagnostics ##
# using chains can illustrate a range of different paths the chain can take
# you want paths to all represent stability
# summary stats - provide estimates of the posterior params estimates
# naive standard errors - provides an estimate of the potential erorr / measure of uncertainty in the estimate
# we can use this to determine the approp. chain length - I.e, estimate mu within a SE of 0.1

## Multiple Chains ##
uninform_jags <- jags.model(
  textConnection(uninform_model),
  data = list(Y = returns),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989),
  n.chains = 4
)

returnSim <- coda.samples(model = uninform_jags,
                          variable.names = c("mu", "tau"),
                          n.iter = 1000)

plot(returnSim)

summary(returnSim)

# --------------------------------

## Visualizing Plotting ##

# Prior distribution, likelihood distribution (observed data), and posterior distribution - same graph

# Returns
prior <- rnorm(n=10000, mean=0.0, sd=0.001)
length(prior)

returnChains <- data.frame(returnSim[[1]], iter = 1: 10000)
posterior <- returnChains$mu
length(posterior)


ggplot()  













