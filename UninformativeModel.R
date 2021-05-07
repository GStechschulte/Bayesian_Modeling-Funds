library(rjags)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(quantmod)
library(bayesplot)

## Getting the data ##
data <- NULL
data <- cbind(data,
              getSymbols.yahoo("SPY", 
                               from = "2017-01-01", 
                               to = "2020-02-01",
                               periodicity = "monthly",
                               auto.assign=FALSE)[,6])

head(data)
class(data)

spy.returns <- na.omit(diff(log(data)))
plot(spy.returns)
spy.returns

returns <- as.vector(spy.returns$SPY.Adjusted)
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
    sigma <- 1/sqrt(tau)
}"

# --------------------------------

## Model Specification - 4 chains ##
uninform_jags <- jags.model(
  textConnection(uninform_model),
  data = list(Y = returns),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989),
  n.chains = 4
)

## Markov Chains ##
##---------------##
# These are results from the markov chain, NOT a random sample from the posterior
# rjags uses markov chains to approximate posteriors 
# Markov chains are dependent on the previous values and time steps
# The trace plot provides an approximation of the parameters

## Using CODA for MCMC sampling - 1000 iterations ##
returnSim <- coda.samples(model = uninform_jags,
                          variable.names = c("mu", "tau", "sigma"),
                          n.iter = 1000)

## Plotting ##
plot(returnSim)

## Summary Statistics ##
summary(returnSim)


# --------------------------------

returnChains <- data.frame(returnSim[[1]], iter = 1: 1000)

# Trace plot
ggplot(returnChains, aes(x = iter, y = mu)) + 
  geom_line()

# Correlation functions
acf(returnChains$mu)
pacf(returnChains$mu)

# Average posterior mean monthly return
ggplot(returnChains, aes(x = mu)) + 
  geom_density() +
  geom_vline(xintercept = mean(returnChains$mu), 
             color = 'red')

# Average return - Uninformative prior has "learned" the parameters
# Posterior mean mu is approx. equal to observational mean mu
ggplot(returnChains, aes(x = mu)) + 
  geom_density() +
  geom_vline(aes(xintercept = mean(returns), 
             color = 'red')) +
  geom_vline(aes(xintercept = mean(mu),
             color = 'blue'))


return_mcmc <- as.mcmc(uninform_jags)

# --------------------------------

## Diagnostics ##
# using chains can illustrate a range of different paths the chain can take
# you want paths to all represent stability
# summary stats - provide estimates of the posterior params estimates
# naive standard errors - provides an estimate of the potential erorr / measure of uncertainty in the estimate
# we can use this to determine the approp. chain length - I.e, estimate mu within a SE of 0.1

# --------------------------------

## Visualizing Plotting ##

# Prior distribution, likelihood distribution (observed data), and posterior distribution - same graph

# Returns
prior <- rnorm(n=10000, mean=0.0, sd=0.001)
length(prior)

returnChains <- data.frame(returnSim[[1]], iter = 1: 1000)
posterior.mu <- returnChains$mu
length(posterior.mu)

dists <- data.frame(prior = rnorm(n=1000, mean=0.0, sd=0.001), 
                    post = returnChains$mu)


## Probabilities ##

# Pr(mu > 0.5%)
pnorm(q = 0.005, mean=mean(returnChains$mu), sd=mean(returnChains$sigma), lower.tail = FALSE)

# Pr(std > 2%)
pnorm(q = 0.02, mean=mean(returnChains$sigma), sd=mean(returnChains$mu), lower.tail = FALSE)


mean(returns)
sd(returns)






