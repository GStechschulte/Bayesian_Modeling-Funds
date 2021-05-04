library(rjags)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(quantmod)


## Getting the data ##
data.before <- NULL
data.before <- cbind(data.before,
              getSymbols.yahoo("AAPL", 
                               from = "2018-01-01", 
                               to = '2020-02-01',
                               periodicity = "monthly",
                               auto.assign=FALSE)[,6])

head(data.before)
class(data.before)

aapl.returns <- na.omit(diff(log(data.before)))
plot(aapl.returns)
aapl.returns

returns <- as.vector(aapl.returns$AAPL.Adjusted)
class(returns)
length(returns)
returns

mean(returns)
StdDev(returns)
var(returns)

# -------------------------

data.after <- NULL
data.after <- cbind(data.after,
                     getSymbols.yahoo("AAPL", 
                                      from = "2018-01-01", 
                                      to = '2020-04-30',
                                      periodicity = "monthly",
                                      auto.assign=FALSE)[,6])
head(data.after)
class(data.after)

aapl.returns.after <- na.omit(diff(log(data.before)))
plot(aapl.returns.after)
aapl.returns

returns.after <- as.vector(aapl.returns.after$AAPL.Adjusted)
class(returns.after)
length(returns.after)
returns.after

mean(returns.after)
StdDev(returns.after)
var(returns.after)


# -------------------------

# Plotting prior distributions
hist(rnorm(1000, mean = -0.025, sd = 6), breaks=25)

# Tau - variance is non-negative, continuous, and with no upper limit, 
#       a gamma distribution might appear to be a candidate prior for the variance
hist(rgamma(1000, 15, 0.01), breaks=25)
# --------------------------------

## Model ## - tau (precision) = inverse of variance
inform_model <- "model{

# Likelihood function for each observation in the returns vector 
for (i in 1:length(Y)) {
Y[i] ~ dnorm(mu, tau)
}

# Prior(s) - expert intuition / knowledge
mu ~ dnorm(-0.025, 6)
tau ~ dgamma(15, 0.001)
sigma <- 1/sqrt(tau)
}"

# --------------------------------

## Compiling Model ##
inform_jags <- jags.model(
  textConnection(inform_model),
  data = list(Y = returns),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989),
  n.chains = 4)
# inits argument is for reproducability

## Simulating 10,000 samples from the posterior of the model using MCMC ##
returnSim <- coda.samples(model = inform_jags,
                          variable.names = c("mu", "tau", "sigma"),
                          n.iter = 10000)

summary(returnSim)

## Plotting posterior of parameters ##
plot(returnSim, trace = FALSE)

returnChains <- data.frame(returnSim[[1]], iter = 1: 10000)

ggplot(returnChains, aes(x = iter, y = mu)) + 
  geom_line()


# Observed Data - Likelihood
ggplot(data = aapl.returns, aes(x = AAPL.Adjusted)) +
  geom_density() +
  geom_vline(xintercept = mean(aapl.returns$AAPL.Adjusted),
             color = 'blue') +
  ggtitle('Observational Distribution of mu')

# Posterior Data - Mu
ggplot(data = returnChains, aes(x = mu)) +
  geom_density() +
  geom_vline(xintercept = mean(returnChains$mu),
             color = 'red') +
  ggtitle('Posterior Distribution of mu')


## Probabilities ##

# Pr(mu > 0.5%)
1 - pnorm(q = 0.005, mean=mean(returnChains$mu), sd=mean(returnChains$sigma))

# Pr(std < 2%)
pnorm(q = 0.02, mean=mean(returnChains$sigma), sd=mean(returnChains$mu))
