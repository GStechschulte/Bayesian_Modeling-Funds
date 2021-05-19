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
                               from = "2015-01-01", 
                               to = "2020-02-01",
                               periodicity = "monthly",
                               auto.assign=FALSE)[,6])

tail(data)

# Continuous returns
spy.returns <- na.omit(diff(log(data)))
plot(spy.returns)

# Chaning data type to vector for the JAGS model
returns <- as.vector(spy.returns$SPY.Adjusted)
class(returns)
length(returns)

# Investigating basic desc. statistics
mean(returns)
StdDev(returns)
var(returns)

# --------------------------------

## Simulating prior distributions ##
sim <- 1000

mu_sim_prior <- rnorm(sim, 0.0, sd = 0.001)

# Tau - variance is non-negative, continuous, and with no upper limit, 
#       a gamma distribution appears to be a candidate prior for the variance
tau_sim_prior <- rgamma(sim, 0.001, 0.001)

sigma_sim_prior <- 1 / sqrt(tau_sim_prior)

sim_priors <- data.frame(mu_sim_prior, tau_sim_prior, sigma_sim_prior)

ggplot(sim_priors, aes(mu_sim_prior, tau_sim_prior)) +
  geom_point(color = 'skyblue', alpha = 0.4) +
  geom_density_2d(color = 'orange', size = 1) +
  ggtitle('Prior Distributions')

ggplot(sim_priors, aes(x = sigma_sim_prior)) +
  geom_histogram(aes(y =..density..), color = 'black', fill = 'white') +
  geom_density(size = 1, color = 'orange')

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

## Compiling Model ##
inform_jags <- jags.model(
  textConnection(inform_model),
  data = list(Y = returns),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989), # inits argument is for reproducability
  n.chains = 4)

# -------------------------------

## update performs a burn in period to "warm up" the simulation ##
update(inform_jags, n.iter = 100, progress.bar = 'none')

## Simulating 1,000 samples from the posterior of the model using MCMC ##
returnSim <- coda.samples(model = inform_jags,
                          variable.names = c("mu", "tau", "sigma"),
                          n.iter = 1000)

# --------------------------------

## Summary Statistics  and Visualizations ##
summary(returnSim)

# Plotting posterior of parameters 
plot(returnSim)

# Putting posterior parameters into a dataframe
returnChains <- data.frame(returnSim[[1]], iter = 1: 1000)
returnChains

## Correlation functions ##
acf(returnChains$mu)
pacf(returnChains$mu)

## Observed Data - Likelihood ##
ggplot(data = spy.returns, aes(x = SPY.Adjusted)) +
  geom_density() +
  geom_vline(xintercept = mean(spy.returns$SPY.Adjusted),
             color = 'blue') +
  ggtitle('Observational Distribution of mu')

## Posterior Data - Mu ##
ggplot(data = returnChains, aes(x = mu)) +
  geom_density() +
  geom_vline(xintercept = mean(returnChains$mu),
             color = 'red') +
  ggtitle('Posterior Distribution of mu')

y_sim <- rnorm(nrow(returnChains), returnChains$mu, returnChains$sigma)
hist(y_sim, freq=FALSE, xlab = 'Mean Monthly Returns', main = 'Posterior Predictive Distribution')
lines(density(y_sim))
abline(v = quantile(y_sim, c(0.025, 0.975)), col = 'orange')


# --------------------------------

## Probabilities ##

# Pr(mu > 0.5%)
pnorm(q = 0.005, mean=mean(returnChains$mu), sd=mean(returnChains$sigma), lower.tail = FALSE)

# Pr(mu < 0.0%)
pnorm(q = 0.0, mean=mean(returnChains$mu), sd=mean(returnChains$sigma), lower.tail = TRUE)

# Pr(std > 2%)
pnorm(q = 0.02, mean=mean(returnChains$sigma), sd=mean(returnChains$sigma), lower.tail = FALSE)
