library(tidyverse)
library(R2jags)

jags.dat <- read_rds("data/jags_dat.rds")

# JAGS code ---------------------------------------------------------------

monod.mod <- function() {
  eps <- 1E-3

  # likelihood
  for (i in 1:n.plt) {
    y[i] ~ dnorm(eta[i], tau[ft[i]])
    eta[i] <- mu[i] * x[i] / (k[i] + x[i])
    mu[i] <- z[i, ] %*% beta[, ft[i]]
    k[i] <- z[i, ] %*% gamma[, ft[i]]
  }

  # priors
  for (j in 1:n.ft) {
    tau[j] ~ dgamma(eps, eps)
    beta[1, j] ~ dunif(0, 1 / eps)
    beta[2, j] ~ dunif(-1 / eps, 1 / eps)
    beta[3, j] ~ dunif(-1 / eps, 1 / eps)
    gamma[1, j] ~ dunif(0, 1 / eps)
    gamma[2, j] ~ dunif(-1 / eps, 1 / eps)
    gamma[3, j] ~ dunif(-1 / eps, 1 / eps)
  }

  # transformed parameter
  sigma <- 1 / sqrt(tau)
}

monod.par <- c("beta", "gamma", "sigma")

# JAGS run ----------------------------------------------------------------

monod.jags <- jags.parallel(
  data = jags.dat,
  model.file = monod.mod,
  parameters.to.save = monod.par,
  n.chains = 5, n.iter = 2e5
)
write_rds(monod.jags, "models/new_monod_jags_2e5.rds")
