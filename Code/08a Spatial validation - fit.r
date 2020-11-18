# as requested by reviewer 3, NC
library(tidyverse)
theme_set(theme_bw())
library(R2jags)

cur.dat <- read_rds("Data/all_dat.rds") %>%
  filter(cen == "new") %>%
  mutate(ft = as.factor(ft))

# Sample training and testing data ----------------------------------------

## 75% of the sample size
smp.size <- floor(0.75 * nrow(cur.dat))

## set the seed to make your partition reproductible
set.seed(123)
train.ind <- sample(seq_len(nrow(cur.dat)), size = smp.size)

train.dat <- cur.dat[train.ind, ]
write_rds(train.dat, "Models/train_dat.rds")
test.dat <- cur.dat[-train.ind, ]
write_rds(test.dat, "Models/test_dat.rds")

# Fit JAGS ----------------------------------------------------------------

# JAGS
ft.dat <- data_frame(id = 1:nlevels(train.dat$ft), ft = levels(train.dat$ft))
write_rds(ft.dat, "Models/train_ft_dat.rds")

jags.dat <- list()

jags.dat$y <- train.dat$agb # aboveground carbon
jags.dat$x <- train.dat$sag # stand age
jags.dat$z <- cbind(1, train.dat$tmp.std, train.dat$ppt.std) # covariates, scaled or centered

jags.dat$ft <- as.numeric(train.dat$ft) # region index, vector
jags.dat$n.plt <- nrow(train.dat) # no. of plots
jags.dat$n.ft <- nrow(ft.dat) # no. of regions

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

monod.jags <- jags(
  data = jags.dat,
  model.file = monod.mod,
  parameters.to.save = monod.par,
  n.chains = 2, n.iter = 2e2
)
write_rds(monod.jags, "Models/spval_jags_2e2.rds")
