

library(rethinking)

#==========================================================


# Probability mass function for the binomial distribution

# Parameters: 
#
# 1) number of trials = 10 
# (Note: This is a parameter that we normally will NOT 
# need to estimate in our statistical models. Rather, it's 
# something we observe. But it is a parameter in the sense 
# that it's a numeric characteristic used to define
# the binomial distribution.)
#
# 2) probability of success = 0.3

# If we have 10 binomial trials, then only 11 different
# outcomes are possible. We could have anywhere from 0
# to 10 successes

# Let's get the probability masses associated with each 
# of these 11 outcomes, given our arbitrarily chosen 
# parameter values
prob.masses <- dbinom(0:10, size = 10, prob = 0.3)

prob.masses

# We can plot these probability masses
plot(
  x = 0:10, y = prob.masses,
  xlab = "Number of successful binomial trials",
  ylab = "Probability",
  pch = 19
)

# Note that the probability mass of all outcomes 
# sums to 1
sum(prob.masses)
# In other words, if we conduct 10 binomial trials, one
# of these 11 outcomes MUST be observed. But some outcomes
# are more likely than others


# Random generation function for the binomial distribution

# 10 simulated draws (observations) from the binomial 
# distribution
# Note that each of these observations is composed of 
# 10 binomial trials. The "size" and "prob" parameters
# have not changed from the examples above
draws.10 <- rbinom(10, size = 10, prob = 0.3)

simplehist(
  draws.10, xlim = c(0, 10),
  xlab = "Number of successes observed"
)

# 100 simulated draws (observations)
draws.100 <- rbinom(100, size = 10, prob = 0.3)

simplehist(
  draws.100, xlim = c(0, 10),
  xlab = "Number of successes observed"
)

# 10,000 simulated draws (observations)
draws.10000 <- rbinom(10000, size = 10, prob = 0.3)

simplehist(
  draws.10000, xlim = c(0, 10),
  xlab = "Number of successes observed"
)

#==========================================================


# Probability density function for the normal distribution

# Parameters: 
#
# 1) mean = 2.5
#
# 2) standard deviation = 1.5

# Plot the probability density function for the
# normal distribution
curve(
  dnorm(x, mean = 2.5, sd = 1.5), 
  from = -10, to = 10,
  ylab = "Probability density"
)

# 10 simulated draws (observations) from the 
# normal distribution
rnorm(10, mean = 2.5, sd = 1.5)


# Probability density function for the Cauchy distribution

# Parameters: 
#
# 1) location = 2.5
#
# 2) scale = 1.5

# Plot the probability density function for the
# Cauchy distribution
curve(
  dcauchy(x, location = 2.5, scale = 1.5),
  from = -10, to = 10,
  ylab = "Probability density"
)

# 10 simulated draws (observations) from the 
# Cauchy distribution
rcauchy(10, location = 2.5, scale = 1.5)

#==========================================================


# Sampling from a posterior distribution

# First, let's generate a posterior distribution using
# grid approximation (with 101 points). The data observed 
# are 6 successes out of 9 binomial trials
p_grid <- seq(from = 0, to = 1, length.out = 101)
prior <- rep(1, 101)
likelihood <- dbinom(6, size = 9, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)

# Plot the posterior
plot(
  p_grid, posterior,
  pch = 19, type = "b",
  xlab = "Proportion of water on globe",
  ylab = "Posterior probability"
)

# Generate 10,000 samples of p (the proportion of water
# parameter) from the grid-approximate posterior
samples <- sample(
  p_grid, size = 10000, 
  prob = posterior, replace = TRUE
)
# Reading the arguments within "samples()" would go
# something like this: I want 10,000 samples (with
# replacement) from the values in "p_grid", taking values
# according to the probability assigned to them by the
# "posterior"

# Plot the posterior samples
plot(samples, pch = 19, col = alpha("black", 0.2))
dens(samples, xlim = c(0, 1),
     xlab = "Proportion of water on globe")


# Grid-approximate posterior for 5 successes in 5
# binomial trials (leads to an extremely skewed 
# posterior distribution for p)
p_grid2 <- seq(from = 0, to = 1, length.out = 101)
prior2 <- rep(1, 101)
likelihood2 <- dbinom(5, size = 5, prob = p_grid2)
unstd.posterior2 <- likelihood2 * prior2
posterior2 <- unstd.posterior2 / sum(unstd.posterior2)

# Plot the posterior
plot(
  p_grid2, posterior2,
  pch = 19, type = "b",
  xlab = "Proportion of water on globe",
  ylab = "Posterior probability"
)

# Generate 10,000 samples of p (the proportion of water
# parameter) from the grid-approximate posterior
samples2 <- sample(
  p_grid2, size = 10000, 
  prob = posterior2, replace = TRUE
)

# Plot the posterior samples
plot(samples2, pch = 19, col = alpha("black", 0.2))
dens(samples2, xlim = c(0, 1),
     xlab = "Proportion of water on globe")

#==========================================================


# Calculate percentile and highest posterior density
# intervals from our posterior samples

# 50% PI
PI(samples2, prob = 0.5)

dens(samples2, xlim = c(0, 1), xaxs = "i",
     xlab = "Proportion of water on globe",
     main = "50% PI shaded")
shade(
  density(samples2, adjust = 0.5), 
  PI(samples2, prob = 0.5),
  col = "darkgrey"
)

# 50% HPDI
HPDI(samples2, prob = 0.5)

dens(samples2, xlim = c(0, 1), xaxs = "i",
     xlab = "Proportion of water on globe",
     main = "50% HPDI shaded")
shade(
  density(samples2, adjust = 0.5), 
  HPDI(samples2, prob = 0.5),
  col = "darkgrey"
)


# 70% PI
PI(samples2, prob = 0.7)

dens(samples2, xlim = c(0, 1), xaxs = "i",
     xlab = "Proportion of water on globe",
     main = "70% PI shaded")
shade(
  density(samples2, adjust = 0.5), 
  PI(samples2, prob = 0.7),
  col = "darkgrey"
)

# 70% HPDI
HPDI(samples2, prob = 0.7)

dens(samples2, xlim = c(0, 1), xaxs = "i",
     xlab = "Proportion of water on globe",
     main = "70% HPDI shaded")
shade(
  density(samples2, adjust = 0.5), 
  HPDI(samples2, prob = 0.7),
  col = "darkgrey"
)


# With a less skewed posterior, the PI and HPDI are
# often going to be very, very similar

# 70% PI
PI(samples, prob = 0.7)

dens(samples, xlim = c(0, 1),
     xlab = "Proportion of water on globe",
     main = "70% PI shaded")
shade(
  density(samples, adjust = 0.5), 
  PI(samples, prob = 0.7),
  col = "darkgrey"
)

# 70% HPDI
HPDI(samples, prob = 0.7)

dens(samples, xlim = c(0, 1),
     xlab = "Proportion of water on globe",
     main = "70% HPDI shaded")
shade(
  density(samples, adjust = 0.5), 
  HPDI(samples, prob = 0.7),
  col = "darkgrey"
)

#==========================================================


# Generate a posterior predictive distribution for 
# binomial trials of size 10, given the posterior in
# samples2
preds <- rbinom(10000, size = 10, prob = samples2)

plot(preds, pch = 19, col = alpha("black", 0.2))
simplehist(preds, xlim = c(0, 10))


# Note that even if we fix the probability of success value
# there would still be variation in our outcomes because
# the data-generating process is subject to outcome 
# uncertainty
test.outcomes <- rbinom(10000, size = 10, prob = 0.8)

simplehist(test.outcomes, xlim = c(0, 10))
