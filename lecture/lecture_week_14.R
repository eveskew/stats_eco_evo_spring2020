

library(rethinking)

#==========================================================


# Load the reed frog data
data(reedfrogs)
d <- reedfrogs

# Make the tank cluster variable
d$tank <- 1:nrow(d)

str(d)

# Plot the observed tank-level survival proportions
plot(propsurv ~ tank, data = d, pch = 19, ylim = c(0, 1))

#==========================================================


# Fit a binomial GLM predicting frog survival using a
# tank-level index variable
m12.1 <- map2stan(
  data = d,
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(0, 5)
  ),
  chains = 4,
  iter = 5000
)

precis(m12.1, prob = 0.97)

# Now need to use the "depth" argument to show all fit
# model parameters...
precis(m12.1, depth = 2, prob = 0.97)

# And when working with posterior samples, we have to
# deal with new data structures...
post <- extract.samples(m12.1)
str(post)
is.list(post)
is.matrix(post$a_tank)

# To get the posterior samples for the first tank:
post$a_tank[, 1]
# To get the posterior samples for the second tank:
post$a_tank[, 2]

# Plot the implied probability of survival for tanks
# 9, 32, and 44
# Note that these all have the same observed proportion
# of tadpole survival...
dens(
  logistic(post$a_tank[, 9]), 
  xlim = c(0, 1), ylim = c(0, 10),
  xlab = "Implied probability of survival",
  ylab = "Posterior density"
)

dens(
  logistic(post$a_tank[, 32]), 
  xlim = c(0, 1), ylim = c(0, 10),
  xlab = "Implied probability of survival",
  ylab = "Posterior density"
)

dens(
  logistic(post$a_tank[, 44]), 
  xlim = c(0, 1), ylim = c(0, 10),
  xlab = "Implied probability of survival",
  ylab = "Posterior density"
)

#==========================================================


# Fit a binomial multilevel model predicting frog survival 
# using a tank-level index variable
m12.2 <- map2stan(
  data = d, 
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1)
  ), 
  chains = 4,
  iter = 5000
)

precis(m12.2, prob = 0.97)
precis(m12.2, depth = 2, prob = 0.97)

#==========================================================


# Visualization to demonstrate that the tank-level
# intercepts are being drawn from (i.e., constrained by)
# a Gaussian distribution

# Extract posterior samples
post <- extract.samples(m12.2)

# Generate an x-axis sequence for plotting
x.seq <- seq(from = -5, to = 7, by = 0.01)

# Plot the Gaussian distribution implied by the posterior
# means for the "a" and "sigma" parameters
plot(
  x.seq, 
  dnorm(
    x.seq, 
    mean = coef(m12.2)["a"], 
    sd = coef(m12.2)["sigma"]
  ), 
  xlab = "log-odds of survival", ylab = "Density",
  type = "n"
)
      
lines(
  x.seq, 
  dnorm(
    x.seq, 
    mean = coef(m12.2)["a"], 
    sd = coef(m12.2)["sigma"]
  )
)

# Plot as vertical lines, with some time lag, the posterior
# means for the tank-level intercepts 
for (i in 1:48) {
  
  Sys.sleep(time = 0.5)
  
  points(
    x = coef(m12.2)[i],
    y = dnorm(
      coef(m12.2)[i], 
      mean = coef(m12.2)["a"], 
      sd = coef(m12.2)["sigma"]
    ),
    col = "red",
    pch = "|"
  )
}

#==========================================================


# How do the tank-level intercept estimates from the GLM
# and the multilevel model compare?
plot(
  coef(m12.1), coef(m12.2)[1:48], pch = 19,
  xlim = c(-7, 7), ylim = c(-7, 7),
  xlab = "Tank-level intercept mean est. from GLM",
  ylab = "Tank-level intercept mean est. from multilevel model"
)
abline(a = 0, b = 1, lty = 2)
