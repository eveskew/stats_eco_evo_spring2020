

library(rethinking)
library(dplyr)

#==========================================================


# Gaussian model of human height data collected by 
# Nancy Howell

# Prepare Howell data for analysis
data(Howell1)
d <- Howell1
d2 <- filter(d, age >= 18) # filter to 18+ years of age

# Visualize priors for the mean and standard deviation
# parameters before fitting the statistical model
curve(
  dnorm(x, 178, 20), 
  from = 50, to = 300,
  main = "Prior for mu",
  xlab = "mu",
  ylab = "Density"
)
curve(
  dunif(x, 0, 50), 
  from = -10, to = 60,
  main = "Prior for sigma",
  xlab = "sigma",
  ylab = "Density"
)

# Fit Gaussian model using map()
m4.1 <- map( 
  data = d2, # specify data to fit
  alist( 
    # define statistical model
    height ~ dnorm(mu, sigma), # likelihood
    mu ~ dnorm(178, 20) , # prior for the mean parameter
    sigma ~ dunif(0, 50) # prior for the sd parameter
  )
)

# Summarize model output
precis(m4.1) # shows PIs by default for map() fits

# Extract posterior samples for all parameters
post <- extract.samples(m4.1, n = 10000)
head(post, 10)

# Plot posterior samples for both parameters
dens(
  post$mu, 
  main = "Posterior for mu", 
  xlab = "mu"
)
dens(
  post$sigma, 
  main = "Posterior for sigma", 
  xlab = "sigma"
) 
# These are called marginal posterior density plots

plot(
  post$mu, post$sigma,
  xlab = "mu", ylab = "sigma",
  pch = 19
)

#==========================================================


# Fit linear model with predictor (weight) using map()

# Visualize the height/weight relationship in the raw data
plot(
  height ~ weight, data = d2,
  xlim = c(20, 70), ylim = c(120, 190),
  pch = 19
)

# Visualize priors for the intercept and slope parameters
curve(
  dnorm(x, 178, 100), 
  from = -300, to = 700,
  main = "Prior for a (intercept)",
  xlab = "a",
  ylab = "Density"
)
curve(
  dnorm(x, 0, 10), 
  from = -50, to = 50,
  main = "Prior for b (slope)",
  xlab = "b",
  ylab = "Density"
)

# Fit linear regression model using map()
m4.3 <- map( 
  data = d2, # specify data to fit
  alist(
    # define statistical model
    height ~ dnorm(mu, sigma), # likelihood
    mu <- a + b*weight, # model mu using a linear formula
    a ~ dnorm(178, 100), # prior for the intercept parameter
    b ~ dnorm(0, 10), # prior for the slope parameter
    sigma ~ dunif(0, 50) # prior for the sd parameter
  )
)

# Summarize model output
precis(m4.3)

# Extract posterior samples for all parameters
post <- extract.samples(m4.3, n = 10000)
head(post, 10)

# Plot posterior samples of a (intercept) and b (slope)
# parameters
dens(
  post$a, 
  main = "Posterior for a (intercept)", 
  xlab = "a"
)
dens(
  post$b, 
  main = "Posterior for b (slope)", 
  xlab = "b"
)

plot(
  post$a, post$b, 
  xlab = "a (intercept)", ylab = "b (slope)",
  pch = 19
)
# We can see in this model these two parameters covary
# strongly with each other

# Why this relationship?
plot(
  height ~ weight, data = d2,
  xlim = c(0, 70), ylim = c(100, 200),
  pch = 19
)
for (i in 1:50) {
  abline(
    a = post$a[i], b = post$b[i],
    col = alpha("seagreen", 0.5)
  )
}

#==========================================================


# Using a centered predictor variable

# Center the weight predictor variable and compare to the
# raw variable
d2$weight.c <- d2$weight - mean(d2$weight)
plot(weight.c ~ weight, data = d2, pch = 19)
cor(d2$weight, d2$weight.c) # perfect correlation

m4.4 <- map(
  data = d2,
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight.c,
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  )
)

precis(m4.4)

# Remember: the intercept is the value of the linear
# portion of the model when all predictors equal 0. 
# That's still what the "a" parameter in model m4.4 
# is telling us. It's just that our predictor has 
# changed meaning slightly, so our interpretation is 
# different. The value of "a" now corresponds to the
# expected mean height of someone with average weight.
plot(
  height ~ weight.c, data = d2, 
  xlim = c(-40, 40), ylim = c(100, 200),
  pch = 19
)

# Can also explicitly define parameter start values
# Not necessary in this case, but sometimes needed to 
# help map() accurately describe the posterior shape

m4.4 <- map(
  data = d2,
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight.c,
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  start = list(a = 100, b = 0, sigma = 2)
)

precis(m4.4)


# To standardize a predictor variable:

d2$weight.s <- 
  (d2$weight - mean(d2$weight)) / sd(d2$weight)
plot(weight.s ~ weight, data = d2, pch = 19)
cor(d2$weight, d2$weight.s) # perfect correlation

# Or:

d2$weight.s <- scale(d2$weight)
plot(weight.s ~ weight, data = d2, pch = 19)
cor(d2$weight, d2$weight.s) # perfect correlation

#==========================================================


# Visualizing posterior parameter estimates

# Show the MAP trend line

# First recognize the information that "coef()" gives you
precis(m4.3)
coef(m4.3)

plot(
  height ~ weight, data = d2,
  xlim = c(0, 70), ylim = c(100, 200),
  pch = 19
)

abline(a = coef(m4.3)["a"], b = coef(m4.3)["b"])

# Show uncertainty in the regression trend line 
# (first 100 estimates)

head(post, 10)

plot(
  height ~ weight, data = d2,
  xlim = c(0, 70), ylim = c(100, 200),
  pch = 19
)

for (i in 1:100) {
  abline(
    a = post$a[i], b = post$b[i],
    col = alpha("seagreen", 0.1)
  )
}

#==========================================================


# Generating model-based predictions

# Generate 10,000 predicted heights for an individual
# of 50 kilograms

preds.50 <- rnorm(
  10000,
  mean = post$a + post$b*50,
  sd = post$sigma
)

plot(
  preds.50, 
  ylab = "Predicted Height (cm) for an Individual of 50 kg"
)
dens(
  preds.50,
  xlab = "Predicted Height (cm) for an Individual of 50 kg"
)

# Or rethinking has built-in convenience functions to
# generate predicted mu values (link) or full 
# predictions (sim) for you

mu.50.rethinking <- 
  link(m4.3, data = list(weight = 50), n = 10000)

dens(
  mu.50.rethinking,
  xlab = "Predicted Mean Height (cm) for an Individual of 50 kg"
)

preds.50.rethinking <-
  sim(m4.3, data = list(weight = 50), n = 10000)

dens(
  preds.50.rethinking, col = "darkgreen",
  xlab = "Predicted Height (cm) for an Individual of 50 kg"
)
dens(preds.50, col = "darkred", add = TRUE)
