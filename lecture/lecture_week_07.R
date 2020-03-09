

library(rethinking)

#==========================================================


# Linear regression of milk energy data

# Single predictor: neocortex percentage (np)


# Prepare milk energy data for analysis
data(milk)
d <- milk
dcc <- d[complete.cases(d), ]

# Plot raw data
plot(kcal.per.g ~ neocortex.perc,
     data = dcc, col = "cornflowerblue", pch = 19,
     xlim = c(0, 100), ylim = c(0, 1))

# Visualize priors for intercept, slope, and standard
# deviation parameters
curve(
  dnorm(x, 0, 100), 
  from = -300, to = 300,
  main = "Prior for a (intercept)",
  xlab = "a",
  ylab = "Density"
)
curve(
  dnorm(x, 0, 1), 
  from = -10, to = 10,
  main = "Prior for bn (slope)",
  xlab = "bn",
  ylab = "Density"
)
curve(
  dunif(x, 0, 1), 
  from = -5, to = 5,
  main = "Prior for sigma",
  xlab = "sigma",
  ylab = "Density"
)

# Fit linear regression using map()
m5.5 <- map(
  data = dcc, # specify data to fit
  alist(
    kcal.per.g ~ dnorm(mu, sigma), # likelihood
    mu <- a + bn*neocortex.perc,
    a ~ dnorm(0, 100), # prior for the intercept
    bn ~ dnorm(0, 1), # prior for the np effect
    sigma ~ dunif(0, 1) # prior for the standard deviation
  )
)

# Summarize model output
precis(m5.5, digits = 3, prob = 0.9)

# Get posterior samples
post1 <- extract.samples(m5.5, n = 10000)

# Plot posterior samples
dens(
  post1$a,
  main = "Posterior for a (intercept)", 
  xlab = "a",
  show.HPDI = 0.9
)
dens(
  post1$bn,
  main = "Posterior for bn (slope)",
  xlab = "bn",
  show.HPDI = 0.9
) 
# Correlation of intercept and slope
plot(
  post1$a, post1$bn,
  xlab = "a (intercept)", ylab = "bn (slope)",
  pch = 19
)


# Counterfactual plot


# Generate counterfactual data
summary(dcc$neocortex.perc)
np.seq <- 0:100 # sequence of np values to use
pred.data <- data.frame(neocortex.perc = np.seq)

# Generate values for Gaussian mean
mu <- link(m5.5, data = pred.data, n = 10000)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.95)

# Plotting counterfactuals
plot(kcal.per.g ~ neocortex.perc, 
     data = dcc, type = "n",
     xlim = c(0, 100), ylim = c(0, 1))
lines(np.seq, mu.mean)
shade(mu.PI, np.seq)

#==========================================================


# Linear regression of milk energy data

# Single predictor: log body mass


# Prepare milk energy data for analysis
dcc$log.mass <- log(dcc$mass)

# Plot raw data
plot(kcal.per.g ~ log.mass,
     data = dcc, col = "cornflowerblue", pch = 19,
     xlim = c(-5, 5), ylim = c(0, 1))

# Fit linear regression using map()
m5.6 <- map(
  data = dcc, # specify data to fit
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bm*log.mass,
    a ~ dnorm(0, 100), # prior for the intercept
    bm ~ dnorm(0, 1), # prior for the body mass effect
    sigma ~ dunif(0, 1) # prior for the standard deviation
  )
)

# Summarize model output
precis(m5.6, digits = 3, prob = 0.9)

# Get posterior samples
post2 <- extract.samples(m5.6, n = 10000)

# Plot posterior samples
dens(
  post2$a,
  main = "Posterior for a (intercept)", 
  xlab = "a",
  show.HPDI = 0.9
)
dens(
  post2$bm,
  main = "Posterior for bm (slope)",
  xlab = "bm",
  show.HPDI = 0.9
) 
# Correlation of intercept and slope
plot(
  post2$a, post2$bm,
  xlab = "a (intercept)", ylab = "bm (slope)",
  pch = 19
)


# Counterfactual plot


# Generate counterfactual data
summary(dcc$log.mass)
lbm.seq <- seq(from = -5, to = 5, length.out = 100)
pred.data <- data.frame(log.mass = lbm.seq)

# Generate values for Gaussian mean
mu <- link(m5.6, data = pred.data, n = 10000)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.95)

# Plotting counterfactuals
plot(kcal.per.g ~ log.mass, 
     data = dcc, type = "n",
     xlim = c(-5, 5), ylim = c(0, 1))
lines(lbm.seq, mu.mean)
shade(mu.PI, lbm.seq)

#==========================================================


# Multiple regression of milk energy data

# Two predictors: neocortex percentage and log body mass


# Fit linear regression using map()
m5.7 <- map(
  data = dcc, # specify data to fit
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc + bm*log.mass,
    a ~ dnorm(0, 100), # prior for the intercept
    bn ~ dnorm(0, 1), # prior for the np effect
    bm ~ dnorm(0, 1), # prior for the body mass effect
    sigma ~ dunif(0, 1) # prior for the standard deviation
  )
)

# Summarize model output
precis(m5.7, digits = 3, prob = 0.9)

# Get posterior samples
post3 <- extract.samples(m5.7, n = 10000)

# Plot posterior samples
dens(
  post3$a,
  main = "Posterior for a (intercept)", 
  xlab = "a",
  show.HPDI = 0.9
)
dens(
  post3$bn,
  main = "Posterior for bn (slope)",
  xlab = "bn",
  show.HPDI = 0.9
) 
dens(
  post3$bm,
  main = "Posterior for bm (slope)",
  xlab = "bm",
  show.HPDI = 0.9
) 


# Compare posteriors for the slope parameters from the 
# first two regressions (black lines) with the multiple 
# regression (red lines)
par(mfrow = c(1, 2))
dens(
  post1$bn,
  main = "Posterior for bn (slope)",
  xlab = "bn",
  xlim = c(-0.05, 0.08)
) 
dens(
  post3$bn,
  col = "red", add = TRUE
) 
dens(
  post2$bm,
  main = "Posterior for bm (slope)",
  xlab = "bm",
  xlim = c(-0.2, 0.05)
) 
dens(
  post3$bm,
  col = "red", add = TRUE
) 


# Counterfactual plots


# For neocortex percentage
mean.log.mass <- mean(dcc$log.mass)
pred.data <- 
  data.frame(neocortex.perc = np.seq,
             log.mass = mean.log.mass)

mu <- link(m5.7, data = pred.data, n = 10000)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.95)

plot(kcal.per.g ~ neocortex.perc, 
     data = dcc, type = "n",
     xlim = c(0, 100), ylim = c(0, 2))
lines(np.seq, mu.mean)
shade(mu.PI, np.seq)

# For log body mass
mean.neocortex.perc <- mean(dcc$neocortex.perc)
pred.data <- 
  data.frame(neocortex.perc = mean.neocortex.perc,
             log.mass = lbm.seq)

mu <- link(m5.7, data = pred.data, n = 10000)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.95)

plot(kcal.per.g ~ log.mass, 
     data = dcc, type = "n",
     xlim = c(-5, 5), ylim = c(0, 2))
lines(lbm.seq, mu.mean)
shade(mu.PI, lbm.seq)

par(mfrow = c(1, 1))

#==========================================================


# Computing predictor residuals and using these to fit
# multiple bivariate regressions


# Residual neocortex percentage, after accounting for
# log body mass

plot(neocortex.perc ~ log.mass, data = dcc,
     col = "cornflowerblue", pch = 19)

# Using lm() as a shortcut to visualize best fit line 
# and compute residuals
fit <- lm(neocortex.perc ~ log.mass, data = dcc)
abline(fit)
mu <- coef(fit)[1] + coef(fit)[2]*dcc$log.mass
dcc$np.residual <- resid(fit)

for (i in 1:length(dcc$np.residual)) {
  
  x <- dcc$log.mass[i] # x location of line segment
  y <- dcc$neocortex.perc[i] # observed endpoint of line segment
  # draw the line segments
  lines(c(x, x), c(mu[i], y), 
        lwd = 0.8, col = col.alpha("black", 0.8))
}

# Fit a linear regression using neocortex percentage
# RESIDUALS as a predictor of milk energy
m.np.residual <- map(
  data = dcc,
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bnr*np.residual,
    a ~ dnorm(0, 100),
    bnr ~ dnorm(0, 1),
    sigma ~ dunif(0, 1) 
  )
)


# Residual log body mass, after accounting for neocortex
# percentage

plot(log.mass ~ neocortex.perc, data = dcc,
     col = "cornflowerblue", pch = 19)

# Using lm() as a shortcut to visualize best fit line 
# and compute residuals
fit <- lm(log.mass ~ neocortex.perc, data = dcc)
abline(fit)
mu <- coef(fit)[1] + coef(fit)[2]*dcc$neocortex.perc
dcc$lbm.residual <- resid(fit)

for (i in 1:length(dcc$lbm.residual)) {
  
  x <- dcc$neocortex.perc[i] # x location of line segment
  y <- dcc$log.mass[i] # observed endpoint of line segment
  # draw the line segments
  lines(c(x, x), c(mu[i], y), 
        lwd = 0.8, col = col.alpha("black", 0.8))
}

# Fit a linear regression using log body mass RESIDUALS
# as a predictor of milk energy
m.lbm.residual <- map(
  data = dcc,
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bmr*lbm.residual,
    a ~ dnorm(0, 100),
    bmr ~ dnorm(0, 1),
    sigma ~ dunif(0, 1) 
  )
)


# Compare fit models using residual predictor variables
# with the multiple regression using both original
# variables
precis(m.np.residual)
precis(m.lbm.residual)
precis(m5.7)

# The take home message is that the multiple regression
# using both of the predictor variables effectively 
# controls for the values of the other predictor, giving
# us equivalent results to what we would see if we ran a
# bivariate regression with predictor residuals

#==========================================================


# Linear regression with a categorical predictor variable


# Import Howell data
data(Howell1)
d <- Howell1
str(d)
d$male

# In this case, "male" is already a dummy variable, but we
# could easily create it with an "ifelse()" statement if
# it was a string variable (see book for examples)

# Fit a linear regression using sex as a predictor of 
# height
m5.15 <- map(
  data = d, # specify data to fit
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bm*male,
    a ~ dnorm(178, 100), # intercept prior
    bm ~ dnorm(0, 10), # (male) dummy variable prior
    sigma ~ dunif(0, 50) # standard deviation prior
  )
)

# Summarize the fit model
precis(m5.15)

# Generate posterior samples
post <- extract.samples(m5.15, n = 10000)

# To visualize the expected mean height value for females
dens(
  post$a, 
  xlim = c(120, 160),
  xlab = "Mean height (cm)",
  show.HPDI = 0.5
)

# To visualize the expected mean height value for males
dens(
  post$a + post$bm, 
  col = "darkred", 
  add = TRUE,
  show.HPDI = 0.5
)
