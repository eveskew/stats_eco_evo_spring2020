

library(rethinking)

#==========================================================


# Load the reed frog data
data(reedfrogs)
d <- reedfrogs

# Make the tank cluster variable
d$tank <- 1:nrow(d)

# Fit a binomial GLM predicting frog survival using a
# tank-level index variable
m12.1 <- map2stan(
  data = d,
  alist(
    surv ~ dbinom(size = density, prob = p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(0, 5)
  ),
  chains = 4,
  iter = 5000
)

precis(m12.1, depth = 2, prob = 0.97)

# Fit a binomial multilevel model predicting frog survival 
# using a tank-level index variable
m12.2 <- map2stan(
  data = d, 
  alist(
    surv ~ dbinom(size = density, prob = p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1)
  ), 
  chains = 4,
  iter = 5000
)

precis(m12.2, depth = 2, prob = 0.97)

#==========================================================


# Show shrinkage of multilevel model parameter estimates


# Extract posterior samples from model m12.1
post.glm <- extract.samples(m12.1)

# Extract posterior samples from model m12.2
post.glmm <- extract.samples(m12.2)

# Note: this will not work since you're asking for more
# samples than you have
post.glmm <- extract.samples(m12.2, n = 20000)

# Compute median intercept for each tank and transform to 
# probability with "logistic()"
d$propsurv.est.glm <- 
  logistic(apply(post.glm$a_tank, 2, median))
d$propsurv.est.glmm <- 
  logistic(apply(post.glmm$a_tank, 2, median))


# Display raw proportions surviving in each tank
plot(d$propsurv, ylim = c(0, 1), pch = 16, xaxt = "n",
     xlab = "tank", 
     ylab = "proportion survival", 
     col = rangi2)
axis(1, at = c(1, 16, 32, 48), 
     labels = c(1, 16, 32, 48))

# Overlay posterior medians from the GLM...
points(d$propsurv.est.glm, col = "darkred")


# Display raw proportions surviving in each tank
plot(d$propsurv, ylim = c(0, 1), pch = 16, xaxt = "n",
     xlab = "tank", 
     ylab = "proportion survival", 
     col = rangi2)
axis(1, at = c(1, 16, 32, 48), 
     labels = c(1, 16, 32, 48))

# Overlay posterior medians from the mulitlevel model
points(d$propsurv.est.glmm)

# Mark posterior median probability across tanks
abline(h = logistic(median(post.glmm$a)), lty = 2)

# Draw vertical dividers between tank densities
abline(v = 16.5, lwd = 0.5)
abline(v = 32.5, lwd = 0.5)
text(8, 0, "small tanks")
text(16+8, 0, "medium tanks")
text(32+8, 0, "large tanks")


# You could get a very similar plot using "coef()" to
# extract posterior means for parameters

# Display raw proportions surviving in each tank
plot(d$propsurv, ylim = c(0, 1), pch = 16, xaxt = "n",
     xlab = "tank", 
     ylab = "proportion survival", 
     col = rangi2)
axis(1, at = c(1, 16, 32, 48), 
     labels = c(1, 16, 32, 48))

# Overlay posterior means from the multilevel model
points(logistic(coef(m12.2)[1:48]))

# Mark posterior mean probability across tanks
abline(h = logistic(mean(coef(m12.2)["a"])), lty = 2)

# Draw vertical dividers between tank densities
abline(v = 16.5, lwd = 0.5)
abline(v = 32.5, lwd = 0.5)
text(8, 0, "small tanks")
text(16+8, 0, "medium tanks")
text(32+8, 0, "large tanks")

#==========================================================


# Simulating frog survival data among "ponds"

a <- 1.4 # average pond's log-odds of survival
sigma <- 1.5 # survival standard deviation among ponds
nponds <- 60 # number of ponds to simulate
ni <- # sample size across ponds
  as.integer(rep(c(5, 10, 25, 35), each = 15))

# Simulate "true" pond-level log-odds of survival
a_pond <- rnorm(nponds, mean = a, sd = sigma)

# Package data into a data frame
dsim <- data.frame( 
  pond = 1:nponds, 
  ni = ni, 
  true_a = a_pond
)

# Simulate survivors across ponds
dsim$si <- rbinom(nponds, size = dsim$ni,
                  prob = logistic(dsim$true_a))


# Plot observed survival across ponds
plot(1:nponds, dsim$si/dsim$ni, ylim = c(0, 1),
     xlab = "pond", 
     ylab = "proportion survival")
# Add on true pond-level survival probabilities
points(logistic(dsim$true_a), col = "red", pch = 20)

# Draw vertical dividers between pond densities
abline(v = 15.5, lwd = 0.5)
abline(v = 30.5, lwd = 0.5)
abline(v = 45.5, lwd = 0.5)
text(7.5, 0, "tiny ponds")
text(15+7.5, 0, "small ponds")
text(30+7.5, 0, "medium ponds")
text(45+7.5, 0, "large ponds")

# Draw horizontal line representing the average 
# pond's true proportion survival 
abline(h = logistic(a), lty = 2)

#==========================================================


# Demonstrate that taking the mean out of the Gaussian
# distribution and treating it as a constant gives
# equivalent results
y1 <- rnorm(10000, 10, 1) # mean inside the Gaussian
y2 <- 10 + rnorm(10000, 0, 1) # mean as a constant

dens(y1, col = alpha("red", 0.5), xlab = "Outcome value")
dens(y2, col = alpha("green", 0.5), add = TRUE)


# Fit a binomial multilevel model predicting frog survival 
# using a tank-level index variable, moving the mean out 
# of the Gaussian distribution and treating it as a 
# constant
m12.2.alt <- map2stan(
  data = d,
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a + a_tank[tank],
    a_tank[tank] ~ dnorm(0, sigma),
    a ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1)
  ), 
  chains = 4,
  iter = 5000 
)

precis(m12.2.alt, depth = 2, prob = 0.97)
precis(m12.2, depth = 2, prob = 0.97)
