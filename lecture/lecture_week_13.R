

library(rethinking)
library(dplyr)

#==========================================================


# Generate a vector of probability values
probabilities <- seq(from = 0, to = 1, by = 0.01)
probabilities

# Compute the odds for each of these probabilities and 
# plot the relationship
odds <- probabilities / (1 - probabilities)
odds
plot(probabilities, odds, type = "n")
lines(probabilities, odds)

# Compute the log-odds for each of these probabilities 
# and plot the relationship
log_odds <- log(odds)
log_odds
plot(probabilities, log_odds, type = "n")
lines(probabilities, log_odds)


# Convert some log-odds values back to probabilities
# using the logistic function
log_odds_seq <- seq(from = -50, to = 50, by = 1)
log_odds_seq
probs_seq <- logistic(log_odds_seq)
plot(log_odds_seq, probs_seq, type = "n")
lines(log_odds_seq, probs_seq)

#==========================================================


# Chimpanzee prosociality binomial models

# Import the chimpanzee data
data(chimpanzees)
d <- chimpanzees

# Fit an intercept-only binomial GLM
m10.1 <- map(
  data = d,
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a,
    a ~ dnorm(0, 10)
  )
)

precis(m10.1, prob = 0.97)

# Consider the fit model parameter
exp(0.32) # on the odds scale
logistic(0.32) # on the probability scale

# Fit a more complex binomial GLM considering the effects
# of prosocial on left and condition treatments
m10.3 <- map(
  data = d,
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + bP*prosoc_left + bPC*prosoc_left*condition,
    a ~ dnorm(0, 10) ,
    bP ~ dnorm(0, 10) ,
    bPC ~ dnorm(0, 10)
  )
)

precis(m10.3, prob = 0.97)


# Visualize predictions from model m10.3

# Dummy data for predictions across treatments
d.pred <- data.frame(
  prosoc_left = c(0, 1, 0, 1), # right/left/right/left
  condition = c(0, 0, 1, 1) # control/control/partner/partner
)

# Build predictions for probability of success using
# "link()"
preds.p <- link(m10.3, data = d.pred)

# Summarize the probability prediction values
preds.p.mean <- apply(preds.p, 2, mean)
preds.p.PI <- apply(preds.p, 2, PI, prob = 0.9)

# Generate an empty plot frame with good axes
plot(0, 0, type = "n", xaxt = "n",
     xlab = "prosoc_left/condition",
     ylab = "proportion pulled left",
     xlim = c(1, 4), ylim = c(0, 1))
axis(1, at = 1:4, labels = c("0/0", "1/0", "0/1", "1/1"))

# Plot raw data, one trend for each of 7 individual
# chimpanzees using "by()"
p <- by(d$pulled_left,
        list(d$prosoc_left, d$condition, d$actor), mean)
for (chimp in 1:7)
  lines(1:4, as.vector(p[, , chimp]), 
        col = rangi2, lwd = 1.5)

# Superimpose posterior predictions
lines(1:4, preds.p.mean)
shade(preds.p.PI, 1:4)


# Replicate model m10.3 using aggregated binomial data

# Generate data in an aggregated form
d.aggregated <- d %>%
  group_by(prosoc_left, condition) %>%
  summarize(
    pulled_left_aggregated = sum(pulled_left),
    n_trials = n()
  ) %>%
  data.frame()

d.aggregated

# Fit the model using aggregated data
m10.5 <- map(
  data = d.aggregated,
  alist(
    pulled_left_aggregated ~ dbinom(n_trials, p),
    logit(p) <- a + bP*prosoc_left + bPC*prosoc_left*condition,
    a ~ dnorm(0, 10),
    bP ~ dnorm(0, 10),
    bPC ~ dnorm(0, 10)
  )
)

# Compare model output
precis(m10.5, prob = 0.97)
precis(m10.3, prob = 0.97)

#==========================================================


# Snow goose color binomial models

# Load in hypothetical snow goose data in aggregated
# binomial format
geese <- data.frame(
  blue_geese = c(215, 84, 7),
  total_geese = c(500, 300, 25),
  study_site = c("Site A", "Site B", "Site C")
)

# Add on a variable indicating the proportion of blue
# morphs at each study site
geese$prop_blue <- geese$blue_geese/geese$total_geese

geese

# Plot the raw proportions of blue morphs
plot(prop_blue ~ study_site, data = geese, 
     ylim = c(0, 0.5))

# Generate dummy variables for site affiliation
geese$site_B <- ifelse(geese$study_site == "Site B", 1, 0)
geese$site_C <- ifelse(geese$study_site == "Site C", 1, 0)

# Fit a binomial GLM using site to predict the 
# probability of a goose being the blue morph
goose.model <- map(
  data = geese,
  alist(
    blue_geese ~ dbinom(size = total_geese, prob = p),
    logit(p) ~ a + b_site_B*site_B + b_site_C*site_C,
    a ~ dnorm(0, 10),
    b_site_B ~ dnorm(0, 10),
    b_site_C ~ dnorm(0, 10)
  )
)

precis(goose.model, prob = 0.97)


# Visualize model inference

# Extract samples from the model posterior
goose.post <- extract.samples(goose.model, n = 10000)

# Show the posterior distribution of the intercept
# parameter (on the log-odds scale), which corresponds
# to study site A
dens(
  goose.post$a, 
  xlab = "Intercept parameter (log-odds scale)"
)
# Show the posterior distribution of the implied 
# probability of blue morphs at study site A
dens(
  logistic(goose.post$a),
  xlab = "Implied probability of a blue goose"
)


# Plot the log-odds of a goose being blue by plotting
# posterior parameter samples

# site A (intercept or reference category)
dens(goose.post$a, xlim = c(-3, 1),
     xlab = "Log-odds of a blue goose")
# site B
dens(goose.post$a + goose.post$b_site_B, 
     add = TRUE, col = "blue")
# site C
dens(goose.post$a + goose.post$b_site_C, 
     add = TRUE, col = "green")

# Plot the implied probability of a goose being blue by 
# plotting posterior parameter samples transformed through
# the logistic function

# site A (intercept or reference category)
dens(logistic(goose.post$a), xlim = c(0, 0.5),
     xlab = "Implied probability of a blue goose")
# site B
dens(logistic(goose.post$a + goose.post$b_site_B), 
     add = TRUE, col = "blue")
# site C
dens(logistic(goose.post$a + goose.post$b_site_C), 
     add = TRUE, col = "green")


# You can do this same type of prediction plot 
# using "link()"

counterfactual.siteA <- data.frame(site_B = 0, site_C = 0)
counterfactual.siteB <- data.frame(site_B = 1, site_C = 0)
counterfactual.siteC <- data.frame(site_B = 0, site_C = 1)

probs.siteA <- link(
  goose.model, 
  data = counterfactual.siteA, 
  n = 10000
)
probs.siteB <- link(
  goose.model, 
  data = counterfactual.siteB, 
  n = 10000
)
probs.siteC <- link(
  goose.model, 
  data = counterfactual.siteC,
  n = 10000
)

dens(probs.siteA, xlim = c(0, 0.5),
     xlab = "Implied probability of a blue goose")
dens(probs.siteB, 
     add = TRUE, col = "blue")
dens(probs.siteC, 
     add = TRUE, col = "green")
