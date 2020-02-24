

# Demonstrate grid approximation of the posterior for a 
# globe tossing problem with 6 water observations out of 
# 9 globe tosses

#==========================================================


# 1) Define the grid to be used to compute the posterior

# In this case, let's say we choose 20 grid points
# Since the parameter of interest (proportion of water on 
# the globe) is bounded by 0 and 1, our grid should have 
# those limits as well
p_grid <- seq(from = 0, to = 1, length.out = 20)

p_grid

#==========================================================


# 2) Compute/define the value of the prior at each 
# parameter value on the grid

# In this case, simply choose a flat prior
prior <- rep(1, 20)

# You could visualize this prior
plot(p_grid, prior, pch = 19, type = "b")

#==========================================================


# 3) Compute the likelihood at each parameter value on 
# the grid

# This requires the use of a likelihood function applied 
# to the observed data, evaluated at each potential 
# parameter value on the grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

# You could also visualize the likelihood
plot(p_grid, likelihood, pch = 19, type = "b")

#==========================================================


# 4) Compute the unstandardized posterior at each 
# parameter value on the grid

# The unstandardized posterior is simply the product of 
# the likelihood and prior
unstd.posterior <- likelihood * prior

# Again, could visualize
plot(p_grid, unstd.posterior, pch = 19, type = "b")

# Note that this unstandardized posterior is not a proper 
# probability distribution since it does not add to 1
sum(unstd.posterior)

#==========================================================


# 5) Standardize the posterior

posterior <- unstd.posterior/sum(unstd.posterior)

# This standardized posterior is a now proper probability 
# distribution
sum(posterior)

# Visualize the posterior
plot(p_grid, posterior, pch = 19, type = "b",
     xlab = "proportion of water on globe",
     ylab = "posterior probability")
