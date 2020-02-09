

# Load relevant packages and data

library(dplyr)
library(ggplot2)
library(treemapify)
# Note: You won't need "treemapify" for further work in
# this course. It's only for demonstration during lecture

data("iris")

#==========================================================


# Plotting continuous data on both the x- and y-axes
# These are commonly called scatter plots or XY plots


# With base R, we can use the "plot()" function, 
# specifying the relevant x and y variables

plot(iris$Petal.Length, iris$Petal.Width)

# Alternatively, we can specify a plotting formula 
# of the form: y ~ x. You can read this formula as 
# saying "y distributed by x". 

plot(iris$Petal.Width ~ iris$Petal.Length)

# If we specify variables using the formula 
# implementation, we can also specify the data frame 
# we're using with the "data" argument, meaning we 
# don't have to use the "$" operator in the y ~ x 
# formula

plot(Petal.Width ~ Petal.Length, data = iris)

# There's a ton of customization possible with various
# arguments to "plot()". Here's the addition of a plot 
# title and better axis labels

plot(Petal.Width ~ Petal.Length, data = iris,
     main = "My Iris Plot",
     xlab = "Petal Length", ylab = "Petal Width")

# We can also customize the plotting area extent using
# "xlim" and "ylim" arguments, which are used to specify
# the minimum and maximum values shown on the x- and 
# y-axis, respectively

plot(Petal.Width ~ Petal.Length, data = iris,
     main = "My Iris Plot",
     xlab = "Petal Length", ylab = "Petal Width",
     xlim = c(0, 8), ylim = c(0, 4))

# The "pch" argument is also pretty important since it
# allows you to specify the point type used

plot(Petal.Width ~ Petal.Length, data = iris,
     main = "My Iris Plot",
     xlab = "Petal Length", ylab = "Petal Width",
     xlim = c(0, 8), ylim = c(0, 4),
     pch = 19)

#==========================================================


# Implementation of scatter plots with ggplot2


# With the "ggplot()" function, we have to set up our 
# plot by first specifying the data to be used, along 
# with plot aesthetics. Of particular importance here is
# the need to specify which variables map onto which 
# axis (i.e., the x and y variables)

ggplot(data = iris, 
       mapping = aes(x = Petal.Length, y = Petal.Width))
# So this code initializes our plot, but there is not data 
# shown. For this, we need a geom...

# The relevant geom for a scatter plot is "geom_point()",
# so let's add this to our plot. Note the "+" syntax,
# which so far we've only encountered in ggplot2

ggplot(data = iris, 
       mapping = aes(x = Petal.Length, y = Petal.Width)) +
  geom_point()

# There are other geoms that could theoretically be
# used to plot continuous x and y data, but they won't
# necessarily make sense here. For example, you 
# could see what happens if you substitute
# "geom_point()" with "geom_line()"...


# With "ggplot()", there are also numerous plot 
# modifications that are possible. Many of these are
# achievable by adding elements to the plot. For
# example, there are multiple "theme" plot
# elements that change the overall plot style.
# "theme_minimal()" is one that reduces some of the
# clutter in the plot

ggplot(data = iris, 
       mapping = aes(x = Petal.Length, y = Petal.Width)) +
  geom_point() +
  theme_minimal()

# Similar to base R plotting, you can change 
# the plot title and axis labels as well

ggplot(data = iris, 
       mapping = aes(x = Petal.Length, y = Petal.Width)) +
  geom_point() +
  theme_minimal() +
  ggtitle("My Iris Plot") +
  xlab("Petal Length") +
  ylab("Petal Width")

# And the axis limits...

ggplot(data = iris, 
       mapping = aes(x = Petal.Length, y = Petal.Width)) +
  geom_point() +
  theme_minimal() +
  ggtitle("My Iris Plot") +
  xlab("Petal Length") +
  ylab("Petal Width") +
  xlim(0, 8) +
  ylim(0, 4)

#==========================================================


# With "ggplot()", there's no reason you can't use 
# multiple geoms on one plot. For example, we can 
# overlay a scatter plot with a smoothed fitted line 
# that is generated for us by "ggplot()"

ggplot(data = iris, 
       mapping = aes(x = Petal.Length, y = Petal.Width)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  ggtitle("My Iris Plot") +
  xlab("Petal Length") +
  ylab("Petal Width") +
  xlim(0, 8) +
  ylim(0, 4)

# There are numerous options just within "geom_smooth()"
# that will alter the look of this plot element

?geom_smooth

# To get rid of the error region around the line:

ggplot(data = iris, 
       mapping = aes(x = Petal.Length, y = Petal.Width)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  ggtitle("My Iris Plot") +
  xlab("Petal Length") +
  ylab("Petal Width") +
  xlim(0, 8) +
  ylim(0, 4)

# To plot a linear best fit line through the points
# instead:

ggplot(data = iris, 
       mapping = aes(x = Petal.Length, y = Petal.Width)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  ggtitle("My Iris Plot") +
  xlab("Petal Length") +
  ylab("Petal Width") +
  xlim(0, 8) +
  ylim(0, 4)

#==========================================================


# We can also add more aesthetic mappings within the
# "aes()" argument. For example, we can see that this
# dataset represents multiple plant species

summary(iris$Species)

# So we could assign the color of the points ("color"
# argument within "aes()") to represent the Species 
# variable

ggplot(data = iris, 
       mapping = aes(x = Petal.Length, y = Petal.Width, 
                     color = Species)) +
  geom_point() +
  theme_minimal() +
  ggtitle("My Iris Plot") +
  xlab("Petal Length") +
  ylab("Petal Width") +
  xlim(0, 8) +
  ylim(0, 4)

# Instead of distinguishing species based on color, 
# another nice option is to "facet" across species. In 
# other words, we generate mini-plots for each value
# of the categorical variable we're interested in. This 
# is a good way to visually compare the same data
# across groups. The easy way to to this is with 
# "facet_wrap()" as a plot element although 
# "facet_grid()" will allow you to output more 
# complex panel layouts

ggplot(data = iris, 
       mapping = aes(x = Petal.Length, y = Petal.Width)) +
  geom_point() +
  theme_minimal() +
  ggtitle("My Iris Plot") +
  xlab("Petal Length") +
  ylab("Petal Width") +
  xlim(0, 8) +
  ylim(0, 4) +
  facet_wrap(~Species) 

#==========================================================


# Note that we don't have to explicitly write out the
# "data" and "mapping" argument names, so we can simplify 
# our written code slightly once we understand what's
# going on with the "ggplot()" function call

ggplot(iris, 
       aes(x = Petal.Length, y = Petal.Width,
           color = Species)) +
  geom_point() +
  theme_minimal() +
  ggtitle("My Iris Plot") +
  xlab("Petal Length") +
  ylab("Petal Width") +
  xlim(0, 8) +
  ylim(0, 4)


# Also, let's recall the "%>%" operator. Using the 
# "%>%" operator to feed into "ggplot()" can be 
# particularly powerful since "ggplot()" is always 
# expecting data as its first argument. So similar to 
# how we can easily chain together dplyr commands using
# "%>%", we can also use the "%>% operator to feed data
# into "ggplot()"

iris %>%
  ggplot(aes(x = Petal.Length, y = Petal.Width, 
             color = Species)) +
  geom_point() +
  theme_minimal() +
  ggtitle("My Iris Plot") +
  xlab("Petal Length") +
  ylab("Petal Width") +
  xlim(0, 8) +
  ylim(0, 4)

# If you think this set up could be useful with the other
# dplyr "verb" functions, you're definitely right. Suppose
# we didn't care to see the data for "virginica" in 
# this plot. We can exclude it with a "filter()"
# call, which results in "ggplot()" getting data that no
# longer has "virginica" in it at all

iris %>%
  filter(Species != "virginica") %>%
  ggplot(aes(x = Petal.Length, y = Petal.Width,
             color = Species)) +
  geom_point() +
  theme_minimal() +
  ggtitle("My Iris Plot") +
  xlab("Petal Length") +
  ylab("Petal Width") +
  xlim(0, 8) +
  ylim(0, 4)

#==========================================================


# We may also be interested in plotting across a 
# categorical variable. In the iris dataset, species is a
# natural category to be interested in

# The "geom_bar()" geom only requires an x variable and
# counts the number of cases (observations) in the data
# across unique values of x

ggplot(iris, aes(x = Species)) +
  geom_bar() +
  theme_minimal()
# So each species category has a count of 50, since there
# are 50 observations of each species in the dataset

# Note that if we use "geom_bar()" with continuous data,
# we get a histogram of sorts

ggplot(iris, aes(x = Petal.Width)) +
  geom_bar() +
  theme_minimal()

# Compare that visual output with what is summarized by:

table(iris$Petal.Width)


# In contrast, "geom_col()" requires both an x and y
# variable. This is the classic bar chart you see in 
# many different contexts. By default, "geom_col()" will
# sum the y-axis variable values within a given 
# x-axis category 

ggplot(iris, aes(x = Species, y = Petal.Width)) +
  geom_col() +
  theme_minimal()

# If you instead want the mean value of a variable
# across categories, you'll need to do some calculation
# manually...

iris %>%
  group_by(Species) %>%
  summarize(mean.Petal.Width = mean(Petal.Width)) %>%
  ggplot(aes(x = Species, y = mean.Petal.Width)) +
  geom_col() +
  theme_minimal()


# However, bar charts in general are pretty bad. They 
# overly simplify your data and leave out important
# information. A better alternative is a boxplot, which
# shows relevant summary information for a variable 
# across the x-axis categories (rather than a simple mean)

ggplot(iris, aes(x = Species, y = Petal.Width)) +
  geom_boxplot(fill = "mistyrose") +
  theme_minimal()

# Even better: show all the raw data when feasible

ggplot(iris, aes(x = Species, y = Petal.Width)) +
  geom_point() +
  theme_minimal()

# Note one significant problem with this approach: 
# overplotting. Points overlap each other, so it's 
# impossible to see all the data. It's unclear how
# many data points are actually represented given the
# overlapping points. "geom_jitter()" is an alternative
# to "geom_point()" that can help...

ggplot(iris, aes(x = Species, y = Petal.Width)) +
  geom_jitter() +
  theme_minimal()

# By default, "geom_jitter()" is going to jitter, or
# generate some amount of random variation, in both the 
# x and y directions. Since we're interested in the
# precise value of the y variable here, we probably 
# want to specify no jitter in the y direction 
# (i.e., no alteration of the raw y values) and some 
# jitter in the x direction to reveal overlapping points

ggplot(iris, aes(x = Species, y = Petal.Width)) +
  geom_jitter(width = 0.25, height = 0) +
  theme_minimal()

# Again, geoms can be combined if you'd like to show some 
# sort of summary plotting strategy and the raw data

ggplot(iris, aes(x = Species, y = Petal.Width)) +
  geom_boxplot(fill = "mistyrose", outlier.shape = NA) +
  geom_jitter(width = 0.25, height = 0) +
  theme_minimal()

#==========================================================


# gapminder data example shows the benefits of 
# displaying the raw data values whenever possible

gapminder <- read.csv("data/gapminder.csv")

# Create a gapminder data subset with data from the 10
# most populous Asian countries as of 2007

g <- gapminder %>%
  # Filter to only Asian countries in 2007
  filter(continent == "Asia", year == 2007) %>%
  # Arrange by population, high to low
  arrange(desc(pop)) %>%
  # Slice off the top 10 rows, so the 10 most populous
  # countries
  slice(1:10)

g

# Here's one example of a way to visualize the data, with
# the area of the tiles representing the population size
# variable for a given country

g %>%
  # Area of the boxes will be mapped to population
  ggplot(aes(area = pop, label = country)) +
  # Use "geom_treemap()" from the package "treemapify"
  geom_treemap(fill = "mistyrose") +
  # Add labels for the countries on the plot
  geom_treemap_text(fontface = "bold", place = "center")

# The problem is, this visualization obscures the raw
# data! Without a legend of some sort, it's impossible to
# know what the actual population values might be. In 
# addition, it's quite hard to accurately compare between
# countries with this visualization. Which had more people
# in 2007, Bangladesh or Pakistan? Thailand or Iran? It's
# very hard to judge...

# A simpler approach just showing the raw data values
# will, in many cases, be preferable

g <- g %>%
  # Create a population in millions variable and ensure
  # that country is a factor variable with levels 
  # corresponding to decreasing population size
  mutate(pop_in_millions = (pop/1000000),
         country = factor(country, levels = country))

g %>%
  # Population in millions will be on the x-axis,
  # country will be on the y-axis
  ggplot(aes(x = pop_in_millions, y = country)) +
  xlab("Population in Millions") +
  ylab("Country") +
  xlim(0, 1500) +
  geom_point() +
  theme_minimal()
