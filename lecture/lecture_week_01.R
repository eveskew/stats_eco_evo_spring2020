

# Explain relationship between R and RStudio, scripts, etc.

#==========================================================


# Basic mathematical operations in R

# All work pretty much as you'd expect, given the core
# mathematical operators

2+2 # addition

2-1 # subtraction

2*3 # multiplication

2/4 # division

2^4 # exponents

#==========================================================


# Assignment 

# In R, as in programming languages generally, we often 
# want to "assign" certain values to objects in order to
# reference those objects (and associated values) later 

# In R, you can assign using the "=" operator

x = 2 + 2 

# In other words, you're saying that "x" should equal
# what is placed on the right side of the equals sign

x # then type the name of the object to see the value(s)

# You can also see existing objects in your "Environment" 
# tab within RStudio

# Alternatively, and preferably, you can use the "<-" 
# operator for assignment

y <- 3 + 3

y

x <- y 

# This is clearer than "x = y", so the "<-" operator is
# often preferred. With the "<-" operator, we see 
# intuitively that "x" takes on the value of "y". 
# In contrast, the "=" operator may seem ambiguous

x # "x" now has the value previously assigned to "y"

y # "y" retains its value

#==========================================================


# Commenting

# You can use the "#" character to write portions of your 
# script that will NOT be interpreted as code, and 
# therefore effectively ignored. You should comment 
# liberally to remind yourself of code purpose and to
# document your ideas for any other readers

#==========================================================


# Get and set working directories

getwd()

# Anything followed by parentheses are functions in R. 
# A function usually takes a series of arguments and 
# performs some operation, but sometimes they don't need 
# any arguments. Function names often state pretty
# clearly what they do

setwd()

# Why didn't this work?

# Get help in R using the "?" operator preceding the 
# function in question

?setwd()

# The "Help" tab in RStudio can also be navigated 
# interactively

setwd("~/Desktop/")

?list.files()

list.files() # lists all files in the working directory

setwd("~/Documents/Statistics and Coding/stats_eco_evo_spring2020/")

list.files()

#==========================================================


# Vectors (with numeric data types)

# A vector in R is a collection of values of the same data
# type

c(5.9, 7.2, 6.3)

weights <- c(5.9, 7.2, 6.3)

weights

# What is "c()" doing?

?c() # concatenates values into a vector or list

sum(weights) # sum all values in "weights"

mean(weights) # find the mean of all values in "weights"

# Note that many operations in R, including some 
# mathematical operations, are vectorized, meaning that the
# operation indicated will be performed on EACH element 
# of a vector

weights + 10 # all elements of "weights" with 10 added

#==========================================================


# Vectors (with character data types)

# We are not limited to using numbers. R also supports 
# character data (i.e., text) 

frogs <- 
  c("Hyla avivoca", "Hyla gratiosa", "Hyla versicolor")

frogs

sum(frogs) 
# this function doesn't make any sense with character data

mean(frogs) 
# this function doesn't make any sense with character data

#==========================================================


# Key functions to inspect objects in R

str(weights) # structure of the data

summary(weights) # summary of the data

View(weights) # view the data in a new RStudio tab

# Note, you can't use "View()" in an R Markdown document

str(frogs)

summary(frogs)

View(frogs)

#==========================================================


# Sequences

# Use the ":" operator to generate a sequence of 
# consecutive integers

1:3 # generates a sequence 1 through 3

1:10 # generates a sequence 1 through 10

1:(3^6) # generates a sequence 1 through 729 (i.e., 3^6)

# Use "seq()" to generate more complex sequences

?seq()

# This function takes more than one argument, as many 
# functions do

seq(from = 1, to = 3, by = 1) 
# within functions, argument values must be assigned 
# with the "=" operator

seq(from = 1, to = 4, by = 1.5)

my.seq <- seq(from = 0, to = 100, by = 0.5)

str(my.seq)

summary(my.seq)

length(my.seq) # how many values are in the object?

View(my.seq)

#==========================================================


# Indexing

# Indexing is the process of subsetting your data to 
# obtain only specific values. In R, we index by using
# bracket notation

my.seq[1] # returns the first value of the vector

my.seq[1, 2] 
# invalid because the vector only has one dimension

my.seq[c(1, 2)] # returns the first and second values

my.seq[1:20] # returns the first twenty values

my.seq[101]

my.seq[500] # invalid index value; only 201 "my.seq" values

my.short.seq <- my.seq[1:101]

#==========================================================


# What happens if we try to combine data types into 
# one vector?

mashup <- c(16.9, "Hyla gratiosa")

str(mashup) # the number has been coerced into a character 

# This can cause confusion and frustration down the road
# if you're unaware this is what's happening

summary(mashup)

# Vectors can only support one data type

#==========================================================


# Data frames

# Data frames can hold collections of different data types

d <- data.frame(frogs, weights, stringsAsFactors = FALSE) 

str(d)

summary(d)

View(d)

# Inspect and change column names in a data frame

?colnames()

colnames(d)

colnames(d)[1]

colnames(d)[1] <- "species"

colnames(d)

View(d)

# You can refer to specific columns within a data frame
# using the "$" operator

d$species

d$weights

# Indexing with data frames

d[1, 1] 
# refers to row and column values, separated by a comma

d[1, 2] # first row, second column

d[1, 3] # first row, third column (doesn't exist)

d[1, ] 
# if you leave the value blank for a given dimension, 
# you'll get all available values

d[ , 2] # all rows, second column

d[3, 2] # third row, second column

# Another way to do this is to call the second column 
# directly and then index

d$weights[3]

# What if we want to add data to the data frame?

frogs <- c(frogs, "Hyla chrysoscelis")

frogs

d <- data.frame(frogs, weights, stringsAsFactors = FALSE) 
# Nope! Every column of the data frame must have the same 
# number of observations

weights <- c(weights, NA) 
# add a value to the "weights" vector indicating 
# missingness

mean(weights)
# our weights vector now has NA values, so mean() 
# returns NA

# We need an additional argument telling "mean()" to 
# remove NA values when doing its calculation

mean(weights, na.rm = TRUE)

d <- data.frame(frogs, weights)

View(d)
