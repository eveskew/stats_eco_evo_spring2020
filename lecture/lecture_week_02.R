

# Reminder: subsetting in base R. We can subset vectors 
# or data frames using brackets

x <- c(25.4, 26.3, 23.2, 28)

x[1] # to get one vector element

x[c(1, 2, 4)] # to get multiple vector elements

# An identical result to the one above could also be
# generated using a logical vector to subset the 
# numeric vector. In this case, an element of the numeric 
# vector will only be returned in cases where the
# matching element of the logical vector is TRUE

x[c(TRUE, TRUE, FALSE, TRUE)]

#==========================================================


# Installing packages 

install.packages("dplyr")

# Or you can do this through the RStudio "Packages" tab

# Loading the package

library(dplyr)

# Getting package help

?dplyr()

#==========================================================


# Importing Ellenton Bay snake capture data

d <- read.csv("data/ebay_snake_captures.csv",
              stringsAsFactors = FALSE)

head(d)

summary(d)

dim(d)

colnames(d)

# Rename columns to be more informative

colnames(d) <- c("date", "time", "trap_type", 
                 "species", "num", "comments")

head(d)

#==========================================================


# Introduction to dplyr

# There are four major dplyr functions that are named
# according to the verbs they behave like: 
# arrange(), select(), filter(), mutate()

# Critically, all of these functions take a data 
# frame as the first argument


# 1) arrange(): sorts the data frame according to the 
# values of a given column

arrange(d, time)

# Use the desc() function within an arrange() call 
# to get the reverse sort order (i.e., from high to 
# low)

arrange(d, desc(time))

# You can arrange by multiple columns simultaneously

arrange(d, species, time)

# sort() in base R is similar, but it only acts on a 
# vector and returns a vector

sort(d$time)

rev(sort(d$time)) # get the reverse sort order with base R


# 2) select(): returns only selected columns from 
# the data frame

select(d, species)

select(d, time, species)

# The analogous base R functionality uses brackets and
# indexing to subset

d[ , "species"] # all rows, "species" column

d[ , c("time", "species")] # all rows, "time" and "species" columns

# You can also select() based simply on column number

select(d, 1)

select(d, 1:3)


# 3) filter(): allows us to filter the data frame based 
# on logical tests

filter(d, time > 1200)

filter(d, time > 2300)

filter(d, species == "Nerodia fasciata")

filter(d, species == "Nerodia fasciata", time >= 1200, time <= 1300)

filter(d, trap_type == "coffee can")

# The analogous base R functionality uses brackets 
# and indexing to subset

d[d$time > 1200, ] 
# "d$time > 1200" returns a TRUE/FALSE logical vector that 
# specifies the rows we want to keep (i.e., the rows 
# that meet the desired condition)

d[d$species == "Nerodia fasciata", ]

d[d$species == "Nerodia fasciata" & d$time >= 1200 & d$time <= 1300, ]


# 4) mutate(): allows us to create new columns

mutate(d, multiple_individuals = ifelse(num > 1, "yes", "no"))

# In base R:

# d$multiple_individuals <- ifelse(d$num > 1, "yes", "no")

#==========================================================


# distinct() is also useful


# distinct(): when given only one variable, shows us 
# the distinct values of that variable

distinct(d, species)

# The base R analog is unique()

unique(d$species)

#==========================================================


# The pipe operator


# The "%>%" operator "pipes" what's on the left side of 
# the operator into the first argument of the subsequent
# function

select(d, species)

d %>% select(species) # same as above

d %>%
  select(species) # same as above with different styling


# The power of this approach is that it allows us to 
# chain together different functions very easily

# start with the data frame "d"
d %>% 
  # only want observations from coffee can traps
  filter(trap_type == "coffee can") %>% 
  # only return the "species", "date", and "time" columns
  select(species, date, time) %>%
  # sort the rows by "species" name
  arrange(species)

# This is (absurdly) equivalent to:

arrange(select(filter(d, trap_type == "coffee can"), species, date, time), species)

# We can use the pipe operator with other functions as 
# well. We just want to be sure that whatever is on the 
# left side of the pipe should be the first argument in 
# the next function

d %>%
  filter(trap_type == "coffee can") %>%
  select(species, date, time) %>%
  arrange(species) %>%
  View()

d %>%
  filter(trap_type == "coffee can") %>%
  select(species, date, time) %>%
  arrange(species, time) %>%
  View()
# this one is also sorted by time, early to late

d %>%
  filter(trap_type == "coffee can") %>%
  select(species, date, time) %>%
  arrange(species, desc(time)) %>%
  View()
# this one is sorted by time, late to early

d %>%
  filter(trap_type == "coffee can") %>%
  select(species, date) %>%
  arrange(species, desc(time)) %>%
  View()
# throws an error because we're not passing it the 
# two columns we're asking to arrange by

#==========================================================


# Commenting out lines is a powerful exploratory tool 
# with this general data filtering pipeline


d %>%
  filter(trap_type == "coffee can") %>%
  select(species, date, time) %>%
  arrange(species, time) %>%
  View()

# Commenting allows us to easily remove lines of code from
# our pipeline and see the result

d %>%
  filter(trap_type == "coffee can") %>%
  select(species, date, time) %>%
  # arrange(species, time) %>%
  View()

d %>%
  filter(trap_type == "coffee can") %>%
  # select(species, date, time) %>%
  # arrange(species, time) %>%
  View()

#==========================================================


# group_by() then summarize()


# group_by() sets up all future operations to be done on 
# a group-wise basis

group_by(d, species)


# Not too exciting on its own, but with summarize(), 
# it's very powerful

d %>%
  # group by snake species
  group_by(species) %>%
  # summarize the total number of captures
  summarize(sum(num)) %>%
  View()

d %>%
  group_by(species) %>%
  # name this new variable "total_snakes_captured"
  summarize(total_snakes_captured = sum(num)) %>%
  View()

d %>%
  group_by(species) %>%
  summarize(total_observations = n()) %>%
  # arrange the data frame by "total_observations"
  arrange(desc(total_observations)) %>%
  View()

d %>%
  # group_by(species) %>%
  summarize(total_observations = n()) %>%
  arrange(desc(total_observations)) %>%
  View()
# with no grouping, the entire data frame is considered 
# a group

# With two variables specified in the "group_by()" call,
# we'll get groups representing every unique combination
# of both variables
d %>%
  group_by(species, trap_type) %>%
  summarize(total_observations = n()) %>%
  arrange(species, trap_type) %>%
  View()

#==========================================================


# Loops: common programming construction that allows us
# to perform repeated operations


beginning <- "Coding"

endings <- c("is useful!", "is fun!", 
             "is great to talk about at social events!")

# What if we wanted to paste together these various
# endings with the beginning word? One option would be
# to write reptitive code that selects each ending in
# turn using bracket indexing:

paste(beginning, endings[1])
paste(beginning, endings[2])
paste(beginning, endings[3])

# Loops allow us to do this sort of repeated operation
# in a compact way, which becomes especially useful
# the more operations we have to perform

for (x in 1:3) {
  
  print(paste(beginning, endings[x]))
}

# You might conceptualize the internal workings of 
# the loop like this:

x <- 1
print(paste(beginning, endings[x]))
x <- 2
print(paste(beginning, endings[x]))
x <- 3
print(paste(beginning, endings[x]))
