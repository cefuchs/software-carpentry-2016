###################################
# Software Carpentry Workshop April 14, 2016
#
#####################################
detach() # will get rid of/clear all your working history
rm(list=ls()) # also gets rid of things

## !!!! IMPORTANT !!!! In read.csv, na.strings = allows you to assign what strings equal "NA." So, NA is NA, but so is w/e you put in there;; e.g. a single period or, maybe !#DIV/0....

# changing work directory
setwd('data/') # I set the home directory when I created the project

# to list files in wd
list.files()

inf1 <- read.csv(file = 'inflammation-01.csv', header = FALSE) # header = F because we don't have column names on the data- the first row IS data
# each row is an individual raptor. each col is a day. data is an inflammation metric

weight_kg <- 55
weight_kg

weight_kg <- 57.5
# convert weight from kg to lbs
weight_lb <- weight_kg*2.2

weight_kg <- 100
weight_lb # must rerun formula to get new, correct amount


dat <- read.csv(file = 'inflammation-01.csv', header = FALSE) # header = F because we don't have column names on the data- the first row IS data
class(dat)
dim(dat) # gives # rows, # cols

# And R always appreaches data from rows first, then cols. Kinda like finding your seat in a theater
dat[1,1] # show first row, first column of 'dat'
dat[30, 20] # 30th row, 20th col

# to grab a range of data; 
dat[1:4, 1:10] # rows 1-4 and cols 1-10
dat[5:10, 1:5]
dat[c(3,8,37,56), c(10,14,29)]
dat[5,] # all of row 5 (all columns)
dat[,16] # all of col. 16 (all rows)

raptor_1 <- dat[1,]
max(raptor_1)
min(raptor_1)

max(dat[2,]) # does the same thing as max(raptor_1), but clunky

mean(dat[,7]) # to find mean inflammation of all raptors on day (col) 7
median(dat[,7])
sd(dat[,7])

?apply
avg_raptor_inf <- apply(dat, 1, mean) # in data "dat", to every row (thus the 1), apply the funciton "mean". rows, so per raptor
avg_day_inf <- apply(dat, 2, mean) # per col = per day
avg_day_inf

## arrows and equal signs are generally interchangeable, BUT not necessarily. So, use <- to assign names/values to variables. Definitely a quirk in R, so just use <- unless you're doing function things. More on that later.

animal <- c("m", "o", "n", "k", "e", "y")
animal[-1]
animal[-1:-4]
animal[c(5, 2, 3)]

max(dat[5, 3:7])

plot(avg_day_inf)
plot(avg_raptor_inf)

sd_day_inf <- apply(dat, 2, sd)
max_day_inf <- apply(dat, 2, max)
plot(sd_day_inf)
plot(max_day_inf)

# calculating min of each col = each day 
min_day_inf <- apply(dat, 2, min)
plot(min_day_inf)

################################################
# Creating functions;
###############################################

?str
str(read.csv) # tells you the structure of a cuntion (e.g. all the arguments it will take)

temp <-67
## Fahrenheit to Kelvin
fahr_to_kelvin<-function(temp){
  kelvin <- ((temp-32)*(5/9))+273.1
  return(kelvin)
}
fahr_to_kelvin(temp)
fahr_to_kelvin(59)
fahr_to_kelvin(82)

kelvin_to_celsius <- function(temp){
  celsius <- temp - 273.15
  return(celsius)
}

kelvin_to_celsius(82)

fahr_to_celsius <- function(temp){
  temp_k <- fahr_to_kelvin(temp)
  result <- kelvin_to_celsius(temp_k)
  return(result)
}

fahr_to_celsius(82)
fahr_to_celsius(76)
fahr_to_celsius(78)
fahr_to_celsius(32)


# Write a function called fence that takes two vectors as arguments, called original and wrapper, and returns a new vector that has the wrapper vector at the beginning and end of the original
original <- c(1, 2, 3)
wrapper <- c(5, 6, 7)

fence <- function(original, wrapper){
  newvector <- c(wrapper, original, wrapper)
  return(newvector)
}
fence(original, wrapper)

best_practice <- c("Write", "programs", "for", "people", "not", "computers")
asterisk <- "***"  # R interprets a variable with a single value as a vector with one element.
fence(best_practice, asterisk)

outside <- function(vector){
  outvector <- c(vector[1], vector[length(vector)]
  return(outvector)
}

dry_principle <- c("Don't", "repeat", "yourself", "or", "others")
outside(dry_principle)

## not quite working

################
# For loops
###################

analyze <- function(filename) {
  # Plots the average, min, and max inflammation over time.
  # Input is character string of a csv file.
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
}

analyze("/data/inflammation-01.csv")

# the crappy way, without a for loop
best_practice <- c("let", "the", "computer", "do", "the", "work")
best_practice
print_words <- function(sentence){
  # function prints a sentence
  print(sentence[1])
  print(sentence[2])
  print(sentence[3])
  print(sentence[4])
  print(sentence[5])
  print(sentence[6])
}

print_words(best_practice)
print_words(best_practice[-6])

# the good way, with a for loop
print_words <- function(sentence){
  for(word in sentence){
    print(word)
  }
}

print_words(best_practice)
print_words(best_practice[-6])

# e.g.
# for(variable in collection){
#     do stuff with variable
#}

len <- 0
vowels <- c("a", "e", "i", "o", "u")
for(v in vowels){
  print(len)
  len <- len +1
}
len
v

letter <- "z"
for(letter in c("a", "b", "c")){
  print(letter)
}
letter

####################################################
# Day 2 - more complex datasets, etc.
####################################################
install.packages("dplyr")
library(dplyr)

dat <- mammal_stats <- read.csv("C:/Users/Cory/Desktop/software-carpentry-2016/software-carpentry-2016/data-files/mammal_stats.csv")

head(dat)
glimpse(dat) # in dplyr. shows each col, its data type, and the first few rows of contained data
attach(dat)

# choosing parts of a datasheet;
# columns; select()
select(dat, order, species) # to output only certain columns of the datset
select(dat, starts_with('adult') # to output cols whose names begin with "adult"
# rows; filter()
filter(dat, order=="Carnviora" & adult_body_mass_g<5000) # output all rows that are carnivores, under 5000g.
# to sort by something; arrange()
arrange(dat, adult_body_mass_g)
arrange(dat, desc(adult_body_mass_g)) # to sort by adult body mass in DESCENDING order
arrange()

dat1 <- arrange(dat, desc(adult_body_mass_g))
head(dat1)

# find the average mass for each order;
# 1. separate data by order
# 2. find average of each
a <- group_by(dat, order)
b <- summarize(a, mean_mass = mean(adult_body_mass_g, na.rm=T))
c <- mutate(a, mean_mass =mean(adult_body_mass_g, na.rm=T)) 
d <- summarize(a, mean_mass =mean(adult_body_mass_g, na.rm=T), sd_mass = sd(adult_body_mass_g, na.rm=T)) # this does the summarize function, but also includes all other data in the rows?!

## summarize = collapses the data to the category chosen and find the mean (or other function)
## mutate = also runs the function by the category, BUT applies the resultant values to every row/individual in the category. E.g. in this example, we found the average adult mass for a given order, and every species within an order gets that average added to a new column. So every different individual antelope will have the same, mean average adult body mass for Artiodactyls entered in a new col.

e <- mutate(a, mean_mass = mean(adult_body_mass_g, na.rm = T),
           norm_mass = adult_body_mass_g / mean_mass) # caluclating how much bigger a given row(animal) is than the average size for its order

a <- group_by(dat, order)
e <- mutate(group_by(dat, order), mean_mass = mean(adult_body_mass_g, na.rm = T),
            norm_mass = adult_body_mass_g / mean_mass) # will also work, but gets VERY. CONFUSING. Very fast.

# Instead of doing that, or making a variable 'a' in-between, you can pipe! Take output of one function, funnel it into a second...
# in R, pipe is "%>%" instead of "|"
e <- dat %>%
  group_by(order) %>%
  mutate(mean_mass = mean(adult_body_mass_g, na.rm=T))
# Group data by order, then calculate each order's avg adult mass and add that value to each row/individual in a new column names mean_mass

# lets group by order, find mean mass, find normalized mass, and then group by normalized mass;
f <- dat %>%
  group_by(order) %>%
  mutate(mean_mass = mean(adult_body_mass_g, na.rm = T), norm_mass = adult_body_mass_g / mean_mass) %>%
  arrange(desc(norm_mass))

## Exercises!
# find order with the most and least species
order_most_species  <- dat %>%
  group_by(order) %>%
  summarize(num_spp = length(species)) %>%
  arrange(desc(num_spp))
# find greatest and smallest size range
order_massrange  <- dat %>%
  group_by(order) %>%
  summarize(max_mass = max(adult_body_mass_g, na.rm=T), min_mass = min(adult_body_mass_g, na.rm=T), mass_range = max_mass - min_mass) %>%
  arrange(desc(mass_range))

## Which species of carnivore has the largest body length to body mass ratio? (Hint: that's adult_head_body_len_mm / adult_body_mass_g')
# for JUST that information
order_lmratio  <- dat %>%
  filter(order=="Carnivora") %>%
  group_by(species) %>%
  summarize(l_to_m = adult_head_body_len_mm / adult_body_mass_g) %>%
  arrange(desc(l_to_m))%>%
  filter(l_to_m!="NA")

order_lmratio_2 <- dat %>%
  filter(order=="Carnivora") %>%
#   group_by(order) %>%
  mutate(l_to_m = adult_head_body_len_mm / adult_body_mass_g) %>%
  arrange(desc(l_to_m))%>%
  filter(l_to_m!="NA")

