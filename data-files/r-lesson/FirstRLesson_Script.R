###################################
# Software Carpentry Workshop April 14, 2016
#
#####################################
detach() # will get rid of/clear all your working history
rm(list=ls()) # also gets rid of things


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
