###################################
# Software Carpentry Workshop April 14, 2016
#
#####################################

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
