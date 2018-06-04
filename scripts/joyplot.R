# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(tidyverse) # loads in all the tidyverse libraries
library(lubridate) # to make dealing with dates easier
library(ggjoy) #the brand new ggjoy package!
# read in data & convert it to a tibble (a special type of dataframe with a lot of nice qualities,
# you can see more info here: https://blog.rstudio.com/2016/03/24/tibble-1-0-0/)
bugs <- as_data_frame(read.csv("../input/Thomsen_Jørgensen_et_al._JAE_All_data_1992-2009.csv"))
 
# take a look at the first couple rows to make sure it all loaded in alright
head(bugs)

# add a coulmn with the month of each observation. mdy() tells the lubridate package what
# format our dates are in & month() says we only want the month from the date
bugs$month <- month(mdy(bugs$date1))
 
# list of months for labelling graph
monthList <- c("Jan","Feb","Mar","April","May", "June","July","Aug","Sep","Oct","Nov","Dec")
 
# remap months from numbers (3:12) to words (March-December)
bugs$month <- plyr::mapvalues(bugs$month, levels(as.factor(bugs$month)), monthList[3:12])
 
# plot the nubmer of bugs caught by month
ggplot(data = bugs, aes(x = month, y = individuals)) + geom_point() +
scale_x_discrete(limit=monthList) 
#head(bugs)

# we're going to have to do some data manipulation to get there.
# let's get the total number of insects observed on each day (binning over years)
bugs$dayInYear <- yday(mdy(bugs$date1))
 
# joyplot of when insects were observed by order. Scale changes how tall the peaks are
ggplot(data = bugs, aes(x = dayInYear, y = order)) + geom_joy(scale = 0.9) + theme_joy()

# joyplot of dates on which insects were observed by year of observation
ggplot(data = bugs, aes(x = dayInYear, y = as.factor(year))) + geom_joy(scale = 0.9) + theme_joy()

# look at the variance
varianceByYear <- bugs %>% group_by(year) %>% summarise(variance = sd(dayInYear))
 
# plot variance by year
ggplot(varianceByYear, aes(year, variance)) + geom_line() +
geom_smooth(method='lm') # this function adds the fitted line (w/ confidence interval)

