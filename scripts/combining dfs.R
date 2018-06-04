# Combining vectors or data frames of unequal length into one data frame
df1 <- data.frame(Intercept = .4, x1=.4, x2=.2, x3=.7)
df2 <- data.frame(Interceptlego = .5,        x2=.8       )
myList <- list(df1, df2)

dat <- data.frame()
for(i in seq(along=myList)) for(j in names(myList[[i]]))
                                 dat[i,j] <- myList[[i]][j]
dat

myList <- list(df2, df1)

  Intercept  x2  x1  x3
1       0.5 0.8  NA  NA
2       0.4 0.2 0.4 0.7

########################################
Count the number of occurance of somethign in two dfs
#  - Compare two data frames of different length for same values in two columns


rec1 <- data.frame(shipName = rep(c("Nina", "Doug", "Alli", "Steve"), each = 5), 
                lon = rep.int(c(1:5), 4), 
                lat = rep.int(c(11:15), 4)
                )    
rec2 <- data.frame(shipName = rep(c("Nina", "Doug", "Alli", "Steve"), each = 7), 
                lon = rep.int(c(2, 3, 4, 4, 5, 5, 6), 4),
                lat = rep.int(c(12, 13, 14, 14, 15, 15, 16), 4)
                )

print(rec1)
print(rec2)

#Merge the two data frames together, keeping only those combinations that match

m <- merge(rec1, rec2, by = c("shipName", "lon", "lat"), all = FALSE)

print(m)
If you want to count how many times each combination appears, try the following. (There are different ways to aggregate. Some are here. Below is my preferred method, which requires you to have data.table installed. It's a great tool, so you may want to install it if you haven't yet.)

library(data.table)

#Convert to a data table and optionally set the sort key for faster processing
m <- data.table(m)
setkey(m, shipName, lon, lat)

#Aggregate and create a new column called "Count" with the number of
    #observations in each group (.N)
m <- m[, j = list("Count" = .N), by = list(shipName, lon, lat)]

print(m)

#If you want to return to a standard data frame rather than a data table:
m <- data.frame(m)


#########################
# Merging dataframes
install.packages('data.table') ## Install package

library(data.table)

## Dummy data:

dat1<- data.table(Gene_Id= 1:10000000, value.x= rnorm(n= 10000000))
dat2<- data.table(Gene_Id= 1:10000000, value.y= rnorm(n= 10000000))

setkeyv(dat1, c('Gene_Id'))
setkeyv(dat2, c('Gene_Id'))

system.time(datm<- merge(dat1, dat2))
 

## Use merge in base pckg
datA<- as.data.frame(dat1)
datB<- as.data.frame(dat2)

system.time(datM<- base:::merge(datA, datB))

#   user  system elapsed
#  0.750   0.004   0.755   <<- data.table

# 73.689   1.203  74.901  <<- base

####################################################
I have a table where every row is a customer and every column is an animal they purchased. Lets call this dataframe table.

> table
#       P1     P2     P3
# 1    cat lizard parrot
# 2 lizard parrot    cat
# 3 parrot    cat lizard
I also have a table that I will reference called lookUp.

> lookUp
#      pet   class
# 1    cat  mammal
# 2 lizard reptile
# 3 parrot    bird

library(dplyr)
library(tidyr)
table %>%
   gather(key = "pet") %>%
   left_join(lookup, by = "pet") %>%
   spread(key = pet, value = class)

#####################################################
Convert a list to a dataframe, do string splitting
#splitdat = do.call("rbind.na", strsplit(dataframe, ",")) # will work only on list of equal length
# splitdat = data.frame(apply(splitdat, 2, as.numeric)) # to conver to numeric

#######################################################
Apply function on every column, change _human to nothin
word.list1 = data.frame(apply(word.list, 2,  function(x) if(x!= 'NA'){ gsub("_HUMAN", "", x)} ))
#####################################################

