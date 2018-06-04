# Looking for the presence of a vector in a list of a matrix
wantVec <- c(3,1,2)
myList <- list(A = c(1:3), B = c(3,1,2), C = c(2,3,1))
sapply(myList, function(x, want) isTRUE(all.equal(x, want)), wantVec)
## or, is the vector in the set?
any(sapply(myList, function(x, want) isTRUE(all.equal(x, want)), wantVec))

# We can do a similar thing with a matrix:

myMat <- matrix(unlist(myList), ncol = 3, byrow = TRUE)
## As the vectors are now in the rows, we use apply over the rows
apply(myMat, 1, function(x, want) isTRUE(all.equal(x, want)), wantVec)
## or
any(apply(myMat, 1, function(x, want) isTRUE(all.equal(x, want)), wantVec))


# If you need to do this a lot, write your own function
vecMatch <- function(x, want) {
    isTRUE(all.equal(x, want))
}
sapply(myMat, vecMatch, wantVec)

vecMatch <- function(x, want) {
    out <- sapply(x, function(x, want) isTRUE(all.equal(x, want)), want)
    any(out)
}

vecMatch(myList, wantVec)


##### String splitting
tes<-c("1.abc","2.di","3.lik")
dat<-c(5,3,2)
h<-data.frame(tes,dat)
h$num<-substr(h$tes,1,1)

library(stringr)
str_split_fixed(h$tes, fixed("."), 2)[, 2]

sapply(strsplit(as.character(h$tes), "\\."), function(x) x[[2]])


##### Use na.omit to remove any na values from a column
DF <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA), z=c(NA, 33, 22))
na.omit(DF) # removes all rows with NA

library(tidyr)
DF %>% drop_na(y)

subset(DF, !is.na(y))

DF[!is.na(DF$y),]
