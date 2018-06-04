# Polynomial Regression
#https://onlinecourses.science.psu.edu/stat501/node/325

#Predit the salary of the new employee who comes between 5 and 6
#make sure the salary he said is true'''
setwd("/media/user/Edison/datascience@BGU/MyProjects/Udemy-Course/Project1")
# Importing the dataset
dataset = read.csv("https://onlinecourses.science.psu.edu/stat501/sites/onlinecourses.science.psu.edu.stat501/files/data/bluegills.txt",
                   sep = "\t", header = T)

# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
library(caTools)
set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Linear Regression to the dataset
lin_reg = lm(formula = length ~ .,
             data = dataset)

# Fitting Polynomial Regression to the dataset
dataset$age2 = dataset$age^2
dataset$age3 = dataset$age^3
dataset$age4 = dataset$age^4
poly_reg = lm(formula = length ~ .,
              data = dataset)

# Visualising the Linear Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$age, y = dataset$length),
             colour = 'red') +
  geom_line(aes(x = dataset$age, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') +
  xlab('Level') +
  ylab('Salary')

# Visualising the Polynomial Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$age, y = dataset$length),
             colour = 'red') +
  geom_line(aes(x = dataset$age, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

# Visualising the Regression Model results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$age), max(dataset$age), 0.1)
ggplot() +
  geom_point(aes(x = dataset$age, y = dataset$length),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(poly_reg,
                                        newdata = data.frame(age = x_grid,
                                                             age2 = x_grid^2,
                                                             age3 = x_grid^3,
                                                             age4 = x_grid^4))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

# Predicting a new result with Linear Regression
predict(lin_reg, data.frame(age = 6.5))

# Predicting a new result with Polynomial Regression
predict(poly_reg, data.frame(Level = 6.5,
                             Level2 = 6.5^2,
                             Level3 = 6.5^3,
                             Level4 = 6.5^4))
