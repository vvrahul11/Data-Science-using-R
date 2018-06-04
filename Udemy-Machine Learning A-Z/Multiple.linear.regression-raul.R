# Simple Linear Regression

# Importing the dataset
load('multiple.linear.regression.RData')

# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

#Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Profit ~ R.D.Spend,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# Visualising the Training set results
# First visualize the training set points and the predicted fitted line (using training set data)
# Second using the same fitted line data linear regresson is going to make prediction for new data
# therefore we wanted to see how far is the points of the test data from the fitted line (that was made
# using the training set data)
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$R.D.Spend, y = training_set$Profit),
             colour = 'red') +
  geom_line(aes(x = training_set$R.D.Spend, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Profit vs R.D.Spend (Training set)') +
  xlab('R.D.Spend') +
  ylab('Profit')

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$R.D.Spend, y = test_set$Profit),
             colour = 'red') +
  geom_line(aes(x = training_set$R.D.Spend, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Profit vs R.D.Spend (Test set)') +
  xlab('R.D.Spend') +
  ylab('Profit')