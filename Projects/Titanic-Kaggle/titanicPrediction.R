# Thanks to all blogs and repositories from where I learned to solve titanic ML challenge:
# https://datascienceplus.com/perform-logistic-regression-in-r/
# http://trevorstephens.com/kaggle-titanic-tutorial/getting-started-with-r/
# Missing value
# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
# https://rpubs.com/dvdbisong/titanic
# http://trevorstephens.com/kaggle-titanic-tutorial/r-part-4-feature-engineering/

exploratory.Analysis <- function(training.data.raw){
  # Percentage missing values
  # Output percentage of missing values for each column and row
  # Output the number of missing values for each column
  sapply(training.data.raw,function(x) sum(is.na(x))/length(x)*100)
  sapply(training.data.raw,function(x) sum(is.na(x)))
  
  # Look at pattern of missing values
  library(mice)
  md.pattern(training.data.raw)
  
  # Missing data visualization
  library(VIM)
  aggr_plot <- aggr(training.data.raw, 
                    col=c('navyblue','red'), 
                    numbers=TRUE, 
                    sortVars=TRUE, 
                    labels=names(data), 
                    cex.axis=.7, 
                    gap=3, 
                    ylab=c("Histogram of missing data","Pattern"))
  
  
  # Quick check for how many different values for each feature
  sapply(training.data.raw, function(x) length(unique(x)))
  
  # A visual way to check for missing data
  library(Amelia)
  missmap(training.data.raw, main = "Missing values vs observed")
  
  # Box plot
  marginplot(training.data.raw[c(6, 11)])
  
}




# Load the raw training data and replace missing values with NA
training.data.raw <- read.csv('train.csv',header=T,na.strings=c(""))
exploratory.Analysis(training.data.raw)
table(training.data.raw$Survived)
prop.table(table(training.data.raw$Survived)) * 100

# proportion of male and female survivors
# proportion of female or male survived or died
prop.table(table(training.data.raw$Sex, training.data.raw$Survived), 1) * 100
# proportion of female + male survived or died
prop.table(table(training.data.raw$Sex, training.data.raw$Survived), 2) * 100


# 0        1 
# 61.61616 38.38384 
# In the training set 61% of patients died. So If you assign 0 to all
# passengerIDs then you can expect kaggle to give you 61% accuracy as the 
# training percentage died ~ test percentage died


# Subsetting the data
drops <- c("PassengerId", "Name", "Ticket", "Cabin");
data <- training.data.raw[!(names(training.data.raw) %in% drops)];
exploratory.Analysis(data)

factor.var <- c("Survived","Pclass","Sex","Embarked");
data[factor.var] <- lapply(data[factor.var], as.factor);
summary(data)


# Substitute the missing values with the average value of age
#data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

# assign missing values to the mode of the data
# https://predictoanalycto.wordpress.com/2015/07/10/titanic-machine-learning-from-disaster-part-1/
mode.embarked <- names(which.max(table(data$Embarked)))
levels.embarked <- levels(data$Embarked)
levels.embarked[1] <- mode.embarked
levels(data$Embarked) <- levels.embarked

# assign missing age as the average value of age grouped by passenger class and passenger sex.
avg.age <- aggregate(data$Age, 
                     by = list(data$Pclass, 
                               data$Sex), 
                     FUN = mean, na.rm = TRUE);
names(avg.age) <- c("Pclass", "Sex", "Age");
na.index <- which(is.na(data$Age));
for (index in na.index){
  Pclass = data[index,]$Pclass;
  Sex = data[index,]$Sex;
  data[index,]$Age = avg.age[avg.age$Pclass == Pclass & avg.age$Sex == Sex,]$Age;
}

# For missing Fare attribute, I checked if passengers corresponding to 0 fare were kids, 
# but as it turned out they were not. Making an assumption here that they actually 
# bought the tickets, I decided to assign the average fare of class the passenger 
# was traveling as his or her fare. This is how I did it :
avg.fare <- aggregate(data$Fare, by = list(data$Pclass), FUN = mean, na.rm = TRUE);
names(avg.fare) <- c("Pclass", "Fare");
zero.index <- which(data$Fare == 0);
for (index in zero.index){
  Pclass = data[index,]$Pclass;
  data[index,]$Fare = avg.fare[avg.fare$Pclass == Pclass,]$Fare;
}


# R should automatically code Embarked as a factor(). A factor is R's way of dealing with
# categorical variables
is.factor(data$Sex)         # Returns TRUE
is.factor(data$Embarked)    # Returns TRUE

# Check categorical variables encoding for better understanding of the fitted model
contrasts(data$Sex)
contrasts(data$Embarked)

# Remove rows (Embarked) with NAs since its only 2 rows
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

# Train test splitting
train <- data[1:800,]
test <- data[801:889,]


modelAnalysis <- function(model, test){
  # Analysis of deviance
  anova(model,test="Chisq")
  
  # McFadden R^2
  library(pscl)
  pR2(model)
  
  #-------------------------------------------------------------------------------
  # MEASURING THE PREDICTIVE ABILITY OF THE MODEL
  
  # If prob > 0.5 then 1, else 0. Threshold can be set for better results
  fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
  fitted.results <- ifelse(fitted.results > 0.5,1,0)
  
  misClasificError <- mean(fitted.results != test$Survived)
  print(paste('Accuracy',1-misClasificError))
  
  # Confusion matrix
  library(caret)
  confusionMatrix(data=fitted.results, reference=test$Survived)
  
  library(ROCR)
  # ROC and AUC
  p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
  pr <- prediction(p, test$Survived)
  # TPR = sensitivity, FPR=specificity
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  plot(prf)
  
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  auc
  return(auc)
}

#### main 
# Model fitting
# --------------------------------------------------logit
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
model <- glm(Survived ~Pclass+Age+Sex+SibSp+Parch+Fare+Embarked,family=binomial(link='logit'),data=train)

summary(model)
auc <- modelAnalysis(model, test)
auc


# -------------------------------------------Decision trees
library(rpart)
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fit <- rpart(Survived ~.,
             data=train,
             method="class"
             #, control=rpart.control(minsplit=2, cp=0)
             )
fancyRpartPlot(fit)

y_pred <- predict(fit, test[,2:8], type = "class")
# Making the Confusion Matrix
cm = table(test$Survived, y_pred)
cm
# y_pred
# 0  1
# 0 51  5
# 1  9 24
#submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
#write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

# Pruning the tree
pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
plot(pfit, uniform=TRUE,
     main="Classification Tree for Titanic Data")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

post(pfit, file = "prunetree.ps",
     title = "Pruned Classification Tree for Titanic")


# ----------------------------------------------------Naivebayes
library(e1071)
classifier = naiveBayes(x = train[, 2:8],
                        y = train$Survived)
plot(fit)
text(fit)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test[,2:8])

# Making the Confusion Matrix
cm = table(test$Survived, y_pred)
cm


#----------------------------------------------Random forest noooooooo
library(randomForest)
set.seed(123)
classifier<- randomForest(as.factor(train$Survived) ~.,
                    data=train[,2:8], 
                    importance=TRUE, 
                    ntree=2000)
varImpPlot(classifier)
# Predicting the Test set results
y_pred = predict(classifier, newdata = test[,2:8])

# Making the Confusion Matrix
cm = table(test$Survived, y_pred)
cm
y_pred
# 0  1
# 0 50  6
# 1  8 25
library(party)
classifier<- cforest(as.factor(train$Survived) ~.,
                          data=train[,2:8], 
                           controls=cforest_unbiased(ntree=2000, mtry=3))
y_pred = predict(classifier, newdata = test[,2:8])
cm = table(test$Survived, y_pred)
cm
y_pred
# 0  1
# 0 51  5
# 1 11 22

# ---------------------------------------------SVM
# SVM always expects scaling
library(e1071)
classifier = svm(formula = Survived ~.,
                 data = train,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test[,2:8])

# Making the Confusion Matrix
cm = table(test$Survived, y_pred)
cm
# y_pred
# 0  1
# 0 51  5
# 1  8 25
# --------------------------------------------K-NN
library(class)
set.seed(123)
train$Sex = factor(train$Sex,
                   levels = c('male', 'female'),
                   labels = c(1, 2))
test$Sex = factor(test$Sex,
                  levels = c('male', 'female'),
                  labels = c(1, 2))

train$Embarked = factor(train$Embarked,
                        levels = c('S', 'C', 'Q'),
                        labels = c(1, 2, 3))
test$Embarked= factor(test$Embarked,
                      levels = c('S', 'C', 'Q'),
                      labels = c(1, 2, 3))

model = knn(train = train[,2:8],
            test = test[,2:8],
            cl = train$Survived,
            k = 5,
            prob = TRUE)
# Making the Confusion Matrix
cm = table(test$Survived, model)
cm

# 
# model
# 0  1
# 0 46 10
# 1 10 23

#### Explanation
# Interpreting the results of our logistic regression model
# Now we can analyze the fitting and interpret what the model is telling us.
# First of all, we can see that SibSp, Fare and Embarked are not statistically significant. As for the statistically significant variables, sex has the lowest p-value suggesting a strong association of the sex of the passenger with the probability of having survived. The negative coefficient for this predictor suggests that all other variables being equal, the male passenger is less likely to have survived. Remember that in the logit model the response variable is log odds: ln(odds) = ln(p/(1-p)) = a*x1 + b*x2 + … + z*xn. Since male is a dummy variable, being male reduces the log odds by 2.75 while a unit increase in age reduces the log odds by 0.037.
# Now we can run the anova() function on the model to analyze the table of deviance
# The 0.84 accuracy on the test set is quite a good result. However, keep in mind that this result is somewhat dependent on the manual split of the data that I made earlier, therefore if you wish for a more precise score, you would be better off running some kind of cross validation such as k-fold cross validation.
# As a last step, we are going to plot the ROC curve and calculate the AUC (area under the curve) which are typical performance measurements for a binary classifier.
# The ROC is a curve generated by plotting the true positive rate (TPR) against the false positive rate (FPR) at various threshold settings while the AUC is the area under the ROC curve. As a rule of thumb, a model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5.
# The difference between the null deviance and the residual deviance shows how our model is doing against the null model (a model with only the intercept). The wider this gap, the better. Analyzing the table we can see the drop in deviance when adding each variable one at a time. Again, adding Pclass, Sex and Age significantly reduces the residual deviance. The other variables seem to improve the model less even though SibSp has a low p-value. A large p-value here indicates that the model without the variable explains more or less the same amount of variation. Ultimately what you would like to see is a significant drop in deviance and the AIC.
# While no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
# 
# Assessing the predictive ability of the model
# In the steps above, we briefly evaluated the fitting of the model, now we would like to see how the model is doing when predicting y on a new set of data. By setting the parameter type='response', R will output probabilities in the form of P(y=1|X). Our decision boundary will be 0.5. If P(y=1|X) > 0.5 then y = 1 otherwise y=0. Note that for some applications different decision boundaries could be a better option.
# 
# The 0.84 accuracy on the test set is quite a good result. However, keep in mind that this result is somewhat dependent on the manual split of the data that I made earlier, therefore if you wish for a more precise score, you would be better off running some kind of cross validation such as k-fold cross validation.
# As a last step, we are going to plot the ROC curve and calculate the AUC (area under the curve) which are typical performance measurements for a binary classifier.
# The ROC is a curve generated by plotting the true positive rate (TPR) against the false positive rate (FPR) at various threshold settings while the AUC is the area under the ROC curve. As a rule of thumb, a model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5.
# 


# ----------------------------------Using real test data
test1 <- read.csv('test.csv',header=T,na.strings=c(""))
#test1<- subset(test1, select=c(2,4,5,6,7,9,11))

test1$Survived = 0
test1$Survived[test1$Sex == 'female'] = 1

summary(test1$Age)













