# Normality test

# http://www.sthda.com/english/wiki/normality-test-in-r

#"Many of statistical tests including correlation, regression, t-test, and analysis of variance (ANOVA) assume some certain characteristics about the data. They require the data to follow a normal distribution or Gaussian distribution. These tests are called parametric tests, because their validity depends on the distribution of the data."
# install.packages("dplyr")

# # Install
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")

library("dplyr")
library("ggpubr")

#Case of large sample sizes

#If the sample size is large enough (n > 30), we can ignore the distribution of the data and use parametric tests.

#The central limit theorem tells us that no matter what distribution things have, the sampling distribution tends to be normal if the sample is large enough (n > 30).

# Use density plot to visualize the normality 
library("ggpubr")
ggdensity(my_data$len, 
          main = "Density plot of tooth length",
          xlab = "Tooth length")

# Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution. A 45-degree reference line is also plotted.
# 
# Normality test
# 
# Visual inspection, described in the previous section, is usually unreliable. It’s possible to use a significance test comparing the sample distribution to a normal one in order to ascertain whether data show or not a serious deviation from normality.
# 
# There are several methods for normality test such as Kolmogorov-Smirnov (K-S) normality test and Shapiro-Wilk’s test.
# 
# The null hypothesis of these tests is that “sample distribution is normal”. If the test is significant, the distribution is non-normal.
# 
# Shapiro-Wilk’s method is widely recommended for normality test and it provides better power than K-S. It is based on the correlation between the data and the corresponding normal scores.
# 
# Note that, normality test is sensitive to sample size. Small samples most often pass normality tests. Therefore, it’s important to combine visual inspection and significance test in order to take the right decision.
# 
# The R function shapiro.test() can be used to perform the Shapiro-Wilk test of normality for one variable (univariate):
#   
#   

shapiro.test(my_data$len)

