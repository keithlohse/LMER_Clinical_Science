## Chapter 6 R code from Long (2012)
### Adapted by Keith Lohse, PhD (2016)

# Installing Packages -------------------------------------------------
wants <- c("AICcmodavg", "lme4", "lmerTest", "nlme", "ggplot2","plyr", "dplyr")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

library("ggplot2");library("lme4"); library("plyr"); library("dplyr");
library("nlme");library('lmerTest');library("AICcmodavg")


##-------------------------------------------------------------
# Reading data into R

getwd()
# should be "C:/Users/krl0022/Documents" or similar

# we can then change the working directory to a specific folder for our project
setwd("C:/Currant/LMER_reading_group/")
# let's see what is in the data folder
list.files("C:/Currant/LMER_reading_group/data")

# Adjust the file path for your system:
load(file="./data/MPLS.LS.Rdata")
head(MPLS.LS)
tail(MPLS.LS)


##-------------------------------------------------------------
# Introduction to Maximum Likelihood Estimation

# To understand some of the fundamental principles of maximum
# likelihood (ML) estimation, we will start by taking an independent
# subset of our data. 

## Select subset.
mysubset <- subset(MPLS.LS, subid == 1 & grade == 5 | subid == 3 & grade == 6 |
                       subid == 5 & grade == 7 | subid == 7 & grade == 8,
                   select = c(subid, read, grade))
mysubset

# With one observation per person per time point, these data are independent.
# We can plot these data and fit them with a regression line based on "ordinary 
# least squares" (OLS) regression. OLS is almost certainly the type of regression
# that you have encountered before in which we try to find the slope and intercept
# that lead to the smallest sum of squared errors (SSE), hence the "least" in OLS.
theme_set(theme_bw())
myx <- scale_x_continuous(breaks = 5:8)
g1 <- ggplot(mysubset, aes(grade, read)) + geom_point()
g2 <- g1 + stat_smooth(method = "lm", se = FALSE) + myx
print(g2)

# In ML, the principle is very similar, but rather than trying to minimize SSE,
# we are going to try and minimize something called the "deviance". Deviance,
# as we will explain below, is based on the likelihood function. It can all get a 
# bit complicated, but remember that our underlying idea is still that
# DATA = MODEL + ERROR

# The deviance and the likelihood are based on the amount of error in our models, 
# and models that produce smaller errors are more likely explanations of our data. 

# Our assumption about these errors is that they follow a normal curve. Thus, to 
# evaluate the probability of an error (i.e., f(e)), we can use the dnorm() function. 
# dnorm() takes three different arguments:
# 1. the value of the random variable
# 2. the mean of the distribution
# 3. the standard deviation of the distribution
dnorm(-1, mean = 0, sd = sqrt(1))

# We can use dnorm to evaluate the probabilities for the values
# -4 to 4 in a standard normal distribution:
norm.ex <- data.frame(eps = seq(from = -4, to = 4, by = 0.1) )
norm.ex$f.of.eps <- dnorm(norm.ex$eps, mean = 0, sd = 1)
head(norm.ex)

# You can see that errors very near the mean are more likely,
# whereas errors that are far away from the mean are less likely.
g1 <- ggplot(data = norm.ex, aes(x = eps, y = f.of.eps)) + geom_line()
g2 <- g1 + geom_abline(intercept = 0, slope = 0, linetype = 2)
g3 <- g2 + ylab(expression(italic(f)(epsilon)))
g4 <- g3 + xlab(expression(paste("Epsilon ", (epsilon))))
print(g4)

# To simply the math a bit, we will look at the deviance rather than the 
# raw likelihood (see Long, 2012, pp. 197-198), but the deviance is just
# a transformation of the likelihood. (Specifically, it is -2 multiplied
# by the log of the likelihood.)

# Refer to the chapter for the exact formula, but below we are going to
# write our own deviance function for a situation in which we already know
# the population intercept (102) and error variance (49).

# Given the model with these known parameter values and the observed data
# our goal here is to find the value of the slope (B1) that produces the 
# smallest deviance. The value that produces the smallest deviance is 
# the value that is most likely to have led to the data we observed (hence,
# maximum likelihood estimation).

# Below, we will write a deviance function that accepts B1 as an argument.
# Note that 102 and 49 are already specified as parameters.
dev.func <- function(B1) {4 * log(2 * pi * 49) + (1 / 49) *
                              sum((mysubset$read - 102 - B1 * mysubset$grade) ^ 2)}

# Next, we will generate a range of slope values to test and store the 
# resulting deviances.
# The first column is our range of values from 13-17. The second column is
# the calculated deviance for that slope given the known intercept and variance.
dev.store <- mdply(data.frame(B1 = seq(from = 13, to = 17, by = 0.1)),
                   dev.func)
dev.store
colnames(dev.store)[2] <- "deviance"
head(dev.store)
# How many deviances do we have?
length(dev.store$deviance)

# But which deviance is the smallest?
subset(dev.store, deviance == min(deviance))
# Thus, a slope of 15.1 is the most likely explanation for our data, given the 
# fixed parameters of 102 for the intercept and 49 for the variance.
# We can see this visually by plotting the deviance for a series of slope estimates:
g1 <- ggplot(data = dev.store, aes(x = B1, y = deviance)) + geom_line()
g2 <- g1 + geom_abline(intercept = min(dev.store$deviance), slope = 0, linetype = 2)
g3 <- g2 + ylab(expression(italic(deviance))) + xlab(expression(beta[1]))
print(g3)

# This is an excellent conceptual demonstration, but it is really unlikely that we are 
# going to know the population intercept but not the slope. So let's consider a more realistic 
# model in which we are going to have to estimate both the intercept and the slope. 
# We will, however, keep the error variance fixed at 49.
## We need to create a new deviance function:
dev.func <- function(B0, B1) {4 * log(2 * pi * 49) + (1 / 49) *
                                  sum((mysubset$read - B0 - B1 * mysubset$grade) ^ 2)}
# Note that this deviance function now takes two arguments: B0 and B1.

# Now, we can generate some "plausible" slope and intercept values to test.
# We will generate a column of intercepts, a column of slopes, and a column 
# of deviances to store the results.
dev.store <- mdply(expand.grid(B0 = seq(98, 106, by = 0.1), B1 = seq(12, 18, by = 0.1)),
                   dev.func)
# Renaming the third column as "deviance"
colnames(dev.store)[3] <- "deviance"
head(dev.store)

# How many deviances do we have?
length(dev.store$deviance)
# But which one is the best fit?
subset(dev.store, deviance == min(deviance))

# We can use the wireframe() function from the lattice package to visualize 
# this three dimensional relationship:
library("lattice")
wireframe(deviance ~ B0 * B1, dev.store)

# Our use of the dev.function is great for learning, but the examples are 
# artificial. For instance, we conveniently set up ranges that would contain
# the ML estimates of our parameters. In reality, the limits of these parameters
# are not going to be known and a more exhaustive search space must be used. 
# Serious estimation involves more precise estimation and greater estimation
# than what we did above. 

# Rather than create a very large and exhaustive parameter space to search 
# (which is really data intensive!), our lmer() function uses "short-cuts"
# based on calculus (such as the Newton-Raphson method). I don't really understand
# how these methods work myself, but very smart people (i.e., Newton, Raphson) have 
# showed how you can use derivatives to more quickly arrive at the minimum deviance. 

# We can gain some insight into this process by setting the verbose argument to TRUE
# in our lmer() function:
lmer.0 <- lmer(read ~ grade + (1 | subid), data = MPLS.LS, REML = FALSE, verbose = TRUE)
# Setting this value to true shows the deviance in the start parameters compared to 
# the deviation in the final estimation. 
# (Note, in R 3.2.3, and lme4 1.1, this will be different than Long's output which
# is based on earlier versions of the software.)

##-------------------------------------------------------------
# Comparing Models

# Thus far, we have described how lmer() uses ML estimation to determine the most
# likely values of parameters, given a specific model. But how do we know that the 
# model we have selected is good? 

# To answer this question, we need to adopt a model comparison approach. (I.e., is a 
# random slopes model an improvement on a random-intercepts model). The good news is 
# that everything we have learned about ML estimation and the calulation of the deviance
# still applies to making comparisons between models. 

# The biggest caveat that we need to remember is that our calulation of likelihood is 
# based on summation of errors. This means that we can only compare models that come from
# the same size of data set (i.e., the same number of observations), because larger 
# datasets will be more "errorful" by default. The other way to think of this is if:
# DATA = MODEL + ERROR, then we need to keep DATA as a fixed quanty if we want to see
# how different models affect our error term. 

# First, for any given model, we can extract the loglikelihood and calculate the deviance
# using the log-likelihood function.
## Estimate the model.
lm.1 <- lm(read ~ grade, data = mysubset)
## Extract the log-likelihood.
ll.1 <- logLik(lm.1)
ll.1
## Compute the deviance. Recall that the deviance is just a transformation of the 
# likelihood, specifically, -2*ln(likelihood)
dev.1 <- -2 * as.numeric(ll.1)
dev.1

# 24.71 (with rounding) is the deviance for the linear model of reading
# scores as a function of grade for our "mysubset" dataframe. 
# Let's compare this model to a simpler model in which we only estimate
# and intercept (i.e., a model were don't think reading is associated with
# with the student's grade level).
lm.0 <- lm(read ~ 1, mysubset)
# We can combine the deviances from our two different models in to a single
# data frame called "comp".
comp <- data.frame(deviance = c(-2 * logLik(lm.1), -2 * logLik(lm.0)))
rownames(comp) <- c("Linear", "Intercept-only")
comp

# From the comparison of the deviances, it seems clear that the linear model
# is a much better approximation of the data. The deviance is essentially the 
# building block from which we will base our model comparisons.

# One thing that we need to be concerned about, however, is that simply
# reducing the deviance is not enough of a criterion by which we can say a 
# model is better. For example, let's generate 4 totally random numbers in a 
# new variables called "rando" and see how this variable affects our model. 
set.seed(4)
mysubset$rando <- c(rnorm(4,0,1))
mysubset
# We can add our random variable to our linear model.
lm.2 <- lm(read ~ grade + rando, mysubset)
# Next, we will combine the deviances from all three of our models into a 
# new dataframe called comp2.
comp2 <- data.frame(deviance = c(-2 * logLik(lm.2), -2 * logLik(lm.1), -2 * logLik(lm.0)))
rownames(comp2) <- c("Rando", "Grade", "Intercept-only")
comp2
# As you can see, simply adding a new variable to the model is going to improve
# the fit, even if that data is completely random! (You can play around with the 
# seed in the code above to see just how much a random variable can affect the 
# deviance in this small sample.) As such, we need to establish some criterion for
# for how much the deviance needs to be reduced before we are willing to say that 
# reduction in deviance is actually informative. Thus, we refer to these "yardsticks"
# as the INFORMATION CRITERION. 

##-------------------------------------------------------------
# Information Criteria: AIC and BIC

# As discussed in the chapter, there are two information criteria that are widely
# used: the Akaike Information Criterion (AIC) and the Bayesian Information Criterion
# (BIC). These ICs are both follow the form:
# IC = deviance + penalty
# Thus, in both the AIC and the BIC, smaller values mean that our models are better,
# because we have smaller errors. The penalities for the AIC and the BIC are also 
# different (see the chapter for a detailed discussion), but essentially the AIC
# only penalized models based on the number of parameters whereas the BIC also
# introduces a penalty for the number of data points. We are going to prefer the AIC
# for our models (see Chapter 7 for a full explanation as to why).

# The AIC is provided in the summary() of an lmer object, but we can also pull out
# these model fit statistics on their own using $AICtab.
lmer.1 <- lmer(read ~ grade + (grade | subid), MPLS.LS, REML = FALSE)
summary(lmer.1)
## Print ML information.
summary(lmer.1)$AICtab

# We can then compare this random slopes to a model that includes a non-linear 
# effect of grade:
lmer.2 <- lmer(read ~ grade + I(grade ^ 2) + (grade | subid), MPLS.LS, REML = FALSE)
# In practice, this is a very good thing to do, because it will allow to decide
# if a linear effect of our time variable is sufficient, or if we are better off
# using a non-linear effect of time. We can use the anova() function to compare
# our different linear models:
anova(lmer.1, lmer.2)
# Note the change in the AIC from model 1 to model 2
# The ANOVA function also outputs a Chi-Squared statistic with degress of 
# freedom and the associated p-value. This the Likelihood Ratio Test of the
# change in deviance. This test gives us another method for comparing between
# models. Like the AIC, it introduces a penalty for the number of parameters in 
# our model. 

##-------------------------------------------------------------
# ML and Standard Errors

# To this point, our discussion has focused on the calculation of point estimates
# (i.e., specific value of the intercept and slope) that produce the smallest deviance.
# However, it is also important for us to appreciate how much these parameters might
# vary from sample to sample. 
# The point estimate is the best estimate of the parameter in question given the 
# model and the data. 
# The standard error is a method of quantifying the uncertainty associated with the 
# point estimate (i.e., a measure of precision). 
# The standard error for these fixed-effects is directly tied to classic notions of 
# statistical "significance". Typically, the effect is expressed a ratio of the 
# coefficient to the standard error (conceptually, the ratio of systematic variation
# to random variation). This is referred to as the t-ratio and higher values of this
# t-ratio are desirable as that indicates the ML estimate should change little 
# under repeated sampling. 
lmer.2 <- lmer(read ~ grade + risk2 + eth2 + (grade | subid), MPLS.LS, REML = FALSE)
summary(lmer.2)$coefficients

# These standard errors can calso be used to calculate 95% confidence intervals for
# your fixed effects
mytable <- as.data.frame(summary(lmer.2)$coefficients)
mytable$LCI <- mytable$Estimate - 2 * mytable$"Std. Error"
mytable$UCI <- mytable$Estimate + 2 * mytable$"Std. Error"
mytable

# And from these t-values, you can calculate the p-value for a two-tailed test
# using the pnorm() function.
mytable$p.value <- pnorm(q = abs(mytable$"t value"), lower.tail = FALSE) * 2
round(mytable, 4)
