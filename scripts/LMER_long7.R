## Chapter 7 R code from Long (2012)
### Adapted by Keith Lohse, PhD (2016)

# Installing Packages -------------------------------------------------
wants <- c("AICcmodavg", "lme4", "lmerTest", "nlme", "ggplot2","plyr", "dplyr")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

library("ggplot2");library("lme4"); library("plyr"); library("dplyr");
library("nlme"); library("AICcmodavg")

# library('lmerTest')
# In order for the AICcmodavg functions to work, we need to exclude the 
# lmerTest package for today.


##-------------------------------------------------------------
# Reading data into R

getwd()
# should be "C:/Users/krl0022/Documents" or similar

# we can then change the working directory to a specific folder for our project
setwd("C:/Currant/LMER_reading_group/")
# let's see what is in the data folder
list.files("C:/Currant/LMER_reading_group/data")

load("./data/MPLS.LS.Rdata")


##-------------------------------------------------------------
## Multimodel Inference and Akaike's Information Criterion
##

## First we will take a subset of the data where grade = 5.
mysample <- subset(MPLS.LS, grade == 5)
mysample

## We can fit the "true" Model 0 (because there is no relationship between
# attendance and reading level at grade 5).
model.0 <- lm(read ~ 1, mysample)
summary(model.0)

plot(mysample$att, mysample$read)
abline(h=mean(mysample$read))

## From these data, we will simulate Sample A.
set.seed(1)                             
# Setting the seed produces consistent results.
sim.dv <- unlist(simulate(model.0)) 
# Simulate generates data based on the model, unlist "flattens" this information
# in to a vector.
# We then combine this simulated outcome datat with the attendence variable in 
# a new dataframe.
sample.a <- data.frame(read = sim.dv, att = mysample$att)
head(sample.a)

# In our new sample of data, we can fit different models:
## Linear
model.1a <- lm(sample.a$read ~ sample.a$att)
## ... and quadratic
model.2a <- lm(sample.a$read ~ sample.a$att + I(sample.a$att ^ 2))
## ... and from these linear models we will extract the deviances and 
## combine them into a dataframe.
dev.a <- data.frame(deviance = c(deviance(model.1a), deviance(model.2a)))
rownames(dev.a) <- c("Model.1a", "Model.2a")
dev.a
# We now have two models, Model 1 and Model 2, where model 1 is "less false"
# than model 2 because it is estimating one less extraneous parameter. 

model.1a$coefficients
head(model.1a$fitted.values)

## Next, we will repeat the same process in a new model:
set.seed(13)
sim.dv <- unlist(simulate(model.0))
sample.b <- data.frame(read = sim.dv, att = mysample$att)
## Fit candidate models.
model.1b <- lm(sample.b$read ~ sample.b$att)
model.2b <- lm(sample.b$read ~ sample.b$att + I(sample.b$att ^ 2))
dev.b <- data.frame(deviance = c(deviance(model.1b), deviance(model.2b)))
rownames(dev.b) <- c("Model.1b", "Model.2b")
dev.b
# As above, you can see that the more complicated model will produce the smaller
# deviance by default.

# Next consider the following the following calculation of predictive deviance 
# in each model. This deviance is "predictive" because the fitted values from
# model A are being used to predict the observed values from Sample B.
N <- nrow(sample.b)
prdev.1b <- N * (log(2 * pi * sum((sample.b$read - model.1a$fitted.values) ^ 2)) + 1)
prdev.2b <- N * (log(2 * pi * sum((sample.b$read - model.2a$fitted.values) ^ 2)) + 1)
prdev.b <- data.frame(preddev = c(prdev.1b, prdev.2b))
rownames(prdev.b) <- c("Model 1", "Model 2")
prdev.b
# Notice that now the situation is reversed, Model 1 (the simpler model), 
# has a smaller predictive deviance than Model 2!

# Next, let's assume that another sample is generated based on the true model
# I.e., no relationship between Reading in Grade 5 and Attendence.
set.seed(21)
sample.c <- data.frame(read = unlist(simulate(model.0)), att = mysample$att)
model.1c <- lm(sample.c$read ~ sample.c$att)
model.2c <- lm(sample.c$read ~ sample.c$att + I(sample.c$att ^ 2))
dev.c <- data.frame(deviance = c(deviance(model.1c), deviance(model.2c)))
rownames(dev.c) <- c("Model.1c", "Model.2c")
dev.c
# As above, the more complicated model is going to produce the smallest deviance
# in a given sample.
# When we look at predictive deviance, however:
prdev.1c <- N * (log(2 * pi * sum((sample.c$read - model.1b$fitted.values) ^ 2)) + 1)
prdev.2c <- N * (log(2 * pi * sum((sample.c$read - model.2b$fitted.values) ^ 2)) + 1)
prdev.c <- data.frame(preddev = c(prdev.1c, prdev.2c))
rownames(prdev.c) <- c("Model 1", "Model 2")
prdev.c
# ... we can again see that the simpler model (which is closer to the truth) 
# produces the smallest predictive deviances. 

# Now, although the formula for the AIC is relatively simple:
### AIC = Deviance + 2*k
### where k = the number of estimated parameters
# some rather sophisticated mathematics have shown that: Given a large sample
# size and the assumption of normality, the AIC is an unbiased estimator of 
# the average predictive deviance. 
# Conceptually, this means that, out of all of the tested models, the model with
# the lowest as AIC is most likely to produce the smallest deviance in a new
# sample of data.
# (But remember that other, better fitting models might not have been tested.)



##-------------------------------------------------------------
## Extension of AIC to LMER
## First, we will center the Grade variable on Grade 5.
MPLS.LS$grade5 <- MPLS.LS$grade - 5
## Next, we will estimate models our different models with different numbers of fixed effects.
model.1 <- lmer(read ~ grade5 + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.2 <- lmer(read ~ grade5 + eth2  + (grade5 | subid), MPLS.LS, REML = FALSE)
model.3 <- lmer(read ~ grade5 + risk2 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
## For each model, we can compute the AIC.
mynames <- paste("M", as.character(1:3), sep = "")
myaic <- aictab(cand.set = list(model.1, model.2, model.3),
                modnames = mynames, sort = FALSE, second.ord = FALSE)
as.data.frame(myaic)[ ,1:3]
# You can see that the "myaic" object contrinas our model names, the number of 
# parameters (k; which includes fixed and random effects), and the AIC for each
# model.
# We can also use these functions to compute the AIC corrected or AICc:
myaicc <- aictab(cand.set = list(model.1, model.2, model.3),
                 modnames = mynames, sort = FALSE)
# Note that the "second.ord" argument is no longer included to get AICc.
as.data.frame(myaicc)[ ,1:4]
# One of features of the AIC/AICc that we are most interested in is the change 
# in AIC or "delta". You can think of this as either the relative improvement 
# compared to the worst fitting model (Model_i - WorstModel) or the difference 
# compared to the best fitting model (Model_i - BestModel). 

# Although there are prescibed values of "effect-size" for the change in AIC,
# the simplest thing to do in practice is adopt a cut-off before model
# comparison begins (e.g., a change of 2 points in the AICc) and apply that to 
# deciding among your best models. Just because this is the simplest approach
# however, does not mean that it is best or only approach. Other work has been
# done to show the "weight of the evidence" based on the change in the AIC.
myaicc <- aictab(list(model.1, model.2, model.3), modnames = mynames)
print(myaicc, LL = FALSE)

confset(cand.set = list(model.1, model.2, model.3), modnames = mynames)
confset(cand.set = list(model.1, model.2, model.3), modnames = mynames, level = 0.75)

evidence(myaicc)
# The weight of hte evidence is a probability scaling of AIC, and as such is a 
# more continuous way of thinking about model comparisons than the application 
# of hard and fast cut-offs. 

## Weight of the Evidence ------------------------------------------------------
# First, we will duplicate our dataframe, excluding logLik.
myaicc2 <- as.data.frame(myaicc)[ ,-7]
myaicc2$Eratio <- max(myaicc2$AICcWt) / myaicc2$AICcWt
## Print with rounding to two decimal places.
data.frame(Model = myaicc2$Modnames, round(myaicc2[ ,-1], 2))
# Note that the Eratio is the weight of the evidence for the best fitting model
# divided by the weight of the evidence for a given model. Thus, the lowest and
# "best" Eratio is 1. 

# Below, we will do the same calculations with additional models (M1 to M6).
model.1 <- lmer(read ~ grade5 + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.2 <- lmer(read ~ grade5 + eth2  + (grade5 | subid), MPLS.LS, REML = FALSE)
model.3 <- lmer(read ~ grade5 + risk2 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.4 <- lmer(read ~ grade5 * risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.5 <- lmer(read ~ grade5 * eth2  + (grade5 | subid), MPLS.LS, REML = FALSE)
model.6 <- lmer(read ~ grade5 * risk2 + grade5 * eth2 + (grade5 | subid),
                MPLS.LS, REML = FALSE)

# For each of these six models, we will now extract the AICc and compute the 
# Eratio to get the "weight of the evidence for each model.
mynames <- paste("M", as.character(1:6), sep = "")
mymodels <- list(model.1, model.2, model.3, model.4, model.5, model.6)
myaicc <- as.data.frame(aictab(cand.set = mymodels, modnames = mynames,
                               sort = FALSE)[,-c(5,7)])
myaicc$Eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
data.frame(Modnames = myaicc$Modnames, round(myaicc[,-1], 2))

# Define components for later use.
myx <- scale_x_continuous(breaks = 5:8)
theme_set(theme_bw())
# Create bar graphs.
## Note there was a problem with Long's code here (either because R or ggplot 2
## got updated). The problem is that the vector that we are trying to "summarize" 
## with geom_bar() is already summarized. 
## To fix this, I have stat=identity to geom_bar().
## Bar graph of weight of evidence.
g1 <- ggplot(myaicc, aes(x = Modnames, y = AICcWt)) + ylab("Weight")
g2 <- g1 + geom_bar(fill = "grey80", colour = "black", stat="identity") + xlab("Model")
g3 <- g2 + scale_y_continuous(limits = c(0,1))
print(g3)
## Bar graph of evidence ratio.
g1 <- ggplot(myaicc, aes(x = Modnames, y = Eratio)) + ylab("Ratio")
g2 <- g1 + geom_bar(fill = "grey80", colour = "black", stat="identity") + xlab("Model")
g3 <- g2 + geom_hline(aes(yintercept = 1), linetype = 2)
print(g3)

## Sort data frame.
myaicc2 <- myaicc[order(myaicc$AICc), ]
myaicc2
## Bar graph of weight of evidence.
## There was also an error in Long's code here (probably due to updates) and
# the only way I could work around it was creating a new variable called Names2
myaicc2$Names2<-factor(myaicc2$Modnames, levels=c("M1","M4","M3","M6","M2","M5"))
r1 <- ggplot(myaicc2, aes(x = Names2, y = AICcWt)) + ylab("Weight")
r2 <- r1 + geom_bar(fill = "grey80", colour = "black", stat="identity") + 
    xlab("Model")
r3 <- r2 + scale_y_continuous(limits = c(0,1))+scale_x_discrete()
print(r3)
## Bar graph of evidence ratio.
s1 <- ggplot(myaicc2, aes(x = Names2, y = Eratio)) + ylab("Weight")
s2 <- s1 + geom_bar(fill = "grey80", colour = "black", stat="identity") + xlab("Model")
s3 <- s2 + geom_hline(aes(yintercept = 1), linetype = 2)
print(s3)

## Confidence Intervals and Key Data for Write Ups -----------------------------
myaicc <- as.data.frame(aictab(cand.set = mymodels, modnames = mynames)[ ,-c(5,7)])
myaicc$Eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
data.frame(Modnames = myaicc$Modnames, round(myaicc[ ,-1], 2))

confset(cand.set = mymodels, modnames = mynames, level = 0.99)

mytab <- as.data.frame(summary(model.1)$coefficients)
mytab

mytab$LCI <- mytab$Estimate - 2 * mytab$"Std. Error"
mytab$UCI <- mytab$Estimate + 2 * mytab$"Std. Error"
round(mytab[ ,-3], 2)

# Plotting Predictions of the Fixed Effect Model -------------------------------
## Create data frame for graphing.
plotdata <- model.1@frame
plotdata$pred <- model.matrix(model.1) %*% fixef(model.1)
plotdata$grade <- plotdata$grade + 5
## ggplot2.
g1 <- ggplot(plotdata, aes(x = grade, y = read, linetype = risk2))
g2 <- g1 + stat_summary(fun.y = "mean", geom = "point", cex = 2)
g3 <- g2 + stat_summary(aes(y = pred), fun.y = "mean", geom = "line")
g4 <- g3 + myx # + theme(legend.position = c(0.54, 0.3), legend.title = theme_bw())
print(g4)
## -----------------------------------------------------------------------------

summary(model.3)$coefficients

mytab2 <- as.data.frame(summary(model.4)$coefficients)
mytab2$LCI <- mytab2$Estimate - 2 * mytab2$"Std. Error"
mytab2$UCI <- mytab2$Estimate + 2 * mytab2$"Std. Error"
mytab2

model.7 <- lmer(read ~ grade5 + risk2 + eth2 + gen + (grade5 | subid),
                MPLS.LS, REML = FALSE)
model.8 <- lmer(read ~ grade5 * risk2 + grade5 * eth2 + grade5 * gen + (grade5 | subid),
                MPLS.LS, REML = FALSE)
mymodels <- list(model.1, model.2, model.3, model.4, model.5, model.6, model.7, model.8)
mynames <- paste("M", as.character(1:8), sep="")
myaicc <- as.data.frame(aictab(mymodels, mynames))[ ,-c(5,7)]
myaicc$Eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
data.frame(Modnames = myaicc$Modnames, round(myaicc[ ,-1], 2))


## Parametric Bootstrap of the Evidence Ratio ----------------------------------
model.1 <- lmer(read ~ grade5 + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.2 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.3 <- lmer(read ~ grade5 + risk2 + eth2 + (grade5 | subid), 
                MPLS.LS, REML = FALSE)
model.4 <- lmer(read ~ grade5 * risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.5 <- lmer(read ~ grade5 * eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.6 <- lmer(read ~ grade5 * risk2 + grade5 * eth2 + (grade5 | subid),
                MPLS.LS, REML = FALSE)
mynames <- paste("M", as.character(1:6), sep = "")
mymodels <- list(model.1, model.2, model.3, model.4, model.5, model.6)
myaicc <- as.data.frame(aictab(mymodels, mynames))[ ,-c(5,7)]
myaicc$Eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
myaicc

## Unfortunately I have not been able to the boot function code to work in  
## R 3.2.3
# > sessionInfo()
# R version 3.2.3 (2015-12-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 7 x64 (build 7601) Service Pack 1
# boot.func <- function(){
#     ## Simulate response vector based on best fitting Model 1.
#     simdv <- simulate(model.1)
#     ## Fit models using refit() and save aictab() output.
#     mynames <- paste("M", as.character(1:6), sep = "")
#     mymodels <- list(refit(model.1, simdv[ ,1]), refit(model.2, simdv[ ,1]),
#                      refit(model.3, simdv[ ,1]), refit(model.4, simdv[ ,1]),
#                      refit(model.5, simdv[ ,1]), refit(model.6, simdv[ ,1]))
#     myaicc <- aictab(mymodels, mynames, sort = F)
#     ## Compute bootstrap evidence ratio. Denominator is initial best model (Model 1).
#     b.eratio <- max(myaicc$AICcWt) / myaicc$AICcWt[1]
# }
# 
# set.seed(1)
# B <- 999
# mystorage0 <- rdply(.n = B, .expr = boot.func, .progress = "text")
# 
# ## Extract and sort bootstrap ratios.
# mystorage.s <- sort(mystorage0[ ,2])
# ## Compute empirical quantiles.
# a <- c(.1, .05, .01)                          # a values.
# b <- (B + 1) * (1 - a)                        # b values.
# myquant <- as.data.frame(mystorage.s[b]) # Save and display quantiles.
# colnames(myquant) <- "quantile"
# rownames(myquant) <- c("90%", "95%", "99%")
# myquant
# 
# ## summary(model.1)@AICtab$BIC