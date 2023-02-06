#Long Chapter 7: Multimodel Inference and Akaike's Information Criterion

library(tidyverse)
library(AICcmodavg)
library(lme4)
library(plyr)

load(file = "C:/Users/miller.allison/Box/Allison Miller/Course Work and Training/Longitudinal Data Analysis in R_Fall 2022/MPLS.Sorted.Rdata")

#7.3. AIC and Predictive Accuracy.
#Select grade 5 data
mysample <- subset(MPLS.Sorted, grade ==5)

#Fit true Model 0
model.0 <- lm(read ~ 1, mysample)

##Simulate Sample A.
set.seed(1)
sim.dv <- unlist(simulate(model.0))
sample.a <- data.frame(read = sim.dv, att = mysample$att)
head(sample.a)

g1 <- ggplot(sample.a, aes(att, read)) + geom_point()
print(g1)

model.1a <- lm(sample.a$read ~ sample.a$att)

model.2a <- lm(sample.a$read ~ sample.a$att + I(sample.a$att ^ 2))

dev.a <- data.frame(deviance = c(deviance(model.1a), deviance(model.2a)))
rownames(dev.a) <- c("Model.1a", "Model.2a")
dev.a

model.1a$coefficients
head(model.1a$fitted.values)

#Generate Sample B
set.seed(13)
sim.dv <- unlist(simulate(model.0))
sample.b <- data.frame(read = sim.dv, att = mysample$att)

#Fit Candidate Models
model.1b <- lm(sample.b$read ~ sample.b$att)
model.2b <- lm(sample.b$read ~ sample.b$att + I(sample.b$att ^ 2))

dev.b <- data.frame(deviance = c(deviance(model.1b), deviance(model.2b)))
rownames(dev.b) <- c("Model.1b", "Model.2b")
dev.b

#Compute the predictive deviance for each model:
N <- nrow(sample.b)
prdev.1b <- N * (log(2 * pi * sum((sample.b$read - model.1a$fitted.values) ^ 2)) + 1)
prdev.2b <- N * (log(2 * pi * sum((sample.b$read - model.2a$fitted.values) ^ 2)) + 1)
prdev.b <- data.frame(preddev = c(prdev.1b, prdev.2b))
rownames(prdev.b) <- c("Model 1", "Model 2")
prdev.b

#Generate Sample C
set.seed(21)
sample.c <- data.frame(read = unlist(simulate(model.0)), att = mysample$att)
model.1c <- lm(sample.c$read ~ sample.c$att)
model.2c <- lm(sample.c$read ~ sample.c$att + I(sample.c$att ^ 2))
dev.c <- data.frame(deviance = c(deviance(model.1c), deviance(model.2c)))
rownames(dev.c) <- c("Model.1c", "Model.2c")
dev.c

#Compute predictive deviance using the fitted values from Sample C and the response from
#Sample C:
prdev.1c <- N * (log(2 * pi * sum((sample.c$read - model.1b$fitted.values) ^ 2)) + 1)
prdev.2c <- N * (log(2 * pi * sum((sample.c$read - model.2b$fitted.values) ^ 2)) + 1)
prdev.c <- data.frame(preddev = c(prdev.1c, prdev.2c))
rownames(prdev.c) <- c("Model 1", "Model 2")
prdev.c


#7.3.1: Extension to LMER.

#Create grade5
MPLS.Sorted$grade5 <- MPLS.Sorted$grade - 5

#Estimate models
model.1 <- lmer(read ~ grade5 + riskC + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.2 <- lmer(read ~ grade5 + eth2  + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.3 <- lmer(read ~ grade5 + riskC + eth2 + (grade5 | subid), MPLS.Sorted, REML = FALSE)

#Compute AIC (second.ord = FALSE)
mynames <- paste("M", as.character(1:3), sep = "")
myaic <- aictab(cand.set = list(model.1, model.2, model.3),
                modnames = mynames, sort = FALSE, second.ord = FALSE)
as.data.frame(myaic)[,1:3]

#Compute the AICc (AIC corrected; second.ord = TRUE which is the default)
myaicc <- aictab(cand.set = list(model.1, model.2, model.3),
                 modnames = mynames, sort = FALSE)
as.data.frame(myaicc)[,1:3]

#AICc and Effect Size
#sort = TRUE sorts the output by the weight of evidence
myaicc <- aictab(list(model.1, model.2, model.3), modnames = mynames, sort = TRUE)
print(myaicc, LL = FALSE)

#Using the confset function to print the confidence set. The default confidence is 95%.
confset(cand.set = list(model.1, model.2, model.3), modnames = mynames)

#Changing the confidence to 75% using level =
confset(cand.set = list(model.1, model.2, model.3), modnames = mynames, level = 0.75)

#Compute the evidence ratio
evidence(myaicc)
#Save data frame, exclude logLik
myaicc2 <- as.data.frame(myaicc)[,-7]
myaicc2$Eratio <- max(myaicc2$AICcWt) / myaicc2$AICcWt
data.frame(Model = myaicc2$Modnames, round(myaicc2[ ,-1], 2))


#7.6.2. Example Set of Models.
model.1 <- lmer(read ~ grade5 + riskC + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.2 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.3 <- lmer(read ~ grade5 + riskC + eth2 + (grade | subid), MPLS.Sorted, REML = FALSE)
model.4 <- lmer(read ~ grade5 * riskC + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.5 <- lmer(read ~ grade5 * eth2 + (grade | subid), MPLS.Sorted, REML = FALSE)
model.6 <- lmer(read ~ grade5 * riskC + grade5 * eth2 + (grade5 | subid),
                MPLS.Sorted, REML = FALSE)

mynames <- paste("M", as.character(1:6), sep = "")
mymodels <- list(model.1, model.2, model.3, model.4, model.5, model.6)                 
myaicc <- as.data.frame(aictab(cand.set = mymodels, modnames = mynames, sort = FALSE)[,-c(5,7)])
myaicc$Eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
data.frame(Modnames = myaicc$Modnames, round(myaicc[,-1], 2))

#7.6.3. Bar Graphs of Results.

#Define the components for later use
myx <- scale_x_continuous(breaks = 5:8)
theme_set(theme_bw())

#Create bar graphs
#Bar graph for weight of evidence
g1 <- ggplot(myaicc, aes(x = Modnames, y = AICcWt)) + ylab("Weight")
g2 <- g1 + geom_bar(stat = 'identity', fill = "grey80", colour = "black") + xlab("Model")
g3 <- g2 + scale_y_continuous(limits = c(0,1))
print(g3)

#Bar graph for evidence ratio
g1 <- ggplot(myaicc, aes(x = Modnames, y = Eratio)) + ylab("Ratio")
g2 <- g1 + geom_bar(stat = 'identity', fill = "grey80", colour = "black") + xlab("Model")
g3 <- g2 + geom_hline(aes(yintercept = 1), linetype = 2)
print(g3)

#Sort data frame
myaicc2 <- myaicc[order(myaicc$AICc), ]

#Bar graph of weight of evidence
r1 <- ggplot(myaicc2, aes(x = reorder(Modnames, -AICcWt), y = AICcWt)) + ylab("Weight")
r2 <- r1 + geom_bar(stat = 'identity', fill = "grey0", colour = "black") + xlab("Model")
r3 <- r2 + scale_y_continuous(limits = c(0,1))
print(r3)

#Bar graph of evidence ratio
s1 <- ggplot(myaicc2, aes(x = reorder(Modnames, +Eratio), y = Eratio)) + ylab("Ratio")
s2 <- s1 + geom_bar(stat = 'identity', fill = "grey0", colour = "black") + xlab("Model")
s3 <- s2 + geom_hline(aes(yintercept = 1), linetype = 2)
print(s3)

#Compute the cumulative weight of evidence
myaicc <- as.data.frame(aictab(cand.set = mymodels, modnames = mynames)[ ,-c(5,7)])
myaicc$Eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
data.frame(Modnames = myaicc$Modnames, round(myaicc[ , -1], 2))

#Compute 99% confidence set
confset(cand.set = mymodels, modnames = mynames, level = 0.99)


#7.6.5. Details of Models.
mytab <- as.data.frame(coef(summary(model.1)))

#Add confidence intervals
mytab$LCI <- mytab$Estimate - 2 * mytab$`Std. Error`
mytab$UCI <- mytab$Estimate + 2 * mytab$`Std. Error`
round(mytab[ ,3], 2)
mytab

#Create a dataframe for graphing
plotdata <- model.1@frame
plotdata$pred <- model.matrix(model.1) %*% fixef(model.1)
plotdata$grade <- plotdata$grade + 5

#ggplot2
g1 <- ggplot(plotdata, aes(x = grade, y = read, linetype = riskC))
g2 <- g1 + stat_summary(fun = "mean", geom = "point", cex = 2)
g3 <- g2 + stat_summary(aes(y = pred), fun = "mean", geom = "line")
g4 <- g3 + myx + theme(legend.position = c(0.9, 0.18))
print(g4)

#Relative effects of Model 3
coef(summary(model.3))

#Examining the slope effect of Model 4
mytab2 <- as.data.frame(coef(summary(model.4)))
mytab2$LCI <- mytab2$Estimate - 2 * mytab2$`Std. Error`
mytab2$UCI <- mytab2$Estimate + 2 * mytab2$`Std. Error`
round(mytab2[ ,3], 2)
mytab2


#Post Hoc Models.
model.7 <- lmer(read ~ grade5 + riskC + eth2 + gen + (grade5 | subid),
                MPLS.Sorted, REML = FALSE)
model.8 <- lmer(read ~ grade5 * riskC + grade5 * eth2 + grade5 * gen + (grade | subid),
                MPLS.Sorted, REML = FALSE)
mymodels <- list(model.1, model.2, model.3, model.4, model.5, model.6, model.7, model.8)
mynames <- paste("M", as.character(1:8), sep = "")
myaicc <- as.data.frame(aictab(mymodels, mynames))[ ,-c(5,7)]
myaicc$Eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
data.frame(Modnames = myaicc$Modnames, round(myaicc[ ,-1], 2))



#7.8.1. Performing the Parametric Bootstrap

#Estimate the 6 models of interest
model.1 <- lmer(read ~ grade5 + riskC + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.2 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.3 <- lmer(read ~ grade5 + riskC + eth2 + (grade | subid), MPLS.Sorted, REML = FALSE)
model.4 <- lmer(read ~ grade5 * riskC + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.5 <- lmer(read ~ grade5 * eth2 + (grade | subid), MPLS.Sorted, REML = FALSE)
model.6 <- lmer(read ~ grade5 * riskC + grade5 * eth2 + (grade5 | subid),
                MPLS.Sorted, REML = FALSE)

mynames <- paste("M", as.character(1:6), sep = "")
mymodels <- list(model.1, model.2, model.3, model.4, model.5, model.6)                 
myaicc <- as.data.frame(aictab(cand.set = mymodels, modnames = mynames)[,-c(5,7)])
myaicc$Eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
myaicc

#Write a function for the operations that will be carried out for each bootstrap replication.
#These four operations are: 
#(1) Simulate the response data using simulate()
#(2) Fit each model with the simulated data using refit()
#(3) Compute the weight of evidence using aictab()
#(4) Compute the replication evidence ratio
boot.func <- function(){
  ##Simulate response vector based on best fitting Model 1
  simdv <- simulate(model.1)
  ##Fit models using refit() and save aictab() output
  mynames <- paste("M", as.character(1:6), sep = "")
  mymodels <- list(refit(model.1, simdv), refit(model.2, simdv),
                   refit(model.3, simdv), refit(model.4, simdv),
                   refit(model.5, simdv), refit(model.6, simdv))
  myaicc <- aictab(mymodels, mynames, sort = FALSE)
  ##Compute bootstrap evidence ratio. Denominator is initial best model (Model 1).
  b.eratio <- max(myaicc$AICcWt) / myaicc$AICcWt[1]
}

#To execute the boot.func() numerous times, the rdply() function is used
set.seed(1)
B <-1000 
mystorage0 <- rdply(.n = B, .expr = boot.func, .progress = "text")

#Extract and sort bootstrap ratios
mystorage.s <- sort(mystorage0[ ,2])

#Compute empirical quantiles
a <- c(.1, 0.05, .01)
b <- (B + 1) * (1 - a)

#Save and display quantiles
myquant <- as.data.frame(mystorage.s[b])
colnames(myquant) <- "quantile"
rownames(myquant) <- c("90%", "95%", "99%")
myquant
