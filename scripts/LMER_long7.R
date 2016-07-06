## Chapter 7 R code from Long (2012)
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
#
##

## Chapter 7 R code.
##

## Select grade 5 data.
mysample <- subset(MPLS.LS, grade == 5)
## Fit true Model 0.
model.0 <- lm(read ~ 1, mysample)
## Simulate Sample A.
set.seed(1)                             # Reader can reproduce the results.
sim.dv <- unlist(simulate(model.0))     # Generate response.
sample.a <- data.frame(read = sim.dv, att = mysample$att)
head(sample.a)

model.1a <- lm(sample.a$read ~ sample.a$att)
model.2a <- lm(sample.a$read ~ sample.a$att + I(sample.a$att ^ 2))
dev.a <- data.frame(deviance = c(deviance(model.1a), deviance(model.2a)))
rownames(dev.a) <- c("Model.1a", "Model.2a")
dev.a

model.1a$coefficients
head(model.1a$fitted.values)

## Generate Sample B.
set.seed(13)
sim.dv <- unlist(simulate(model.0))
sample.b <- data.frame(read = sim.dv, att = mysample$att)
## Fit candidate models.
model.1b <- lm(sample.b$read ~ sample.b$att)
model.2b <- lm(sample.b$read ~ sample.b$att + I(sample.b$att ^ 2))
dev.b <- data.frame(deviance = c(deviance(model.1b), deviance(model.2b)))
rownames(dev.b) <- c("Model.1b", "Model.2b")
dev.b

N <- nrow(sample.b)
prdev.1b <- N * (log(2 * pi * sum((sample.b$read - model.1a$fitted.values) ^ 2)) + 1)
prdev.2b <- N * (log(2 * pi * sum((sample.b$read - model.2a$fitted.values) ^ 2)) + 1)
prdev.b <- data.frame(preddev = c(prdev.1b, prdev.2b))
rownames(prdev.b) <- c("Model 1", "Model 2")
prdev.b

set.seed(21)
sample.c <- data.frame(read = unlist(simulate(model.0)), att = mysample$att)
model.1c <- lm(sample.c$read ~ sample.c$att)
model.2c <- lm(sample.c$read ~ sample.c$att + I(sample.c$att ^ 2))
dev.c <- data.frame(deviance = c(deviance(model.1c), deviance(model.2c)))
rownames(dev.c) <- c("Model.1c", "Model.2c")
dev.c

prdev.1c <- N * (log(2 * pi * sum((sample.c$read - model.1b$fitted.values) ^ 2)) + 1)
prdev.2c <- N * (log(2 * pi * sum((sample.c$read - model.2b$fitted.values) ^ 2)) + 1)
prdev.c <- data.frame(preddev = c(prdev.1c, prdev.2c))
rownames(prdev.c) <- c("Model 1", "Model 2")
prdev.c

## Create grade5.
MPLS.LS$grade5 <- MPLS.LS$grade - 5
## Estimate models.
model.1 <- lmer(read ~ grade5 + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.2 <- lmer(read ~ grade5 + eth2  + (grade5 | subid), MPLS.LS, REML = FALSE)
model.3 <- lmer(read ~ grade5 + risk2 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
## Compute AIC.
mynames <- paste("M", as.character(1:3), sep = "")
myaic <- aictab(cand.set = list(model.1, model.2, model.3),
                modnames = mynames, sort = FALSE, second.ord = FALSE)
as.data.frame(myaic)[ ,1:3]

myaicc <- aictab(cand.set = list(model.1, model.2, model.3),
                 modnames = mynames, sort = FALSE)
as.data.frame(myaicc)[ ,1:3]

myaicc <- aictab(list(model.1, model.2, model.3), modnames = mynames)
print(myaicc, LL = FALSE)

confset(cand.set = list(model.1, model.2, model.3), modnames = mynames)
confset(cand.set = list(model.1, model.2, model.3), modnames = mynames, level = 0.75)

evidence(myaicc)

## Save data frame, exclude logLik.
myaicc2 <- as.data.frame(myaicc)[ ,-7]
myaicc2$Eratio <- max(myaicc2$AICcWt) / myaicc2$AICcWt
## Print with rounding to two decimal places.
data.frame(Model = myaicc2$Modnames, round(myaicc2[ ,-1], 2))

model.1 <- lmer(read ~ grade5 + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.2 <- lmer(read ~ grade5 + eth2  + (grade5 | subid), MPLS.LS, REML = FALSE)
model.3 <- lmer(read ~ grade5 + risk2 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.4 <- lmer(read ~ grade5 * risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.5 <- lmer(read ~ grade5 * eth2  + (grade5 | subid), MPLS.LS, REML = FALSE)
model.6 <- lmer(read ~ grade5 * risk2 + grade5 * eth2 + (grade5 | subid),
                MPLS.LS, REML = FALSE)

mynames <- paste("M", as.character(1:6), sep = "")
mymodels <- list(model.1, model.2, model.3, model.4, model.5, model.6)
myaicc <- as.data.frame(aictab(cand.set = mymodels, modnames = mynames,
                               sort = FALSE)[,-c(5,7)])
myaicc$Eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
data.frame(Modnames = myaicc$Modnames, round(myaicc[,-1], 2))

## Define components for later use.
myx <- scale_x_continuous(breaks = 5:8)
theme_set(theme_bw())
## Create bar graphs.
g1 <- ggplot(myaicc, aes(x = Modnames, y = AICcWt)) + ylab("Weight")
g2 <- g1 + geom_bar(fill = "grey80", colour = "black") + xlab("Model")
g3 <- g2 + scale_y_continuous(limits = c(0,1))
print(g3)

g1 <- ggplot(myaicc, aes(x = Modnames, y = Eratio)) + ylab("Ratio")
g2 <- g1 + geom_bar(fill = "grey80", colour = "black") + xlab("Model")
g3 <- g2 + geom_hline(aes(yintercept = 1), linetype = 2)
print(g3)

## Sort data frame.
myaicc2 <- myaicc[order(myaicc$AICc), ]
## Bar graph of weight of evidence.
r1 <- ggplot(myaicc2, aes(x = I(as.character(Modnames)), y = AICcWt)) + ylab("Weight")
r2 <- r1 + geom_bar(fill = "grey80", colour = "black") + xlab("Model")
r3 <- r2 + scale_y_continuous(limits = c(0,1))
print(r3)
## Bar graph of evidence ratio.
s1 <- ggplot(myaicc2, aes(x = I(as.character(Modnames)), y = Eratio)) + ylab("Weight")
s2 <- s1 + geom_bar(fill = "grey80", colour = "black") + xlab("Model")
s3 <- s2 + geom_hline(aes(yintercept = 1), linetype = 2)
print(s3)

myaicc <- as.data.frame(aictab(cand.set = mymodels, modnames = mynames)[ ,-c(5,7)])
myaicc$Eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
data.frame(Modnames = myaicc$Modnames, round(myaicc[ ,-1], 2))

confset(cand.set = mymodels, modnames = mynames, level = 0.99)

mytab <- as.data.frame(summary(model.1)@coefs)
mytab

mytab$LCI <- mytab$Estimate - 2 * mytab$"Std. Error"
mytab$UCI <- mytab$Estimate + 2 * mytab$"Std. Error"
round(mytab[ ,-3], 2)

## Create data frame for graphing.
plotdata <- model.1@frame
plotdata$pred <- model.matrix(model.1) %*% fixef(model.1)
plotdata$grade <- plotdata$grade + 5
## ggplot2.
g1 <- ggplot(plotdata, aes(x = grade, y = read, linetype = risk2))
g2 <- g1 + stat_summary(fun.y = "mean", geom = "point", cex = 2)
g3 <- g2 + stat_summary(aes(y = pred), fun.y = "mean", geom = "line")
g4 <- g3 + myx + opts(legend.position = c(0.54, 0.3), legend.title = theme_blank())
print(g4)

summary(model.3)@coefs

mytab2 <- as.data.frame(summary(model.4)@coefs)
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

model.1 <- lmer(read ~ grade5 + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.2 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.3 <- lmer(read ~ grade5 + risk2 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.4 <- lmer(read ~ grade5 * risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.5 <- lmer(read ~ grade5 * eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.6 <- lmer(read ~ grade5 * risk2 + grade5 * eth2 + (grade5 | subid),
                MPLS.LS, REML = FALSE)
mynames <- paste("M", as.character(1:6), sep = "")
mymodels <- list(model.1, model.2, model.3, model.4, model.5, model.6)
myaicc <- as.data.frame(aictab(mymodels, mynames))[ ,-c(5,7)]
myaicc$Eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
myaicc

boot.func <- function(){
    ## Simulate response vector based on best fitting Model 1.
    simdv <- simulate(model.1)
    ## Fit models using refit() and save aictab() output.
    mynames <- paste("M", as.character(1:6), sep = "")
    mymodels <- list(refit(model.1, simdv[ ,1]), refit(model.2, simdv[ ,1]),
                     refit(model.3, simdv[ ,1]), refit(model.4, simdv[ ,1]),
                     refit(model.5, simdv[ ,1]), refit(model.6, simdv[ ,1]))
    myaicc <- aictab(mymodels, mynames, sort = F)
    ## Compute bootstrap evidence ratio. Denominator is initial best model (Model 1).
    b.eratio <- max(myaicc$AICcWt) / myaicc$AICcWt[1]
}

set.seed(1)
B <- 999
mystorage0 <- rdply(.n = B, .expr = boot.func, .progress = "text")

## Extract and sort bootstrap ratios.
mystorage.s <- sort(mystorage0[ ,2])
## Compute empirical quantiles.
a <- c(.1, .05, .01)                          # a values.
b <- (B + 1) * (1 - a)                        # b values.
myquant <- as.data.frame(mystorage.s[b]) # Save and display quantiles.
colnames(myquant) <- "quantile"
rownames(myquant) <- c("90%", "95%", "99%")
myquant

## summary(model.1)@AICtab$BIC