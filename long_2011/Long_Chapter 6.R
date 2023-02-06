#Long Chapter 6: Overview of Maximum Likelihood Estimation
library(tidyverse)
library(plyr)
library(lme4)
library(lattice)

load(file = "C:/Users/miller.allison/Box/Allison Miller/Course Work and Training/Longitudinal Data Analysis in R_Fall 2022/MPLS.Sorted.Rdata")


#6.2: Maximum Likelihood and LM
#Select subset
mysubset <- subset(MPLS.Sorted, subid == 1 & grade ==5 | subid == 3 & grade ==6 |
                     subid == 5 & grade == 7 | subid == 7 & grade == 8,
                   select = c(subid, read, grade))

#Graph with regression line
theme_set(theme_bw())
myx <- scale_x_continuous(breaks = 5:8)
g1 <- ggplot(mysubset, aes(grade, read)) + geom_point()
g2 <- g1 + stat_smooth(method = "lm", se = FALSE) + myx
print(g2)

dnorm(-1, mean = 0, sd = sqrt(1))
norm.ex <- data.frame(eps = seq(from = -4, to = 4, by = 0.1))
norm.ex$f.of.eps <- dnorm(norm.ex$eps, mean = 0, sd = 1)
head(norm.ex)

g1 <- ggplot(data = norm.ex, aes(x = eps, y = f.of.eps)) + geom_line()
g2 <- g1 + geom_abline(intercept = 0, slope = 0, linetype = 2)
g3 <- g2 + ylab(expression(italic(f)(epsilon)))
g4 <- g3 + xlab(expression(paste("Epsilon ", (epsilon))))
print(g4)

#Simulating ML estimation: Finding the best values of Beta by substituting several candidate
#values, compute the deviance, and then pick the Beta candidate that produces the smallest deviance.

#Create the function:
dev.func <- function(B1) {4 * log(2 * pi * 49) + (1/49) *
    sum((mysubset$read - 102 - B1 * mysubset$grade) ^ 2)}

#Generate the values of the deviance and the store results.
dev.store <- mdply(data.frame(B1 = seq(from = 13, to = 17, by = 0.1)),
                   dev.func)
colnames(dev.store)[2] <- "deviance"
head(dev.store)
subset(dev.store, deviance == min(deviance))

g1 <- ggplot(data = dev.store, aes(x = B1, y = deviance)) + geom_line()
g2 <- g1 + geom_abline(intercept = min(dev.store$deviance), slope = 0, linetype = 2)
g3 <- g2 + ylab(expression(italic(deviance))) + xlab(expression(beta[1]))
print(g3)


#6.2.1. Several Unknown Parameters
#Create the function
dev.func <- function(B0, B1) {4 * log(2 * pi * 49) + (1/49) *
    sum((mysubset$read - B0 - B1 * mysubset$grade) ^ 2)}

#Generate the values of the deviance and store the results
dev.store <- mdply(expand.grid(B0 = seq(98,106,by = 0.1), B1 = seq(12,18,by = 0.1)),
                   dev.func)
colnames(dev.store)[3] <- "deviance"
head(dev.store)
subset(dev.store, deviance == min(deviance))

#use the wireframe function to create a 3D plot
wireframe(deviance ~ B0 * B1, dev.store)

print(lmer.0 <- lmer(read ~ grade + (1 | subid), MPLS.Sorted,
                     REML = FALSE, verbose = TRUE), cor = FALSE)


#6.2.4. Extracting the Log-Likelihood and the Deviance:
#Estimate the model
lm.1 <- lm(read ~ grade, data = mysubset)

#Extract the loglikelihood
ll.l <- logLik(lm.1)

#Compute the deviance
dev.1 <- -2 * as.numeric(ll.l)


#6.2.5. Comparing Models: 
#Comparing a linear model to an intercept only model of the mysubset data, calculating the 
#deviance, and comparing models to see which is better:
lm.0 <- lm(read ~ 1, mysubset)
comp <- data.frame(deviance = c(-2 * logLik(lm.1), -2 * logLik(lm.0)))
rownames(comp) <- c("Linear", "Intercept Only")
comp


#6.3. Maximum Likelihood and LMER
#Estimate linear model
lmer.1 <- lmer(read ~ grade + (grade | subid), MPLS.Sorted, REML = FALSE)

#Print ML information
summary(lmer.1)

#Estimate the full model
lmer.2 <- lmer(read ~ grade + I(grade ^ 2) + (grade | subid), MPLS.Sorted, REML = FALSE)

#LRT
anova(lmer.1, lmer.2)



#6.3.2. ML Standard Errors
lmer.2 <- lmer(read ~ grade + riskC + eth2 + (grade | subid), MPLS.Sorted, REML = FALSE)
summary(lmer.2)

#Add confidence intervals
mytable <- as.data.frame(fixef(lmer.2))
mytable$LCI <- mytable$fixef - 2 * mytable