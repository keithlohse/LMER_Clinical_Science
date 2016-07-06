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
#
##

## Select subset.
mysubset <- subset(MPLS.LS, subid == 1 & grade == 5 | subid == 3 & grade == 6 |
                       subid == 5 & grade == 7 | subid == 7 & grade == 8,
                   select = c(subid, read, grade))
mysubset

## Graph with regression line.
theme_set(theme_bw())
myx <- scale_x_continuous(breaks = 5:8)
g1 <- ggplot(mysubset, aes(grade, read)) + geom_point()
g2 <- g1 + stat_smooth(method = "lm", se = FALSE) + myx
print(g2)

dnorm(-1, mean = 0, sd = sqrt(1))

norm.ex <- data.frame(eps = seq(from = -4, to = 4, by = 0.1) )
norm.ex$f.of.eps <- dnorm(norm.ex$eps, mean = 0, sd = 1)
head(norm.ex)

g1 <- ggplot(data = norm.ex, aes(x = eps, y = f.of.eps)) + geom_line()
g2 <- g1 + geom_abline(intercept = 0, slope = 0, linetype = 2)
g3 <- g2 + ylab(expression(italic(f)(epsilon)))
g4 <- g3 + xlab(expression(paste("Epsilon ", (epsilon))))
print(g4)

## Create the function.
dev.func <- function(B1) {4 * log(2 * pi * 49) + (1 / 49) *
                              sum((mysubset$read - 102 - B1 * mysubset$grade) ^ 2)}
## Generate the values of the deviance and the store results.
dev.store <- mdply(data.frame(B1 = seq(from = 13, to = 17, by = 0.1)),
                   dev.func)
colnames(dev.store)[2] <- "deviance"
head(dev.store)

subset(dev.store, deviance == min(deviance))

g1 <- ggplot(data = dev.store, aes(x = B1, y = deviance)) + geom_line()
g2 <- g1 + geom_abline(intercept = min(dev.store$deviance), slope = 0, linetype = 2)
g3 <- g2 + ylab(expression(italic(deviance))) + xlab(expression(beta[1]))
print(g3)

## Create the function.
dev.func <- function(B0, B1) {4 * log(2 * pi * 49) + (1 / 49) *
                                  sum((mysubset$read - B0 - B1 * mysubset$grade) ^ 2)}
## Generate the values of the deviance and store the results.
dev.store <- mdply(expand.grid(B0 = seq(98, 106, by = 0.1), B1 = seq(12, 18, by = 0.1)),
                   dev.func)
colnames(dev.store)[3] <- "deviance"
head(dev.store)

subset(dev.store, deviance == min(deviance))

wireframe(deviance ~ B0 * B1, dev.store)

print(lmer.0 <- lmer(read ~ grade + (1 | subid), MPLS.LS,
                     REML = FALSE, verbose = TRUE), cor = FALSE)

## Estimate the model.
lm.1 <- lm(read ~ grade, data = mysubset)
## Extract the log-likelihood.
ll.1 <- logLik(lm.1)
## Compute the deviance.
dev.1 <- -2 * as.numeric(ll.1)
dev.1

lm.0 <- lm(read ~ 1, mysubset)
comp <- data.frame(deviance = c(-2 * logLik(lm.1), -2 * logLik(lm.0)))
rownames(comp) <- c("Linear", "Intercept-only")
comp

## Estimate linear model.
lmer.1 <- lmer(read ~ grade + (grade | subid), MPLS.LS, REML = FALSE)
## Print ML information.
summary(lmer.1)@AICtab

## Estimate the full model.
lmer.2 <- lmer(read ~ grade + I(grade ^ 2) + (grade | subid), MPLS.LS, REML = FALSE)
## LRT.
anova(lmer.1, lmer.2)

lmer.2 <- lmer(read ~ grade + risk2 + eth2 + (grade | subid), MPLS.LS, REML = FALSE)
summary(lmer.2)@coefs

mytable <- as.data.frame(summary(lmer.2)@coefs)
mytable$LCI <- mytable$Estimate - 2 * mytable$"Std. Error"
mytable$UCI <- mytable$Estimate + 2 * mytable$"Std. Error"
mytable

mytable$p.value <- pnorm(q = abs(mytable$"t value"), lower.tail = FALSE) * 2
round(mytable, 4)
