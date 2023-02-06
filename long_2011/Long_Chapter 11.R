##Long Chapter 11: Extending Linear Mixed Effects Regression

library(tidyverse)
library(plyr)
library(lme4)
library(lattice)
library(AICcmodavg)
library(dplyr)
library(kml)

load(file = "C:/Users/miller.allison/Box/Allison Miller/Course Work and Training/Longitudinal Data Analysis in R_Fall 2022/MPLS.Sorted.Rdata")

MPLS.Sorted$grade5 <- MPLS.Sorted$grade - 5

#11.1 Graphing Fitted Curves
FittedFE <- function(x) model.matrix(x) %*% fixef(x)

lmer.1 <- lmer(read ~ grade5 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
head(FittedFE(lmer.1))

plotdata <- data.frame(lmer.1@frame, fitted = FittedFE(lmer.1))
plotdata$grade <- plotdata$grade5 + 5
head(plotdata, n = 11)

theme_set(theme_bw())
myx <- scale_x_continuous(breaks = 5:8)
g1 <- ggplot(plotdata, aes(x = grade, y = read)) + geom_point(shape = 19)
g2 <- g1 + stat_summary(fun = mean, geom = "point", size = 5, shape = 1)
g3 <- g2 + geom_line(aes(y = fitted), lwd = 1.5) + myx + theme("aspect.ratio" = 1)
print(g3)

lmer.2 <- lmer(read ~ grade5 * riskC + grade5 * eth2 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
plotdata <- data.frame(lmer.2@frame, fitted = FittedFE(lmer.2))
plotdata$grade <- plotdata$grade5 + 5
head(plotdata)

#my.m <- ddply(plotdata, .(plotdata$riskC, plotdata$grade, plotdata$fitted), mean = mean(fitted))
#colnames(my.m) <- c("riskC", "grade", "mfitted")
#my.m

my.m <- plotdata %>% dplyr::select(riskC, grade, fitted) %>% group_by(riskC, grade, .add = TRUE) %>%
  dplyr::summarise(mfitted = mean(fitted, na.rm = TRUE))
my.m

g1 <- ggplot(plotdata, aes(x = grade, y = read, shape = riskC))
g2 <- g1 + stat_summary(fun = mean, geom = "point")
g3 <- g2 + stat_summary(fun = mean, geom = "line", aes(y = fitted, linetype = riskC))
g4 <- g3 + theme("aspect.ratio" = 1)
print(g4)

#11.2. Static Predictors with Multiple Levels.
MPLS.Sorted$risk <- as.factor(MPLS.Sorted$risk)
levels(MPLS.Sorted$risk)
#In this example, ADV is the reference group (i.e., it is never assigned a 1- it is always 0/the
#reference category)

lmer.3 <- lmer(read ~ grade5 * risk + (grade5 | subid), MPLS.Sorted, REML = FALSE)
print(lmer.3)

#Changing the reference category to HHM:
rrisk <- relevel(MPLS.Sorted$risk, ref = "HHM")
lmer.3A <- lmer(read ~ grade5 * rrisk + (grade5 | subid), MPLS.Sorted, REML = FALSE)
print(lmer.3A)

#Alternative approach to changing the reference category to HHM:
lmer.3AA <- lmer(read ~ grade5 * C(risk, base = 2) + (grade5 | subid), MPLS.Sorted, REML = FALSE)
summary(lmer.3AA)$coefficients

#The overall fit of the model does not change based on which set of dummy variable is used:
summary(lmer.3)$AICtab
summary(lmer.3A)$AICtab

#Plotting the observed means for the groups along with their fitted values to visualize group differences
plotdata <- data.frame(lmer.3@frame, fitted = FittedFE(lmer.3))
plotdata$grade <- plotdata$grade5 + 5

g1 <- ggplot(plotdata, aes(x = grade, y = read, shape = risk))
g2 <- g1 + stat_summary(fun = mean, geom = "point")
g3 <- g2 + stat_summary(fun = mean, geom = "line", aes(y = fitted, linetype = risk))
g4 <- g3 + myx + theme("aspect.ratio" = 1) + scale_shape(solid = FALSE)
print(g4)

#11.2.1. Evaluating Sets of Dummy Variables
reduced <- lmer(read ~ grade5 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
full <- lmer(read ~ grade5 * risk + (grade5 | subid), MPLS.Sorted, REML = FALSE)
anova(reduced, full)

full.1 <- lmer(read ~ grade5 + risk + (grade5 | subid), MPLS.Sorted, REML =  FALSE)
full.2 <- lmer(read ~ grade5 * risk + (grade5 | subid), MPLS.Sorted, REML = FALSE)
anova(reduced, full.1, full.2)

#11.2.2. Evaluating Individual Dummy Variables.
require(multcomp)
#Create contrast coefficients
K <- rbind("Int: POV - ADV" = c(0, 0, 1, 0, 0, 0),
           "Int: HHM - ADV" = c(0, 0, 0, 1, 0, 0),
           "Int: HHM - POV" = c(0, 0, 1, -1, 0, 0),
           "Slo: POV - ADV" = c(0, 0, 0, 0, 1, 0),
           "Slo: HHM - ADV" = c(0, 0, 0, 0, 0, 1),
           "Slo: HHM - POV" = c(0, 0, 0, 0, 1, -1))
#Save glht() output object
glht.3 <- glht(lmer.3, K)

summary(glht.3, test = adjusted(type = "none"))

confint(glht.3, calpha = 1.96)

#11.3.1. Static Predictor Interactions with lmer()

#verify non-zero counts for combinations of the static predictor levels
with(MPLS.Sorted[MPLS.Sorted$grade ==5, ], table(riskC, eth2))

lmer.4 <- lmer(read ~ grade5 * riskC * eth2 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
print(lmer.4)

lmer.0 <- lmer(read ~ grade5 * eth2 + grade5 * riskC + (grade5 | subid), MPLS.Sorted, REML = FALSE)
anova(lmer.0, lmer.4)

#11.3.2. Interpreting Interactions.
plotdata <- data.frame(lmer.4@frame, fitted = FittedFE(lmer.4))
plotdata$group4 <- with(plotdata, interaction(eth2, riskC))
plotdata$grade <- plotdata$grade5 + 5
head(plotdata)

levels(plotdata$group4)

#Facet graphs for risk and ethnicity combinations (interactions)
g1 <- ggplot(plotdata, aes(x = grade, y = read))
g2 <- g1 + geom_line(aes(group = subid), colour = "grey80")
g3 <- g2 + stat_summary(fun = mean, geom = "point", aes(group = 1))
g4 <- g3 + stat_summary(fun = mean, geom = "line", aes(y = fitted))
g5 <- g4 + facet_grid(eth2 ~ riskC) + myx
print(g5)

#Superimposing group curves on the same graph
g1 <- ggplot(plotdata, aes(x = grade, y = fitted, shape = group4, linetype = group4))
g2 <- g1 + stat_summary(fun = mean, geom = "point", size = 2.5) + myx
g3 <- g2 + stat_summary(fun = mean, geom = "line") + theme("aspect.ratio" = 1)
g4 <- g3 + scale_shape(solid = FALSE)
print(g4)


#11.4. Indexes of Absolute Effect Size in LMER

#Estimate models
lmer.1 <- lmer(read ~ grade5 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
lmer.2 <- lmer(read ~ grade5 * riskC + (grade5 | subid), MPLS.Sorted, REML = FALSE)
lmer.3 <- lmer(read ~ grade5 * riskC + grade5 * eth2 + (grade5 | subid),
               MPLS.Sorted, REML = FALSE)

#Compute R^2
my.rsq <- c(cor(y = lmer.1@frame$read, x = FittedFE(lmer.1)) ^ 2,
            cor(y = lmer.2@frame$read, x = FittedFE(lmer.2)) ^ 2,
            cor(y = lmer.3@frame$read, x = FittedFE(lmer.3)) ^ 2)
data.frame(model = c("lmer.1", "lmer.2", "lmer.3"), rsq = my.rsq)

#Compute AICc
myaicc <- as.data.frame(aictab(list(lmer.1, lmer.2, lmer.3), sort = FALSE,
                               c("lmer.1", "lmer.2", "lmer.3"))[ ,-c(5,7)])

#Compute R^2
myaicc$rsq <- c(cor(y = lmer.1@frame$read, x = FittedFE(lmer.1)) ^ 2,
                cor(y = lmer.2@frame$read, x = FittedFE(lmer.2)) ^ 2,
                cor(y = lmer.3@frame$read, x = FittedFE(lmer.3)) ^ 2)
myaicc

#11.4.1. Alternative Indexes
#Compute variance of the random intercepts:
reduced <- lmer(read ~ grade5 + (1 | subid), MPLS.Sorted, REML = FALSE)
full <- lmer(read ~ grade5 + riskC + (1 | subid), MPLS.Sorted, REML = FALSE)
v.r <- attr(VarCorr(reduced)$subid, "stddev") ^ 2
v.f <- attr(VarCorr(full)$subid, "stddev") ^ 2
v.r; v.f

#Compute PRV for random intercepts
PRV <- (v.r - v.f) / v.r
PRV

#Compute variance of the random slopes:
reduced <- lmer(read ~ grade5 + riskC + (grade5 | subid), MPLS.Sorted, REML = FALSE)
full <- lmer(read ~ grade5 * riskC + (grade5 | subid), MPLS.Sorted, REML = FALSE)
v.r <- attr(VarCorr(reduced)$subid, "stddev")[2] ^ 2
v.f. <- attr(VarCorr(full)$subid, "stddev")[2] ^ 2

#Compute PRB for random slopes
PRV <- (v.r - v.f) / v.r
PRV

#Computing the omnibus PRB measure using the determinant of the G matrix
reduced <- lmer(read ~ grade5 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
full <- lmer(read ~ grade5 * riskC + (grade5 | subid), MPLS.Sorted, REML = FALSE)

#Determinant
v.r <- det(as.matrix(VarCorr(reduced)$subid))
v.f <- det(as.matrix(VarCorr(full)$subid))
v.r ; v.f

#PRV
PRV <- (v.r - v.f) / v.r
PRV

#Things do not always work out.
mywide <- reshape(MPLS.Sorted, v.names = "read", timevar = "grade",
                  idvar = "subid", direction = "wide", drop = "grade5")
set.seed(1234)
mywide$X1 <- round(rnorm(nrow(mywide), mean = 100, sd = 15), 0)
mylong <- reshape(mywide, direction = "long")
mylong <- mylong[order(mylong$subid), ]
head(mylong)

#Estimate the models
sim.1 <- lmer(read ~ grade + (grade | subid), MPLS.Sorted, REML = FALSE)
sim.2 <- lmer(read ~ grade * riskC + (grade | subid), MPLS.Sorted, REML = FALSE)
sim.3 <- lmer(read ~ grade * riskC + grade * eth2 + (grade | subid),
               MPLS.Sorted, REML = FALSE)
sim.4 <- lmer(read ~ grade * riskC + grade * eth2 + grade * X1 +(grade | subid),
              mylong, REML = FALSE)

#Extract the variances
myvar <- data.frame(rbind(attr(VarCorr(sim.1)$subid, "stddev") ^ 2,
                          attr(VarCorr(sim.2)$subid, "stddev") ^ 2,
                          attr(VarCorr(sim.3)$subid, "stddev") ^ 2,
                          attr(VarCorr(sim.4)$subid, "stddev") ^ 2))
rownames(myvar) <- c("(none)", "riskC", "riskC.eth2", "riskC.eth2.X1")

#Compute the determinant of G
myvar$det <- c(det(as.matrix(VarCorr(sim.1)$subid)),
               det(as.matrix(VarCorr(sim.2)$subid)),
               det(as.matrix(VarCorr(sim.3)$subid)),
               det(as.matrix(VarCorr(sim.4)$subid)))

#Compute R^2
myvar$rsq <- c(cor(x = sim.1@frame$read, y = FittedFE(sim.1)) ^ 2,
               cor(x = sim.2@frame$read, y = FittedFE(sim.2)) ^ 2,
               cor(x = sim.3@frame$read, y = FittedFE(sim.3)) ^ 2,
               cor(x = sim.4@frame$read, y = FittedFE(sim.4)) ^ 2)
#Deviance
myvar$deviance <- c(deviance(sim.1), deviance(sim.2), deviance(sim.3), deviance(sim.4))

#AIC
myvar$AIC <- c(AIC(sim.1), AIC(sim.2), AIC(sim.3), AIC(sim.4))

#Print results
colnames(myvar)[1:2] <- c("Var(int)", "Var(slopes)")
myvar

#11.5.1. Time Units and Variances
year <- MPLS.Sorted$grade5
month <- year * 12
day <- year * 360
head(data.frame(year, month, day), n = 4)

#Estimate models
year.out <- lmer(read ~ year + (year | subid), MPLS.Sorted, REML = TRUE)
month.out <- lmer(read ~ month + (month | subid), MPLS.Sorted, REML = TRUE)
day.out <- lmer(read ~ day + (day | subid), MPLS.Sorted, REML = TRUE)

#Concatenate and print
my.f <- rbind(summary(year.out)$coefficients[2,],
              summary(month.out)$coefficients[2,],
              summary(day.out)$coefficients[2,])
row.names(my.f) <- c("year", "month", "day")
my.f

my.v <- data.frame(rbind(attr(VarCorr(year.out)$subid, "stddev") ^ 2,
                         attr(VarCorr(month.out)$subid, "stddev") ^ 2,
                         attr(VarCorr(day.out)$subid, "stddev") ^ 2),
                   Cor = rbind(attr(VarCorr(year.out)$subid, "correlation")[1,2],
                               attr(VarCorr(month.out)$subid, "correlation")[1,2],
                               attr(VarCorr(day.out)$subid, "correlation")[1,2]))
row.names(my.v) <- c("year", "month", "day")
colnames(my.v)[1:2] <- c("Var(int)", "Var(slopes")
my.v

day.out0 <- lmer(read ~ day + (1 | subid), MPLS.Sorted)
anova(day.out0, day.out)


#11.5.2. Transforming for Standardized Change
MPLS.Sorted$read.norm <- scale(MPLS.Sorted$read, center = 206.7, scale = 15.6)
with(MPLS.Sorted, head(data.frame(subid, grade, read, read.norm)))

norm.1 <- lmer(read.norm ~ grade5 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
summary(norm.1)$coefficients
