##Long Chapter 10: Selecting Random Effects

library(tidyverse)
library(plyr)
library(lme4)
library(lattice)
library(AICcmodavg)
library(dplyr)
library(kml)

load(file = "C:/Users/miller.allison/Box/Allison Miller/Course Work and Training/Longitudinal Data Analysis in R_Fall 2022/MPLS.Sorted.Rdata")

MPLS.Sorted$grade5 <- MPLS.Sorted$grade - 5

#10.2.1. Restricted Maximum Likelihood
reml.out <- lmer(read ~ grade5 + (grade5 | subid), MPLS.Sorted)
ml.out <- lmer(read ~ grade5 + (grade5 | subid), MPLS.Sorted, REML = FALSE)

summary(reml.out)
summary(ml.out)

VarCorr(reml.out)

attr(VarCorr(reml.out)$subid, "correlation")

print("---REML---", quote = FALSE); as.data.frame(VarCorr(reml.out)$subid)
print("---ML---", quote = FALSE); as.data.frame(VarCorr(ml.out)$subid)

#10.2.2. Random Effects and Correlated Data
#No Random Effects
s0 <- summary(lm(read ~ grade5, data = MPLS.Sorted))$sigma  #Estimated error SD
V0 <- diag(s0 ^ 2, nrow = 4, ncol = 4)  #4 by 4 diagonal matrix
round(V0, 2)  #Round results

Vstar.func <- function(x) diag(1 / sqrt(diag(x))) %*% x %*% diag(1 / sqrt(diag(x)))
Vstar0 <- Vstar.func(V0)
round(Vstar0, 2)

#Random Intercepts
linear.1 <- lmer(read ~ grade5 + (1 | subid), data = MPLS.Sorted)
G1 <- matrix(as.numeric(VarCorr(linear.1)$subid), ncol = 1)
s1 <- attr(VarCorr(linear.1), "sc")
G1; s1

V.func <- function(w, x, y, z){
  nRE <- nrow(as.data.frame(VarCorr(z))) - 1  #Count random effects
  Z <- matrix(model.matrix(z)[1:4, 1:nRE], ncol = nRE)  #Create Z matrix
  R <- diag(w) * y ^ 2  #Create R matrix
  V <- Z %*% x %*% t(Z) + R  #Colmpute V
}

V1 <- V.func(4, G1, s1, linear.1)
round(V1, 2)

Vstar1 <- Vstar.func(V1)
round(Vstar1, 2)

#Two Uncorrelated Random Effects
linear.2 <- lmer(read ~ grade5 + (1 | subid) + (0 + grade5 | subid), MPLS.Sorted)

as.data.frame(VarCorr(linear.2))

G2 <- diag(as.numeric(VarCorr(linear.2)))
s2 <- attr(VarCorr(linear.2), "sc")
G2; s2

V2 <- V.func(4, G2, s2, linear.2)
round(V2, 2)

Vstar2 <- Vstar.func(V2)
round(Vstar2, 2)

#Two Correlated Random Effects
linear.3 <- lmer(read ~ grade5 + (grade5 | subid), data = MPLS.Sorted)
G3 <- matrix(as.numeric(VarCorr(linear.3)$subid), ncol = 2)
s3 <- attr(VarCorr(linear.3), "sc")
G3; s3

V3 <- V.func(4, G3, s3, linear.3)
round(V3, 2)

Vstar3 <- Vstar.func(V3)
round(Vstar3, 2)

#Three Uncorrelated Random Effects
quad.4 <- lmer(read ~ grade5 + I(grade5 ^ 2) + (grade5 + I(grade5 ^ 2) | subid),
               data = MPLS.Sorted)
G4 <- matrix(as.numeric(VarCorr(quad.4)$subid), ncol = 3)
s4 <- attr(VarCorr(quad.4), "sc")
G4; s4

attr(VarCorr(quad.4)$subid, "correlation")

V4 <- V.func(4, G4, s4, quad.4)
round(V4, 2)

Vstar4 <- Vstar.func(V4)
round(Vstar4, 2)


#10.3.1. OLS Estimates
my.ols <- dlply(.data = MPLS.Sorted, .variables = .(MPLS.Sorted$subid),
                .fun = function(x) {lm(read ~ grade5, data = x)})
my.coefs <- ldply(my.ols, function(x) coef(x))
my.coefs

cov(my.coefs[ ,-1])

plotdata <- data.frame(coef = c(my.coefs[,2], my.coefs[,3]),
                       label = c(rep("Intercept", nrow(my.coefs)),
                                 rep("Slope", nrow(my.coefs))),
                       ph = rep(" ", nrow(my.coefs)))
head(plotdata)

g1 <- ggplot(plotdata, aes(x = ph, y = coef)) + geom_boxplot() + theme_bw()
g2 <- g1 + facet_grid(label ~ ., scales = "free") + xlab(" ") + ylab("Scale")
print(g2)

cor(my.coefs[ ,-1])

g1 <- ggplot(my.coefs, aes(x = my.coefs[ ,2], y = my.coefs[ ,3])) + geom_point()
g2 <- g1 + stat_smooth(method = "lm", se = FALSE)
g3 <- g2 + theme_bw() + xlab("Intercept") + ylab("Slope")
print(g3)

#Correlations with Static Predictors
inter.r2 <- summary(lm(my.coefs[ ,2] ~ MPLS.Sorted$riskC[MPLS.Sorted$grade ==5]))$r.squared
slope.r2 <- summary(lm(my.coefs[ ,3] ~ MPLS.Sorted$riskC[MPLS.Sorted$grade ==5]))$r.squared
round(inter.r2, 2) ; round(slope.r2, 2)


#10.3.2. Examining Residuals
#Estimate the models (assessing the need for a random intercept)
lm.1 <- lm(read ~ grade5, MPLS.Sorted)
lmer.1 <- lmer(read ~ grade5 + (1 | subid), MPLS.Sorted)

#Residual Variances
lm.s2 <- summary(lm.1)$sigma ^ 2
lmer.s2 <- attr(VarCorr(lmer.1), "sc")
names(lmer.s2) <- ""
data.frame(lm.s2, lmer.s2, row.names = "")
lm.s2 ; lmer.s2

#PRE measure
PRE <- (lm.s2 - lmer.s2)/ lm.s2
PRE

#Plotting residuals
my.resid <- data.frame(subid = lmer.1@frame$subid,
                       lm.resid = resid(lm.1),
                       lmer.resid = resid(lmer.1),
                       lm.1 = I("LM"), lmer.1 = I("LMER"))
head(my.resid)

plotdata <- with(my.resid, data.frame(subid = c(subid, subid),
                                      resid = c(lm.resid, lmer.resid),
                                      model = c(lm.1, lmer.1)))
head(plotdata)

g1 <- ggplot(data = plotdata, aes(x = subid, y = resid, group = subid)) + theme_bw()
g2 <- g1 + geom_boxplot(fill = "grey80") + coord_flip() + geom_hline(yintercept = 0)
g3 <- g2 + facet_grid(. ~ model) + ylab("Residual") + xlab("Subject") + ylim(-50, 50)
print(g3)

#Estimate the models (assessing the need for a random slope)
lmer.2 <- lmer(read ~ grade5 + (1 + grade5 | subid), MPLS.Sorted)
lmer.1.s2 <- lmer.s2
lmer.2.s2 <- attr(VarCorr(lmer.2), "sc")
names(lmer.2.s2) <- ""
PRE <- (lmer.1.s2 - lmer.2.s2) / lmer.1.s2
PRE

#Plotting residuals
plotdata <- data.frame(subid = c(lmer.2@frame$subid, lmer.2@frame$subid),
                       resid = c(resid(lmer.1), resid(lmer.2)),
                       model = c(rep(1, nrow(lmer.2@frame)), rep(2, nrow(lmer.2@frame))))
plotdata$model <- factor(plotdata$model, labels = c("Reduced", "Full"))
head(plotdata)

g1 <- ggplot(data = plotdata, aes(x = subid, y = resid, group = subid)) + theme_bw()
g2 <- g1 + geom_boxplot(fill = "grey80") + coord_flip() + geom_hline(yintercept = 0)
g3 <- g2 + facet_grid(. ~ model) + ylab("Residual") + xlab("Subject") + ylim(-50, 50)
print(g3)


#10.3.3. Residuals and Normality.
lmer.2 <- lmer(read ~ grade5 + (1 + grade5 | subid), MPLS.Sorted)
myresid <- data.frame(resid = resid(lmer.2))
myrand <- data.frame(resid = rnorm(n = nrow(myresid), mean = 0, sd = sd(myresid)))
plotdata <- data.frame(resid = rbind(myresid, myrand),
                       type = c(rep("Sample", nrow(myresid)),
                                rep("Generated", nrow(myresid))))
g1 <- ggplot(plotdata, aes(sample = resid)) + geom_point(stat = "qq")
g2 <- g1 + facet_grid(. ~ type) + theme_bw()
print(g2)


#10.4.1. Likelihood Ratio Test
#Testing the variance of the intercepts
library(RLRsim)
exactRLRT(linear.1)

#Testing with Uncorrelated Random Effects
full <- lmer(read ~ grade5 + (1 | subid) + (0 + grade5 | subid), MPLS.Sorted)
reduced.int <- lmer(read ~ grade5 + (1 | subid), MPLS.Sorted)
reduced.slope <- lmer(read ~ grade5 + (0 + grade5 | subid), MPLS.Sorted)

exactRLRT(reduced.slope, full, reduced.int)

reduced <- reduced.int
anova(reduced, full)

#Slow bootstrap approach
slow.b <- function(x, y, z) {
  chisq.star <- numeric(x)  #Storage vector
  for(i in 1:x){  #Loop for bootstrap
    simDV <- simulate(y)  #Simulate on reduced model
    full.s <- refit(z, simDV[ ,1])  #Refit full model
    reduced.s <- refit(reduced, simDV[ ,1])  #Refit reduced model
    chisq.star[i] <- -2*(logLik(reduced.s) - logLik(full.s))  #Store the LR statistic
  }
  mean(anova(y, z)[2,5] < chisq.star)  #p value
}

slow.b(99, reduced, full)

#Testing with Correlated Random Effects.
attr(VarCorr(linear.3)$subid, "correlation")

full <- linear.3
cor.re <- slow.b(999, reduced, full)
cor.re

anova(reduced, full)

#Testing the Covariance
reduced <- lmer(read ~ grade5 + (1 | subid) + (0 + grade5 | subid), data = MPLS.Sorted)
full <- lmer(read ~ grade5 + (grade5 | subid), data = MPLS.Sorted)
anova(reduced, full)

cov.test <- slow.b(99, reduced, full)
cov.test

#10.4.2. AICc
model.1 <- lmer(read ~ grade5 + (1 | subid), data = MPLS.Sorted)
model.2 <- lmer(read ~ grade5 + (1 | subid) + (0 + grade5 | subid), data = MPLS.Sorted)
model.3 <- lmer(read ~ grade5 + (grade5 | subid), data = MPLS.Sorted)
print(aictab(cand.set = list(model.1, model.2, model.3),
             modnames = c("M1", "M2", "M3")), LL = FALSE)


#10.5. Variance Components and Static Predictors.
full <- lmer(read ~ grade5 + riskC + eth2 + (1 | subid) + (0 + grade5 | subid), MPLS.Sorted)
reduced.int <- lmer(read ~ grade5 + riskC + eth2 + (1 | subid), MPLS.Sorted)
reduced.slope <- lmer(read ~ grade5 + riskC + eth2 + (0 + grade5 | subid), MPLS.Sorted)
exactRLRT(reduced.slope, full, reduced.int)


#10.6. Predicted Random Effects.
my.lmer <- lmer(read ~ grade5 + (grade5 | subid), MPLS.Sorted)
my.c.re <- ldply(ranef(my.lmer))[-1]
colnames(my.c.re) <- c("b0i", "b1i")
my.c.re
round(mean(my.c.re), 8)

#10.6.1. Evaluating the Normality Assumption.
#Density plot b0i
d1 <- ggplot(my.c.re, aes(x = b0i)) + geom_density(fill = "grey80")
d2 <- d1 + theme_bw() + theme("aspect.ratio" = 1)

#Boxplot b0i
b1 <- ggplot(my.c.re, aes(x = "b0", y = b0i)) + geom_boxplot()
b2 <- b1 + theme_bw() + theme("aspect.ratio" = 1)

#Q-Q plot b0
q1 <- ggplot(my.c.re, aes(sample = b0i)) + geom_point(stat = "qq")
q2 <- q1 + theme_bw() + theme("aspect.ratio" = 1)

#Density plot b1
dd1 <- ggplot(my.c.re, aes(x = b1i)) + geom_density(fill = "grey80")
dd2 <- dd1 + theme_bw() + theme("aspect.ratio" = 1)

#Boxplot b1
bb1 <- ggplot(my.c.re, aes(x = "b1", y = b1i)) + geom_boxplot()
bb2 <- bb1 + theme_bw() + theme("aspect.ratio" = 1)

#Q-Q plot b1
qq1 <- ggplot(my.c.re, aes(sample = b1i)) + geom_point(stat = "qq")
qq2 <- qq1 + theme_bw() + theme("aspect.ratio" = 1)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 3)))
print(d2, vp = vplayout(1, 1))
print(b2, vp = vplayout(1, 2))
print(q2, vp = vplayout(1, 3))
print(dd2, vp = vplayout(2, 1))
print(bb2, vp = vplayout(2, 2))
print(qq2, vp = vplayout(2, 3))

sim.data <- data.frame(sim.b0i = rnorm(n = 22, mean = 0, sd = sd(my.c.re[ , 1])),
                       sim.b1i = rnorm(n = 22, mean = 0, sd = sd(my.c.re[ , 2])))


#10.6.2. Predicted Values for an Individual
my.beta <- data.frame(matrix(fixef(my.lmer), nrow = nrow(my.c.re),
                             ncol = ncol(my.c.re), byrow = TRUE))
colnames(my.beta) <- c("B0", "B1")
head(my.beta)

my.un.re <- my.beta + my.c.re
colnames(my.un.re) <- c("B0i", "B1i")
my.un.re

#Fit LMER model using REML
lmer.1 <- lmer(read ~ grade5 + (grade5 | subid), MPLS.Sorted)

#Create data frame 1
plotdata <- data.frame(lmer.1@frame, fitted.re = fitted(lmer.1))
head(plotdata)

#Create data frame 2
fixed <- data.frame(fixef(lmer.1))
fixed

#ggplot
g1 <- ggplot(plotdata, aes(x = grade5, y = read)) + geom_point()
#Facet
g2 <- g1 + facet_wrap(~subid, nrow = 2)
#Individual fitted curve
g3 <- g2 + geom_line(aes(y = fitted.re), linetype = 2) + scale_x_continuous(breaks = 0:3)
#Group fitted curve
g4 <- g3 + geom_abline(intercept = fixed[1,1], slope = fixed[2,1]) + theme_bw()
print(g4)


g1 <- ggplot(plotdata, aes(x = grade5, y = read)) + geom_point()
g2 <- g1 + facet_wrap(~ subid, nrow = 2)
#EBLUP lines
g3 <- g2 + geom_line(aes(y = fitted.re), linetype = 2) + scale_x_continuous(breaks = 0:3)
#OLS lines
g4 <- g3 + stat_smooth(method = "lm", se = F) + theme_bw()
print(g4)
