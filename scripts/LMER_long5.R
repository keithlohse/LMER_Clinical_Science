## Chapter 5 R code from Long (2012)
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
## Chapter 5 R code.

require(ggplot2)  

## Tailor to your system:
## load(file="C:\\Mine\\MPLS.LS.Rdata")
head(MPLS.LS)
tail(MPLS.LS)

lm.1 <- lm(read ~ 1 + grade, data = MPLS.LS)
summary(lm.1)

## options(scipen = 999) ## Optional to turn off scientific notation. 

plotdata <- data.frame(lm.1$model, fitted = fitted(lm.1))
head(plotdata)

## Set some defaults.
theme_set(theme_bw())
myX <- scale_x_continuous(breaks = 5:8, name = "Grade")
myY <- scale_y_continuous(name = "Reading")
## Use ggplot().
g1 <- ggplot(data = plotdata, aes(x = grade, y = read)) + geom_point(shape = 1)
g2 <- g1 + geom_line(aes(x = grade, y = fitted)) + myX + myY
print(g2)

lm.2 <- lm(read ~ 1 + grade + risk2, data = MPLS.LS)
summary(lm.2)

plotdata <- data.frame(lm.2$model, fitted = fitted(lm.2))
g1 <- ggplot(data = plotdata, aes(x = grade, y = read, shape = risk2)) + geom_point()
g2 <- g1 + geom_line(aes(x = grade, y = fitted, linetype = risk2))
g3 <- g2 + opts(legend.position = c(.8,.3), "aspect.ratio" = 1)
g4 <- g3 + scale_shape(solid = F) + myX + myY
print(g4)

lm.3 <- lm(read ~ 1 + grade * risk2, data = MPLS.LS)
summary(lm.3)

plotdata <- data.frame(lm.3$model, fitted = fitted(lm.3))
g1 <- ggplot(data = plotdata, aes(x = grade, y = read, shape = risk2)) + geom_point()
g2 <- g1 + geom_line(aes(x = grade, y = fitted, linetype = risk2))
g3 <- g2 + opts(legend.position = c(.8,.3), "aspect.ratio" = 1)
g4 <- g3 + scale_shape(solid = F) + myX + myY
print(g4)

head(MPLS.LS, n = 8)

require(lme4)
lmer.1 <- lmer(read ~ 1 + grade + (1 + grade | subid), data = MPLS.LS, REML = FALSE)
summary(lmer.1)

print("-- LM table --", quote = F)
round(summary(lm.1)$coefficients,4)
print("-- LMER table -- ", quote = F)
round(summary(lmer.1)@coefs, 4)

## Intercept-only model.
lmer.0 <- lmer(read ~ 1 + (1 | subid), data = MPLS.LS, REML = FALSE)
## Linear model.
lmer.1 <- lmer(read ~ 1 + grade + (1 | subid), data = MPLS.LS, REML = FALSE)

se2.0 <- round(summary(lmer.0)@sigma ^ 2, 2)
se2.1 <- round(summary(lmer.1)@sigma ^ 2, 2)
## Printing.
myse2 <- data.frame(Intercept = se2.0, Linear = se2.1)
rownames(myse2) <- "Est. Sigma2"
myse2

## Estimation.
lmer.1a <- lmer(read ~ I(grade - 5) + (1 + I(grade - 5) | subid), MPLS.LS, REML = FALSE)
lmer.1b <- lmer(read ~ I(grade - 8) + (1 + I(grade - 8) | subid), MPLS.LS, REML = FALSE)
## Print out coefficients.
data.frame(grade = fixef(lmer.1), grade5 = fixef(lmer.1a), grade8 = fixef(lmer.1b))

lmer.2 <- lmer(read ~ 1 + grade + risk2 + (1 + grade | subid),
               data = MPLS.LS, REML = FALSE)
print(lmer.2, cor = FALSE)

lmer.3 <- lmer(read ~ 1 + grade + risk2 + risk2 : grade + (1 + grade | subid),
               data = MPLS.LS, REML = FALSE)
print(lmer.3, cor = FALSE)

grade5 <- subset(MPLS.LS, grade == 5, select = c(subid, read))
colnames(grade5)[2] <- "read.int"
grade6to8 <- subset(MPLS.LS, grade != 5)
grade6to8a <- merge(grade5, grade6to8, by = "subid")
head(grade6to8a)

head(model.matrix(lmer.1))
head(model.matrix(lmer.3))

## Estimate LMER model.
lmer.1 <- lmer(read ~ grade + (grade | subid), MPLS.LS, REML = FALSE)
## Random effects design matrix for first person.
Z <- model.matrix(lmer.1)[1:4, ]
Z

G <- VarCorr(lmer.1)$subid[1:2,1:2]
G

## Extract and save error variance.
sigma2 <- summary(lmer.1)@sigma ^ 2
## Create 4 x 4 Identity matrix.
Ident <- diag(4)
# Compute W.
W <- sigma2 * Ident
W

B <- Z %*% G %*% t(Z)
B

V <- B + W
V

## Create diagonal matrix, D.
D  <- diag(1 / sqrt(diag(V)))
## Compute Vstar.
Vstar <- D %*% V %*% D
Vstar
