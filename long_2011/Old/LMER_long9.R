## Chapter 9 R code from Long (2012)
### Adapted by Keith Lohse, PhD (2016)

# Installing Packages -------------------------------------------------
wants <- c("AICcmodavg", "lme4", "lmerTest", "nlme", "ggplot2","plyr", "dplyr")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

library("ggplot2");library("lme4"); library("plyr"); library("dplyr");
library("nlme"); library("AICcmodavg"); 

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
MPLS.LS$grade5 <- MPLS.LS$grade - 5
save(MPLS.LS, file="./data/MPLS.LS.Rdata")

## Estimate models.
model.i <- lmer(read ~ 1 + (1 | subid), MPLS.LS, REML = FALSE)
model.l <- lmer(read ~ grade5 + (1 | subid), MPLS.LS, REML = FALSE)
model.q <- lmer(read ~ grade5 + I(grade5 ^ 2) + (1 | subid), MPLS.LS, REML = FALSE)
mynames <- c("I", "L", "Q")
## AICc, etc.
myaicc <- as.data.frame(aictab(cand.set = list(model.i, model.l, model.q),
                               modnames = mynames))[ ,-c(5,7)]
## Evidence ratio:
myaicc$eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
## Print.
data.frame(Model = myaicc[ ,1], round(myaicc[ ,2:7], 4))

summary(model.q)$coefficients

summary(model.l)$coefficients

## Estimate Models.
model.1  <- lmer(read ~ grade5 + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.1q <- lmer(read ~ grade5 + risk2 + I(grade5 ^ 2) + (grade | subid),
                 MPLS.LS, REML = FALSE)
model.3  <- lmer(read ~ grade5 + risk2 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.3q <- lmer(read ~ grade5 + risk2 + eth2 + I(grade5 ^ 2) + (grade | subid),
                 MPLS.LS, REML = FALSE)
## AICc.
mynames <- c("1", "1q", "3", "3q")
myaicc <- as.data.frame(aictab(cand.set = list(model.1, model.1q, model.3, model.3q),
                               modnames = mynames))[ ,-c(5,7)]
myaicc$eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
myaicc

model.i <- lmer(read ~ risk2 + eth2 + (1 | subid), MPLS.LS, REML = FALSE)
model.l <- lmer(read ~ grade5 * risk2 + grade5 * eth2 + (1 | subid),
                MPLS.LS, REML = FALSE)
model.q <- lmer(read ~ grade5 * risk2 + grade5 * eth2 + I(grade5 ^ 2) * risk2 +
                I(grade5 ^ 2) * eth2 + (1 | subid), MPLS.LS, REML = FALSE)
mynames <- c("I", "L", "Q")
myaicc <- as.data.frame(aictab(cand.set = list(model.i, model.l, model.q),
                               modnames = mynames))[ ,-c(5,7)]
data.frame(Modnames = myaicc[ ,1], round(myaicc[ ,-1], 4))

print(aictab(cand.set = list(model.1, model.1q),
             modnames = c("1", "1q")), LL = FALSE)

model.i <- lmer(read ~ 1 + (1 | subid), MPLS.LS, REML = FALSE)
model.l <- lmer(read ~ grade5 + (1 | subid), MPLS.LS, REML = FALSE)
model.q <- lmer(read ~ grade5 + I(grade5 ^ 2) + (1 | subid), MPLS.LS, REML = FALSE)
anova(model.i, model.l, model.q)

MPLS.LS$miss <- as.numeric(is.na(MPLS.LS$read))
mysel <- ddply(.data = data.frame(MPLS.LS$miss),
               .variables = .(MPLS.LS$subid), .fun = sum)
colnames(mysel) <- c("subid", "totmiss")
MPLS.LS2 <- merge(MPLS.LS, mysel, by = "subid")
head(subset(MPLS.LS2, select = -c(riskC, ethC)))

mylm.1 <- dlply(.data = MPLS.LS, .variables = .(MPLS.LS$subid),
                .fun = function(x) {lm(read ~ grade5, data = x)})

ldply(.data = mylm.1, .fun = function(x) {x$coefficients})

mylm.2 <- dlply(.data = MPLS.LS, .variables = .(MPLS.LS$subid),
                .fun = function(x) {lm(read ~ grade5 + I(grade5 ^ 2), data = x)})
ldply(mylm.2, function(x) {x$coefficients})

temp1 <- ldply(.data = mylm.1, .fun = function(x) summary(x)$r.squared)
colnames(temp1) <- c("subid", "Rsq")
head(temp1)

Rsq1 <- merge(mysel, temp1, by = "subid")
Rsq1

temp2 <- ldply(.data = mylm.2, .fun = function(x) summary(x)$r.squared)
colnames(temp2) <- c("subid", "Rsq")
Rsq2 <- merge(mysel, temp2, by = "subid")
Rsq2

N <- nrow(Rsq1)        # Number of subjects.
plotdata <- data.frame(rbind(Rsq1, Rsq2), c(rep(1, N), rep(2, N)))
colnames(plotdata)[4] <- "poly"
plotdata$poly.f <- factor(plotdata$poly, labels = c("Linear", "Quadratic"))
plotdata$missing.f <- factor(plotdata$totmiss, labels = c("Complete", "Missing"))
head(plotdata)

g1 <- ggplot(plotdata, aes(x = poly.f, y = Rsq)) + geom_boxplot(fill = "grey80")
g2 <- g1 + geom_point(position = "jitter") + facet_grid(. ~ missing.f)
g3 <- g2 + theme_bw() + xlab("Polynomial") + ylab(expression(R ^ 2))
print(g3)

ddply(.data = data.frame(plotdata$Rsq),
      .variables = .(plotdata$missing.f, plotdata$poly.f),
      .fun = each(Md = median), na.rm = TRUE)

mysub <- subset(MPLS.LS2, totmiss == 0)
mylm.1 <- dlply(.data = mysub, .variables = .(mysub$subid),
                .fun = function(x) {lm(read ~ grade, data = x)})
mylm.2 <- dlply(.data = mysub, .variables = .(mysub$subid),
                .fun = function(x) {lm(read ~ grade + I(grade ^ 2), data = x)})

myfunc <- function(x) summary(x)$adj.r.squared  # Define the function.
## Linear.
adjRsq1 <- ldply(.data = mylm.1, .fun = myfunc)
colnames(adjRsq1) <- c("subid", "adjRsq")
## Quadratic.
adjRsq2 <- ldply(.data = mylm.2, .fun = myfunc)
colnames(adjRsq2) <- c("subid", "adjRsq")

N <- nrow(adjRsq1)
## Create plot data.
plotdata <- data.frame(rbind(adjRsq1, adjRsq2), c(rep(1, N), rep(2, N)))
colnames(plotdata)[3] <- "poly"
plotdata$poly.f <- factor(plotdata$poly, labels = c("Linear", "Quadratic"))
## ggplot().
g1 <- ggplot(plotdata, aes(x = poly.f, y = adjRsq)) + geom_boxplot(fill = "grey80")
g2 <- g1 + geom_point(position = "jitter")
g3 <- g2 + theme_bw() + xlab("") + ylab(expression(bar(R)^2))
print(g3)

## Estimate coefficients.
my1 <- dlply(MPLS.LS, .(MPLS.LS$subid), function(x) lm(read ~ grade, data = x))
my2 <- dlply(MPLS.LS, .(MPLS.LS$subid), function(x) lm(read ~ grade + I(grade ^ 2),
                                                       data = x))
## SSE.
sse1 <- sum(ldply(my1, function(x) sum(resid(x) ^ 2))[, 2])
sse2 <- sum(ldply(my2, function(x) sum(resid(x) ^ 2))[, 2])
## Residual df.
df1 <- sum(ldply(my1, function(x) x$df.residual)[, 2])
df2 <- sum(ldply(my2, function(x) x$df.residual)[, 2])
## Compute RSE meta.
RSEmeta.1 <- sqrt(sse1/df1)
RSEmeta.2 <- sqrt(sse2/df2)
RSEmeta.1
RSEmeta.2

########################################################################
mylm.1 <- dlply(MPLS.LS, .(MPLS.LS$subid), function(x) lm(read ~ grade, data = x))
mylm.2 <- dlply(MPLS.LS, .(MPLS.LS$subid), function(x) lm(read ~ grade + I(grade ^ 2),
                                                          data = x))
lm.objects <- c("mylm.1", "mylm.2")
########################################################################
Rsqmeta <- numeric(length(lm.objects))
adjRsqmeta <- numeric(length(lm.objects))
for(i in 1:length(lm.objects)){
    ## Define functions.
    myfunc1 <- function(x) sum((x$model[ ,1] - mean(x$model[ ,1])) ^ 2) # SST.
    myfunc2 <- function(x) sum(resid(x) ^ 2)                            # SSR.
    myfunc3 <- function(x) length(x$model[ ,1]) - 1                     # df Total.
    myfunc4 <- function(x) x$df.residual                                # df Resid.
    ## SST.
    SSTotal <- ldply(eval(parse(text = lm.objects[i])), myfunc1)
    ## SSE.
    SSResid <- ldply(eval(parse(text = lm.objects[i])), myfunc2)
    ## Rsq-meta.
    Rsqmeta[i] <- 1 - (sum(SSResid[ ,2]) / sum(SSTotal[ ,2]))
    ## Degrees of freedom.
    dfTotal <- sum(ldply(eval(parse(text = lm.objects[i])), myfunc3)[ ,2])
    dfResid <- sum(ldply(eval(parse(text = lm.objects[i])), myfunc4)[ ,2])
    ## Adjusted Rsq-meta.
    adjRsqmeta[i] <- 1 - ((sum(SSResid) / dfResid) / (sum(SSTotal) / dfTotal))
}

Rsq.meta    <- data.frame(lm.objects, Rsqmeta)
adjRsq.meta <- data.frame(lm.objects, adjRsqmeta)

Rsq.meta
adjRsq.meta

temp <- subset(MPLS.LS, select = c(subid, read, grade))
read.wide <- reshape(temp, v.names = "read", timevar = "grade",
                     idvar = "subid", direction = "wide")
read.wide

require(kml)
mycld <- as.cld(read.wide, timeReal = 5:8)

kml(Object = mycld, print.cal = TRUE, print.traj = TRUE)

par(mfrow = c(1, 1), pty = "s")
plotCriterion(mycld)
mtext("Cluster Number (k)", side = 1, line = 3)   # Set label in bottom margin.
mtext("CHC", side = 2, line = 3)                  # Set label in left margin.

par(mfrow = c(2,2), pty = "s")                        # 2 x 2 matrix, square graphs.
plot(mycld, y = 2, ylab = "Response", main = "k = 2") # Optional labeling included.
plot(mycld, y = 3, ylab = "Response", main = "k = 3")
plot(mycld, y = 4, ylab = "Response", main = "k = 4")
plot(mycld, y = 5, ylab = "Response", main = "k = 5")
par(mfrow = c(1,1), pty = "m")                        # Default settings.

