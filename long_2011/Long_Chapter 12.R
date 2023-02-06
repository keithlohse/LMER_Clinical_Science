##Long Chapter 12: Modeling Nonlinear Change

library(tidyverse)
library(plyr)
library(lme4)
library(lattice)
library(AICcmodavg)
library(dplyr)
library(kml)
library(grid)

#12.1. Data Set and Analysis Strategy
MPLS2.W <- read.table("C:/Users/miller.allison/Box/Allison Miller/Course Work and Training/Longitudinal Data Analysis in R_Fall 2022/MPLS2.W.txt", header = TRUE, na.strings = "-9")


#Read in data, grades 5-8
MPLS.W <- read.table("C:/Users/miller.allison/Box/Allison Miller/Course Work and Training/Longitudinal Data Analysis in R_Fall 2022/MPLS.txt", header = TRUE, na.strings = "-9")

#Define risk2
MPLS.W$riskC[MPLS.W$risk == "HHM"] <- "DADV"
MPLS.W$riskC[MPLS.W$risk == "POV"] <- "DADV"
MPLS.W$riskC[MPLS.W$risk == "ADV"] <- "ADV"
MPLS.W$risk2 <- factor(MPLS.W$riskC)

#Merge
MPLS2.WM <- merge(MPLS2.W, subset(MPLS.W, select = c(subid, risk2)), by = "subid")
head(MPLS2.WM)

#Reshape to long format
MPLS2.L <- reshape(MPLS2.WM, varying = 2:8, times = 2:8, idvar = "subid",
                   timevar = "grade", direction = "long")
MPLS2.LS <- MPLS2.L[order(MPLS2.L$subid), ]
head(MPLS2.LS, n = 14)

#Set options for all graphs
theme_set(theme_bw())
myx <- scale_x_continuous(breaks = 2:8)

#ggplot
g1 <- ggplot(MPLS2.LS, aes(x = grade, y = read, group = subid)) + myx
g2 <- g1 + geom_line(colour = "grey80") + facet_grid(. ~ risk2, margins = TRUE)
g3 <- g2 + stat_summary(aes(group = 1), fun = mean, geom = "point", size = 3.5)
g4 <- g3 + stat_summary(aes(group = 2), fun = mean, geom = "line", lwd = 1.5)
print(g4)

#Construct individual change curves with faceting by subject
#Create data frames
DADV <- subset(MPLS2.LS, risk2 == "DADV")
ADV <- subset(MPLS2.LS, risk2 == "ADV")

#DADV group
d1 <- ggplot(DADV, aes(x = grade, y = read)) + geom_line()
d2 <- d1 + facet_wrap( ~ subid) + labs(title = "DADV") + ylim(140, 240)

#ADV group
a1 <- ggplot(ADV, aes(x = grade, y = read)) + geom_line()
a2 <- a1 + facet_wrap( ~ subid) + labs(title = "ADV") + ylim(140, 240)

#Print graphs to one page
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(d2, vp = vplayout(1, 1))
print(a2, vp = vplayout(1, 2))


#12.3. Polynomials.
quad <- expression(B0 + B1 * grade + B2 * grade ^ 2)
dey <- D(quad, "grade")
dey

#Estimate model and extract fixed effects
myfe <- fixef(lmer(read ~ grade + I(grade ^ 2) + (grade | subid), MPLS2.LS, REML = FALSE))

#Assign values to B0, B1, B2
for(i in 1:3){assign(paste("B", i - 1, sep = ""), myfe[i])}
print(c(B0, B1, B2))

#Compute predicted values and derivatives
grade <- 2:8
myd <- data.frame(grade, fitted = eval(quad), dey = eval(dey))
myd

#Intercept of the tangent line
myd$int <- myd$fitted - myd$dey * myd$grade

#Construct separate graphs
for(i in c(3, 5, 7)){
  df <- data.frame(a = myd$int[i], b = myd$dey[i],
                   y1 = myd$fitted[i], x1 = myd$grade[i])
  assign(paste("g", i, sep = ""),
         ggplot(data = myd, aes(x = grade, y = fitted)) + geom_line() +
           geom_abline(aes(intercept = a, slope = b), data = df, linetype = 2) +
           geom_point(aes(x = x1, y = y1), data = df, size = 3) +
           ylab("read") + theme(aspect.ratio = 1))
}

#Print graphs to one page
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(g3, vp = vplayout(1, 1))
print(g5, vp = vplayout(1, 2))
print(g7, vp = vplayout(1, 3))

#Derivative of the cubic polynomial
cubic <- expression(B0 + B1 * grade + B2 * grade ^2 + B3 * grade ^ 3)
dey <- D(cubic, "grade")
dey

#Compute derivative for cubic polynomial for subject 1
myfe <- coef(lm(read ~ grade + I(grade ^ 2) + I(grade ^ 3),
                MPLS2.LS, subset = (subid ==1)))
for(i in 1:4){assign(paste("B", i - 1, sep = ""), myfe[i])}
print(c(B0, B1, B2, B3))

#Compute fitted values and derivatives
grade <- 2:8
myd <- data.frame(grade, fitted = eval(cubic), dey = eval(dey))
myd

#Intercept of the tangent line
myd$int <- myd$fitted - myd$dey * myd$grade

#Construct separate graphs
for(i in c(3, 5, 7)){
  df <- data.frame(a = myd$int[i], b = myd$dey[i],
                   y1 = myd$fitted[i], x1 = myd$grade[i])
  assign(paste("g", i, sep = ""),
         ggplot(data = myd, aes(x = grade, y = fitted)) + geom_line() +
           geom_abline(aes(intercept = a, slope = b), data = df, linetype = 2) +
           geom_point(aes(x = x1, y = y1), data = df, size = 3) +
           ylab("read") + theme(aspect.ratio = 1))
}

#Print graphs to one page
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(g3, vp = vplayout(1, 1))
print(g5, vp = vplayout(1, 2))
print(g7, vp = vplayout(1, 3))


#12.3.3. The poly() function
poly.ex <- subset(MPLS2.LS, subid < 2, select = -c(read, risk2))
poly.ex$rawp <- poly(poly.ex$grade, 3, raw = TRUE)
poly.ex$mcor <- poly(poly.ex$grade - mean(poly.ex$grade), 3, raw = TRUE)
poly.ex$orth <- poly(poly.ex$grade, 3)
round(poly.ex[ , -1], 4)

print("-- Raw --", quote = FALSE); round(cor(poly.ex$rawp), 2)
print("-- Mean-Corrected --", quote = FALSE); round(cor(poly.ex$mcor), 2)
print("-- Orthogonal --", quote = FALSE); round(cor(poly.ex$orth), 2)

#Create polynomials
MPLS2.LS$rp <- poly(MPLS2.LS$grade - 2, degree = 2, raw = TRUE)
MPLS2.LS$mc <- poly(MPLS2.LS$grade - mean(MPLS2.LS$grade), degree = 2, raw = TRUE)
MPLS2.LS$op <- poly(MPLS2.LS$grade, degree = 2)

#Estimate models
poly.rawp <- lmer(read ~ rp + (rp[ ,1] | subid), MPLS2.LS, REML = FALSE)
poly.mcor <- lmer(read ~ mc + (mc[ ,1] | subid), MPLS2.LS, REML = FALSE)
poly.orth <- lmer(read ~ op + (op[ ,1] | subid), MPLS2.LS, REML = FALSE)

#Print fixed effects
print("-- Raw--", quote = FALSE); round(summary(poly.rawp)$coefficients, 4)
print("-- Mean-Corrected--", quote = FALSE); round(summary(poly.mcor)$coefficients, 4)
print("-- Orthogonal--", quote = FALSE); round(summary(poly.orth)$coefficients, 4)

print("--Raw--", quote = FALSE); print(VarCorr(poly.rawp), comp = c("Variance", "Std.Dev"))
print("--Mean-Corrected--", quote = FALSE); print(VarCorr(poly.mcor), comp = c("Variance", "Std.Dev"))
print("--Orthogonal--", quote = FALSE); print(VarCorr(poly.orth), comp = c("Variance", "Std.Dev"))


#12.3.4. Polynomial Example
#Construct orthogonal polynomials
op1 <- poly(MPLS2.LS$grade, 1)
op2 <- poly(MPLS2.LS$grade, 2)
op3 <- poly(MPLS2.LS$grade, 3)

#Estimate the models
l.out <- lmer(read ~ op1 + (op1 | subid), MPLS2.LS, REML = FALSE)
q.out <- lmer(read ~ op2 + (op2[ ,1] | subid), MPLS2.LS, REML = FALSE)
c.out <- lmer(read ~ op3 + (op3[ ,1] | subid), MPLS2.LS, REML = FALSE)

#Model fit
print(aictab(list(l.out, q.out, c.out), c("linear", "quadratic", "cubic")), LL = FALSE)

#LRT
anova(l.out, q.out, c.out)


#12.5. Trigonometric Functions.
MPLS2.LS$c.grade <- {2 * pi * (MPLS2.LS$grade - min(MPLS2.LS$grade))/
    max(MPLS2.LS$grade - min(MPLS2.LS$grade))}
head(data.frame(subid = MPLS2.LS$subid, grade = MPLS2.LS$grade,
                c.grade = MPLS2.LS$c.grade), n = 7)

cos <- expression(B0 + B1 * c.grade + B2 * cos(c.grade))
dey <- D(cos, "c.grade")
dey

subid13 <- with(MPLS2.LS[MPLS2.LS$subid == 13, ], data.frame(read, grade, c.grade))

#Estimate linear-cosine model
lm.cos <- lm(read ~ c.grade + I(cos(c.grade)), subid13)

#Create data frame for graphing
plotdata <- data.frame(read = subid13$read, grade = subid13$grade, fitted = fitted(lm.cos))

#Linear cosine graph
c1 <- ggplot(plotdata, aes(x = grade, y = read)) + geom_point()
c2 <- c1 + geom_line(aes(y = fitted)) + theme(aspect.ratio = 1)

#Quadratic polynomial graph
q1 <- ggplot(plotdata, aes(x = grade, y = read)) + geom_point() + theme(aspect.ratio = 1)
q2 <- q1 + stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE)

#Print to one page
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(c2, vp = vplayout(1, 1))
print(q2, vp = vplayout(1, 2))

quad <- lm(read ~ poly(grade, 2), subid13)
cosine <- lm(read ~ c.grade + I(cos(c.grade)), subid13)
sine <- lm(read ~ c.grade + I(sin(c.grade)), subid13)
print(aictab(list(quad, cosine, sine), c("quad", "cosine", "sine")), LL = FALSE)

quad <- lmer(read ~ poly(grade, 2) + (poly(grade, 1) | subid), MPLS2.LS, REML = FALSE)
lincos <- lmer(read ~ c.grade + I(cos(c.grade)) + (c.grade | subid), MPLS2.LS, REML = FALSE)
print(aictab(list(quad, lincos), c("Quad", "Linear Cosine")), LL = FALSE)

#Estimate "dummy" model.
lmer.d <- lmer(read ~ grade + (grade | subid), MPLS2.LS, REML = FALSE)

#Create plotting data set
plotdata <- data.frame(grade = lmer.d@frame$grade, read = lmer.d@frame$read,
                       q.pred = model.matrix(quad) %*% fixef(quad),
                       c.pred = model.matrix(lincos) %*% fixef(lincos))

#ggplot
c1 <- ggplot(plotdata, aes(x = grade, y = read)) + geom_point(colour = "grey60")
c2 <- c1 + stat_summary(fun = mean, geom = "point", size = 3) + theme(aspect.ratio = 1)
c3 <- c2 + stat_summary(fun = mean, geom = "line", lwd = 1.5, aes(y = c.pred))
c4 <- c3 + labs(title = "linear-cosine")
##
q1 <- ggplot(plotdata, aes(x = grade, y = read)) + geom_point(colour = "grey60")
q2 <- q1 + stat_summary(fun = mean, geom = "point", size = 3) + theme(aspect.ratio = 1)
q3 <- q2 + stat_summary(fun = mean, geom = "line", lwd = 1.5, aes(y = q.pred))
q4 <- q3 + labs(title = "quadratic")

#Print graphs
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(c4, vp = vplayout(1, 1))
print(q4, vp = vplayout(1, 2))


#12.6.1.1. Fixed Effects Interpretation
D(expression(B0 + B1 * grade), "grade")
D(expression(B0 + B1 * grade ^ -1), "grade")
D(expression(B0 + B1 * log(grade)), "grade")


#12.6.1.2. Selection of FPS.
#Quadratic model
quad <- lmer(read ~ poly(grade, 2, raw = TRUE) + (grade | subid),
             MPLS2.LS, REML = FALSE)
#1st Order FP Models.
n3 <- lmer(read ~ I(grade ^ -3) + (I(grade ^ -3) | subid), MPLS2.LS, REML = FALSE)
n2 <- lmer(read ~ I(grade ^ -2) + (I(grade ^ -2) | subid), MPLS2.LS, REML = FALSE)
n1 <- lmer(read ~ I(grade ^ -1) + (I(grade ^ -1) | subid), MPLS2.LS, REML = FALSE)
n05 <- lmer(read ~ I(grade ^ -.5) + (I(grade ^ -.5) | subid), MPLS2.LS, REML = FALSE)
ze <- lmer(read ~ I(log(grade)) + (I(log(grade)) | subid), MPLS2.LS, REML = FALSE)
p05 <- lmer(read ~ I(grade ^ .5) + (I(grade ^ .5) | subid), MPLS2.LS, REML = FALSE)
p1 <- lmer(read ~ grade + (grade | subid), MPLS2.LS, REML = FALSE)
p2 <- lmer(read ~ I(grade ^ 2) + (I(grade ^ 2) | subid), MPLS2.LS, REML = FALSE)
p3 <- lmer(read ~ I(grade ^ 3) + (I(grade ^ 3) | subid), MPLS2.LS, REML = FALSE)

#Fit
mymods <- list(n3, n2, n1, n05, ze, p05, p1, p2, p3, quad)
mynames <- c("-3", "-2", "-1", "-.5", "0", ".5", "1", "2", "3", "Quad")
print(aictab(mymods, mynames), LL = FALSE)

#Graph data
graphdata <- data.frame(read = p1@frame$read, grade = p1@frame$grade,
                        ze.pred = model.matrix(ze) %*% fixef(ze),
                        quad.pred = model.matrix(quad) %*% fixef(quad))
#ggplot
l1 <- ggplot(graphdata, aes(x = grade, y = read)) + geom_point(colour = "grey80")
l2 <- l1 + geom_line(aes(y = ze.pred), size = 1.5)
l3 <- l2 + stat_summary(aes(y = read), fun = mean, geom = "point", size = 4)
l4 <- l3 + theme(aspect.ratio = 1.0)
##
q1 <- ggplot(graphdata, aes(x = grade, y = read)) + geom_point(colour = "grey80")
q2 <- q1 + geom_line(aes(y = quad.pred), size = 1.5)
q3 <- q2 + stat_summary(aes(y = read), fun = mean, geom = "point", size = 4)
q4 <- q3 + theme(aspect.ratio = 1.0)
#Print graphs
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(l4, vp = vplayout(1, 1))
print(q4, vp = vplayout(1, 2))


#12.6.2. Second-Order Fractional Polynomials.
n2n1 <- lmer(read ~ I(grade ^ -2) + I(grade ^ -1) + (I(grade ^ -2) | subid),
             MPLS2.LS, REML = FALSE)
zeze <- lmer(read ~ I(log(grade)) + I(log(grade) ^ 2) + (I(log(grade)) | subid),
             MPLS2.LS, REML = FALSE)
mymods <- list(ze, n2n1, zeze, quad)
mynames <- c("(0, )", "(-2, -1)", "(0, 0)", "(1, 2)")
print(aictab(mymods, mynames), LL = FALSE)


#12.6.3. Static Predictors
quadrisk <- lmer(read ~ poly(grade, 2) * risk2 + (poly(grade, 1) | subid),
                 MPLS2.LS, REML = FALSE)
logrisk <- lmer(read ~ I(log(grade)) * risk2 + (I(log(grade)) | subid),
                MPLS2.LS, REML = FALSE)
print(aictab(list(quadrisk, logrisk), c("Quad", "Log")), LL = FALSE)

#Graph data
plotdata <- data.frame(read = logrisk@frame$read, grade = exp(logrisk@frame[ ,2]),
                       risk2 = logrisk@frame$risk2,
                        q.pred = model.matrix(quadrisk) %*% fixef(quadrisk),
                        l.pred = model.matrix(logrisk) %*% fixef(logrisk))
#ggplot
l1 <- ggplot(plotdata, aes(x = grade, y = read)) + geom_point(colour = "grey80")
l2 <- l1 + stat_summary(fun = mean, geom = "point", size = 4)
l3 <- l2 + geom_line(aes(y = l.pred), lwd = 1.5) + facet_grid(. ~ risk2)
l4 <- l3 + theme(aspect.ratio = 1.0) + labs(title = "Log")
##
q1 <- ggplot(plotdata, aes(x = grade, y = read)) + geom_point(colour = "grey80")
q2 <- q1 + stat_summary(fun = mean, geom = "point", size = 4)
q3 <- q2 + geom_line(aes(y = q.pred), lwd = 1.5) + facet_grid(. ~ risk2)
q4 <- q3 + theme(aspect.ratio = 1.0) + labs(title = "Quadratic")
#Print graphs
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(l4, vp = vplayout(1, 1))
print(q4, vp = vplayout(1, 2))


#12.6.4. Caveats Regarding the Use of Fractional Polynomials.
grade1 <- MPLS2.LS$grade - 1
cquad <- lmer(read ~ poly(grade1, 2, raw = TRUE) + (grade1 | subid),
             MPLS2.LS, REML = FALSE)
#1st Order FP Models.
cn3 <- lmer(read ~ I(grade1 ^ -3) + (I(grade1 ^ -3) | subid), MPLS2.LS, REML = FALSE)
cn2 <- lmer(read ~ I(grade1 ^ -2) + (I(grade1 ^ -2) | subid), MPLS2.LS, REML = FALSE)
cn1 <- lmer(read ~ I(grade1 ^ -1) + (I(grade1 ^ -1) | subid), MPLS2.LS, REML = FALSE)
cn05 <- lmer(read ~ I(grade1 ^ -.5) + (I(grade1 ^ -.5) | subid), MPLS2.LS, REML = FALSE)
cze <- lmer(read ~ I(log(grade1)) + (I(log(grade1)) | subid), MPLS2.LS, REML = FALSE)
cp05 <- lmer(read ~ I(grade1 ^ .5) + (I(grade1 ^ .5) | subid), MPLS2.LS, REML = FALSE)
cp1 <- lmer(read ~ grade1 + (grade1 | subid), MPLS2.LS, REML = FALSE)
cp2 <- lmer(read ~ I(grade1 ^ 2) + (I(grade1 ^ 2) | subid), MPLS2.LS, REML = FALSE)
cp3 <- lmer(read ~ I(grade1 ^ 3) + (I(grade1 ^ 3) | subid), MPLS2.LS, REML = FALSE)

#Fit
mymods <- list(cn3, cn2, cn1, cn05, cze, cp05, cp1, cp2, cp3, cquad)
mynames <- c("-3", "-2", "-1", "-.5", "0", ".5", "1", "2", "3", "Quad")
print(aictab(mymods, mynames), LL = FALSE)


#12.7.1. Linear Spline Models.
#Create spline terms within the data frame
MPLS2.LS$spline1 <- ifelse(test = MPLS2.LS$grade > 6,
                           yes = MPLS2.LS$grade - 6, no = 0)
head(data.frame(MPLS2.LS$grade, spline1 = MPLS2.LS$spline1), n = 7)

#Run lmer() with spline term as second predictor
sp1 <- lmer(read ~ grade + spline1 + (grade | subid), data = MPLS2.LS, REML = FALSE)
sp1

#Testing the fixed effect of the spline transformation compared to a reduced model (no spline)
#Reduced model
sp0 <- lmer(read ~ grade + (grade | subid), data = MPLS2.LS, REML = FALSE)

#LRT
anova(sp0, sp1)

#AICc
print(aictab(list(sp1, sp0), c("Spline", "Linear")), LL = FALSE)

#Graphing
graphdata <- data.frame(read = sp1@frame$read, grade = sp1@frame$grade,
                        pred = model.matrix(sp1) %*% fixef(sp1))
g1 <- ggplot(graphdata, aes(x = grade, y = read)) + geom_point(colour = "grey80")
g2 <- g1 + stat_summary(fun = mean, geom = "point", size = 5)
g3 <- g2 + geom_line(aes(y = pred), size = 1.5)
print(g3)


#12.7.1.1. Linear Spline with Multiple Knots
MPLS2.LS$spline1 <- ifelse(test = MPLS2.LS$grade > 4,
                           yes = MPLS2.LS$grade - 4, no = 0)
MPLS2.LS$spline2 <- ifelse(test = MPLS2.LS$grade > 6,
                           yes = MPLS2.LS$grade -6, no = 0)

#Use spline terms in lmer()
sp2 <- lmer(read ~ grade + spline1 + spline2 + (grade | subid),
            MPLS2.LS, REML = FALSE)
sp2

#Graphing
graphdata <- data.frame(read = sp2@frame$read, grade = sp2@frame$grade,
                        sp2.pred = model.matrix(sp2) %*% fixef(sp2))
g1 <- ggplot(graphdata, aes(x = grade, y = read)) + geom_point(colour = "grey80")
g2 <- g1 + stat_summary(fun = mean, geom = "point", size = 4) + theme(aspect.ratio = 1.0)
g3 <- g2 + geom_line(aes(y = sp2.pred), size = 1.5)
print(g3)


#12.8.1. Computing Orthogonal Polynomials.
X <- matrix(c(rep(1,4), 1:4, (1:4) ^ 2, (1:4) ^ 3), ncol = 4)
X
T <- t(X) %*% X
T
C <- chol(T)
C
X.star <- X %*% solve(C)
X.star

#poly() output
poly(1:4, 3)
