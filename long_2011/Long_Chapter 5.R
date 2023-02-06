#Long Chapter 5: Introduction to Linear Mixed Effects Regression

library(ggplot2)
library(lme4)

#Longitudinal Data Analysis in R, Chapter 5
load(file = "C:/Users/miller.allison/Box/Allison Miller/Course Work and Training/Longitudinal Data Analysis in R_Fall 2022/MPLS.Sorted.Rdata")
head(MPLS.Sorted)
tail(MPLS.Sorted)

#Linear model of grade predicting read scores. The 1+ is an optional term that specifies the intercept.
lm.1 <- lm(read~ 1+ grade, data = MPLS.Sorted)
summary(lm.1)

#Saving the raw and fitted values as a data frame in preparation for graphing
plotdata <- data.frame(lm.1$model, fitted = fitted(lm.1))
head(plotdata)

#Set some defaults
theme_set(theme_bw())
myX <- scale_x_continuous(breaks = 5:8, name = "Grade")
myY <- scale_y_continuous(name = "Reading")

#Use ggplot
g1 <- ggplot(data = plotdata, aes(x = grade, y = read)) + geom_point(shape = 1)
g2 <- g1 + geom_line(aes(x = grade, y = fitted)) + myX + myY
print(g2)

#Analysis of covariance
lm.2 <- lm(read ~ 1 + grade + riskC, data = MPLS.Sorted)
summary(lm.2)


#Graphical illustration of the intercept difference:
plotdata <- data.frame(lm.2$model, fitted = fitted(lm.2))
g1 <- ggplot(data = plotdata, aes(x = grade, y = read, shape = riskC)) + geom_point()
g2 <- g1 + geom_line(aes(x = grade, y = fitted, linetype = riskC))
g3 <- g2 + theme(legend.position = c(0.8, 0.3), "aspect.ratio" = 1)
g4 <- g3 + scale_shape(solid = F) + myX + myY
print(g4)


#Interaction Model
#You can calculate an interaction using the * or :
#The * will automatically include the single effects if they are not specified separately, whereas
#the : will not
#Adding an interaction terms adds a slope effect (the above models just had an intercept effect)

lm.3 <- lm(read ~ 1 + grade*riskC, data = MPLS.Sorted)
summary(lm.3)

plotdata <- data.frame(lm.3$model, fitted = fitted(lm.3))
g1 <- ggplot(data = plotdata, aes(x = grade, y = read, shape = riskC)) + geom_point()
g2 <- g1 + geom_line(aes(x = grade, y = fitted, linetype = riskC))
g3 <- g2 + theme(legend.position = c(0.8, 0.3), "aspect.ratio" = 1)
g4 <- g3 + scale_shape(solid = F) + myX + myY
print(g4)



#Estimating the LMER Model
#The lmer() function uses the lm() syntax conventions for the fixed effects portion of the model,
#with the added requirement that the random effects portion appears in parentheses and explicitly
#references the subject identification number. The random effects properly associate subjects with
#their vectors of repeated measures.

#Time as a Predictor (a single predictor, grade, and two random effects- intercepts and slopes)
lmer.1 <- lmer(read ~ 1 + grade + (1 + grade|subid), data = MPLS.Sorted, REML = FALSE)
summary(lmer.1)

#The corr of -0.74 means that those with lower intercepts tend to have higher slopes and vice-versa.

#Comparison of LM and LMER Fixed Effect Estimates:
print("-- LM table --", quote = F)
round(summary(lm.1)$coefficients, 4)
print("-- LMER table --", quote = F)
fixef(lmer.1)

#Intercept-only model:
lmer.0 <- lmer(read ~ 1 + (1|subid), data = MPLS.Sorted, REML = FALSE)

#Linear model (with grade as a time predictor):
lmer.1 <- lmer(read ~ 1 + grade + (1|subid), data = MPLS.Sorted, REML = FALSE)

#Display the estimated error variance of the two models:
se_lmer.0 <- (sigma(lmer.0)^2)
se_lmer.1 <- (sigma(lmer.1)^2)

myse2 <- data.frame(Intercept = se_lmer.0, Linear = se_lmer.1)
rownames(myse2) <- "Est. Sigma2"
myse2

lmer.1a <- lmer(read ~I(grade - 5) + (1 + I(grade - 5) | subid), MPLS.Sorted, REML = FALSE)
lmer.1b <- lmer(read ~I(grade - 8) + (1 + I(grade - 8) | subid), MPLS.Sorted, REML = FALSE)

#Print coefficients:
data.frame(grade = fixef(lmer.1), grade5 = fixef(lmer.1a), grade8 = fixef(lmer.1b))



#LMER With Static Predictors:
#Intercept Effects:
lmer.2 <- lmer(read ~ 1 + grade + riskC + (1 + grade | subid),
               data = MPLS.Sorted, REML = FALSE)
print(lmer.2, cor = FALSE)


#Slope and Intercept Effects
lmer.3 <- lmer(read ~ 1 + grade + riskC + riskC:grade + (1 + grade | subid),
               data = MPLS.Sorted, REML = FALSE)
print(lmer.3, cor = FALSE)


#Initial Status as a Static Predictor:
#For example, using reading at the fifth grade as a static predictor.
#The below syntax creates a data frame for just Grade 5 and a separate data frame for Grades 6-8, then
#merges them by subject ID so that reading scores at grade 5 becomes a static predictor and can now
#be used as a static predictor in a LMER analysis.
grade5 <- subset(MPLS.Sorted, grade ==5, select = c(subid, read))
colnames(grade5)[2] <- "read.int"
grade6to8 <- subset(MPLS.Sorted, grade != 5)
grade6to8a <- merge(grade5, grade6to8, by = "subid")

#Optional section 5.6:
#Estimate LMER Model:
lmer.1 <- lmer(read ~ 1 + grade + (1 + grade|subid), data = MPLS.Sorted, REML = FALSE)

#Random effects design matrix for first person.
Z <- model.matrix(lmer.1)[1:4,]

G <- VarCorr(lmer.1)$subid[1:2,1:2]

#Extract and save error variance:
sigma2 <- summary(lmer.1)$sigma ^ 2

#Create 4 x 4 Identity matrix:
Ident <- diag(4)

#Compute W:
W <- sigma2 * Ident

B <- Z %*% G %*% t(Z)

V <- B + W

#Examine correlations among the repeated measures:
#Create diagonal matrix, D:
D <- diag(1/sqrt(diag(V)))

#Compute Vstar:
Vstar <- D %*% V %*% D
