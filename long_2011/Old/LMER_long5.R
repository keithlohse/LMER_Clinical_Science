## Chapter 5 R code from Long (2012)
### Adapted by Keith Lohse, PhD (2016)

# Installing Packages -------------------------------------------------
wants <- c("AICcmodavg", "lme4", "lmerTest", "nlme", 
           "ggplot2","plyr", "dplyr")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

library("ggplot2");library("lme4"); library("plyr"); library("dplyr");
library("nlme");library('lmerTest');library("AICcmodavg")


##---------------------------------------------------------------------
# Reading data into R

getwd()
# should be "C:/Users/krl0022/Documents" or similar

# we can then change the working directory to a specific folder 
# for our project:
setwd("C:/Currant/LMER_reading_group/")
# let's see what is in the data folder
list.files("C:/Currant/LMER_reading_group/data")
#
## Chapter 5 R code.

## Tailor to your system:
load(file="./data/MPLS.LS.Rdata")
head(MPLS.LS)
tail(MPLS.LS)

##----------------------------------------------------------------
# The General Linear Model (GLM)

# In this chapter, we will explore two different general forms 
# of the LMER model. 
# 1. Accounting for within-subject variation (i.e., the time 
# level) using a dynamic Time predictor.

# 2. Accounting for between-subject variation (i.e, the subject
# level), using static predictor. Note that we can also allow 
# these static variables to interact with our dynamic variables.
# These interactions test how between-subject differences 
# affect change over time (i.e., interactions with subjects' 
# slopes), beyond average differences between groups (i.e., 
# subjects' intercepts).

# To get started LMER can largely be viewed as an extension of 
# the general linear model (glm). We will start with a 'classic' 
# glm before we move on to analyze these data correctly using 
# LMER (which accounts for the nested nature of our data).

# In a classic glm, all of our data are independent (i.e., one
# data point per person). Statistically, we can write this as
# a linear model like:
# yi = B0 + B1(gradei) + Ei

# Each subject's actual reading score (yi) is the result of an
# intercept (B0) and a modifier based on their grade (the slope
# multiplied by their grade). The intercept and slope are 
# collectively referred to as our statistical MODEL. Our model
# is not going to be perfect, however, so we need to include 
# and error term (Ei). Good models will have small errors and
# thus be a better approximation of our data. As such, we can
# more generally say that:
# DATA = MODEL + ERROR

# For now, we will ignore that fact that we have multiple 
# observations from the same subject and use the lm() function
# to create our linear model:
lm.1 <- lm(read ~ 1 + grade, data = MPLS.LS)
# By using the summary function on our model, we can see what
# the best fitting slope and intercept are, and we can see all
# of the inferential statistics that test their signficance. 
summary(lm.1)
# How do we interpret the magnitude/significance of the intercept?
# How do we interpret the magnitude/significance of the slope?

## One thing that you will notice is the scientific notation for
## the p-value of the intercept. Long (2012) points out that you
## can turn off scientific notation: 
### options(scipen = 999) 
## But we don't want to turn off scientific notation, because
## we are straight up ballers!!!

# However, there are also some things in our linear model that
# the summary function does not show us.
# Specifically, the lm() function also creates a vector of our
# models' predictions (called 'fitted' values) and a vector of our
# models' errors (called 'residuals').
# Having access to our fitted values and residuals is tremendously
# useful. For instance, let's take our model and fitted values and
# put them together into a new data frame:

plotdata <- data.frame(lm.1$model, fitted = fitted(lm.1))
head(plotdata)

# From there, we can use our plotdata to create some very 
# informative visualizations. 
## Let's set some defaults.
theme_set(theme_bw())
myX <- scale_x_continuous(breaks = 5:8, name = "Grade")
myY <- scale_y_continuous(name = "Reading")
# And then use ggplot() do how individual data stack up 
# against our models predictions.
g1 <- ggplot(data = plotdata, aes(x = grade, y = read)) + 
    geom_point(shape = 1, size = 2)
g2 <- g1 + geom_line(aes(x = grade, y = fitted), 
                     size = 2, col = "dodgerblue") + myX + myY
print(g2)

# As you can see, there is considerable variablilty between our 
# subjects, but there is an increase in reading scores over time,
# as shown by our linear model (the blue line). 
# If you really feel like measuring it, you will see that the blue 
# line is described by:
# y_fitted = 183.915 + 4.427*(Grade)

##----------------------------------------------------------------
# The General Linear Model (GLM) as an ANCOVA
# Next, we can expand our model by adding a second predictor
# In this case, we will add the students' risk status (ADV or DADV) 
# as a predictor to our linear model.

# yi = B0 + B1(gradei) + B3(riski)+ Ei

lm.2 <- lm(read ~ 1 + grade + risk2, data = MPLS.LS)
summary(lm.2)

# In statistical terms, these are the main effect of grade and the
# main effect of risk, there is no interaction of these terms. We can
# see this visually in that models will predict independent effects 
# of grade (i.e., the slope of the lines will not depend on which group
# you are in) and risk (i.e., the difference between the lines will 
# not depend on which grade you are in).

plotdata <- data.frame(lm.2$model, fitted = fitted(lm.2))
g1 <- ggplot(data = plotdata, 
             aes(x = grade, y = read, shape = risk2, col=risk2)) + 
    geom_point(size=2)
g2 <- g1 + geom_line(aes(x = grade, 
                         y = fitted, 
                         linetype = risk2, 
                         col=risk2), size =1.5)
g3 <- g2 + theme(legend.position = c(.8,.3), "aspect.ratio" = 1)
g4 <- g3 + scale_shape(solid = FALSE) + myX + myY
print(g4)

##----------------------------------------------------------------
# The General Linear Model (GLM) with and interaction

# Within the GLM framework, we can also test the interaction of 
# grade with risk. That is, we can ask if the effects of a 
# students grade depend on their risk status. 
# From a modeling stand point, adding an interaction looks like
# this:
# yi = B0 + B1(gradei) + B3(riski) + B3(grade*riski) + Ei

# Mathematically, the interaction term is a new variable that is
# literally the product of one variable multiplied by the other 
# variable. You should only ever include the interaction in a model
# after adding the main effects.

lm.3 <- lm(read ~ 1 + grade * risk2, data = MPLS.LS)
summary(lm.3)

# We can also vizualize this interaction by making reading scores
# conditional on both Grade and Risk
plotdata <- data.frame(lm.3$model, fitted = fitted(lm.3))
g1 <- ggplot(data = plotdata, 
             aes(x = grade, y = read, shape = risk2)) + geom_point()
g2 <- g1 + geom_line(aes(x = grade, y = fitted, linetype = risk2))
g3 <- g2 + theme(legend.position = c(.8,.3), "aspect.ratio" = 1)
g4 <- g3 + scale_shape(solid = F) + myX + myY
print(g4)
# Note that the slopes for the ADV and the DADV groups are slightly 
# different in this new figure (that is the result of our 
# interaction term), but the slopes are only SLIGHTLY different,
# which is why our interaction is not statistically significant.

head(MPLS.LS, n = 8)

##-----------------------------------------------------------------
# Hack Your Way to Linear Mixed-Effect Regression!
# Okay, now we should have a pretty good handle on GLM and how to
# interpret the outputs of a regular regression. However, we know
# that these models are not actually correct, because our data points
# are not independent of each other. Our data come from indivudal 
# participants measured at multiple time points. Thus, time is 
# nested within participants.

# One way to get around this would be to calculate individual linear
# models for each participant. This is not the ideal way to analyze
# our data (we are still working toward LMER) but it can be useful 
# to see what each of our participants' slopes and intercepts are.

# Using the subset function, we can break our data into individual 
# subjects and run separate regressions:
sub1<-subset(MPLS.LS,subid == 1)
sub2<-subset(MPLS.LS,subid == 2)
sub3<-subset(MPLS.LS,subid == 3)

## modeling DV1~time for subject #1
summary(lm(read~grade, data=sub1))
# The intercept should be 143.5
# The slope should be 6.0

## modeling DV1~time for subject #2
summary(lm(read~grade, data=sub2))
# The intercept should be 179.3
# The slope should be 4.5

# This 'by hand' approaches totally works, but it would rapidly 
# become tedious if we had to do this many more subjects or if 
# we had to do it for all of our different dependent variables. 
# Yikes!
# Instead, let's automate it!

# We will start with doing this by hand using a for{} loop in 
# the base package. First, we will create a numeric vector:
index<-c(1:22) 
# We set the length of the index to be equal to number of subjects
# Next, we will put this vector into a new data frame:
DAT2<-data.frame(index)
# and then we can write a "for" loop to pull our the slope and
# the intercept for each participant:
for (i in 1:length(DAT2$index)) {
    DAT2$mod1_int[i]<-summary(lm(read~grade, data=MPLS.LS[MPLS.LS$subid==i,]))$coefficients[1] 
    # This line runs a linear model dv1~time and extracts the intercept
    ## lm()$coefficients[1] extracts the intercept because
    ## lm()$coefficients is a vector of all coefficients in 
    ## the model and the intercept is the first. 
    DAT2$mod1_slope[i]<-summary(lm(read~grade, data=MPLS.LS[MPLS.LS$subid==i,]))$coefficients[2] 
    # This line runs a linear model dv1~time and extracts the slope
    ## lm()$coefficients[2] extracts the slope because
    ## lm()$coefficients is a vector of all coefficients in 
    ## the model and the slope of grade is the second. Note,
    ## However, that if we had more variables, we could 
    ## increase this number to get the coefficient we want.
    
    # We could also do this for different variables or models
    ## DAT2$mod2_int[i]<-summary(lm(dv2~time, data=DATA[DATA$subID==i,]))$coefficients[1] 
    ## DAT2$mod2_slope[i]<-summary(lm(dv2~time, data=DATA[DATA$subID==i,]))$coefficients[2] 
    # ...
}

# To make sure that our for loop ran correctly, lets look at DAT2:
DAT2                      
# Hopefully you have a 22 X 3 matrix showing your slopes and intercepts!
# Next, we will save these values to our working directory as a spreadsheet
write.csv(DAT2, file="./data/individual_slopes.csv")
# We can also plot/summarize the intercepts and slopes to better
# understand where our participants start, and how they change 
# over time
hist(DAT2$mod1_int)
summary(DAT2$mod1_int)
hist(DAT2$mod1_slope)
summary(DAT2$mod1_slope)

# This is great, but it still does not answer the "overall" questions
# that we want to answer. In order to look at the effects of grade,
# risk, and the risk by grade interaction, we need to use LMER!


##-----------------------------------------------------------------
# Linear Mixed-Effect Regression (LMER)
# ... easily the best thing you'll learn all day!

# The code for LMER is essentially the same as GLM, except that we 
# will be using lmer() instead of lm(). (Note that you need lme4 
# installed to have access to the lmer() function.)
# Conceptually, LMER is a lot like GLM but we need to 'partition'
# or variance. 

# yij = BO + U0j + B1(gradeij) + U1j + Eij

# In LMER, we now have data indexed by time point (i) and 
# by participant (j). However, each datapoint, yij, can still be 
# described by the group average intercept and slope, BO and B1, 
# plus a random effect for each subject, U0j and U1j. 
# Note that these random effects could be positive or negative,
# but the represent how this participant deviates from the norm.
# Thus, in LMER our MODEL is the combination of our 'fixed'-effects
# (all of B's) and the 'random'-effects (all of the Ujs).
# However DATA = MODEL + ERROR still applies, so we need to 
# include a random error term for each data point, Eij.

# To recap, we have a few kinds of terms in our DATA:
# The MODEL: includes fixed effects and random effects
# FIXED EFFECTS: includes the group-level B's
# RANDOM EFFECTS: includes participant-level Uj's
# RANDOM ERRORS: the difference between our predictions and the
# actual data.

# We can write this as:
# yij = BO + U0j + B1(gradeij) + U1j + Eij
# Or in the equivalent form:
# yij = B0 + B1(gradei) + (U0 + U1(gradei)|Subject)
#... which translates directly into the LMER code structure:
lmer.1 <- lmer(read ~ 1 + grade + (1 + grade | subid), data = MPLS.LS, REML = FALSE)
summary(lmer.1)
# Note that our code is neatly broken into the fixed-effects and 
# random-effects! Our random-errors are in there too as 'scaled
# residuals'. See Chapter 5 in Long (2012) for detailed 
# information on how to interpret these results. 

# One major different you will note between my output and the 
# original output in Long (2012) is the addition of T-values
# and p-values with our fixed-effects. Now, I 100% agree with 
# Long and would recommend that you base your interpretation
# of the different models on the change in the AIC. However, 
# I have also submitted enough manuscripts where people want
# to see t-/p-values for the fixed-effect coefficients, so we
# went ahead and installed the "lmerTest" package. The 
# "lmerTest" package rides in on the back of "lme4" to give
# you t-/p-values based on the Welch-Satterthwaite approximation
# (Satterthwaite, 1946; Welch, 1947). I am not exaggerating when
# I say that I really do not understand the math behind these 
# approximatations (suffice to say that people much smarter
# than me say it is correct), but in essence, the maximum-
# likelihood estimation used in LMER does not lend itself
# directly to some of our classic inferential statistics,
# and calculating the degrees of freedom in LMER is not trivial,
# the Welch-Satterthwaite approximation gives an approximate
# t-value and corresponding p-value (to a very good approximation).

# So, again, I would recommend using AIC to make decisions about
# your different models, but if a review wants p-values, make sure 
# that you have the "lmerTest" package installed and cite it 
# accordingly (always cite all packages you use!). We will get 
# into more detail about model comparisons later, but for now 
# remember that lower AICs mean better model fit (i.e. less
# error in our predictions).


print("-- LM table --", quote = FALSE)
round(summary(lm.1)$coefficients,2)
# print("-- LMER table -- ", quote = FALSE)
# round(summary(lmer.1)@coefs, 4)
# I think that if you have lmerTest installed, is disrupts this
# code... try this instead
round(fixef(lmer.1),2)

##-----------------------------------------------------------------
# The Model Comparison Approach

# As I mentioned above, we are really interested in making comparisons
# between our different models to decide which model is best (that is,
# which model has the smalled error). We will compare and contrast the
# different methods for assessing model fit later, but first, let's just 
# start with creating the "null" or random intercepts model:
## Random Intercepts model.
lmer.0 <- lmer(read ~ 1 + (1 | subid), data = MPLS.LS, REML = FALSE)
summary(lmer.0)
# In this model, we estimate the average reading score for each
# paricipant in our data set:
# yij = BO + U0j + Eij

# We can compare that to the more complicated " conditional random-intercepts"
# model, in which we have a random-effect of subject for the intercept and
# we make or predicitons conditional on Grade:
# yij = BO + U0j + B1(gradeij) + Eij

## Conditional Random Intercepts model.
lmer.1 <- lmer(read ~ 1 + grade + (1 | subid), data = MPLS.LS, REML = FALSE)
summary(lmer.1)

# However, the most appropriate model for us to create is the full
# "random-slopes" model, in which we allow the effect of grade to
# vary for each participant:
# yij = BO + U0j + B1(gradeij) + U1j + Eij

## Random Slopes LMER model.
lmer.2 <- lmer(read ~ grade + (grade | subid), MPLS.LS, REML = FALSE)
summary(lmer.2)

## Anchoring the intercept.
# Recall that the intercept is the predicted value of Y when X is 0.
# In some situations, 0 is meaning value of X. In the current models,
# however, Grade 0 is not really meaningful, and even if it was, we
# do not have any participants in grade 0. Thus, we might instead
# want to "anchor" the intercept at either the first or the last
# time-point: 
lmer.2a <- lmer(read ~ I(grade - 5) + (1 + I(grade - 5) | subid), MPLS.LS, REML = FALSE)
lmer.2b <- lmer(read ~ I(grade - 8) + (1 + I(grade - 8) | subid), MPLS.LS, REML = FALSE)
## We can then compare the coefficients of these different models.
data.frame(grade = fixef(lmer.2), grade5 = fixef(lmer.2a), grade8 = fixef(lmer.2b))
# Note the effect of grade never changes, but the value of the 
# intercept changes in each model.


##-----------------------------------------------------------------
# LMER with Static Predictors

# We are in the home streatch now, but on thing that we have not yet
# done in LMER is adding the static "risk" variable that we used in 
# our GLMs. From a coding perspective, this can be done pretty easily
# by adding "risk2" as a fixed effect. Note there is no random effect
# of "risk2" because risk is a static predictor and does not change
# within a subject:
lmer.3 <- lmer(read ~ 1 + grade + risk2 + (1 + grade | subid),
               data = MPLS.LS, REML = FALSE)
summary(lmer.3)

# We can also add an interaction between our dynamic (grade) and 
# static predictors (risk2) to see if the effect of grade depends on
# a students risk status. Note that the two different codes are 
# equivalent, but the "grade*risk2" notation implies that you want
# both main effects and the interaction, whereas the 
# "grade + risk2 + risk2 : grade" notation gives you more control.

lmer.4 <- lmer(read ~ 1 + grade*risk2 + (1 + grade | subid),
               data = MPLS.LS, REML = FALSE)

# lmer.4 <- lmer(read ~ 1 + grade + risk2 + risk2 : grade + (1 + grade | subid),
#               data = MPLS.LS, REML = FALSE)

summary(lmer.4)

##-----------------------------------------------------------------
# Controlling for Initial Status
# In some situations, we may want to treat initial status as a 
# fixed-effect, especially if the first time-point is some kind
# of pre-test or baseline measurement.
# Really, this is already done in the random-effects (and we can
# look at the correlation between random-effects to understand
# slope~intercept relationships), but sometimes we might want
# to explicitly control for initial status as a fixed effect.

# Assuming that this variable has not already be pulled out as 
# a static predictor, we need to pull out the first time-point
# in to a separate data frame.
grade5 <- subset(MPLS.LS, grade == 5, select = c(subid, read))
head(grade5)
# We will then rename this variable as "read.int"
colnames(grade5)[2] <- "read.int"

# Next, we can put all of our other data into a data frame
# that excludes grade 5:
grade6to8 <- subset(MPLS.LS, grade != 5)

# And then merge these two data frames together, keeping
# fifth grade reading scores as a static predictor. 
grade6to8a <- merge(grade5, grade6to8, by = "subid")
head(grade6to8a)
# Note that "read.int" is static what "grade" is still a 
# dynamic predictor.

lmer.5 <- lmer(read ~ 1 + grade+risk2+read.int + (1 + grade | subid),
               data = grade6to8a, REML = FALSE)
summary(lmer.5)

##-----------------------------------------------------------------
# Additional details of LMER
# head(model.matrix(lmer.1))
# head(model.matrix(lmer.3))

# ## Random effects design matrix for first person.
# Z <- model.matrix(lmer.1)[1:4, ]
# Z
# 
# G <- VarCorr(lmer.1)$subid[1:2,1:2]
# G
# 
# ## Extract and save error variance.
# sigma2 <- summary(lmer.1)@sigma ^ 2
# ## Create 4 x 4 Identity matrix.
# Ident <- diag(4)
# # Compute W.
# W <- sigma2 * Ident
# W
# 
# B <- Z %*% G %*% t(Z)
# B
# 
# V <- B + W
# V
# 
# ## Create diagonal matrix, D.
# D  <- diag(1 / sqrt(diag(V)))
# ## Compute Vstar.
# Vstar <- D %*% V %*% D
# Vstar
