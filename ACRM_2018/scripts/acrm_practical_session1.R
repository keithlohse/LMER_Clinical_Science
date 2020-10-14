# ACRM 2018 Longitudinal Data Analysis Workshop
# By Keith Lohse, Neurorehabilitation Informatics Lab, 2018-09-07

# Loading the essential libraries. 
library("ggplot2"); library("lme4"); library("car"); library("dplyr"); library("lmerTest"); 

# If these packages are not installed already, run the following code:
install.packages("ggplot2"); install.packages("lme4"); install.packages("car"); 
install.packages("dplyr"); install.packages("lmerTest"); 


##----------------------- Data Cleaning and QA ---------------------------------
## Setting the Directory -------------------------------------------------------
getwd()
setwd("C:/Users/USERNAME/Box Sync/Collaboration/")
list.files()
# Make sure that the file data_session1.csv is saved in your working directory.

# Import the .csv file into R. 
# We will save this file in the R environment as an object called "DATA".
DATA<-read.csv("./data/data_session1.csv", header = TRUE, sep=",",  
               na.strings=c("NA","NaN"," ",""), stringsAsFactors = TRUE)

# Use the head() function to check the structure of the data file.
head(DATA)

# Alternately you can also download the data file from the web here:
# DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/LMER_Clinical_Science/master/data/data_nonlinear_v4.csv")
# head(DATA)


## ----------------------- Basic Data Visualization ----------------------------
## FIM scores by time and subject -----------------------------------------------
g1<-ggplot(DATA, aes(x = month, y = rasch_FIM)) +
  geom_point(aes(fill=as.factor(subID)), pch=21, size=2, stroke=1.25) +
  geom_line(aes(group=subID)) +
  facet_wrap(~AIS_grade)
g2<-g1+scale_x_continuous(name = "Time from Admission (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.position="none")

plot(g3)
## -----------------------------------------------------------------------------

## Linear Fit for each person --------------------------------------------------
g1<-ggplot(DATA, aes(x = month, y = rasch_FIM)) +
  geom_point(aes(fill=as.factor(subID)), pch=21, size=2, stroke=1.25) +
  stat_smooth(aes(col=subID), se=FALSE, method="lm") +
  facet_wrap(~AIS_grade)
g2<-g1+scale_x_continuous(name = "Time from Admission (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.position="none")

plot(g3)
## -----------------------------------------------------------------------------


## Linear Fit by gender --------------------------------------------------------
g1<-ggplot(DATA, aes(x = month, y = rasch_FIM)) +
  geom_point(aes(fill=sex), pch=21, size=2, stroke=1.25) +
  stat_smooth(aes(lty=sex), col="black", lwd=1.5,se=FALSE, method="lm") +
  facet_wrap(~AIS_grade)
g2<-g1+scale_x_continuous(name = "Time from Admission (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))

plot(g3)
## -----------------------------------------------------------------------------


# Understanding basic random-effects ------------------------------------------
# Random Intercepts Model ----
raneff_00<-lmer(rasch_FIM~
                  # Fixed-effects
                  1+
                  # Random-effects
                  (1|subID), data=DATA, REML=FALSE)
summary(raneff_00)

# Recall that the fixed-effect for the intercept is the overall, "group-level"
# intercept in our model.
# However, we also have a random-effect of subject for the intercenpt. This means
# that our model estimates a deviate for each subject from the group-level 
# intercept. To see these random-effects using the raneff function. 

ranef(raneff_00)

# Remember that these are deviates from the fixed-effect, so if we want to see
# what the model is actually estimating for each person, we need to add the 
# fixed-effect back in:
fixef(raneff_00)

# We could do this manually, adding the fixef() output to the ranef() output, 
# but we can also get the indivudal values using the coef() function:
coef(raneff_00)$subID

# If you want the actual predictions of the model, rather than the estimated
# effects, you can use the fitted() function. 
# Note the difference in the size of these arrays. The fitted() function gives
# us a prediction for each person at each point.
fitted(raneff_00)


# To help us understand the model, we can plot these predictions for each person.
# First, lets take a subset of the first 10 people:
first10<-DATA[c(1:180),]

# Second, we'll make a smaller dataset with the predictions for these 10:
PRED<-data.frame(subID=c("s01","s02","s03","s04","s05","s06","s07","s08","s09", "s10"),
                 Intercepts=c(coef(raneff_00)$subID[c(1:10),]))
PRED


g1<-ggplot(first10, aes(x = month, y = rasch_FIM)) +
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
  geom_line(aes(group=subID)) +
  facet_wrap(~subID, ncol=3)
g2<-g1+scale_x_continuous(name = "Time from Admission (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))
print(g2)
g3<-g2+geom_abline(aes(intercept=Intercepts, slope=0), col="red", lwd=1.5, PRED)
plot(g3)


# Hopefully this plot will clearly show that our model is estimating a different
# intercept for each person, but everyone has the same slope, because we have not 
# allowed FIM scores to vary over time. 


# Fixed Slope Model ----
raneff_01<-lmer(rasch_FIM~
                  # Fixed-effects
                  1+month+
                  # Random-effects
                  (1|subID), data=DATA, REML=FALSE)
summary(raneff_01)

# Note that when we run the ranef() function, there is still only a random effect
# of the intercept:
ranef(raneff_01)

# When we run the fixef() function, however, now have both a slopes and intercept:
fixef(raneff_01)

# When we run the coef() function, you can see that each person has a unique 
# intercept, but everyone has the same slope:
coef(raneff_01)$subID


# As before, let's make a smaller dataset with the predictions for these 10 people:
PRED<-data.frame(subID=c("s01","s02","s03","s04","s05","s06","s07","s08","s09", "s10"),
                 Intercepts=c(coef(raneff_01)$subID[c(1:10),1]),
                 Slopes=c(coef(raneff_01)$subID[c(1:10),2]))
PRED

# We can now plot the predictions for each person, note that although each person
# has a different intercept, they all have the same slope. 
g1<-ggplot(first10, aes(x = month, y = rasch_FIM)) +
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
  geom_line(aes(group=subID)) +
  facet_wrap(~subID, ncol=3)
g2<-g1+scale_x_continuous(name = "Time from Admission (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))
g3<-g2+geom_abline(aes(intercept=Intercepts, slope=Slopes), col="red", lwd=1.5, PRED)
plot(g3)






# Random Slopes and Intercepts Model ----
raneff_02<-lmer(rasch_FIM~
                  # Fixed-effects
                  1+month+
                  # Random-effects
                  (1+month|subID), data=DATA, REML=FALSE)
summary(raneff_02)

# Now when we run the ranef() function, there is random effect for both the 
# slope and the intercept:
ranef(raneff_02)

# When we run the fixef() function, it returns the group-level intercept and slope:
fixef(raneff_02)

# When we run the coef() function, the random-effects (deviates) are combined
# with the group-level fixed-effects to get a unique slope and intercept 
# for each person:
coef(raneff_02)$subID


# As before, let's make a smaller dataset with the predictions for these 10 people:
PRED<-data.frame(subID=c("s01","s02","s03","s04","s05","s06","s07","s08","s09", "s10"),
                 Intercepts=c(coef(raneff_02)$subID[c(1:10),1]),
                 Slopes=c(coef(raneff_02)$subID[c(1:10),2]))
PRED

# Now we can plot the predictions for each person, note that each person has a 
# different intercept and slope. 
g1<-ggplot(first10, aes(x = month, y = rasch_FIM)) +
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
  geom_line(aes(group=subID)) +
  facet_wrap(~subID, ncol=3)
g2<-g1+scale_x_continuous(name = "Time from Admission (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))
g3<-g2+geom_abline(aes(intercept=Intercepts, slope=Slopes, col=subID), lwd=1.5, PRED)
plot(g3)


## ------------------- Comparing between Models --------------------------------
# Now that we have our three random-effects models:
# 1. Random Intercepts
# 2. Fixed Slopes and Random Intercepts
# 3. Random Slopes and Random Intercepts
# ... how do we decide which one is most appropriate?

# We can do this is by analyzing the the variance explained by each model. 
anova(raneff_00,raneff_01,raneff_02)

# The Wald Test of the change in deviance suggests that Model raneff_01 is a 
# better explanation of the data than Model raneff_00, and furthermore that 
# Model raneff_02 is a better explanation of the data than Model raneff_01.
# More conservatively, we can use the AIC. Without going into detail about the 
# AIC, lower numbers indicate better model fit and AIC is based on the deviance 
# but introduces a penalty based on the number of parameters to reduce over-
# fitting. 

# A complementary step we can take is calculate how much residual variance 
# we might have left to  explain. 
# In our multilevel model, keep in mind that we have variance at two levels:
# 1. Level One: which are the data-points within subjects.
# 2. Level Two: variation between subjects slopes and intercepts.

# We can get a summary of the random-effects from the "null" model and the 
# best fitting model by using the VarCorr() function. 
VarCorr(raneff_00)
VarCorr(raneff_02)

# These values will help us keep track of how much variance is "available" to 
# be explained in our subsequent models. Similarly, these values will inform us
# how much variance our fixed-effects explain at Level 1 and Level 2 (sort of 
# like an R^2 that has been divided into two parts).



## --------------- Unconditional and Conditional Models ------------------------
# Creating our Time variable ---------------------------------------------------

summary(DATA$month)
# Note the wide range in the month variable (1-18). 
# There are two issues with this:
# 1. Time starts at 1 instead of 0
# 2. This scale is a lot larger than other variables we might want to include.

# To address these issues, we will convert Months to Years and try "centering"
# the time variable in different locations. 

# The first time variable, year0 will set the first assessment equal to 0
DATA$year<-DATA$month/12
DATA$year.0<-DATA$year-min(DATA$year)
summary(DATA$year.0)

# Next, we will create a mean centered time variable, year.c:
mean(DATA$year)
DATA$year.c<-DATA$year-mean(DATA$year)
summary(DATA$year.c)

# Let's look at the effects these two different time variables have on the 
# fixed-effects and random-effects. 

# Time 0 = the first time time point ----
time_00<-lmer(rasch_FIM~
            # Fixed-effects
            1+year.0+
            # Random-effects
            (1+year.0|subID), data=DATA, REML=FALSE)
summary(time_00)

# Time 0 = the mean time of 0.79 years ----
time_01<-lmer(rasch_FIM~
                # Fixed-effects
                1+year.c+
                # Random-effects
                (1+year.c|subID), data=DATA, REML=FALSE)
summary(time_01)

# Compare the fixed-effects of the intercept and the slope between the two models.
# Compare the random-effects between the two models.


# Centering the time-variable on the first time point makes a lot of sense 
# conceptually, but it is important to remember what that means when interpreting 
# other variables. 
# As you might recall from multiple regression in previous courses, when 
# interaction terms are included in a model, the effect of one variable is 
# being interpreted when the other variable is equal to 0. 
# Thus, if we had year.0 in our model, that would mean the effects of other 
# variables are being evaluated when time = 0, at the initial assessment. 
# Conversely, if we use year.c in our model, that would mean the effect of other
# variables are being evaluated when time = 0, which is at 0.79 yrs (or 9.5 months).


## Conditional Model: Does the slope change as a function of AIS grade? ----
# With our new variable of time in years, we want to see if we can explain the
# residual variation in subject's slopes and intercepts.

# Main-Effect Model: Equivalent to Conditional Intercepts
cond_00<-lmer(rasch_FIM~
                # Fixed-effects
                1+year.0+AIS_grade+
                # Random-effects
                (1+year.0|subID), data=DATA, REML=FALSE)
summary(cond_00)

# Interaction Model: Conditional Intercepts and Slopes
cond_01<-lmer(rasch_FIM~
                # Fixed-effects
                1+year.0*AIS_grade+
                # Random-effects
                (1+year.0|subID), data=DATA, REML=FALSE)
summary(cond_01)


anova(cond_00,cond_01)


# Contrast coding the group variable ---- 
contrasts(DATA$AIS_grade)

DATA$AIS_grade.c<-DATA$AIS_grade
contrasts(DATA$AIS_grade.c)<-contr.poly(3)
contrasts(DATA$AIS_grade.c)

# Main Effects with contrast coded group variable
cond_02<-lmer(rasch_FIM~
                # Fixed-effects
                1+year.0+AIS_grade.c+
                # Random-effects
                (1+year.0|subID), data=DATA, REML=FALSE)
summary(cond_02)

# Interaction Model with contrast coded group variable
cond_03<-lmer(rasch_FIM~
                # Fixed-effects
                1+year.0*AIS_grade.c+
                # Random-effects
                (1+year.0|subID), data=DATA, REML=FALSE)
summary(cond_03)


anova(cond_02,cond_03)


## Evaluating our Models -------------------------------------------------------
# Although the interaction model was not statistically better than the main-
# effects model, it can useful to present the full model to show the non-
# significant interactions. 

summary(cond_01)

# How much variation in individual slopes and intercepts did we explain with
# AIS Grade?
VarCorr(time_00)
VarCorr(cond_01)


# We can also plot the fixed-effects estimates from our models "on top" of the 
# raw data to show the conditional effects. 

## Linear Fit by AIS Category --------------------------------------------------
summary(cond_01)
FIXED<-data.frame(AIS_grade=c("C1-4","C5-8","paraplegia"),
                 Intercepts=c(20.113, (20.113+7.779), (20.113+15.89)),
                 Slopes=c(22.348, (22.348+4.714), (22.348+7.257)))
FIXED


g1<-ggplot(DATA, aes(x = year, y = rasch_FIM)) +
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
  facet_wrap(~AIS_grade)
g2<-g1+scale_x_continuous(name = "Time from Admission (Years)") +
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))

plot(g3)

g4<-g3+geom_abline(aes(intercept=Intercepts, slope=Slopes, col=AIS_grade,
                       lty = AIS_grade), lwd=2, FIXED)
plot(g4)

## -----------------------------------------------------------------------------


## Checking the Assumptions of our Model ---------------------------------------
# The DATA data frame is what we might call our Level 1 matrix. That is, each
# person and time point is represented. For checking out statistical assumptions, 
# it will also be helpful for us to create a Level 2 matrix. The Level 2 matrix
# will preserve participant information, but ignore time information.

LVL2<-summarize(group_by(DATA, subID),
                  AIS_grade = AIS_grade[1])
LVL2
# At the moment, our LVL2 dataframe contains subject IDs and group information. 
# Next, lets add the random-effects from our models:
LVL2$RE_int<-ranef(cond_01)$subID$`(Intercept)`
LVL2$RE_year<-ranef(cond_01)$subID$year
# Note that we can add this data on directly because the matrix is sorted by 
# subID and the the array we are adding is sorted by subID.

LVL2



# Normality ----
# Residuals at Level 1
qqnorm(resid(cond_01))

# Residuals at Level 2
qqnorm(LVL2$RE_int)
qqnorm(LVL2$RE_year)



# Homoscedasticity ----
# Comparable Variances at Level 1
x <- fitted(cond_01)
y <- resid(cond_01)/sd(resid(cond_01))
plot(x=x, y=y, xlab = "Fitted Values", ylab="Standardized Residuals",
     ylim=c(-4,4))


# Comparable Variances at Level 2
# Intercepts
LVL2$STD_int<-LVL2$RE_int/sd(LVL2$RE_int)
plot(x=LVL2$AIS_grade, y=LVL2$STD_int, 
     xlab = "Fitted Values", ylab="Standardized Residuals",
     ylim=c(-4,4))

# Slopes
LVL2$STD_year<-LVL2$RE_year/sd(LVL2$RE_year)
plot(x=LVL2$AIS_grade, y=LVL2$STD_year, 
     xlab = "Fitted Values", ylab="Standardized Residuals",
     ylim=c(-4,4))



# Influential Participants ----
# Residuals at Level 1
plot(DATA$subID, resid(cond_01)/sd(resid(cond_01)), 
     ylab="Standardized Residuals", xlab="Subject ID",
     ylim=c(-4,4))


# Residuals at Level 2
# Variation in Intercepts
plot(x=LVL2$subID, y=LVL2$STD_int, 
     xlab="Subject ID", ylab="Standardized Residual",
     ylim=c(-4,4))

# Variation in Slopes
plot(x=LVL2$subID, y=LVL2$STD_year, 
     xlab="Subject ID", ylab="Standardized Residual",
     ylim=c(-4,4))


## -------------- Exporting the Revised Data File ------------------------------
# Finally, we want to save our revised data file to include the different 
# variables we created in a new .csv file.

write.csv(DATA, "./data_session2.csv")








