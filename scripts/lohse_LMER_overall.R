# Longitudinal Data Analysis for the Clinical Sciences. 
# Keith Lohse, 2017/01/30


##----------------------- Data Cleaning and QA ---------------------------------
## Setting the Directory -------------------------------------------------------
getwd()
setwd("C:/Users/krl0022/Documents/GitHub/LMER_Clinical_Science/")
list.files()
list.files("./data/")
# the "./" appends the additional text to the end of the current working 
# directory. Here, we are peaking into the "data" folder.

# Next, we will want to open several libraries whose functions we will use.
# Be sure to install these libraries first if you haven't already:
# install.packages("ggplot2"); install.packages("lme4"); 
# install.packages("dplyr"); install.packages("AICcmodavg")
library("ggplot2");library("lme4");library("dplyr");library("AICcmodavg")

## Importing Data and Quality Assurance ----------------------------------------
# Next, we will read in a "dummy" dataset that I created. 
# This data emulates datastructures in the Brain Recovery Core Database 
# See Lang et al. J. Neurologic Physical Therapy. 2011 and Lohse et al. Arch.
# Phys Med Rehabil. 2016.
DATA<-read.csv("./data/data_LOHSE_EXAMPLE.csv", header = TRUE) 
DATA #Clearly the raw data is a bit of a mess, but notice NAs in multiple rows 
# and columns. Some participants are missing data for entire assessments, 
# others are missing data for individual timepoints. 

# Using ggplot2, we can visualize some of our data over time:
myX<-scale_x_continuous(name = "Time (Months from Outpatient Admission)")
myY<-scale_y_continuous(name = "Berg Balance Scale")
g1<-ggplot(data=DATA, aes(x = time, y = BERG, group = subID))+geom_line()
g2<-g1+geom_point()+facet_wrap(~subID)+myX+myY
g3<-g2 + stat_smooth(method=lm, se=FALSE)
plot(g3)
# Note that participants have different amounts of data collected at different 
# time points. 

# For other variables, we might be more insterested in specific numeric values:
head(DATA) # Peak at the top rows.
tail(DATA) # Peak at the bottom rows.
summary(DATA$time) # Using the summary function on a numeric variable
summary(DATA$event_name) # Using hte summary function on a factor variable


## Creating Tidy Data (Wickham, J Stat Soft, 2014)------------------------------
# We can create a new dataset that removes the acute admission timepoint from 
# each participant. We are doing this because we already have variables in our 
# data set that were specifically collected at admission. These variables are 
# static (meaning that they do not change overtime for our individuals).
# All acute admission time points are coded as -1 in the time variable, so we 
# can remove those data points like this:
DAT2<-subset(DATA, time != -1)
head(DAT2)
# Note that now the time variable only ranges from 0 to 6 months
summary(as.factor(DAT2$time))

# You can also see that we have mulitple time points for all of our subjects. 
# This can be a real hassle if we want descriptive statistics for our sample. 
# Thus we want to create a second dataset that has one observation per subject 
# otherwise  subjects with more timepoints will count more!)
DAT_UNIQUE<-summarize(group_by(DAT2, subID),
                       meanDPS = mean(DPS,na.rm=TRUE), 
                      #calculate the average days post stroke
                       meanAGE = mean(Age), 
                      #average all age values down to a single number for each subject
                       MALE = mean(as.numeric(Sex)-1),  
                      #code a Male subject as 1, female subject as 0
                       BergADMIT = mean(BERG_ACUTE_AD),
                       X10mADMIT = mean(X10mWT_ACUTE_AD),
                       TimePoints = n(), 
                      #count the number of timepoints within each subject
                       MaxTime = max(time, na.rm=TRUE),
                       minBERG = min(BERG, na.rm=TRUE),
                       maxBERG = max(BERG, na.rm=TRUE),
                       minARAT = min(ARAT, na.rm=TRUE),
                       maxARAT = max(ARAT, na.rm=TRUE), 
                       min10mWT = min(X10mSpeed, na.rm=TRUE),
                       max10mWT = max(X10mSpeed, na.rm=TRUE), 
                       missingBERG = sum(as.numeric(is.na(BERG))), 
                      #count the number of NAs for the BERG
                       missing10m = sum(as.numeric(is.na(X10mSpeed))), 
                      #count the number of NAs for the 10mWT
                       missingARAT = sum(as.numeric(is.na(ARAT))), 
                      #count the number of NAs for the ARAT
                       missBergAcute = mean(as.numeric(is.na(BERG_ACUTE_AD))), 
                      #Count the number of missing ACUTE BERG scores
                      miss10mAcute = mean(as.numeric(is.na(X10mWT_ACUTE_AD)))) 
                      #Count the number of missing 10mWT scores in each subject

# We now have a dataset that consists of one observation per subject.
# That is, we have  "independent and identically distributed" data.
DAT_UNIQUE<-data.frame(DAT_UNIQUE) 
DAT_UNIQUE
# We can also save this data frame as a .csv for later reference and use
# outside of R. 
write.csv(DAT_UNIQUE, "./data/data_UNIQUE.csv")

# Notice that we also have one participant ID # 11 that only has one data point
# following their intake assessment. As this participant has no data at 
# subsequent time points, we will want to exclude them from our analysis.

DAT2<-subset(DAT2, subID != "p11") # drops all of #11's data
# For factors, we need to put the argument in quotes, for numerics, we can just
# write the number we want to exclude
# We can now summarize the subject ID factor to see that p11 has been excluded:
summary(DAT2$subID)
DAT2$subID<-factor(DAT2$subID)
summary(DAT2$subID)

##--------------------- Visualizing Longitudinal Data --------------------------
## Spagetti Plot Overall -------------------------------------------------------

# Plotting individual subject trajectories is very important for deciding how 
# we want to model the effects of time (linear or non-linear?)
# ARAT impaired arm
myX<-scale_x_continuous(breaks = 0:12, 
                        name = "Time (Months from OutPatient Admission)")
myY<-scale_y_continuous(name = "ARAT")
g1<-ggplot(data=DAT2, aes(x = time, y = ARAT, group = subID))+geom_line()
g2<-g1+geom_point()+facet_wrap(~subID)+myX+myY
plot(g2)

#BERG
myX<-scale_x_continuous(breaks = 0:12, 
                        name = "Time (Months from OutPatient Admission)")
myY<-scale_y_continuous(name = "Berg Balance Scale")
g5<-ggplot(data=DAT2, aes(x = time, y = BERG, group = subID))+geom_line()
g6<-g5+geom_point()+facet_wrap(~subID)+myX+myY
plot(g6)

#10m walk test
myX<-scale_x_continuous(breaks = 0:12, 
                        name = "Time (Months from OutPatient Admission)")
myY<-scale_y_continuous(name = "10m Walk Test")
g7<-ggplot(data=DAT2, aes(x = time, y = X10mSpeed, group = subID))+geom_line()
g8<-g7+geom_point()+facet_wrap(~subID)+myX+myY
plot(g8)


# Creating spaghetti plots to visualize our data.
# In these plots, we will plot a line for each subject.
# Berg Balance Scale
myX<-scale_x_continuous(name = "Time (Months from Outpatient Admission)")
myY<-scale_y_continuous(name = "Berg Balance Scale")
g1<-ggplot(data=DAT2, aes(x=time, y = BERG, group=subID))+geom_line()+myX+myY
g2<-g1+theme_bw()
print(g2)

# 10 Meter Walk Test
myX<-scale_x_continuous(name = "Time (Months from Outpatient Admission)")
myY<-scale_y_continuous(name = "Ten Meter Walk Test (m/s)")
g1<-ggplot(data=DAT2, aes(x=time, y = X10mSpeed, group=subID))+geom_line()+myX+myY
g2<-g1+theme_bw()
print(g2)

# Action Research Arm Test
myX<-scale_x_continuous(name = "Time (Months from Outpatient Admission)")
myY<-scale_y_continuous(name = "Action Research Arm Test")
g1<-ggplot(data=DAT2, aes(x=time, y = ARAT, group=subID))+geom_line()+myX+myY
g2<-g1+theme_bw()
print(g2)


# Example of plot conditioned on a categorical variable. In this case, whether
# or not a participant went to an inpatent rehabilitation facility (IRF).
head(DAT2)
DAT2$IRF_nom<-factor(DAT2$IRF)
# Customizing our graph, note the the additional shape argument in aes()
myX<-scale_x_continuous(name = "Time (Months from Outpatient Admission)")
myY<-scale_y_continuous(name = "Berg Balance Scale Score", limits=c(0,60))
g1 <- ggplot(data = DAT2, aes(x = time, y = BERG, group=subID, 
                              shape = IRF_nom)) + geom_point()
# We still specify that the linear fit for each participant is conditional on 
# on whether or not that participant went to an IRF previously.
g2 <- g1 + geom_smooth(method=lm, se=FALSE, aes(color=IRF_nom)) + myX + myY
g3 <- g2 + theme_bw()
print(g2)




##---------------------- Building Statistical Models ---------------------------
# We will build a series of linear models using Berg Balance Scale data as our 
# example. 

# In a classic glm, all of our data are independent (i.e., one # data point per 
# person). Statistically, we can write this as a linear model like:
# yi = B0 + B1(TIMEi) + Ei

# Each subject's actual reading score (yi) is the result of an intercept (B0) 
# and a modifier based on their grade (the slope multiplied by their grade). 
# The intercept and slope are collectively referred to as our statistical MODEL. 
# Our model is not going to be perfect, however, so we need to include and 
# error term (Ei). Good models will have small errors and thus be a better 
# approximation of our data. As such, we can more generally say that:
# DATA = MODEL + ERROR

# The code for LMER is essentially the same as GLM, except that we will be 
# using lmer() instead of lm(). (Note that you need lme4 installed to have 
# access to the lmer() function.) Conceptually, LMER is a lot like GLM but we 
# need to 'partition' our variance. 

# yij = BO + U0j + B1(TIMEij) + U1j + Eij

# In LMER, we now have data indexed by time point (i) and by participant (j). 
# However, each datapoint, yij, can still be  described by the group average 
# intercept and slope, BO and B1, plus a random effect for each subject, U0j 
# and U1j. 
# Note that these random effects could be positive or negative, but they 
# represent how this participant deviates from the norm. Thus, in LMER our MODEL 
# is the combination of our 'fixed'-effects (all of B's) and the 
# 'random'-effects (all of the Ujs). However DATA = MODEL + ERROR still applies, 
# so we need to include a random error term for each data point, Eij.

# To recap, we have a few kinds of terms in our DATA:
# The MODEL: includes fixed effects and random effects
# FIXED EFFECTS: includes the group-level B's
# RANDOM EFFECTS: includes participant-level Uj's
# RANDOM ERRORS: the difference between our predictions and the actual data.

# We can write this as:
# yij = BO + U0j + (B1+ U1j)*(TIMEij) + Eij
# Or in the equivalent form:
# yij = B0 + B1(TIMEi) + (U0 + U1(TIMEi)|Subject)
# This second form matches the R syntax.

## Random Intercepts Model -----------------------------------------------------
# In our Random Intercepts model, we estimate a constant (intercept) for each
# participant and the overall constant. This overall constant is the 
# fixed-effect, and individual deviations away from this constant are our 
# random-effects.
B0<-lmer(BERG~1+(1|subID),data=DAT2, REML=FALSE)
# yij = BO + U0j + Eij
summary(B0)

myX<-scale_x_continuous(breaks = 0:12, 
                        name = "Time (Months from OutPatient Admission)")
myY<-scale_y_continuous(name = "10m Walk Test")
g1<-ggplot(data=DAT2, aes(x = time, y = BERG, group = subID))
g2<-g1+geom_point()+facet_wrap(~subID)+myX+myY
plot(g2)


pred<-data.frame(subID=c("p01","p02","p03","p04","p05","p06","p07","p08","p09",
                         "p10","p11","p12"),
                fitted=c(19.78, 4.54, 41.84, 42.99, 19.64, 13.47, 42.01, 24.74, 
                         49.11, 49.85, 9.74, 48.87))

g3<-g2+geom_hline(aes(yintercept=fitted), pred)
plot(g3)



## Random-Intercept Fixed-Slopes model -----------------------------------------
B1<-lmer(BERG~1+time+(1|subID),data=DAT2, REML=FALSE)
# yij = BO + U0j + B1*(TIMEij) + Eij
summary(B1)

# We can get the individual intercepts by extracting the random-effects 
# from the model, B1.
dd <- ranef(B1)[["subID"]]
colnames(dd)[1] <- "Intercepts"
# But these random-effects are deviations from the fixed-effect, so we need
# to add the fixed-effect back into these deviates.
dd$Intercepts<-dd$Intercepts+27.2915
dd <- cbind(subID=rownames(dd),dd)
# However, the slope is fixed for everyone, so we can just repeat that slope 
# equal to the number of participants we have.
dd$Slopes <- c(rep(2.0017, 12)) 
dd

myX<-scale_x_continuous(breaks = 0:12, 
                        name = "Time (Months from OutPatient Admission)")
myY<-scale_y_continuous(name = "10m Walk Test")
g1<-ggplot(data=DAT2, aes(x = time, y = BERG, group = subID))
g2<-g1+geom_point()+facet_wrap(~subID)+myX+myY
g3<-g2+geom_abline(aes(intercept=Intercepts, slope=Slopes), dd)
plot(g3)



## Random Slopes Model ---------------------------------------------------------
B2<-lmer(BERG~1+time+(1+time|subID),data=DAT2, REML=FALSE)
# yij = BO + U0j + (B1+ U1j)*(TIMEij) + Eij
summary(B2)

# We can get the individual intercepts and slope by extracting the 
# random-effects from the model, B1.
dd <- ranef(B2)[["subID"]]
colnames(dd)[1] <- "Intercepts"
# But these random-effects are deviations from the fixed-effect, so we need
# to add the fixed-effects back into these deviates to get intercepts and slopes
# in their original units.
dd$Intercepts<-dd$Intercepts+27.1662
dd$time<-dd$time+2.1606
dd <- cbind(subID=rownames(dd),dd)

myX<-scale_x_continuous(breaks = 0:12, 
                        name = "Time (Months from OutPatient Admission)")
myY<-scale_y_continuous(name = "10m Walk Test")
g1<-ggplot(data=DAT2, aes(x = time, y = BERG, group = subID))
g2<-g1+geom_point()+facet_wrap(~subID)+myX+myY
g3<-g2+geom_abline(aes(intercept=Intercepts, slope=time), dd)
plot(g3)













# Adding in additional Fixed Effects of Interest
B2<-lmer(BERG~1+time*DPS.c+(1+time|subID),data=DAT2, REML=FALSE)
summary(B2)

B3<-lmer(BERG~1+time*DPS.c+time*Age.c+(1+time|subID),data=DAT2, REML=FALSE)
summary(B3)

anova(B0,B1,B2,B3,B4,B5)







