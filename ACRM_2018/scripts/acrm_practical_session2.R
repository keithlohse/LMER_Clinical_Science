# ACRM 2018 Longitudinal Data Analysis Worksho
# By Keith Lohse, Neurorehabilitation Informatics Lab, 2018-07-21

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
# Make sure that the file data_session2.csv is saved in your working directory.

# Import the .csv file into R. 
# We will save this file in the R environment as an object called "DATA".
DATA<-read.csv("./data_session2.csv", header = TRUE, sep=",",  
               na.strings=c("NA","NaN"," ",""))

# Use the head() function to check the structure of the data file.
head(DATA)

# Alternately you can also download the data file from the web here:
# DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/LMER_Clinical_Science/master/data/data_session2.csv")
# head(DATA)


## ------------------- Visualizing the Effects of Time -------------------------
# One of the major questions we address in this module is how to best model the
# effects of time. That is, what is the most appropriate "shape" of the time
# curve? Is it perfectly straight? Is curved? 
# In this module we will build from our linear model (that we used in Module 1)
# to a curvilinear model in which we add quadratic and cubic components

## FIM scores by group and time -----------------------------------------------
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

# We can see that these patterns are almost certainly not linear:
first6<-DATA[c(1:108),]

g1<-ggplot(first6, aes(x = month, y = rasch_FIM)) +
  geom_point(aes(fill=as.factor(subID)), pch=21, size=2, stroke=1.25) +
  geom_line() +
  stat_smooth(method=lm, se=FALSE)+
  facet_wrap(~subID)
g2<-g1+scale_x_continuous(name = "Time from Admission (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))+
  ggtitle("Linear Effect of Time") 
g3 <- g2 + theme_bw() + 
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5), 
        axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14),
        legend.position="none")

plot(g3)



# Visually, we can test the effect of adding a quadratic effect to the model
g1<-ggplot(first6, aes(x = month, y = rasch_FIM)) +
  geom_point(aes(fill=as.factor(subID)), pch=21, size=2, stroke=1.25) +
  geom_line() +
  stat_smooth(method=lm, formula = y~x+I(x^2), se=FALSE)+
  facet_wrap(~subID)
g2<-g1+scale_x_continuous(name = "Time from Admission (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))+
  ggtitle("Quadratic Effect of Time") 
g3 <- g2 + theme_bw() + 
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5), 
        axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14),
        legend.position="none")

plot(g3)




# Further, we can see the effect of adding a cubic effect to the model
g1<-ggplot(first6, aes(x = month, y = rasch_FIM)) +
  geom_point(aes(fill=as.factor(subID)), pch=21, size=2, stroke=1.25) +
  geom_line() +
  stat_smooth(method=lm, formula = y~x+I(x^2)+I(x^3), se=FALSE)+
  facet_wrap(~subID)
g2<-g1+scale_x_continuous(name = "Time from Admission (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))+
  ggtitle("Cubic Effect of Time") 
g3 <- g2 + theme_bw() + 
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5), 
        axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14),
        legend.position="none")

plot(g3)






## -------------- Comparing different Fixed-Effects of Time --------------------
# In order to quantify what our visualizations show us qualitatively, 
# we need to statistically compare models with linear, quadratic, and cubic
# effects of time. 

DATA$year.0_sq<-DATA$year.0^2
DATA$year.0_cu<-DATA$year.0^3


# Linear Effect of Time
time_linear<-lmer(rasch_FIM~
                # Fixed-effects
                1+year.0+
                # Random-effects
                (1+year.0|subID), data=DATA, REML=FALSE)
summary(time_linear)


# Quadratic Effect of Time
time_square<-lmer(rasch_FIM~
                    # Fixed-effects
                    1+year.0+year.0_sq+
                    # Random-effects
                    (1+year.0+year.0_sq|subID), data=DATA, REML=FALSE)
summary(time_square)


# Cubic Effect of Time
time_cube<-lmer(rasch_FIM~
                    # Fixed-effects
                    1+year.0+year.0_sq+year.0_cu+
                    # Random-effects
                    (1+year.0+year.0_sq+year.0_cu|subID), data=DATA, REML=FALSE)
summary(time_cube)

anova(time_linear, time_square, time_cube)


# Cubic Fixed-Effect Only 
time_cube_fixed<-lmer(rasch_FIM~
                  # Fixed-effects
                  1+year.0+year.0_sq+year.0_cu+
                  # Random-effects
                  (1+year.0+year.0_sq|subID), data=DATA, REML=FALSE)

anova(time_cube_fixed, time_cube)



## ------------------ Conditional Curvilinear Models ---------------------------

# Effect of AIS Grade on Time
cond_01<-lmer(rasch_FIM~
                  # Fixed-effects
                  1+year.0*AIS_grade+year.0_sq+year.0_cu+
                  # Random-effects
                  (1+year.0+year.0_sq+year.0_cu|subID), data=DATA, REML=FALSE)
summary(cond_01)


# Effect of AIS Grade on Quadratic Time
cond_02<-lmer(rasch_FIM~
                # Fixed-effects
                1+year.0*AIS_grade+year.0_sq*AIS_grade+year.0_cu+
                # Random-effects
                (1+year.0+year.0_sq+year.0_cu|subID), data=DATA, REML=FALSE)
summary(cond_02)



# Effect of AIS Grade on Cubic Time
cond_03<-lmer(rasch_FIM~
                # Fixed-effects
                1+year.0*AIS_grade+year.0_sq*AIS_grade+year.0_cu*AIS_grade+
                # Random-effects
                (1+year.0+year.0_sq+year.0_cu|subID), data=DATA, REML=FALSE)
summary(cond_03)

# Comparing between Models
anova(cond_01, cond_02, cond_03)


# Checking the Assumptions of our Best Fitting Model
## Level 1 Assumptions ----
plot(fitted(cond_01),resid(cond_01), ylim=c(-20,20))

# Normality
qqnorm(resid(cond_01))


# Homoscedasticity 
# Comparable Variances at Level 1
x <- fitted(cond_01)
y <- resid(cond_01)/sd(resid(cond_01))
plot(x=x, y=y, xlab = "Fitted Values", ylab="Standardized Residuals",
     ylim=c(-4,4))


# Influential Participants
plot(DATA$subID, resid(cond_01)/sd(resid(cond_01)), 
     ylab="Standardized Residuals", xlab="Subject ID",
     ylim=c(-4,4))


## Level 2 Assumptions ---- 
LVL2<-summarize(group_by(DATA, subID),
                AIS_grade = AIS_grade[1])
head(LVL2)
ranef(cond_01)

LVL2$RE_int<-ranef(cond_01)$subID$`(Intercept)`
LVL2$STD_int<-LVL2$RE_int/sd(LVL2$RE_int)

LVL2$RE_year<-ranef(cond_01)$subID$year.0
LVL2$STD_year<-LVL2$RE_year/sd(LVL2$RE_year)

LVL2$RE_year_sq<-ranef(cond_01)$subID$year.0_sq
LVL2$STD_year_sq<-LVL2$RE_year_sq/sd(LVL2$RE_year_sq)

LVL2$RE_year_cu<-ranef(cond_01)$subID$year.0_cu
LVL2$STD_year_cu<-LVL2$RE_year_cu/sd(LVL2$RE_year_cu)

head(LVL2)

# Normality of Random-Effects
qqnorm(LVL2$RE_int)
qqnorm(LVL2$RE_year)
qqnorm(LVL2$RE_year_sq)
qqnorm(LVL2$RE_year_cu)


# Homoscedasticity
plot(x=LVL2$AIS_grade, y=LVL2$STD_int, 
     xlab = "Fitted Values", ylab="Standardized Residuals",
     ylim=c(-4,4))

plot(x=LVL2$AIS_grade, y=LVL2$STD_year, 
     xlab = "Fitted Values", ylab="Standardized Residuals",
     ylim=c(-4,4))

plot(x=LVL2$AIS_grade, y=LVL2$STD_year_sq, 
     xlab = "Fitted Values", ylab="Standardized Residuals",
     ylim=c(-4,4))

plot(x=LVL2$AIS_grade, y=LVL2$STD_year_cu, 
     xlab = "Fitted Values", ylab="Standardized Residuals",
     ylim=c(-4,4))


# Influential Participants
plot(x=LVL2$subID, y=LVL2$STD_int, 
     xlab="Subject ID", ylab="Standardized Residual",
     ylim=c(-4,4))

plot(x=LVL2$subID, y=LVL2$STD_year, 
     xlab="Subject ID", ylab="Standardized Residual",
     ylim=c(-4,4))

plot(x=LVL2$subID, y=LVL2$STD_year_sq, 
     xlab="Subject ID", ylab="Standardized Residual",
     ylim=c(-4,4))

plot(x=LVL2$subID, y=LVL2$STD_year_cu, 
     xlab="Subject ID", ylab="Standardized Residual",
     ylim=c(-4,4))


## -------------- Exporting the Revised Data File ------------------------------
# Finally, we want to save our revised data file to include the different 
# variables we created in a new .csv file.

write.csv(DATA, "./data_session3.csv")
