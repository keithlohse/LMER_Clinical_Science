# Longitudinal Data Analysis for the Clinical Sciences. 
# Keith Lohse, 2017/10/22


## Setting the Working Directory -----------------------------------------------
getwd()
setwd("C:/Users/u6015231/Documents/GitHub/LMER_Clinical_Science/")


list.files()
# These are all of the files/folders in the working directory.
list.files("./data/")
# These are all of the files in the data sub-folder of the working directory.


# install.packages("ggplot2"); install.packages("lme4"); 
# install.packages("dplyr"); install.packages("AICcmodavg")
library("ggplot2");library("lme4");library("dplyr");library("AICcmodavg")


DATA<-read.csv("./data/data_LOHSE_EXAMPLE.csv", header = TRUE) 
DATA[1:20,c(1,2,4,5,6,8,10,12)]
# Note that I am calling the dataframe "DATA", but you can call it anything.
# For the ease of reading, I am also only printing select columns and the 
# first 20 rows of the data frame.


# Note that now I am looking at all of the columns, but only the first six rows.
head(DATA)
# The structure function does not show data, but instead shows properties of 
# the different columns/variables.
str(DATA)

## Creating Basic Plots --------------------------------------------------------
myX<-scale_x_continuous(name = "Time (Months from Outpatient Admission)")
myY<-scale_y_continuous(name = "Berg Balance Scale")
g1<-ggplot(data=DATA, aes(x = time, y = BERG, group = subID))+geom_line()
g2<-g1+geom_point()+facet_wrap(~subID)+myX+myY
g3<-g2 + stat_smooth(method=lm, se=FALSE)
plot(g3)


## Creating Tidy Data ----------------------------------------------------------
head(DATA) # Peak at the top rows.
tail(DATA) # Peak at the bottom rows.
summary(DATA$time) # Using the summary function on a numeric variable
summary(DATA$event_name) # Using the summary function on a factor variable


DAT2<-subset(DATA, time != -1)
head(DAT2)


summary(as.factor(DAT2$time))
# We are using the as.factor() funtion to treat time as a factor even though it
# is numeric. As such, the summary function returns the different categories of 
# time (top row) and the number of observations in each category (bottom row).

# Berg Balance Scale Plot
myX<-scale_x_continuous(name = "Time (Months from Outpatient Admission)")
myY<-scale_y_continuous(name = "Berg Balance Scale")
g1<-ggplot(data=DAT2, aes(x=time, y = BERG, group=subID))+geom_line()+myX+myY
g2<-g1+theme_bw()
print(g2)

# 10mWT Plot
myX<-scale_x_continuous(name = "Time (Months from Outpatient Admission)")
myY<-scale_y_continuous(name = "Ten Meter Walk Test (m/s)")
g1<-ggplot(data=DAT2, aes(x=time, y = X10mSpeed, group=subID))+geom_line()+myX+myY
g2<-g1+theme_bw()
print(g2)

# ARAT Plot
myX<-scale_x_continuous(name = "Time (Months from Outpatient Admission)")
myY<-scale_y_continuous(name = "Action Research Arm Test")
g1<-ggplot(data=DAT2, aes(x=time, y = ARAT, group=subID))+geom_line()+myX+myY
g2<-g1+theme_bw()
print(g2)


## Creating Faceted/Lattice Plots ----------------------------------------------
# BBS
myX<-scale_x_continuous(breaks = 0:12, 
                        name = "Time (Months from OutPatient Admission)")
myY<-scale_y_continuous(name = "Berg Balance Scale")
g5<-ggplot(data=DAT2, aes(x = time, y = BERG, group = subID))+geom_line()
g6<-g5+geom_point()+facet_wrap(~subID)+myX+myY
plot(g6)

# ARAT
myX<-scale_x_continuous(breaks = 0:12, 
                        name = "Time (Months from OutPatient Admission)")
myY<-scale_y_continuous(name = "ARAT")
g1<-ggplot(data=DAT2, aes(x = time, y = ARAT, group = subID))+geom_line()
g2<-g1+geom_point()+facet_wrap(~subID)+myX+myY
plot(g2)

# 10mWT
myX<-scale_x_continuous(breaks = 0:12, 
                        name = "Time (Months from OutPatient Admission)")
myY<-scale_y_continuous(name = "10m Walk Test")
g7<-ggplot(data=DAT2, aes(x = time, y = X10mSpeed, group = subID))+geom_line()
g8<-g7+geom_point()+facet_wrap(~subID)+myX+myY
plot(g8)


## Creating a Conditional Plot -------------------------------------------------
# First we will create a factor out of the IRF variable (which is coded as 
# 1s and 0s).
DAT2$IRF_Category<-factor(DAT2$IRF)
# Note the the additional shape argument in aes()
myX<-scale_x_continuous(name = "Time (Months from Outpatient Admission)")
myY<-scale_y_continuous(name = "Berg Balance Scale Score", limits=c(0,60))
g1 <- ggplot(data = DAT2, aes(x = time, y = BERG, group=subID, 
                              shape = IRF_Category)) + geom_point()
# We now specify that the linear fit for each participant is conditional on 
# on whether or not that participant went to an IRF previously.
g2 <- g1 + geom_smooth(method=lm, se=FALSE, aes(color=IRF_Category)) + myX + myY
g3 <- g2 + theme_bw()
print(g2)


## Building Statistical Models -------------------------------------------------
B0<-lmer(BERG~1+(1|subID),data=DAT2, REML=FALSE)
summary(B0)




B1<-lmer(BERG~1+time+(1|subID),data=DAT2, REML=FALSE)
summary(B1)




B2<-lmer(BERG~1+time+(1+time|subID),data=DAT2, REML=FALSE)
summary(B2)




B3<-lmer(BERG~1+time+IRF+(1+time|subID),data=DAT2, REML=FALSE)
summary(B3)




B4<-lmer(BERG~1+time*IRF+(1+time|subID),data=DAT2, REML=FALSE)
summary(B4)




## Facet Plot Showing Fixed Effects for Different IRF Groups -------------------
IRF<-c(0,1)
Ints<-c(32.27, 11.72)
Slopes<-c(1.86,3.90)
dd<-data.frame(IRF,Ints,Slopes)

myX<-scale_x_continuous(breaks = 0:12, 
                        name = "Time (Months from OutPatient Admission)")
myY<-scale_y_continuous(name = "Berg Balance Scale")
g1<-ggplot(data=DAT2, aes(x = time, y = BERG, group = subID))
g2<-g1+geom_point()+stat_smooth(method="lm", se=FALSE) + facet_wrap(~IRF)+myX+myY
g3<-g2+geom_abline(aes(intercept=Ints, slope=Slopes), lwd=2, dd)
plot(g3)

