# Longitudinal Data Analysis for the Clinical Sciences. 
# Keith Lohse, 2017/09/25


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

