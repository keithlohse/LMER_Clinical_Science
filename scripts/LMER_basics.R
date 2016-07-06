# Installing Packages -------------------------------------------------
wants <- c("AICcmodavg", "lme4", "lmerTest", "nlme", "ggplot2","dplyr")
wants
has   <- wants %in% rownames(installed.packages())
has
if(any(!has)) install.packages(wants[!has])

library("ggplot2");library("lme4");library("dplyr");
library("nlme");library('lmerTest');library("AICcmodavg")


##-------------------------------------------------------------
getwd()
# should be "C:/Users/krl0022/Documents" or similar

# we can then change the working directory to a specific folder for our project
setwd("C:/Currant/LMER_reading_group/")
getwd()
# let's see what is in the data folder
list.files("C:/Currant/LMER_reading_group/data")
#

DATA<-read.table("./data/data_DIFF_UNIQUE.txt", header = TRUE, sep="\t") 
tail(DATA)

##-------------------------------------------------------------

mean(DATA$countChange)
plot(DATA$EngTotal~DATA$countChange, pch=21,
     bg="chartreuse", cex=1.5)
m1<-lm(DATA$EngTotal~DATA$countChange)
summary(m1)

p1<-plot(DATA$EngTotal~DATA$countChange, pch=21,
     bg="dodgerblue", cex=1.5)
p1+abline(m1)
