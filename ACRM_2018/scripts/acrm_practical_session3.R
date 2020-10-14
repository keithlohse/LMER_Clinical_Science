# ACRM 2018 Longitudinal Data Analysis Worksho
# By Keith Lohse, Neurorehabilitation Informatics Lab, 2018-09-19

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
DATA<-read.csv("./data_session3.csv", header = TRUE, sep=",",  
               na.strings=c("NA","NaN"," ",""), stringsAsFactors = TRUE)

# Use the head() function to check the structure of the data file.
head(DATA)

# Alternately you can also download the data file from the web here:
# DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/LMER_Clinical_Science/master/data/data_session3.csv")
# head(DATA)


## ------------------- Visualizing Missing Data --------------------------------
## FIM scores with complete data -----------------------------------------------
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



## FIM scores with data missing random -----------------------------------------
g1<-ggplot(DATA, aes(x = month, y = rasch_FIM_MAR)) +
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






## FIM scores with data missing not at  random ---------------------------------
g1<-ggplot(DATA, aes(x = month, y = rasch_FIM_MNAR)) +
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


## FIM scores with Last Observation Carried Forward ----------------------------
g1<-ggplot(DATA, aes(x = month, y = rasch_FIM_LOCF)) +
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


## -------------------- Identifying Missing Data -------------------------------
# Missing at Random
DATA$MAR_missing<-as.numeric(is.na(DATA$rasch_FIM_MAR))
summary(DATA$MAR_missing)

xtabs(MAR_missing ~ month, DATA)

x<-as.data.frame(xtabs(MAR_missing ~ month, DATA))
x


g1<-ggplot(x, aes(x = month, y=Freq)) +
  geom_col(fill= "light grey", color="black")
g2<-g1+scale_x_discrete(name = "Time from Admission (Months)") +
  scale_y_continuous(name = "Count of Missing Data", limits=c(0,40))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.position="none")

plot(g3)


# Missing Not at Random
DATA$MNAR_missing<-as.numeric(is.na(DATA$rasch_FIM_MNAR))
summary(DATA$MNAR_missing)

xtabs(MNAR_missing ~ month, DATA)

y<-as.data.frame(xtabs(MNAR_missing ~ month, DATA))
y


g1<-ggplot(y, aes(x = month, y=Freq)) +
  geom_col(fill= "light grey", color="black")
g2<-g1+scale_x_discrete(name = "Time from Admission (Months)") +
  scale_y_continuous(name = "Count of Missing Data", limits=c(0,40))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.position="none")

plot(g3)



## -------------- The Effects of Missingness on Time ---------------------------
# Cubic model with complete data
complete<-lmer(rasch_FIM~
                    # Fixed-effects
                    1+year.0+year.0_sq+year.0_cu+
                    # Random-effects
                    (1+year.0+year.0_sq+year.0_cu|subID), data=DATA, REML=FALSE)



# Cubic model with data Missing at Random
MAR<-lmer(rasch_FIM_MAR~
                 # Fixed-effects
                 1+year.0+year.0_sq+year.0_cu+
                 # Random-effects
                 (1+year.0+year.0_sq+year.0_cu|subID), data=DATA, REML=FALSE)



# Cubic model with data Missing Not at Random
MNAR<-lmer(rasch_FIM_MNAR~
                 # Fixed-effects
                 1+year.0+year.0_sq+year.0_cu+
                 # Random-effects
                 (1+year.0+year.0_sq+year.0_cu|subID), data=DATA, REML=FALSE)



# Cubic model with Last Observation Carried Forward
LOCF<-lmer(rasch_FIM_LOCF~
             # Fixed-effects
             1+year.0+year.0_sq+year.0_cu+
             # Random-effects
             (1+year.0+year.0_sq+year.0_cu|subID), data=DATA, REML=FALSE)



summary(complete)
summary(MAR)
summary(MNAR)
summary(LOCF)







