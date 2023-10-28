# Loading the essential libraries. 
library("ggplot2"); library("lme4"); library("car"); 
library("dplyr"); library("lmerTest");


# If these packages are not installed already, run the following code: 
install.packages("ggplot2"); install.packages("lme4"); 
install.packages("car"); install.packages("dplyr"); 
install.packages("lmerTest");

# We will read these data in from the web:
DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/LMER_Clinical_Science/master/ACRM_2018/data/data_session1.csv",
                 stringsAsFactors = TRUE, na.strings=c("NA","NaN"," ",""))
# Use the head() function to check the structure of the data file. 
head(DATA)


ggplot(DATA, aes(x = month, y = rasch_FIM)) + 
  geom_line(aes(group=subID), col="grey") + 
  geom_point(aes(fill=AIS_grade), col="grey", pch=21, size=1, stroke=1) + 
  stat_summary(fun=mean, geom="point", shape=15, col="black")+
  stat_summary(fun=mean, geom="line", col="black")+
  facet_wrap(~AIS_grade) +
  scale_x_continuous(name = "Time from Admission (Months)", breaks=c(0:18)) + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100)) + 
  theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"), 
                     axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="none") 
## --------------------------------------------------------------------------



DATA$year.0 <- (DATA$month-1)/12


raneff_quad_rand<-lmer(rasch_FIM~ 
                         # Fixed-effects 
                         1+year.0+I(year.0^2)+ 
                         # Random-effects 
                         (1+year.0+I(year.0^2)|subID), data=DATA, REML=FALSE) 
anova(raneff_quad_rand)


summary(raneff_quad_rand)


cond_mod_01<-lmer(rasch_FIM~ 
                    # Fixed-effects 
                    1+year.0*AIS_grade+I(year.0^2)*AIS_grade+ 
                    # Random-effects 
                    (1+year.0+I(year.0^2)|subID), data=DATA, REML=FALSE)

anova(cond_mod_01)


summary(cond_mod_01)

