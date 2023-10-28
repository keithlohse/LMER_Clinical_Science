# Loading the essential libraries. 
library("ggplot2"); library("lme4"); 
library("car"); library("dplyr"); 
library("lmerTest");

# If these packages are not installed already, run the following code: 
install.packages("ggplot2"); install.packages("lme4"); 
install.packages("car"); install.packages("dplyr"); 
install.packages("lmerTest");

## ----------------------- Basic Data Visualization ------------------------- 
## FIM scores by group and time -------------------------------------------- 
ggplot(DATA, aes(x = month, y = rasch_FIM)) + 
  geom_line(aes(group=subID)) + 
  geom_point(aes(fill=as.factor(subID)), pch=21, size=1, stroke=1) + 
  facet_wrap(~AIS_grade) +
  scale_x_continuous(name = "Time from Admission (Months)", breaks=c(0:18)) + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100)) + 
  theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"), 
                     axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="none") 
## --------------------------------------------------------------------------


## Linear Fit for each person ----------------------------------------------- 
ggplot(DATA, aes(x = month, y = rasch_FIM)) + 
  geom_point(aes(fill=as.factor(subID)), pch=21, size=1, stroke=0.5) + 
  stat_smooth(aes(col=subID), se=FALSE, method="lm", lwd=0.5) + 
  stat_smooth(aes(group=AIS_grade), col="black", se=FALSE, method="lm", lwd=1) + 
  facet_wrap(~AIS_grade) +
  scale_x_continuous(name = "Time from Admission (Months)", breaks=c(0:18)) + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100)) + 
  theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"), 
                     axis.title=element_text(size=12,face="bold")) + 
  theme(legend.position="none") 
## --------------------------------------------------------------------------


ggplot(DATA, aes(x = month, y = rasch_FIM)) + 
  geom_point(aes(fill=as.factor(subID)), pch=21, size=1, stroke=0.5) + 
  stat_smooth(aes(col=subID), se=FALSE, method="lm", formula=y~x+I(x^2), lwd=0.5) + 
  stat_smooth(aes(group=AIS_grade), col="black", se=FALSE, method="lm", 
              formula=y~x+I(x^2), lwd=1) +
  facet_wrap(~AIS_grade)+
  scale_x_continuous(name = "Time from Admission (Months)", breaks=c(0:18)) + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100)) + 
  theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+ theme(legend.position="none") 


DATA$year.0 <- (DATA$month-1)/12

raneff_int<-lmer(rasch_FIM~ 
                   # Fixed-effects 
                   1+ 
                   # Random-effects 
                   (1|subID), data=DATA, REML=FALSE)

summary(raneff_int)

# Recall that the fixed-effect for the intercept is the overall, "group-level" 
# intercept in our model. However, we also have a random-effect of subject for 
# the intercept. This means that our model estimates a deviate for each subject 
# from the group-level intercept. To see these random-effects using the raneff() 
# function.
ranef(raneff_int) 

# Remember that these are deviates from the fixed-effect, so if we want to see 
# what the model is actually estimating for each person, we need to add the 
# fixed-effect back in: 
fixef(raneff_int) 

# We could do this manually, by adding the fixef() output to the ranef() output, 
# but we can also get the individual values using the coef() function: 
coef(raneff_int)$subID 

# If you want the actual predictions of the model, rather than the estimated 
# effects, you can use the fitted() function. Note the difference in the size 
# of these arrays. The fitted() function gives us a prediction for each person 
# at each point. 
fitted(raneff_int)



first10<-DATA[c(1:180),] 
# Second, we'll make a smaller dataset with the predictions for these 10: 
PRED<-data.frame(
  subID=c("s01","s02","s03","s04","s05","s06","s07","s08","s09", "s10"),
  Intercepts=c(coef(raneff_int)$subID[c(1:10),])) 
PRED


ggplot(first10, aes(x = year.0, y = rasch_FIM)) + 
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) + 
  geom_line(aes(group=subID)) + facet_wrap(~subID, ncol=5)+
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))+
  geom_abline(aes(intercept=Intercepts, slope=0), col="red", lwd=1.5, PRED)


raneff_slope<-lmer(rasch_FIM~ 
                     # Fixed-effects 
                     1+year.0+ 
                     # Random-effects 
                     (1|subID), data=DATA, REML=FALSE) 
summary(raneff_slope)



PRED<-data.frame(
  subID=c("s01","s02","s03","s04","s05","s06","s07","s08","s09", "s10"),
  Intercepts=c(coef(raneff_slope)$subID[c(1:10),1]), Slopes=c(coef(raneff_slope)$subID[c(1:10),2])) 
PRED

ggplot(first10, aes(x = year.0, y = rasch_FIM)) + 
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) + 
  geom_line(aes(group=subID)) + facet_wrap(~subID, ncol=5)+
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))+
  geom_abline(aes(intercept=Intercepts, slope=Slopes), col="red", lwd=1.5, PRED) 



raneff_lin_rand<-lmer(rasch_FIM~ 
                        # Fixed-effects 
                        1+year.0+ 
                        # Random-effects 
                        (1+year.0|subID), data=DATA, REML=FALSE) 
summary(raneff_lin_rand)



PRED<-data.frame(
  subID=c("s01","s02","s03","s04","s05","s06","s07","s08","s09", "s10"),
  Intercepts=c(coef(raneff_lin_rand)$subID[c(1:10),1]),
  Slopes=c(coef(raneff_lin_rand)$subID[c(1:10),2])) 
PRED

ggplot(first10, aes(x = year.0, y = rasch_FIM)) + 
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) + 
  geom_line(aes(group=subID)) + facet_wrap(~subID, ncol=5)+
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))+
  geom_abline(aes(intercept=Intercepts, slope=Slopes), col="red", lwd=1.5, PRED) 



raneff_quad_rand<-lmer(rasch_FIM~ 
                         # Fixed-effects 
                         1+year.0+I(year.0^2)+ 
                         # Random-effects 
                         (1+year.0+I(year.0^2)|subID), data=DATA, REML=FALSE) 
summary(raneff_quad_rand)



DATA$quad_pred <- fitted(raneff_quad_rand) # Save model predictions to data frame
first10<-DATA[c(1:180),] # Second, we'll make a smaller dataset with the predictions for these 10: 


ggplot(first10, aes(x = year.0, y = rasch_FIM)) + 
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) + 
  geom_line(aes(group=subID)) + facet_wrap(~subID, ncol=5)+
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))+
  geom_line(aes(y=quad_pred, col=subID), lwd=1.5) 