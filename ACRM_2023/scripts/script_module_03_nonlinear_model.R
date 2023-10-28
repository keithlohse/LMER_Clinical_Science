# Loading the essential libraries. 
library("ggplot2"); library("lme4"); 
library("car"); library("dplyr"); library("lmerTest");library("nlme")


# If these packages are not installed already, run the following code: 
install.packages("ggplot2"); install.packages("lme4"); 
install.packages("car"); install.packages("dplyr"); 
install.packages("lmerTest"); install.packages("nlme")

DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/LMER_Clinical_Science/master/ACRM_2018/data/data_session1.csv",
                 stringsAsFactors = TRUE, na.strings=c("NA","NaN"," ",""))

DATA$year <- DATA$month/12

# Adding Fixed-Effect to Make Conditional Models ---
ggplot(DATA, aes(x = year, y = rasch_FIM)) + 
  geom_line(aes(group=subID), col="grey") + 
  stat_smooth(method="loess", col="black", lwd=1.5, se=FALSE)+
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100)) +
  theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+ theme(legend.position="none")


# Let's create an example spline with a knot at 0.4 years.
DATA$spline04 <- ifelse(DATA$year>0.4, # Test
                        DATA$year-0.4, # Value if True
                        0) # Value if False

plot(y=DATA$spline04, x=DATA$year)


# Fitting a spline model
mod01 <- lmer(rasch_FIM~
                #Fixed Effects
                1+year+spline04+
                # Random Effects
                (1+year|subID), data=DATA, REML=FALSE)

anova(mod01)


MEAN <- data.frame(year=seq(from=0.0, to=1.5, by=0.1))
MEAN$spline04 <- ifelse(MEAN$year>0.4, # Test
                        MEAN$year-0.4, # Value if True
                        0) # Value if False
head(MEAN, 10)

MEAN$rasch_FIM <- 7.428+85.571*MEAN$year-69.697*MEAN$spline04
head(MEAN, 10)


# Plotting a Basic Spline model ---
ggplot(DATA, aes(x = year, y = rasch_FIM)) + 
  geom_line(aes(group=subID), col="grey") + 
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100)) +
  theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+ theme(legend.position="none")+
  geom_line(data=MEAN, aes(x=year, y=rasch_FIM), col="black", lwd=1.5)




# Write a for-loop to select best fitting spline ----
SPLINES <- data.frame()

# Because the loop works with integers, we will use the months variable rather 
# than the year variable to determine our "optimal" knot.

# Let's look at months 2 to 12
2:17
length(2:17)
length(1:16)

for(i in 1:16){
  print(i)
  
  # Create the spline
  DATA$spline <- ifelse(DATA$month>i+1, # Test
                        DATA$month-i+1, # Value if True
                        0) # Value if False
  
  
  # Define the model
  mod01 <- lmer(rasch_FIM~
                  #Fixed Effects
                  1+month+spline+
                  # Random Effects
                  (1+month|subID), data=DATA, REML=FALSE)
  
  
  # Store the AIC, other parameters can be added
  SPLINES[i, 1] <- i+1   
  SPLINES[i,2] <- AIC(mod01)
}

SPLINES

# Pick the row with lowest AIC
SPLINES[SPLINES$V2==min(SPLINES$V2),]
# Month 8 appears to be the best knot

# Plot the AIC as a function of knot location
ggplot(data=SPLINES, aes(x=V1, y=V2))+
  geom_line(col="black") + 
  geom_point(shape=1, size=1)+
  theme_bw()+
  scale_x_continuous(name="Knot Location") +
  scale_y_continuous(name="Model AIC")


# Modeling the Best Fitting Spline ----
8/12
DATA$spline66 <- ifelse(DATA$year>(8/12), # Test
                        DATA$year-(8/12), # Value if True
                        0) # Value if False

mod01 <- lmer(rasch_FIM~
                #Fixed Effects
                1+year+spline66+
                # Random Effects
                (1+year|subID), data=DATA, REML=FALSE)

anova(mod01)



MEAN <- data.frame(year=seq(from=0.0, to=1.5, by=0.1))
MEAN$spline66 <- ifelse(MEAN$year>(8/12), # Test
                        MEAN$year-(8/12), # Value if True
                        0) # Value if False
head(MEAN, 10)

MEAN$rasch_FIM <- 14.241+52.413*MEAN$year-42.533*MEAN$spline66
head(MEAN, 10)

ggplot(DATA, aes(x = year, y = rasch_FIM)) + 
  geom_line(aes(group=subID), col="grey") + 
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100)) +
  theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+ theme(legend.position="none") +
  geom_line(data=MEAN, aes(x=year, y=rasch_FIM), col="black", lwd=1)



# Adding Fixed-Effects of Group ----
mod02 <- lmer(rasch_FIM~
                #Fixed Effects
                1+year*AIS_grade+spline66*AIS_grade+
                # Random Effects
                (1+year|subID), data=DATA, REML=FALSE)

anova(mod01, mod02)




# Plotting the group level interaction
MEAN <- data.frame(year=rep(seq(from=0.0, to=1.5, by=0.1),3))

MEAN$spline66 <- ifelse(MEAN$year>(8/12), # Test
                        MEAN$year-(8/12), # Value if True
                        0) # Value if False
MEAN$AIS_grade <- factor(c(rep("C1-4", 16), 
                           rep("C5-8",16), 
                           rep("T1-S5", 16)))

fixef(mod02)


MEAN$rasch_FIM <- as.numeric(with(MEAN, ifelse(AIS_grade == "C1-4", # Test
                                               yes= 8.805181+46.956590*year-39.414498*spline66, 
                                               no = ifelse(AIS_grade=="C5-8", 
                                                           yes = (8.805181+6.309606)+(46.956590+7.519133)*year+
                                                             (-39.414498-4.493187)*spline66,
                                                           no = ifelse(AIS_grade=="T1-S5",
                                                                       yes=(8.805181+13.936953)+
                                                                         (46.956590+10.768901)*year+
                                                                         (-39.414498-5.625241)*spline66,
                                                                       no = "ERROR!")
                                               ))))

ggplot(DATA, aes(x = year, y = rasch_FIM)) + 
  geom_line(aes(group=subID), col="grey") + 
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100)) +
  facet_wrap(~AIS_grade) +
  theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+ theme(legend.position="none") + 
  geom_line(data=MEAN, aes(x=year, y=rasch_FIM, lty=AIS_grade), col="black", lwd=1.5)


anova(mod02)
summary(mod02)


ggplot(DATA, aes(x = year, y = rasch_FIM)) + 
  geom_line(aes(group=subID), col="grey") + 
  stat_smooth(method="loess", col="black", lwd=1.5, se=FALSE)+
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100)) +
  theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+ theme(legend.position="none")



set.seed(100)
neg_exp_rand_mod <- nlme(rasch_FIM ~ b_1i + 
                           (b_2)*(exp(b_3i * year)),
                         data = DATA,
                         fixed = b_1i + b_2 + b_3i ~ 1,
                         random = list(b_1i ~ 1, b_3i ~1),
                         groups = ~ subID,
                         start = c(80, -70, -1),
                         na.action = na.omit)
summary(neg_exp_rand_mod)


ggplot(DATA, aes(x = year, y = rasch_FIM)) + 
  geom_line(aes(group=subID), col="grey") + 
  stat_smooth(method="loess", col="black", lwd=1.5, se=FALSE)+
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100)) +
  facet_wrap(~AIS_grade)+
  theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+ theme(legend.position="none")


set.seed(100)
neg_exp_group_mod2 <- nlme(rasch_FIM ~ 
                             (b_1i) +(b_2)*(exp(b_3i * year)),
                           data = DATA,
                           fixed = list(b_1i ~ 1 + AIS_grade, 
                                        b_2 ~ 1 + AIS_grade,
                                        b_3i ~ 1+ AIS_grade),
                           random = list(b_1i ~ 1, b_3i ~1),
                           groups = ~ subID,
                           start = c(50, 15, 25, -35, -10, -15, -2, 0, 0),
                           na.action = na.omit)


anova(neg_exp_group_mod2)
summary(neg_exp_group_mod2)