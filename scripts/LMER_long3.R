## Chapter 3 R code from Long (2012)
### Adapted by Keith Lohse, PhD (2016)

# Installing Packages -------------------------------------------------
wants <- c("AICcmodavg", "lme4", "lmerTest", "nlme", "ggplot2","plyr", "dplyr")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

library("ggplot2");library("lme4"); library("plyr"); library("dplyr");
library("nlme");library('lmerTest');library("AICcmodavg")


##-------------------------------------------------------------
# Reading data into R

getwd()
# should be "C:/Users/krl0022/Documents" or similar

# we can then change the working directory to a specific folder for our project
setwd("C:/Currant/LMER_reading_group/")
# let's see what is in the data folder
list.files("C:/Currant/LMER_reading_group/data")
#

# The MPLS data file should be saved in the 'data' folder in your directory
MPLS<-read.table("./data/MPLSdata.txt", header = TRUE, sep="\t", na.string="-99") 
tail(MPLS)

# We can also use "file.choose()" to get a popup window
MPLS <- read.table(file.choose(), header = TRUE, na.strings = "-99")

# We could also specify separate file formats with 'sep = ' or 'read.csv()'.
## MPLS <- read.table("C:\\Mine\\MPLSdata.txt", header = TRUE, na.strings = "-99", sep = ",")
## MPLS <- read.csv("C:\\Mine\\MPLSdata.txt", header = TRUE, na.strings = "-99")

head(MPLS)
tail(MPLS)

# MPLS is a matrix, and we can chop it up into parts
MPLS[1:2,1:2]
MPLS[1:5, ]
# we can also use 'str()' to look at the structure of an object
str(MPLS)
# One thing to note is that 'ell' is a categorical variable, but it is coded numerically
# We can convert 'ell' to a categorical using the factor() function
MPLS$ell2 <- factor(MPLS$ell, levels = c(0, 1), labels = c("No", "Yes"))
str(MPLS)

# Creating a contrast code
MPLS$ellc<-(as.numeric(MPLS$ell2)-1.5)*2
MPLS$ellc

# We can also assign new factors based on existing variables
# One easy way to do this is using subseting: [DF$variable == "instance"]
MPLS$riskC[MPLS$risk == "HHM"] <- "DADV" 
# Note "==" rather than "=" .
MPLS$riskC[MPLS$risk == "POV"] <- "DADV"
# Both risk = POV and risk = HHM are coded as riskC=DADV
MPLS$riskC[MPLS$risk == "ADV"] <- "ADV"
MPLS$riskC
# RiskC currently exists as independent characters, we need to gather them up into factors
MPLS$risk2 <- factor(MPLS$riskC)

str(MPLS)
summary(MPLS)

##---------------------------------------------------
# Descriptive Statistics
mean(MPLS$read.8, na.rm=TRUE)
mymeans <- mean(MPLS[ ,2:5], na.rm = TRUE)
mymeans
# Does Long's code work anymore?
# Try the colMeans() function instead
colMeans(MPLS[ ,c(2,5)], na.rm=TRUE)
mean(MPLS[ ,5], na.rm = TRUE)
# We can also do this more elegantly using other functions below

# Long's SD code did not work for me either
# mysds <- sd(MPLS[ ,2:5], na.rm = TRUE)
# mysds

# We can, however get correlation matrices...
table1<-cor(MPLS[ ,c(2,3,5)], use = "complete.obs")
table1
# .. and covariance matrices
cov(MPLS[ ,2:5], use = "complete.obs")
# again, we are subsetting MPLS to take ALL rows, but only columns 2 through 5

mymiss <- colSums(is.na(MPLS[ ,2:5]))
mean(is.na(MPLS$read.8))
# Note that Long has colMeans here, but I think that colSums is more helpful
# mymiss <- colMeans(is.na(MPLS[ ,2:5]))
mymiss

# What if I want means and standard deviations for my different groups?
# Sadly, Long's code was not working for me here either...
# ... I suspect because it was written using plyr when dplyr is more current
# Either way below is what Long says you can do:
# library("plyr"); library("dplyr")
# myrisk <- ddply(.data = MPLS[ ,2:5], .variables = .(MPLS$risk),
#                .fun = mean, na.rm = TRUE)
# Although you can make it work in plyr
ddply(MPLS,~gen,summarise,
      sd5=sd(read.5, na.rm=TRUE),
      mean6=mean(read.6, na.rm=TRUE),
      mean7=mean(read.7, na.rm=TRUE),
      mean8=mean(read.8, na.rm=TRUE))
?ddply()

# but in dplyr it looks like:
by_risk<-summarize(group_by(MPLS,risk), 
          mean5 = mean(read.5, na.rm=TRUE),
          mean6 = mean(read.6, na.rm=TRUE),
          mean7 = mean(read.7, na.rm=TRUE),
          mean8 = mean(read.8, na.rm=TRUE))

by_risk<-as.data.frame(by_risk)
by_risk
# at first this might seem like more work in dplyr, but trust me
# once we learn about 'piping' dplyr is oodles more efficient and effective


##---------------------------------------------------
# Reshaping Data

# The MPLS data is currently in wide format, suppose we want it in long format?
MPLS.L <- reshape(data = MPLS, varying = 2:5, v.names = "read", timevar = "grade",
                  times = 5:8, idvar = "subid", direction = "long")
head(MPLS.L, n = 10)
head(MPLS)

# As you can see the MPLS.L is grouped by grade
# What if we want all of the data from one subject together in order?
# (i.e., group by subject then by grade)
MPLS.LS <- MPLS.L[order(MPLS.L$subid, MPLS.L$grade), ]
head(MPLS.LS, n = 10)
# Note that the row names (far left column) are a wierd concatenation
# We can remove these row nows with:
rownames(MPLS.LS) <- NULL

tail(MPLS.LS)
MPLS.LS <- subset(MPLS.LS, select = -risk)
save(MPLS.LS, file = "./data/MPLS.LS.Rdata")
load(file = "./data/MPLS.LS.Rdata")

write.csv(MPLS.LS, file="./data/MPLS.LS.csv")


# Sometimes we will have data in long format and want it back in wide format
# We can also go from long to wide format using the reshape function
MPLS.W <- reshape(data = MPLS.LS, v.names = "read", idvar = "subid",
                  drop = c("riskC", "ell2"), timevar = "grade", direction = "wide")
MPLS.W
save(MPLS.W, file = "./data/MPLS.W.Rdata")
load(file = "./data/MPLS.W.Rdata")

##---------------------------------------------------
# Descriptive Statistics in Long Format
# Before we calculated means by risk now lets do it longitudinally by grade:
tail(MPLS.LS)
by_grade<-summarize(group_by(MPLS.LS,grade),
                    mean = mean(read, na.rm=TRUE))

by_grade<-as.data.frame(by_grade)
by_grade

# And now we can efficiently get additional information:
by_grade<-summarize(group_by(MPLS.LS,grade),
                    mRead = mean(read, na.rm=TRUE),
                    sdRead = sd(read, na.rm=TRUE),
                    countRead = n(),
                    complete = sum(!is.na(read)))

by_grade<-as.data.frame(by_grade)
by_grade
# Neat!
# Also note that if we wanted to change the column names in 'by_grade'
# we could use the colnames() function.
?colnames()

# For instance, assume that we wanted the correlation between the different grade levels
# Fist we need to reformat our data
mylist <- split(MPLS.LS$read, MPLS.LS$grade)
mylist
# The split function (from the base package) splits the reading scores according to 
# our time variable, in this case "grade".
str(mylist)
# We can then use do.call() to bind this list back together into a dataframe
myread <- do.call(cbind, mylist)
myread
# and then we can use the colnames() function to rename columns in myread
colnames(myread) <- paste("grade", as.character(5:8), sep = ".")
myread
# Correlation and covariance matrices are now super easy to calculate!
cov(myread, use = "pairwise.complete.obs")
cor(myread, use = "pairwise.complete.obs")

##---------------------------------------------------
# Missing Data
# It is often important for us to understand how much data is missing 
# and how/why it is missing. For instance, data could be:
## 1. Missing Completely at Random
## 2. Missing at Random
## 3. Not missing at Random

# As above, Long's code no long seems to work in current R/plyr...
ddply(.data = data.frame(is.na(MPLS.LS$read)), .variables = .(grade = MPLS.LS$grade),
      each(prop.miss = mean))

# But I have rewritten working code here:
head(MPLS.LS)
ddply(MPLS.LS,~grade,summarise,
      missing=sum(is.na(read)))

# Similarly, we might want to condition our calculation on multiple predictors.
# Again, I can't get Long's original code to work...
mgrge <- ddply(data.frame(MPLS.LS$read), .(grade = MPLS.LS$grade, gender = MPLS.LS$gen),
               each(mean.read = mean), na.rm = TRUE)
mgrge

# ... but here is a functional rewrite:
head(MPLS.LS)
ddply(MPLS.LS,~grade+gen,summarise,
      meanRead=mean(read, na.rm=TRUE))

# We can also condition or calculations on variables we create.
# For instance, we can use the cut_number() function from ggplot2
# to create a "median split" of the attendance variable:
MPLS.LS$att.split<-cut_number(MPLS.LS$att, n=2)
head(MPLS.LS)
mgrat <- ddply(MPLS.LS,~grade+att.split,summarise,
               meanRead=mean(read, na.rm=TRUE))
mgrat

# If we don't like R's automatic levels from the cut_number() function,
# We can assign our own labels to that factors
mgrat$att.split <- factor(mgrat$att.split, labels = c("Low", "High"))
mgrat

# In longitudinal data, especially clinical or "non-experimental" data,
# you are likekly to have a lot of missing data ('NAs').
# It is important for us to understand where these NAs are missing, and
# how different functions in R handle NAs.

# One of the strengthes of the LMER approach, compared to RM ANOVA,
# is that LMER can glide seemlessly over missing values, fitting lines
# based on the data available for each subject. 
# However, this is only true for the outcome variable (i.e., reading scores
# over time). If a subject is missing a value for a 'Sex' variable and sex
# is one of the variables we are trying to model, that subject will be 
# dropped from our analysis. 

# In some cases, we can control when/where we encounter NAs. As we have 
# seen, we can use the 'na.rm =' argument in a function. We can also use
# the na.omit() function to strip ROWS THAT HAVE NAS out of a data frame:
MPLS.LS3 <- subset(MPLS.LS, subid < 4, select = c(subid, read, grade, gen))
MPLS.LS3
# Note that subject 2 is missing a reading score in grade 8

omit1 <- na.omit(MPLS.LS3)
omit1
# Using na.omit() removes all grade information for subject 2

# Now let's suppose that we are missing gender information for subject 1
MPLS.LS3a <- MPLS.LS3
MPLS.LS3a[1:4,4] <- NA
MPLS.LS3a

# Using the na.omit() function here mimics how LMER will treat a subject
# who is missing a static variable:
omit2 <- na.omit(MPLS.LS3a)
omit2
# As you can see, subject 1's data is dropped completely.

# It is important to understand how LMER (and R more generally) handle
# missing cases. 
# The reason understanding missing cases is so important is that the 
# statistical modelling procedures we will use ASSUME that all models 
# are based on the same amount of data. 
# Therefore, if data are used in one model, but missing in another model, 
# this means those two models cannot be compared.
#
# Consider:
# Model 1: Reading ~ Grade 
# Model 2: Reading ~ Grade + Sex
# If even one subject is missing a value for 'Sex', that means that 
# Model 2 is based on less data and, therefore, any model comparison is
# not going to be valid.


##---------------------------------------------------
# More on Missing Data
# Below are Long's codes for generating missing data to varying degress
# of randomness. I have not gotten round to commenting/updating this code yet.
set.seed(222)
mydat <- data.frame(risk2 = c(rep("DADV", 500), rep("ADV", 500)),
                    read.7 = sort(rnorm(1000, mean = 200, sd = 40)))
mydat

set.seed(333)
mydat$unif <- runif(1000, min = 0, max = 100)
head(mydat)

MCAR <- mydat # Copy data frame.
MCAR$read.7m <- MCAR$read.7 # Copy complete variable.
MCAR <- MCAR[order(MCAR$unif), ] # Sort by random numbers.
MCAR$read.7m[1:250] <- NA # Assign NAs.

wt <- mean(MCAR$read.7m, na.rm = TRUE)
wt
unwt <- ddply(data.frame(MCAR$read.7m), .(MCAR$risk2), each(Mean = mean), na.rm = TRUE)
unwt <- mean(unwt$Mean)
myresults1 <- data.frame(complete = mean(MCAR$read.7), wt.mean = wt, unwt.mean = unwt)
rownames(myresults1) <- "MCAR"
round(myresults1, 1)

MAR <- mydat
MAR$read.7m <- MAR$read.7
## Sort by risk2 (decreasing) and unif (increasing).
MAR <- MAR[order(MAR$risk2, -MAR$unif, decreasing = TRUE), ]
MAR$read.7m[1:250] <- NA
head(MAR)

wt <- mean(MAR$read.7m, na.rm = TRUE)
unwt <- ddply(data.frame(MAR$read.7m ), .(MAR$risk2), each(Mean = mean), na.rm = TRUE)
unwt <- mean(unwt$Mean)
myresults2 <- data.frame(complete = mean(MAR$read.7), wt.mean = wt, unwt.mean = unwt)
rownames(myresults2) <- "MAR"
round(myresults2, 1)

NMAR <- mydat
NMAR$read.7m <- NMAR$read.7
NMAR$read.7m[1:250] <- NA
head(NMAR)

wt <- mean(NMAR$read.7m, na.rm = TRUE)
unwt <- ddply(data.frame(NMAR$read.7m), .(NMAR$risk2), each(Mean = mean), na.rm = TRUE)
unwt <- mean(unwt$Mean)
myresults3 <- data.frame(complete = mean(NMAR$read.7), wt.mean = wt, unwt.mean = unwt)
rownames(myresults3) <- "NMAR"
round(myresults3, 1)
round(rbind(myresults1, myresults2, myresults3), 1)

## Tailor for your own system:
## MPLSc1 <- read.table("C:\\Mine\\MPLScomp1.txt", header = TRUE, na.strings = c("-99"))
MPLSc1.L <- reshape(data = MPLSc1, varying = list(2:5, 6:9), idvar = "subid",
                    v.names = c("read","math"), timevar = "grade", times = 5:8, direction = "long")
MPLSc1.L <- MPLSc1.L[order(MPLSc1.L$subid, MPLSc1.L$grade), ]
head(MPLSc1.L, n = 8)

## Tailor for your own system:
## MPLSc2 <- read.table("C:\\Mine\\MPLScomp2.txt", header = TRUE, na.strings = c("-99"))
MPLSc2.L <- reshape(data = MPLSc2, varying = list(2:5, 6:9), idvar = "subid",
                    v.names = c("read", "grade"), timevar = "waves", times = 1:4, direction = "long")
MPLSc2.L <- MPLSc2.L[order(MPLSc2.L$subid, MPLSc2.L$grade), ]
head(MPLSc2.L, n = 8)

MPLSc2.W <- reshape(data = MPLSc2.L, v.names = "read", idvar = "subid",
                    timevar = "grade", direction = "wide", drop = "waves")
MPLSc2.W
