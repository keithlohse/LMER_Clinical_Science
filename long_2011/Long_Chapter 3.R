#Long Chapter 3: Data Structures and Longitudinal Analysis

library(ggplot2)
library(dplyr)
library(plyr)
library(tidyverse)
library(tidyr)

#Read in data
MPLS <- read.table("C:\\Users\\miller.allison\\Box\\Allison Miller\\Course Work and Training\\Longitudinal Data Analysis in R_Fall 2022\\MPLS.txt", header = TRUE, na.strings = "-99")
MPLS <- read.table(file.choose(), header = TRUE, na.strings = "-99")

#Inspect data and make corrections
head(MPLS)
tail(MPLS)
str(MPLS)

MPLS$ell2 <- factor(MPLS$ell, levels = c(0,1), labels = c("No", "Yes"))
str(MPLS)

MPLS$riskC[MPLS$risk == "HHM"] <- "DADV"
MPLS$riskC[MPLS$risk == "POV"] <- "DADV"
MPLS$riskC[MPLS$risk == "ADV"] <- "ADV"
MPLS$riskC <- factor(MPLS$riskC)
str(MPLS)

#Perform basic statistics
summary(MPLS)
summary(as.factor(MPLS$riskC))
table(MPLS$riskC)

mymeans <- colMeans(MPLS[,2:5], na.rm = TRUE)
mySDs <- sapply(MPLS[,2:5], sd, na.rm = TRUE)
mycor <- cor(MPLS[,2:5], use = "pairwise.complete.obs")
mycov <- cov(MPLS[,2:5], use = "pairwise.complete.obs")

#Missing data analysis
mymiss <- colMeans(is.na(MPLS[,2:5]))

#Examining descriptive statistics by static predictors
MPLS %>% group_by(MPLS$risk) %>% summarise(Mean = mean(read.5, na.rm = TRUE))
MPLS %>% group_by(MPLS$risk) %>% summarise(Mean = mean(read.6, na.rm = TRUE))
MPLS %>% group_by(MPLS$risk) %>% summarise(Mean = mean(read.7, na.rm = TRUE))
MPLS %>% group_by(MPLS$risk) %>% summarise(Mean = mean(read.8, na.rm = TRUE))

#A more efficient version of the above code (without writing out each column to compute the mean)
MPLS %>% group_by(MPLS$risk) %>% summarise_at(vars("read.5", "read.6", "read.7", "read.8"), mean, na.rm = TRUE)

#Perform the function above but based on a median split of attendance proportion
AttMed <- ifelse(MPLS$att > median(MPLS$att), 1, 0)
MPLS2 <- cbind(MPLS, AttMed)

MPLS2 %>% group_by(AttMed) %>% summarise_at(vars("read.5", "read.6", "read.7", "read.8"), mean, na.rm = TRUE)

#Reshape the data from wide to long format
MPLS.Long <- reshape(data = MPLS, varying = 2:5, idvar = "subid", v.names = "read",
                     timevar = "grade", times = 5:8, direction = "long")
head(MPLS.Long, n = 10)

MPLS %>% pivot_longer(cols = (read.5:read.8), names_to = "time", values_to = "score") %>%
  group_by(eth, time) %>%
  dplyr::summarize(Mean = mean(score, na.rm = TRUE),
            SD = sd(score, na.rm = TRUE),
            Count = n(),
            Missing = sum(is.na(score)),
            Complete = sum(!is.na(score)))

#Group data by participant and by grade
MPLS.Sorted <- MPLS.Long[order(MPLS.Long$subid, MPLS.Long$grade),]
rownames(MPLS.Sorted) <- NULL
head(MPLS.Sorted)

#Alternative using dplyr
MPLS.Sorted <- MPLS.Long %>% arrange(MPLS.Long$subid) %>% arrange(MPLS.Long$grade)

#Save long format data
save(MPLS.Sorted, file = "C:/Users/miller.allison/Box/Allison Miller/Course Work and Training/Longitudinal Data Analysis in R_Fall 2022/MPLS.Sorted.Rdata")
load(file = "C:\\Users\\miller.allison\\Box\\Allison Miller\\Course Work and Training\\Longitudinal Data Analysis in R_Fall 2022\\MPLS.Sorted.Rdata")

#Basic statistics in long format data
MPLS.Sorted %>% group_by(MPLS.Sorted$grade)%>% summarise_at(vars("read"), mean, na.rm = TRUE)

#Alternative
mylist <- split(MPLS.Sorted$read, MPLS.Sorted$grade)
myread <- do.call(cbind, mylist)
colnames(myread) <- paste("grade", as.character (5:8), sep = ".")

cov(myread, use = "pairwise.complete.obs")
cor(myread, use = "pairwise.complete.obs")

#Missing data statistics
MPLS.Sorted %>% group_by(MPLS.Sorted$grade)%>% summarise_each(funs(sum(is.na(.))/length(.)))

#Conditioning on Static Predictors
mgrge <- MPLS.Sorted %>% group_by(grade, gen, .add = TRUE)%>% summarise_at(vars("read"), mean, na.rm = TRUE)

medatt <- cut_number(MPLS.Sorted$att, n = 2)
mgrat <- cbind(MPLS.Sorted, medatt)
mgrat <- mgrat %>% group_by(grade, medatt, .add = TRUE) %>% summarise_at(vars("read"), mean, na.rm = TRUE)
mgrat$medatt <- factor(mgrat$medatt, labels = c("Low", "High"))

#Missing data in LMER
mgrge.miss <- MPLS.Sorted[,c("gen", "grade", "read")] %>% group_by(grade, gen, .add = TRUE)%>% summarise_each(funs(sum(is.na(.))/length(.)))
MPLS.Sorted.Sub <- subset(MPLS.Sorted, subid <4, select = c(subid, read, grade, gen))
omit1 <- na.omit(MPLS.Sorted.Sub)

MPLS.Sorted.Sub.NA <- MPLS.Sorted.Sub
MPLS.Sorted.Sub.NA[1:4, 4] <- NA


#Missing data simulation
set.seed(222)
mydat <- data.frame(risk2 = c(rep("DADV", 500), rep("ADV", 500)),
                    read.7 = sort(rnorm(1000, mean = 200, sd = 40)))
set.seed(333)
mydat$unif <- runif(1000, min = 0, max = 100)
head(mydat)


#MCAR
MCAR <- mydat
MCAR$read.7m <- MCAR$read.7
MCAR <- MCAR[order(MCAR$unif), ]
MCAR$read.7m[1:250] <- NA

#MCAR weighted mean
nomissMCAR <- na.omit(MCAR)
wt <- mean(nomissMCAR$read.7m)

#MCAR unweighted mean
mrgroups <- nomissMCAR %>% group_by(risk2)%>% summarise_at(vars("read.7m"), mean, na.rm = TRUE)
unwt <- mean(mrgroups$read.7m)

#Summarize MCAR results
myresults1 <- data.frame(complete = mean(MCAR$read.7), wt.mean = wt, unwt.mean = unwt)
rownames(myresults1) <- "MCAR"
round(myresults1, 1)

#MAR
MAR <- mydat
MAR$read.7m <- MAR$read.7
MAR <- MAR[order(MAR$risk2, -MAR$unif, decreasing = TRUE),]
MAR$read.7m[1:250] <- NA
head(MAR)

#MAR weighted mean
nomissMAR <- na.omit(MAR)
wt <- mean(nomissMAR$read.7m)

#MAR unweighted mean
mrgroups <- nomissMAR %>% group_by(risk2)%>% summarise_at(vars("read.7m"), mean, na.rm = TRUE)
unwt <- mean(mrgroups$read.7m)

#Summarize MAR results
myresults2 <- data.frame(complete = mean(MAR$read.7), wt.mean = wt, unwt.mean = unwt)
rownames(myresults2) <- "MAR"
round(myresults2, 1)


#NMAR
NMAR <- mydat
NMAR$read.7m <- NMAR$read.7
NMAR$read.7m[1:250] <- NA
head(NMAR)

#NMAR weighted mean
nomissNMAR <- na.omit(NMAR)
wt <- mean(nomissNMAR$read.7m)

#NMAR unweighted mean
mrgroups <- nomissNMAR %>% group_by(risk2)%>% summarise_at(vars("read.7m"), mean, na.rm = TRUE)
unwt <- mean(mrgroups$read.7m)

#Summarize NMAR results
myresults3 <- data.frame(complete = mean(NMAR$read.7), wt.mean = wt, unwt.mean = unwt)
rownames(myresults3) <- "NMAR"
round(myresults3, 1)


#Summarize MCAR, MAR, NMAR simulation results
misssum <- round(rbind(myresults1, myresults2, myresults3), 1)


#Multiple dynamic variables
MPLSwMath <- read.table("C:\\Users\\miller.allison\\Box\\Allison Miller\\Course Work and Training\\Longitudinal Data Analysis in R_Fall 2022\\MPLS with Math.txt", header = TRUE, na.strings = "-99")
MPLSwMatch.Long <- reshape(data = MPLSwMath, varying = list(2:5, 6:9), idvar = "subid",
                           v.names = c("read", "math"), timevar = "grade", times = 5:8,
                           direction = "long")
MPLSwMatch.Long <- MPLSwMatch.Long[order(MPLSwMatch.Long$subid, MPLSwMatch.Long$grade), ]
head(MPLSwMatch.Long, n = 8)

#Unbalanced data
MPLS.unbalanced <- read.table("C:\\Users\\miller.allison\\Box\\Allison Miller\\Course Work and Training\\Longitudinal Data Analysis in R_Fall 2022\\MPLS read unbalanced.txt", header = TRUE, na.strings = "-99")
MPLS.unbalanced.Long <- reshape(data = MPLS.unbalanced, varying = list(2:5, 6:9), idvar = "subid",
                           v.names = c("read", "grade"), timevar = "waves", times = 1:4,
                           direction = "long")
MPLS.unbalanced.Long <- MPLS.unbalanced.Long[order(MPLS.unbalanced.Long$subid, MPLS.unbalanced.Long$grade), ]
head(MPLS.unbalanced.Long, n = 8)

MPLS.unbalanced.wide <- reshape(data = MPLS.unbalanced.Long, idvar = "subid",
                                v.names = "read", timevar = "grade", drop = "waves",
                                direction = "wide")
