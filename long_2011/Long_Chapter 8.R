##Long Chapter 8: Likelihood Ratio Text

library(tidyverse)
library(plyr)
library(lme4)
library(lattice)

load(file = "C:/Users/miller.allison/Box/Allison Miller/Course Work and Training/Longitudinal Data Analysis in R_Fall 2022/MPLS.Sorted.Rdata")

MPLS.Sorted$grade5 <- MPLS.Sorted$grade - 5

model.0 <- lmer(read ~ grade5 + (grade5 | subid), MPLS.Sorted, REML = FALSE)

model.1 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), MPLS.Sorted, REML = FALSE)

anova(model.0, model.1)

myout <- anova(model.0, model.1)

#Compute delta.aic
myout$delta.aic <- myout$Chisq - 2 * myout$Df

#Weight of evidence
myout$w.r <- exp(-.5 * myout$delta.aic) / (1 + exp(-.5 * myout$delta.aic))
myout$w.f <- 1 - myout$w.r

#Print excluding columns 3 and 4
myout[ ,-(3:4)]


#8.3.1. Calibrating p-values based on predictive accuracy
pchisq(q = 2, df = 1, lower.tail = FALSE)
pchisq(q = 8, df = 1, lower.tail = FALSE)

#Below is a case in which the full and reduced models differ by 1 parameter
mychi <- data.frame(chisq = seq(from = 0, to = 12, by = 0.05))
mychi$alpha <- pchisq(q = mychi$chisq, df = 1, lower.tail = FALSE)
mychi$delta.aic <- mychi$chisq - 2
mychi$w.r <- exp(-.5 * mychi$delta.aic) / (1 + exp(-.5 * mychi$delta.aic))
mychi$w.f <- 1 - mychi$w.r
head(mychi)
tail(mychi)

g1 <- ggplot(mychi, aes(x = w.f, y = alpha)) + geom_line(lwd = 1.5) + theme_bw()
g2 <- g1 + geom_hline(yintercept = c(0.01, 0.05), linetype = 2)
g3 <- g2 + geom_vline(xintercept = c(0.50, 0.90, 0.95), linetype = 2)
g4 <- g3 + scale_x_continuous(breaks = seq(0.20, 1, 0.05))
g5 <- g4 + scale_y_continuous(breaks = seq(0, 1, 0.05))
g6 <- g5 + xlab("Full model weight of evidence")
g7 <- g6 + ylab(expression(paste("Alpha")))
print(g7)       


#Below is a case in which the full and reduced models differ by 8 parameters
mychi <- data.frame(chisq = seq(from = 0, to = 30, by = 0.05))
mychi$alpha <- pchisq(q = mychi$chisq, df = 8, lower.tail = FALSE)
mychi$delta.aic <- mychi$chisq - 2 * 8
mychi$w.r <- exp(-.5 * mychi$delta.aic) / (1 + exp(-.5 * mychi$delta.aic))
mychi$w.f <- 1 - mychi$w.r
head(mychi)
tail(mychi)

g1 <- ggplot(mychi, aes(x = w.f, y = alpha)) + geom_line(lwd = 1.5) + theme_bw()
g2 <- g1 + geom_hline(yintercept = c(0.01, 0.05), linetype = 2)
g3 <- g2 + geom_vline(xintercept = c(0.50, 0.90, 0.95), linetype = 2)
g4 <- g3 + scale_x_continuous(breaks = seq(0, 1, 0.1))
g5 <- g4 + scale_y_continuous(breaks = seq(0, 1, 0.05))
g6 <- g5 + xlab("Full model weight of evidence")
g7 <- g6 + ylab(expression(paste("Alpha")))
print(g7)   


#8.5. Step-Up Approach

#Estimate models
model.0 <- lmer(read ~ grade5 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.1 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.2 <- lmer(read ~ grade5 + eth2 + riskC + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.3 <- lmer(read ~ grade5 + eth2 + riskC + grade5 * eth2 + (grade5 | subid),
                MPLS.Sorted, REML = FALSE)
model.4 <- lmer(read ~ grade5 + eth2 + riskC + grade5 * eth2 + grade5 * riskC +
                  (grade5 | subid), MPLS.Sorted, REML = FALSE)

#LRT
myout <- anova(model.0, model.1, model.2, model.3, model.4)

#Effect size
myout$delta.aic <- c(myout$Chisq - 2 * myout$Df)
myout$weight.f <- (1 - exp(-.5 * myout$delta.aic) / (1 + exp(-.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)

#Print
myout[ ,-c(3:4)]

#Printing the output of model.1 as it is the most complex full model accepted after evaluating the
#models in pairwise ascending fashion
print(model.1)

#8.5.1. Order of Testing
#The order of testing is crucial as different results can be obtained. Consider what happens when
#risk is entered before ethnicity.

#Estimate models
model.0 <- lmer(read ~ grade5 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.1 <- lmer(read ~ grade5 + riskC + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.2 <- lmer(read ~ grade5 + riskC + eth2 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.3 <- lmer(read ~ grade5 + riskC + eth2 + grade5 * eth2 + (grade5 | subid),
                MPLS.Sorted, REML = FALSE)
model.4 <- lmer(read ~ grade5 + riskC + eth2 + grade5 * eth2 + grade5 * riskC +
                  (grade5 | subid), MPLS.Sorted, REML = FALSE)

#LRT
myout <- anova(model.0, model.1, model.2, model.3, model.4)

#Effect size
myout$delta.aic <- c(myout$Chisq - 2 * myout$Df)
myout$weight.f <- (1 - exp(-.5 * myout$delta.aic) / (1 + exp(-.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
myout[ ,-c(3:4)]


#8.6. Top Down
model.1 <- lmer(read ~ grade5 + riskC + eth2 + grade5 * eth2 + grade5 * riskC +
                  (grade5 | subid), MPLS.Sorted, REML = FALSE)

round(coef(summary(model.1)), 2)

model.2 <- lmer(read ~ grade5 + riskC + eth2 + grade5 * eth2 + (grade5 | subid),
                MPLS.Sorted, REML = FALSE)

myout <- anova(model.2, model.1)
myout$delta.aic <- c(myout$Chisq - 2 * myout$Df)
myout$weight.f <- (1 - exp(-.5 * myout$delta.aic) / (1 + exp(-.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
myout[ ,-c(3:4)]

model.3 <- lmer(read ~ grade5 + riskC + eth2 + (grade5 | subid), MPLS.Sorted, REML = FALSE)

myout <- anova(model.3, model.2)
myout$delta.aic <- c(myout$Chisq - 2 * myout$Df)
myout$weight.f <- (1 - exp(-.5 * myout$delta.aic) / (1 + exp(-.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
myout[ ,-c(3:4)]

round(coef(summary(model.3)), 2)

model.4 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), MPLS.Sorted, REML = FALSE)

myout <- anova(model.4, model.3)
myout$delta.aic <- c(myout$Chisq - 2 * myout$Df)
myout$weight.f <- (1 - exp(-.5 * myout$delta.aic) / (1 + exp(-.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
myout[ ,-c(3:4)]

model.5 <- lmer(read ~ grade5 + (grade5 | subid), MPLS.Sorted, REML = FALSE)

myout <- anova(model.5, model.4)
myout$delta.aic <- c(myout$Chisq - 2 * myout$Df)
myout$weight.f <- (1 - exp(-.5 * myout$delta.aic) / (1 + exp(-.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
myout[ ,-c(3:4)]


#8.8. Parametric Bootstrap
#Consider a single bootstrap sample

#Estimate models
model.0 <- lmer(read ~ grade5 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.1 <- lmer(read ~ grade5 + riskC + (grade5 | subid), MPLS.Sorted, REML = FALSE)

#Simulate bootstrap sample data
simDV <- simulate(model.0)

#Fit the full and reduced models
b.full <- refit(model.1, simDV)
b.reduced <- refit(model.0, simDV)

#Compute bootstrap chi squared statistic
chisq.star <- deviance(b.reduced) - deviance(b.full)
chisq.star

#Write a function to perform more replications for bootstrapping
boot.func <- function(r, f){
  simDV <- simulate(r)
  b.full <- refit(f, simDV)
  b.reduced <- refit(r, simDV)
  chisq.star <- deviance(b.reduced) - deviance(b.full)
  return(chisq.star)
}

#Use rdply() to execute boot.func a large number of times
set.seed(101)
system.time(store.chisq.star <- rdply(.n = 999,
                                      .expr = boot.func(r = model.0, f = model.1),
                                      .progress = "text"))

head(store.chisq.star)
tail(store.chisq.star)

#The bootstrapped empirical distribution of X^2 can be used as a reference for the observed X^2
#Then there is a determination of how many bootstrapped X^2 values are greater than the observed
#X^2, the proportion of which yields a bootstrap LRT-p

#Compute observed chi squared
chisq.obs <- deviance(model.0) - deviance(model.1)
round(as.numeric(chisq.obs), 2)

#Compute bootstrap LRT-p. The mean function computes the proportion of cases above the observed
#X^2 value
mean(store.chisq.star[ ,2] > chisq.obs)


#8.9. Planning a study: determining the required sample size
#Define power.func()
power.func <- function(r, f, sample.rep, power.rep){
  power.results <- data.frame(matrix(ncol = 4, nrow = sample.rep))
  
  #sample size loop
  
  for(k in 1:sample.rep){
    
    #print to console
    
    cat("", "\n")
    cat(paste("Sample Size =", k*length(unique(r@frame[ ,ncol(r@frame)]))), "\n")
    
    #storage vector
    
    pvalue <- numeric(power.rep)
    
    #progress bar
    
    pb <- txtProgressBar(max = power.rep, style = 3)
    
    #power replications
    
    for(j in 1:power.rep){
      Sys.sleep(0.001); setTxtProgressBar(pb, j) #update progress bar
      
      ## simulate response data, put in vector
      
      simdv <- matrix(unlist(simulate(f, nsim = k)), ncol = 1)
      
      #simulate predictor data, renumbering subject ID
      
      mm <- NULL; mm1 <- NULL; c<- 0
      
      ##concatenate sample size
      
      for(i in 1:k) {
        mm1 <- f@frame
        mm1[ ,ncol(mm1)] <- as.integer(mm1[ ,ncol(mm1)] + c)
        c <- max(mm1[ ,ncol(mm1)])
        mm <- rbind(mm, mm1)
      }
      
      #run anova
      
      mm[ ,1] <- simdv
      s.full <- lmer(formula(f), mm, REML = F)
      s.reduced <- lmer(formula(r), mm, REML = F)
      pvalue[j] <- anova(s.reduced, s.full)[2,7]
      
      #progress bar cleanup
      
      Sys.sleep(.002)
      close(pb)
    }
    
    #Bootstrap sample size
    
    power.results[k,1] <- max(mm[ ,ncol(mm)])
    
    #power calculations for different alpha
    
    power.results[k,2] <- mean(pvalue <= 0.01)
    power.results[k,3] <- mean(pvalue <= 0.05)
    power.results[k,4] <- mean(pvalue <= 0.15)
    
    #clean up for console screen
    
    cat("", "\n")
    
  }
  
  cat("", "\n")
  cat("Finished", "\n")
  cat("", "\n")
  
  #return power results
  
  colnames(power.results) <- c("N", "alpha.01", "alpha.05", "alpha.15")
  return(power.results)
}
  

#Estimate models
model.0 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), MPLS.Sorted, REML = FALSE)
model.1 <- lmer(read ~ grade5 * eth2 + (grade5 | subid), MPLS.Sorted, REML = FALSE)

#power.func(r, f, sample.rep, power.rep):
#r = reduced model object
#f = full model object
#sub.rep = subject size replication (e.g., 10)
#power.rep = power replication (e.g., 999)

system.time(power.results <- power.func(model.0, model.1, 10, 999))


#Graph the estimated power as points and fit a smoother for each curve
g1 <- ggplot(power.results, aes(x = N, y = alpha.15)) + geom_point() + theme_bw()
g2 <- g1 + stat_smooth(se = FALSE) + ylab("Power")
g3 <- g2 + geom_point(aes(y = alpha.05)) + stat_smooth(aes(y = alpha.05), se = FALSE)
g4 <- g3 + geom_point(aes(y = alpha.01)) + stat_smooth(aes(y = alpha.01), se = FALSE)
g5 <- g4 + annotate("text", x = 45, y = .7, label = "alpha == .15", parse = TRUE)
g6 <- g5 + annotate("text", x = 115, y = .6, label = "alpha == .05", parse = TRUE)
g7 <- g6 + annotate("text", x = 145, y = .5, label = "alpha == .01", parse = TRUE)
g8 <- g7 + scale_x_continuous(breaks = seq(25, 225, 25))
print(g8)
