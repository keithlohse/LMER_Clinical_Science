## Chapter 8 R code.

require(ggplot2)
require(lme4)
require(AICcmodavg)

##

model.0 <- lmer(read ~ grade5 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.1 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
anova(model.0, model.1)

myout <- anova(model.0, model.1)
## Compute delta.aic.
myout$delta.aic <- myout$Chisq - 2 * myout$"Chi Df"
## Weight of evidence.
myout$w.r <- exp(-.5 * myout$delta.aic) / (1 + exp(-.5 * myout$delta.aic))
myout$w.f <- 1 - myout$w.r
## Print excluding columns 3 and 4.
myout[ ,-(3:4)]

pchisq(q = 2, df = 1, lower.tail = FALSE)
pchisq(q = 8, df = 1, lower.tail = FALSE)

mychi <- data.frame(chisq = seq(from = 0, to = 12, by = 0.05))
mychi$alpha <- pchisq(q = mychi$chisq, df = 1, lower.tail = FALSE)
mychi$delta.aic <- mychi$chisq - 2
mychi$w.r <-  exp(-.5 * mychi$delta.aic) / (1 + exp(-.5 * mychi$delta.aic))
mychi$w.f <- 1 - mychi$w.r
head(mychi)
tail(mychi)

g1 <- ggplot(mychi, aes(x = w.f, y = alpha)) + geom_line(lwd = 1.5) + theme_bw()
g2 <- g1 + geom_hline(yintercept = c(0.01, 0.05), linetype = 2)
g3 <- g2 + geom_vline(xintercept = c(0.50, 0.90, 0.95), linetype = 2)
g4 <- g3 + scale_x_continuous(breaks = seq(0.20, 1, 0.05))
g5 <- g4 + scale_y_continuous(breaks = seq(0, 1, 0.05))
g6 <- g5 + xlab("Full model weight of evidence")
g7 <- g6 + ylab(expression(paste("Alpha (", alpha, ")")))
print(g7)

mychi <- data.frame(chisq = seq(from = 0, to = 30, by = 0.05))
mychi$alpha <- pchisq(q = mychi$chisq, df = 8, lower.tail = FALSE)
mychi$delta.aic <- mychi$chisq - 2 * 8
mychi$w.r <-  exp(-0.5 * mychi$delta.aic) / (1 + exp(-0.5 * mychi$delta.aic))
mychi$w.f <- 1 - mychi$w.r
head(mychi)
tail(mychi)

g1 <- ggplot(mychi, aes(x = w.f, y = alpha)) + geom_line(lwd = 1.5) + theme_bw()
g2 <- g1 + geom_hline(yintercept = c(0.01, 0.05), linetype = 2)
g3 <- g2 + geom_vline(xintercept = c(0.5, 0.90, 0.95), linetype = 2)
g4 <- g3 + scale_x_continuous(breaks = seq(0, 1, 0.1))
g5 <- g4 + scale_y_continuous(breaks = seq(0, 1, 0.05))
g6 <- g5 + xlab("Full model weight of evidence")
g7 <- g6 + ylab(expression(paste("Alpha (", alpha, ")")))
print(g7)

## Estimate models.
model.0 <- lmer(read ~ grade5 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.1 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.2 <- lmer(read ~ grade5 + eth2 + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.3 <- lmer(read ~ grade5 + eth2 + risk2 + grade5 * eth2 +
                (grade5 | subid), MPLS.LS, REML = FALSE)
model.4 <- lmer(read ~ grade5 + eth2 + risk2 + grade5 * eth2 + grade5 * risk2 +
                (grade5 | subid), MPLS.LS, REML = FALSE)
## LRT.
myout <- anova(model.0, model.1, model.2, model.3, model.4)
## Effect size.
myout$delta.aic <- c(myout$Chisq - 2 * myout$"Chi Df")
myout$weight.f <- (1 - exp(-0.5 * myout$delta.aic) / (1 + exp(-0.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
## Print.
myout[ ,-c(3:4)]

print(model.2, cor = FALSE)

## Estimate models.
model.0 <- lmer(read ~ grade5 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.1 <- lmer(read ~ grade5 + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.2 <- lmer(read ~ grade5 + risk2 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.3 <- lmer(read ~ grade5 + risk2 + eth2 + grade5 * eth2 +
                (grade5 | subid), MPLS.LS, REML = FALSE)
model.4 <- lmer(read ~ grade5 + risk2 + eth2 + grade5 * risk2 + grade5 * eth2 +
                (grade5 | subid), MPLS.LS, REML = FALSE)
## LRT.
myout <- anova(model.0, model.1, model.2, model.3, model.4)
## Effect size.
myout$delta.aic <- c(myout$Chisq - 2 * myout$"Chi Df")
myout$weight.f <- (1 - exp(-0.5 * myout$delta.aic) / (1 + exp(-0.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
myout[ ,-c(3:4)]

model.1 <- lmer(read ~ grade5 * risk2 + grade5 * eth2 + (grade5 | subid),
                MPLS.LS, REML = FALSE)
round(summary(model.1)@coefs, 2)

model.2 <- lmer(read ~ grade5 + risk2 + grade5 * eth2 + (grade5 | subid),
                MPLS.LS, REML = FALSE)
myout <- anova(model.2, model.1)
myout$delta.aic <- c(myout$Chisq - 2 * myout$"Chi Df")
myout$weight.f <- (1 - exp(-0.5 * myout$delta.aic) / (1 + exp(-0.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
myout[ ,-c(3:4)]

model.3 <- lmer(read ~ grade5 + risk2 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
myout <- anova(model.3, model.2)
myout$delta.aic <- c(myout$Chisq - 2 * myout$"Chi Df")
myout$weight.f <- (1 - exp(-0.5 * myout$delta.aic) / (1 + exp(-0.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
myout[ ,-c(3:4)]

round(summary(model.3)@coefs, 2)

model.4 <- lmer(read ~ grade5 + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
myout <- anova(model.4, model.3)
myout$delta.aic <- c(myout$Chisq - 2 * myout$"Chi Df")
myout$weight.f <- (1 - exp(-0.5 * myout$delta.aic) / (1 + exp(-0.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
myout[ ,-c(3:4)]

model.5 <- lmer(read ~ grade5 + (grade5 | subid), MPLS.LS, REML = FALSE)
myout <- anova(model.5, model.4)
myout$delta.aic <- c(myout$Chisq - 2 * myout$"Chi Df")
myout$weight.f <- (1 - exp(-0.5 * myout$delta.aic) / (1 + exp(-0.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
myout[ ,-c(3:4)]

## Estimate models.
model.0 <- lmer(read ~ grade5 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.1 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
## Simulate bootstrap sample data.
simDV <- simulate(model.0)
## Fit full and reduced models.
b.full <- refit(model.1, simDV[ ,1])
b.reduced <- refit(model.0, simDV[ ,1])
## Compute bootstrap chi-squared statistic.
chisq.star <- deviance(b.reduced) - deviance(b.full)
chisq.star

boot.func <- function(r, f){              # r (reduced), f (full).
 simDV <- simulate(r)
 b.full <- refit(f, simDV[ ,1])
 b.reduced <- refit(r, simDV[ ,1])
 chisq.star <- deviance(b.reduced) - deviance(b.full)
 return(chisq.star)}

set.seed(101)                           # Allows reader to replicate results.
system.time(store.chisq.star <- rdply(.n = 999,
                                      .expr = boot.func(r = model.0, f = model.1),
                                      .progress = "text"))

head(store.chisq.star)

## Compute observed chi-squared.
chisq.obs <- deviance(model.0) - deviance(model.1)
round(as.numeric(chisq.obs), 2)
## Compute bootstrap LRT-p.
mean(store.chisq.star[ ,2] > chisq.obs)

############################################################
## power.func()
############################################################
power.func <- function(r, f, sample.rep, power.rep){
power.results <- data.frame(matrix(ncol = 4, nrow = sample.rep))
## Sample size loop.
for(k in 1:sample.rep){
## Print to console.
cat("", "\n")
cat(paste("Sample Size =", k*length(unique(r@frame[,ncol(r@frame)]))), "\n")
## Storage vector.
pvalue <- numeric(power.rep)
## Progress bar.
pb <- txtProgressBar(max = power.rep, style=3)
## Power replications.
for(j in 1:power.rep){
Sys.sleep(0.001); setTxtProgressBar(pb, j) # Update progress bar.
## Simulate response data, put in vector.
simdv <- matrix(unlist(simulate(f, nsim = k)), ncol = 1)
## Simulate predictor data, renumbering subject ID.
mm <- NULL; mm1 <- NULL; c <- 0
## Concatenate sample size.
for(i in 1:k) {
mm1 <- f@frame
mm1[ ,ncol(mm1)] <- as.integer(mm1[ ,ncol(mm1)] + c)
c <- max(mm1[ ,ncol(mm1)])
mm <- rbind(mm, mm1)
}
## Run anova().
mm[ ,1] <- simdv
s.full <- lmer(formula(f), mm, REML = F)
s.reduced <- lmer(formula(r), mm, REML = F)
pvalue[j] <- anova(s.reduced, s.full)[2,7]
## Progress bar cleanup.
Sys.sleep(.002)
close(pb)
}
## Bootstrap sample size.
power.results[k,1] <- max(mm[ ,ncol(mm)])
## Power calculations for different alpha.
power.results[k,2] <- mean(pvalue <= .01)
power.results[k,3] <- mean(pvalue <= .05)
power.results[k,4] <- mean(pvalue <= .15)
## Clean up for console screen.
cat("", "\n")
}
cat("", "\n")
cat("Finished", "\n")
cat("", "\n")
## Return power results.
colnames(power.results) <- c("N", "alpha.01", "alpha.05", "alpha.15")
return(power.results)
}

## Estimate models.
model.0 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.1 <- lmer(read ~ grade5 * eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
#######################################################
## power.func(r, f, sample.rep, power.rep):
## r = reduced model object
## f = full model object
## sub.rep = subject size replication (e.g., 10)
## pow.rep = power replication (e.g., 999).
#######################################################
system.time(power.results <- power.func(model.0, model.1, 10, 999))

g1 <- ggplot(power.results, aes(x = N, y = alpha.15)) + geom_point() + theme_bw()
g2 <- g1 + stat_smooth(se = FALSE) + ylab("Power")
g3 <- g2 + geom_point(aes(y = alpha.05)) + stat_smooth(aes(y = alpha.05), se = FALSE)
g4 <- g3 + geom_point(aes(y = alpha.01)) + stat_smooth(aes(y = alpha.01), se = FALSE)
g5 <- g4 + annotate("text", x = 45,  y = .7, label = "alpha == .15", parse = TRUE)
g6 <- g5 + annotate("text", x = 115, y = .6, label = "alpha == .05", parse = TRUE)
g7 <- g6 + annotate("text", x = 145, y = .5, label = "alpha == .01", parse = TRUE)
g8 <- g7 + scale_x_continuous(breaks = seq(25, 225, 25))
print(g8)

