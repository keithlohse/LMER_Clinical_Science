# Loading the essential libraries. 
library("tidyverse"); library("lme4"); library("MASS")
library("car"); library("lmerTest");library("nlme"); library("patchwork")


# If these packages are not installed already, run the following code: 
install.packages("tidyverse"); install.packages("lme4"); 
install.packages("car"); install.packages("lmerTest"); install.packages("nlme")


differences <- c(seq(-10, 10, 0.1))
null_dist <- dnorm(x=differences, mean=0, sd=2)
alt_dist <- dnorm(x=differences, mean=4, sd=2)

DATA <- data.frame(differences, null_dist, alt_dist)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#F0E442",  
               "#555555",  "#D55E00","#0072B2", "#03c478", "#661100", "#f3f319",  
               "#222222","#FBD7A2", "#6699CC", "#99edcc", "#b804a2", "#F9E999")

ggplot(data=DATA, aes(x=differences)) +
  geom_density(aes(y=null_dist), fill=cbPalette[3], stat="identity", alpha=0.5)+
  geom_density(aes(y=alt_dist), fill=cbPalette[5], stat="identity", alpha=0.5)+
  geom_vline(xintercept = c(-4, 4), lty=2, lwd=1, col="black")+
  scale_x_continuous(name = expression(delta)) +
  scale_y_continuous(name = "Density") +
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "none")


N <- 10 # set number of individuals

# Fixed Effects
beta0 <- 50.0 # population intercept 
beta1 <- 1.0  # population slope

# Random Effects and Errors
tau0  <- 10 # intercept SD, 
tau1  <- 2.0 # slope SD, 
tau01 <- 0.5 # correlation between slope and intercept,
sigma <- 5 # true error SD


# number of possible observations per person
max_obs <- 5
min_obs <- 4


# simulate MISSING AT RANDOM observations for each individual
set.seed(42)
p <- round(runif(n=N, min=min_obs, max=max_obs))

# simulate observations per person (everyone has 1st observation)
time <- unlist(sapply(p, function(x) c(1, sort(sample(x=2:max_obs, size = x-1, replace=FALSE)))))

# set up data frame
DATA <- data.frame(id=rep(1:N, times=p), time=time)

head(DATA)



mu  <- c(0,0) # random effects are assumed to be normally distributed with a mean of 0
S   <- matrix(c(1, tau01, tau01, 1), nrow=2) # correlation matrix for the randomw slope and intercept
taus <- c(tau0, tau1) # vector of random effect variances
S   <- diag(taus) %*% S %*% diag(taus) # rescaling our correlation matrix to be a covariance matrix
U   <- mvrnorm(N, mu=mu, Sigma=S) # Creating random deviates for the random slope and intercept, equal to length N, and means mu and variances S.


# simulate (uncorrelated) residuals
# you could instead simulate correlated residuals, but that takes a bit more work
set.seed(42)
DATA$eij <- rnorm(n=nrow(DATA), mean=0, sd=sigma)

head(DATA)


DATA$yij <- (beta0 + rep(U[,1], times=p)) + 
  (beta1 + rep(U[,2], times=p)) * DATA$time + DATA$eij

head(DATA)


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#F0E442",  
               "#555555",  "#D55E00","#0072B2", "#03c478", "#661100", "#f3f319",  
               "#222222","#FBD7A2", "#6699CC", "#99edcc", "#b804a2", "#F9E999")

# Lattice plot of the example data ----
ggplot(data=DATA, aes(x=time, y=yij)) +
  geom_point(shape=16, col="black")+
  geom_line(col="black")+
  stat_smooth(aes(group=id), col="blue", se=FALSE, 
              method="lm")+
  scale_x_continuous(name = "Time") +
  scale_y_continuous(name = "Outcome") +
  facet_wrap(~id, ncol=5) +
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "none")


# 1.0 Set the parameters for Population 1 ----
N <- 10000 # set number of individuals

beta0 <- 50.0 # true intercept
beta1 <- 0.0 # true slope

sigma <- 5  # true error SD
tau0  <- 10    # true intercept SD
tau1  <- 5  # true slope SD
tau01 <- 0.5  # slope-intercept correlation

# number of possible observations
max_obs <- 5
min_obs <- 4

# simulate MISSING AT RANDOM observations for each individual
set.seed(1)
p <- round(runif(n=N, min=min_obs, max=max_obs))

# simulate observations per person (everyone has 1st observation)
time <- unlist(sapply(p, function(x) c(1, sort(sample(x=2:max_obs, size = x-1, replace=FALSE)))))

# set up data frame
POP1 <- data.frame(id=factor(rep(1:N, times=p)), time=time) %>%
  mutate(id = factor(paste("s1", id, sep="_")),
         group="A")

# simulate (correlated) random effects for intercepts and slopes
mu  <- c(0,0)
S   <- matrix(c(1, tau01, tau01, 1), nrow=2)
taus <- c(tau0, tau1)
S   <- diag(taus) %*% S %*% diag(taus)
U   <- mvrnorm(N, mu=mu, Sigma=S)

# simulate (uncorrelated) residuals
# you can simulate correlated residuals, but that takes a bit more work
set.seed(2)
POP1$eij <- rnorm(n=nrow(POP1), mean=0, sd=sigma)

POP1$yij <- (beta0 + rep(U[,1], times=p)) + 
  (beta1 + rep(U[,2], times=p)) * POP1$time + POP1$eij


# 2.0 Set the parameters for Population 2 ----
N <- 10000 # set number of individuals

beta0 <- 50.0 # true intercept
beta1 <- 2.5 # true slope

sigma <- 5  # true error SD
tau0  <- 10    # true intercept SD
tau1  <- 5  # true slope SD
tau01 <- 0.5  # slope-intercept correlation

# number of possible observations
max_obs <- 5
min_obs <- 4

# simulate MISSING AT RANDOM observations for each individual
set.seed(1)
p <- round(runif(n=N, min=min_obs, max=max_obs))

# simulate observations per person (everyone has 1st observation)
time <- unlist(sapply(p, function(x) c(1, sort(sample(x=2:max_obs, size = x-1, replace=FALSE)))))

# set up data frame
POP2 <- data.frame(id=factor(rep(1:N, times=p)), time=time)%>%
  mutate(id = factor(paste("s2", id, sep="_")),
         group="B")

# simulate (correlated) random effects for intercepts and slopes
mu  <- c(0,0)
S   <- matrix(c(1, tau01, tau01, 1), nrow=2)
taus <- c(tau0, tau1)
S   <- diag(taus) %*% S %*% diag(taus)
U   <- mvrnorm(N, mu=mu, Sigma=S)

# simulate (uncorrelated) residuals
# you can simulate correlated residuals, but that takes a bit more work
set.seed(2)
POP2$eij <- rnorm(n=nrow(POP2), mean=0, sd=sigma)

POP2$yij <- (beta0 + rep(U[,1], times=p)) + 
  (beta1 + rep(U[,2], times=p)) * POP1$time + POP1$eij


# set sample sizes
sample_sizes = c(20, 60)

# set number of iterations at each sample size
k = 1000



# initialize null variables to populate:
sample_size = NULL
iteration = NULL
random_effects=NULL
fixed_effects=NULL
anova_results=NULL

count=0
set.seed(1)
for (size in sample_sizes){
  #print(size)
  
  for (i in c(1:k)) {
    count=count+1
    #print(count)
    
    # Sample from each population
    SAMP1 <- POP1[POP1$id %in% sample(x=unique(POP1$id), size=size, replace=FALSE),]
    SAMP2 <- POP2[POP2$id %in% sample(x=unique(POP2$id), size=size, replace=FALSE),]
    
    # Binding the two different samples together
    SAMPLE <- rbind(SAMP1, SAMP2)
    
    # Specify the model you want to fit
    mod <- lmer(yij~1+time*group+(1+time|id), 
                data=SAMPLE, 
                REML=TRUE)
    
    sample_size[[count]] = size
    iteration[[count]] = count
    random_effects[[count]] = data.frame(VarCorr(mod))
    fixed_effects[[count]] = data.frame(fixef(mod))
    anova_results[[count]] = data.frame(anova(mod))
  }
}



# Bind all of our lists together into one big list
SIM_RESULTS <- list(sample_size=sample_size,
                    iteration=iteration,
                    random_effects=random_effects,
                    fixed_effects=fixed_effects,
                    anova_results=anova_results)

# Flatten out the iteration number and sample size into their own data frame
SAMP <- data.frame(iteration = as.character(unlist(SIM_RESULTS$iteration)),
                   sample_size = unlist(SIM_RESULTS$sample_size))


# Tidying the random effects output
RE_DATA <- bind_rows(SIM_RESULTS$random_effects, .id = "iteration") %>%
  pivot_wider(values_from = vcov:sdcor, names_from = grp:var2, names_sep="_") %>%
  left_join(SAMP, by="iteration") %>%
  relocate(iteration, sample_size)

# Tidying the fixed effects output
FE_DATA <- bind_rows(SIM_RESULTS$fixed_effects, .id = "iteration") %>%
  rownames_to_column(var="parameter") %>%
  mutate(parameter=str_split(parameter, "\\.{2,}", simplify = TRUE)[,1]) %>%
  pivot_wider(values_from = fixef.mod., names_from = parameter, names_sep="_") %>%
  left_join(SAMP, by="iteration") %>%
  relocate(iteration, sample_size)

# Tidying the ANOVA results
ANOVA_DATA <- bind_rows(SIM_RESULTS$anova_results, .id = "iteration")   %>%
  rownames_to_column(var="parameter") %>%
  mutate(parameter=str_split(parameter, "\\.{2,}", simplify = TRUE)[,1]) %>%
  left_join(SAMP, by="iteration") %>%
  relocate(iteration, sample_size)


head(RE_DATA)
head(FE_DATA)
head(ANOVA_DATA)



# Plots showing the estimation of the true parameters ----
cbPalette <- c("#D55E00", "#56B4E9", "#009E73", "#000000", 
               "#F0E442", "#0072B2", "#E69F00", "#CC79A7",
               "#999933", "#882255", "#661100", "#6699CC")


head(ANOVA_DATA)
# distribution of p-values ----
A<-ggplot(data=ANOVA_DATA, aes(x=Pr..F.)) +
  geom_histogram(aes(fill=Pr..F.<0.05), col="black", binwidth = 0.02)+
  scale_x_continuous(name = NULL, limits=c(-0.5,1)) +
  scale_y_continuous(name = "Frequency") +
  ggtitle(label="Distribution of P-Values")+
  facet_wrap(~sample_size+parameter, ncol=3, scales = "free")+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "none")

# statistical power ----
ANOVA_DATA %>% group_by(sample_size, parameter) %>%
  summarize(sig=sum(Pr..F.<0.05)/k,
            ns=sum(Pr..F.>=0.05)/k) %>% 
  pivot_longer(cols=sig:ns, names_to = "result", values_to = "freq")

B<-ggplot(data=ANOVA_DATA %>% group_by(sample_size, parameter) %>%
            summarize(sig=sum(Pr..F.<0.05)/k,
                      ns=sum(Pr..F.>=0.05)/k) %>% 
            pivot_longer(cols=sig:ns, names_to = "result", values_to = "freq"),
          aes(x=result, y=freq)) +
  geom_bar(aes(fill=result), col="black", stat="identity")+
  geom_hline(yintercept=0.8, lty=2, lwd=1)+
  ggtitle(label="Statistical Power")+
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Proportion of Results", limits=c(0,1)) +
  facet_wrap(~sample_size+parameter, ncol=3)+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "none")
(A)/(B)


# Plots showing the estimation of the true parameters ----
cbPalette <- c("#D55E00", "#56B4E9", "#009E73", "#000000", 
               "#F0E442", "#0072B2", "#E69F00", "#CC79A7",
               "#999933", "#882255", "#661100", "#6699CC")


# Group Effect at baseline ----
FE1 <- ggplot(data=FE_DATA, aes(x=groupB)) +
  geom_histogram(aes(fill=factor(sample_size)), col="black", bins=30,
                 position="identity", alpha=0.5)+
  #geom_vline(xintercept=beta0, lty=2, lwd=1, col="black")+
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "Frequency") +
  ggtitle(label="Simple Effect of Group (intercept)")+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  labs(fill="Sample Size")+
  #facet_wrap(~sample_size, ncol=1)+
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        plot.title=element_text(size=11, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "bottom")

# Effect of Time in Reference Groups ----
FE2 <- ggplot(data=FE_DATA, aes(x=time)) +
  geom_histogram(aes(fill=factor(sample_size)), col="black", bins=30,
                 position="identity", alpha=0.5)+
  #geom_vline(xintercept=beta0, lty=2, lwd=1, col="black")+
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "Frequency") +
  ggtitle(label="Simple Effect of Time (reference)")+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  labs(fill="Sample Size")+
  #facet_wrap(~sample_size, ncol=1)+
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        plot.title=element_text(size=11, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "bottom")

# Difference between slopes ----
FE3 <- ggplot(data=FE_DATA, aes(x=`time:groupB`)) +
  geom_histogram(aes(fill=factor(sample_size)), col="black", bins=30,
                 position="identity", alpha=0.5)+
  #geom_vline(xintercept=beta1, lty=2, lwd=1, col="black")+
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "Frequency") +
  ggtitle(label="Group x Time Interaction")+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  labs(fill="Sample Size")+
  #facet_wrap(~sample_size, ncol=1)+
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        plot.title=element_text(size=11, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "bottom")

FE1|FE2|FE3



# Plots showing the estimation of the true parameters ----
cbPalette <- c("#D55E00", "#56B4E9", "#009E73", "#000000", 
               "#F0E442", "#0072B2", "#E69F00", "#CC79A7",
               "#999933", "#882255", "#661100", "#6699CC")


# tau0  true standard deviation of the random intercept ----
RE1<-ggplot(data=RE_DATA, aes(x=`sdcor_id_(Intercept)_NA`)) +
  geom_histogram(aes(fill=factor(sample_size)), col="black", bins=30,
                 position="identity", alpha=0.5)+
  geom_vline(xintercept=tau0, lty=2, lwd=1, col="black")+
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "Frequency") +
  ggtitle(label="Random Intercept SD")+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  labs(fill="Sample Size")+
  #facet_wrap(~sample_size, ncol=1)+
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        plot.title=element_text(size=11, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "none")

# tau1 true standard deviation of the random slope ----
RE2 <- ggplot(data=RE_DATA, aes(x=`sdcor_id_time_NA`)) +
  geom_histogram(aes(fill=factor(sample_size)), col="black", bins=30,
                 position="identity", alpha=0.5)+
  geom_vline(xintercept=tau1, lty=2, lwd=1, col="black")+
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "Frequency") +
  ggtitle(label="Random Slope SD")+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  labs(fill="Sample Size")+
  #facet_wrap(~sample_size, ncol=1)+
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        plot.title=element_text(size=11, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "none")

# tau01 true correlation between random-slopes/intercepts ----
RE3<-ggplot(data=RE_DATA, aes(x=`sdcor_id_(Intercept)_time`)) +
  geom_histogram(aes(fill=factor(sample_size)), col="black", bins=30,
                 position="identity", alpha=0.5)+
  geom_vline(xintercept=tau01, lty=2, lwd=1, col="black")+
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "Frequency") +
  ggtitle(label="Correlation (Slopes~Intercepts)")+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  labs(fill="Sample Size")+
  #facet_wrap(~sample_size, ncol=1)+
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        plot.title=element_text(size=11, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "bottom")


# sigma true standard deviation of the residuals ----
RE4<-ggplot(data=RE_DATA, aes(x=sdcor_Residual_NA_NA)) +
  geom_histogram(aes(fill=factor(sample_size)), col="black", bins=30,
                 position="identity", alpha=0.5)+
  geom_vline(xintercept=sigma, lty=2, lwd=1, col="black")+
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "Frequency") +
  ggtitle(label="Residual SD")+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  labs(fill="Sample Size")+
  #facet_wrap(~sample_size, ncol=1)+
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        plot.title=element_text(size=11, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "bottom")

(RE1|RE2)/(RE3|RE4)