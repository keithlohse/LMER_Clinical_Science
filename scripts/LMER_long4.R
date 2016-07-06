## Chapter 4 R code from Long (2012)
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

# Back in Chapter 3, we created a file called "MPLS.LS.Rdata"
# Let's make sure that file is our data folder:
list.files("C:/Currant/LMER_reading_group/data")
# We can load the data file into R
load("./data/MPLS.LS.Rdata")

head(MPLS.LS)
# Let's start with creating a spagetti plot:
g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read, group = subid)) + geom_line()
print(g1)

# The gg in ggplot2 stands for grammer of graphics
# The reason for this is that the code allows you to add custom elements to a plot
# Ideally this gg give you generative flexibility of grammar in everyday language
g1 <- ggplot(data = MPLS.LS , aes(x = grade, y = read, group = subid)) + geom_line ()
g2 <- g1 + theme_bw() + scale_x_continuous(breaks = 5:8, name = "Grade")
g3 <- g2 + scale_y_continuous(name = "Reading Score")
print(g3)

# As a compliment to the spagetti plot, we can also create something called a lattice plot
# We will start with defining the properties of our x- and y-axes
myX <- scale_x_continuous(breaks = 5:8, name = "Grade")
myY <- scale_y_continuous(name = " Reading Score ")
# Note the similarities between the code below and the code above
# The key features of the lattice plot are the group and facet_wrap arguments
g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read, group = subid))
g2 <- g1 + geom_line() + geom_point() + facet_wrap(~ subid) + myX + myY
print(g2)

# We can control the organization of the lattice plot in a number of ways
g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read, group = subid))
g2 <- g1 + geom_line() + geom_point() + myX + myY
# For instance, we can fix the number of rows in our plot
g3 <- g2 + facet_wrap(~ subid, nrow = 2, as.table = TRUE)
print(g3)

# Alternatively, we can also choose a subset of partipants to plot
mysub6 <- subset(MPLS.LS, subid < 7)
tail(mysub6)

# Or select a random subset of subjects (this is one I use a lot in big datasets)
## set.seed fixes the seed for the random number generator
set.seed(123)
## ... then we can select random subset.
mysub4 <- subset(MPLS.LS, subid %in% sample(unique(MPLS.LS$subid), size = 4))
tail(mysub4)
## ... and then we can graph the random subset.
g1 <- ggplot(data = mysub4, aes(x = grade, y = read, group = subid)) + geom_line()
g2 <- g1 + geom_point() + facet_wrap(~ subid) + myX + myY
print(g2)

# Let's return our subset of the first six subjects, suppose that wanted to plot
# the linear fit along with the data points
g1 <- ggplot(data = mysub6, aes(x = grade, y = read, group = subid)) + geom_point()
g2 <- g1 + stat_smooth(method = "lm", se = FALSE, col="blue") + facet_wrap(~ subid) + myX + myY
print(g2)

# The stat_smooth argument is pretty flexible, we can pass it linear and nonlinear functions
g1 <- ggplot(data = mysub6, aes(x = grade, y = read, group = subid)) + geom_point()
g2 <- g1 + stat_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 2))
g3 <- g2 + facet_wrap(~ subid) + myX + myY
print(g3)

##---------------------------------------------------
# Dealing with Missing Data
# Suppose we wanted to remove subjects with missing data from our dataset
# First, we can pullout the IDs of subject who have missing data
mysel <- ddply(.data = data.frame(as.numeric(is.na(MPLS.LS$read))),
               .variables = .(subid = MPLS.LS$subid), each(missing = sum))
mysel

# Then we can select subjects with no missing values.
myids <- with(mysel, subid[missing == 0])
# ... and create a subset of the original data frame.
mysub.comp <- subset(MPLS.LS, subid %in% myids)
head(mysub.comp)

# Alternatively, we can add a new variable indicating missing data to our original dataset
MPLS.LS2 <- merge(MPLS.LS, mysel, by = "subid")
head(MPLS.LS2)

# After adding this variable, we can look at how our missing data are distributed
## First, we can turn the missing data variable into a factor
MPLS.LS2$missing.f <- factor(MPLS.LS2$missing, labels = c("Not Missing", "Missing"))
## ... and then we can use this factor in our graphs
g1 <- ggplot(MPLS.LS2, aes(x = grade, y = read, group = subid)) + geom_point()
g2 <- g1 + geom_line() + facet_grid(. ~ missing.f) + myX
print(g2)

# g1 <- ggplot(data = mysub6, aes(x = grade, y = read, group = subid)) + geom_point()
# g2 <- g1 + stat_smooth(se = FALSE) + facet_wrap(~ subid) + myX + myY
# print(g2)



##---------------------------------------------------
# Jittering Points on a Graph
# We have non interval/ratio data, we will often have overlapping datapoints
# The Grade variable, for instance, only takes four values
g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read)) + geom_point(shape = 1) + myX + myY
g2 <- g1 + stat_summary(fun.y = mean, geom = "line", lwd = 1.5, linetype = 5)
g3 <- g2 + stat_summary(fun.y = mean, geom = "point", size=3, shape = 19)
print(g3)

# Sometimes this is okay, but at other times, we will want to "jitter" the grade variable
# Jittering essentially add a random value (positive or negative) to a number so that
# there is less overlap in our datapoints
MPLS.LS$jgrade <- jitter(MPLS.LS$grade)
# Let's compare the Grade variable to the Jittered variables.
with(MPLS.LS, head(cbind(subid, grade, jgrade), n = 8))
# Let's plot the resulting variables
g1 <- ggplot(data = MPLS.LS, aes(x = jgrade, y = read)) + geom_point()
print(g1)
# However, you can see that this now changes how some of our other plotting 
# functions will work.
g2 <- g1 + stat_summary(fun.y = mean, geom = "line")
print(g2)

# However, we can still get a plot with jittered grade and a mean slope
g1 <- ggplot(data = MPLS.LS, aes(x = jgrade, y = read)) + geom_point(shape = 1)
g2 <- g1 + stat_smooth(method = "lm", se = FALSE, lwd = 2) + myX + myY
g3 <- g2 + stat_summary(fun.y = "mean", geom = "point", size = 2, shape = 19)
print(g3)

# Remeber that if we don't specify a method in the stat_smooth function
# it will draw a high-order polynomial that provides the best fit through all 
# of the data points
g1 <- ggplot(data = MPLS.LS, aes(x = jgrade, y = read)) + geom_point()
g2 <- g1 + stat_smooth(se = FALSE, lwd = 2) + myX + myY
print(g2)


##---------------------------------------------------
# Plotting Categorical Data (i.e., Different Groups)
g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read, line = risk2)) + geom_point()
# In our next step we need to add a new arugment, specifying that we want our 
# stat_summary() to be conditional on students risk
g2 <- g1 + stat_summary(fun.y = mean, aes(line = risk2, col=risk2), geom="line") + 
                        myX + myY
g3 <- g2 + scale_shape_manual(values = c(1, 19))
print(g3)

## We can create a similar graph with a bit more customization
MPLS.LS$Risk <- MPLS.LS$risk2
levels(MPLS.LS$Risk) <- c("Advantaged", "Disadvantaged")
## Customizing our graph, note the the additional shape argument in aes()
g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read, shape = Risk)) + geom_point()
## We still specify that the stat_summary is conditional on Risk
g2 <- g1 + stat_summary(fun.y = mean, aes(line = Risk), geom = "line") + myX + myY
## We can also create summary (i.e., average) data points at each grade
g3 <- g2 + stat_summary(aes(shape = Risk), fun.y = mean, geom = "point", size = 3)
g4 <- g3 + scale_shape_manual(values = c(1, 19))
print(g4)

##---------------------------------------------------
# Dichotomizing ethnicity.
MPLS.LS$eth2 <- factor(ifelse(MPLS.LS$eth == "Whi", yes = "W", no = "NW"))
## Save the data frame.
save(MPLS.LS, file="./data/MPLS.LS.Rdata") ## Tailor to your system. 
# With our new ethnicity variable, we ca do cross-tabulation.
with(MPLS.LS[MPLS.LS$grade == 5, ], table(gen, eth2))

# The new factor of two-level ethnicity also allows us to create some different figures
g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read)) + myX + myY
g2 <- g1 + stat_summary(fun.y = mean, aes(line = gen : eth2), geom = "line")
g3 <- g2 + stat_summary(fun.y = mean, aes(shape = gen : eth2), geom = "point", size = 3)
g4 <- g3 + theme(legend.position = c(.55, .35))
print(g4)

# Previously, we created lattice plots that were essentially "conditional" on subject ID
# But we can also create lattice plots that are conditional on other factors...
g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read, group = subid)) + geom_line()
g2 <- g1 + stat_summary(fun.y = mean, aes(group = 1), geom = "line", lwd = 3)
g3 <- g2 + facet_grid(. ~ gen) + myX + myY
print(g3)

# We can even creating lattice plots conditional on two different vairables
levels(MPLS.LS$eth2) <- c("Non-White", "White")
g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read, group = subid)) + geom_line()
g2 <- g1 + stat_summary(fun.y = mean, geom = "line", lwd = 2, aes(group = 1))
g3 <- g2 + facet_grid(eth2 ~ gen2) + myX + myY
print(g3)

# If we don't want a spagetti plot and instead want a more classic line plot,
# then we can run code like the following
g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read)) +  myY + myX
g2 <- g1 + stat_summary(fun.y = mean, aes(line = gen2), geom = "line")
g3 <- g2 + stat_summary(fun.y = mean, aes(shape = gen2), geom = "point", size = 3)
g4 <- g3 + theme(legend.position = c(.78,.3))
g5 <- g4 + scale_linetype(name = "Gender") + scale_shape(name = "Gender", solid = FALSE)
g6 <- g5 + facet_grid(. ~ eth2)
print(g6)
# Naturally, if you spend a bit more time exploring and messing with the code, then you 
# can create much more aesthetically pleasing figures.

## For "(all)" panel:
MPLS.LSa <- MPLS.LS # Copy data frame.
MPLS.LSa$gen2 <- "(all)" # Relabel gen2.
plotdata <- rbind(MPLS.LS, MPLS.LSa) # Stack data frames. 

set.seed(123)                                 # Enables replication of results.
x <- rnorm(n = 100, mean = 100, sd = 15)      # 100 scores from normal distribution.
table(cut_interval(x, n = 4))
table(cut_number(x, n = 4))

with(MPLS.LS[MPLS.LS$grade == 5,], median(att))
with(MPLS.LS[MPLS.LS$grade == 5,], table(att))
with(MPLS.LS[MPLS.LS$grade == 5,], table(cut_number(att, n = 2)))

# Create groups.
MPLS.LS$att2 <- cut_number(MPLS.LS$att, n = 2)                   
# Name levels.
levels(MPLS.LS$att2) <- c("Low Attendance", "High Attendance")  
g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read, group = subid)) +  geom_line()
g2 <- g1 + stat_summary(fun.y = mean, geom = "line", lwd = 3, aes(group = 1))
g3 <- g2 + facet_grid(. ~ att2) + myY + myX
print(g3)

MPLS.LS$att4 <- cut_number(MPLS.LS$att, n = 4)
levels(MPLS.LS$att4) <- c("Attend 1", "Attend 2", "Attend 3", "Attend 4")
g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read, group = subid)) +  geom_line()
g2 <- g1 + stat_summary(fun.y = mean, geom = "line", lwd = 3, aes(group = 1))
g3 <- g2 + facet_grid(. ~ att4) + myY + myX
print(g3)

g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read, group = subid)) +  geom_points()
g2 <- g1 + stat_summary(fun.y = mean, geom = "line", lwd = 3, aes(group = 1))
g3 <- g2 + scale_y_continuous(limits = c(200, 220))

g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read, group = subid)) +  geom_points()
g2 <- g1 + stat_summary(fun.y = mean, geom = "line", lwd = 3, aes(group = 1))
g3 <- g2 + coord_cartesian(ylim = c(200, 220))

g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read))
g2 <- g1 + stat_summary(fun.y = mean, geom = "line", lwd = 3, aes(colour = gen))
g3 <- g2 + scale_colour_brewer(palette = 3)
print(g3)

g1 <- ggplot(data = MPLS.LS, aes(x = grade, y = read, shape = risk2)) + geom_point()
g2 <- g1 + stat_summary(fun.y = mean, aes(line = risk2), geom = "line") + myX + myY

####################################################################################
g3 <- g2 + scale_linetype_manual(name = "Risk Group", values = c(1, 2),
                                 breaks = c("ADV", "DADV"), labels = c("Advantaged", "Disadvantaged"))
g4 <- g3 + scale_shape_manual(name = "Risk Group", values = c(1, 19),
                              breaks = c("ADV", "DADV"), labels = c("Advantaged", "Disadvantaged"))
####################################################################################
g5 <- g4 + theme(legend.position = c(.7,.22))
print(g5)
