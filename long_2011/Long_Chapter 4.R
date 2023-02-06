#Long Chapter 4: Graphing Longitudinal Data

library(ggplot2)
library(tidyverse)

load("C:/Users/miller.allison/Box/Allison Miller/Course Work and Training/Longitudinal Data Analysis in R_Fall 2022/MPLS.Sorted.Rdata")

#Superimposed individual curves
g1<- ggplot(data = MPLS.Sorted, aes(x = grade, y = read, group = subid)) + geom_line()
g2 <- g1 + theme_bw() + scale_x_continuous(breaks = 5:8, name = "Grade")
g3 <- g2 + scale_y_continuous(name = "Reading Score")
print(g3)

png(filename = "C:/Users/miller.allison/Box/Allison Miller/Course Work and Training/Longitudinal Data Analysis in R_Fall 2022/graph1.png")
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read, group = subid)) + geom_line()
print(g1)
graphics.off()

#Facet plots of individual curves
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read, group = subid))
g2 <- g1 + theme_bw() + geom_line() + geom_point() + facet_wrap(~subid)
g3 <- g2 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
print(g3)

g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read, group = subid))
g2 <- g1 + theme_bw() + geom_line() + geom_point() + facet_wrap(~subid, nrow = 2, as.table = FALSE)
g3 <- g2 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
print(g3)

#Selecting subsets
set.seed(123)
mysub4 <- subset(MPLS.Sorted, subid %in% sample(unique(MPLS.Sorted$subid), size = 4))
g1 <- ggplot(data = mysub4, aes(x = grade, y = read, group= subid)) + geom_line()
g2 <- g1 + geom_point() + facet_wrap(~subid)
g3 <- g2 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
print(g3)

#Regression curves
mysub6 <- subset(MPLS.Sorted, subid < 7)
g1 <- ggplot(data = mysub6, aes(x = grade, y = read, group = subid)) + geom_point()
g2 <- g1 + stat_smooth(method = "lm", se = FALSE) + facet_wrap(~subid)
g3 <- g3 <- g2 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
print(g3)

#Polynomial curves
#Quadratic
g1 <- ggplot(data = mysub6, aes(x = grade, y = read, group = subid)) + geom_point()
g2 <- g1 + stat_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 2)) + facet_wrap(~subid)
g3 <- g3 <- g2 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
print(g3)

#Cubic
g1 <- ggplot(data = mysub6, aes(x = grade, y = read, group = subid)) + geom_point()
g2 <- g1 + stat_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 3)) + facet_wrap(~subid)
g3 <- g3 <- g2 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
print(g3)

#Identify participants with missing data and filter them out
#Mutate allows you to add a new variable (column) to the data that says whether that participant
#had missing data for the read variable (0 = no, 1 = yes)
mysel <- MPLS.Sorted %>% group_by(subid) %>% mutate(Missing = sum(is.na(read)))

mysub.comp <- MPLS.Sorted %>% group_by(subid) %>% mutate(Missing = sum(is.na(read))) %>%
  filter(Missing != 1)


#Faceting by missing data:
mysel$Missing.F <- factor(mysel$Missing, labels = c("Not Missing", "Missing"))
g1 <- ggplot(mysel, aes(x = grade, y = read, group = subid)) + geom_point()
g2 <- g1 + geom_line() + facet_grid(. ~Missing.F) + scale_x_continuous(breaks = 5:8, name = "Grade")
print(g2)

#Smoothed curves
g1 <- ggplot(data = mysub6, aes(x = grade, y = read, group = subid)) + geom_point()
g2 <- g1 + stat_smooth(se = FALSE) + facet_wrap(~ subid)
g3 <- g2 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
print(g3)

#Graphing Group-Level Curves:
#Curve of the Means
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read)) + geom_point(shape = 1)
g2 <- g1 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
g3 <- g2 + stat_summary(fun = mean, geom = "line", lwd = 1.5, linetype = 5)
g4 <- g3 + stat_summary(fun = mean, geom = "point", size = 3, shape = 19)
print (g4)

#Mean curves and unbalanced data
MPLS.Sorted$jgrade <- jitter(MPLS.Sorted$grade)
with(MPLS.Sorted, head(cbind(subid, grade, jgrade), n = 8))
g1 <- ggplot(data = MPLS.Sorted, aes(x = jgrade, y = read)) + geom_point()
print(g1)
g2 <- g1 + stat_summary(fun = mean, geom = "line")
print(g2)

#Graphing fitted curves
#Regression curves- linear
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read)) + 
  geom_point(position = position_jitter(width = 0.1), shape = 1)
g2 <- g1 + stat_smooth(method = "lm", se = FALSE, lwd = 2)
g3 <- g2 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
g4 <- g3 + stat_summary(fun = "mean", geom = "point", size = 4, shape = 19)
g5 <- g4 + coord_fixed(ratio = .08)
print(g5)

#Regression curves- quadratic
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read)) + geom_point(shape = 1)
g2 <- g1 + stat_smooth(method = "lm", formula = y ~ poly(x,2), se = FALSE, lwd = 2)
g3 <- g2 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
g4 <- g3 + stat_summary(fun = "mean", geom = "point", size = 4, shape = 19)
g5 <- g4 + coord_fixed(ratio = .05)
print(g5)

#Smoothed curves
#Stat_smooth(span = 0.75)(default)
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read)) + geom_point(shape = 1)
g2 <- g1 + stat_smooth(se = FALSE, lwd = 2)
g3 <- g2 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
g4 <- g3 + stat_summary(fun = "mean", geom = "point", size = 4, shape = 19)
print(g4)

#Stat_smooth(span = 0.50)
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read)) + geom_point(shape = 1)
g2 <- g1 + stat_smooth(se = FALSE, lwd = 2, span = 0.5)
g3 <- g2 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
g4 <- g3 + stat_summary(fun = "mean", geom = "point", size = 4, shape = 19)
print(g4)

#Stat_smooth(span = 0.90)
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read)) + geom_point(shape = 1)
g2 <- g1 + stat_smooth(se = FALSE, lwd = 2, span = 0.9)
g3 <- g2 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
g4 <- g3 + stat_summary(fun = "mean", geom = "point", size = 4, shape = 19)
print(g4)


#Smoothed curves and unbalanced data
#Smoothed curves and regression curves are more appropriate than mean curves for data
#unbalanced on time. Here's an illustration of this point:
g1 <- ggplot(data = MPLS.Sorted, aes(x = jgrade, y = read)) + geom_point()
g2 <- g1 + stat_smooth(se = FALSE, lwd = 2)
g3 <- g2 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
print(g3)

#Graphing individual-level and group-level curves
g1 <- ggplot(data = mysub6, aes(x = grade, y = read, group = subid)) + geom_line()
g2 <- g1 + stat_summary(aes(group = 1), fun = "mean", geom = "line", lwd = 2)
g3 <- g2 + stat_summary(aes(group = 1), fun = "mean", geom = "point",
                        size = 4)
g4 <- g3 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
print(g4)

#This code graphs the individual lines for the mysub6 sample as well as a bold line representing
#the mean of the entire sample 
g1 <- ggplot(data = NULL, aes(x = grade, y = read))
g2 <- g1 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
g3 <- g2 + stat_summary(data = MPLS.Sorted, fun = mean, geom = "line", lwd = 1.5)
g4 <- g3 + stat_summary(data = MPLS.Sorted, fun = mean, geom = "point", size = 3, shape = 19)
g5 <- g4 + geom_line(data = mysub6, aes(x = grade, y = read, group = subid))
print (g5)

#Checking the means at each grade again to see if the above plot is correct:
RMeans <- MPLS.Sorted %>% group_by(MPLS.Sorted$grade)%>% summarise_at(vars("read"), mean, na.rm = TRUE)


#Conditioning on static predictors
#Categorical static predictors: superimposed curves

MPLS.Sorted$Risk <- MPLS.Sorted$riskC
levels(MPLS.Sorted$Risk) <- c("Advantaged", "Disadvantaged")

#First, create group data (i.e., read scores by grade and risk)
group_data <- MPLS.Sorted %>% group_by(Risk, grade) %>% summarise(read = mean(read, na.rm = TRUE))

#Then, combine group data with individual data (g1):
g1 <- ggplot(data= MPLS.Sorted, aes(x = grade, y = read, shape = Risk, group = Risk)) + geom_point() +
  geom_line(data = group_data, aes(linetype = Risk))
print(g1)

#Move location of legend and add border:
g1 <- ggplot(data= MPLS.Sorted, aes(x = grade, y = read, shape = Risk, group = Risk)) + geom_point() +
  geom_line(data = group_data, aes(linetype = Risk))
g2 <- g1 + scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
g3 <- g2 + theme(legend.position = c(0.7, 0.22), 
                 legend.box.background = element_rect(color = "black", size = 2))
print(g3)

#Combinations of static predictors
#Interaction between gender and ethnicity:
MPLS.Sorted$eth2 <- factor(ifelse(MPLS.Sorted$eth == "Whi", yes = "W", no = "NW"))
with(MPLS.Sorted[MPLS.Sorted$grade == 5, ], table(gen, eth2))

#The above but using pipes:
MPLS.Sorted <- MPLS.Sorted %>% mutate(eth2 = factor(ifelse(MPLS.Sorted$eth == "Whi", yes = "W", no = "NW")))
save(MPLS.Sorted, file = "C:/Users/miller.allison/Box/Allison Miller/Course Work and Training/Longitudinal Data Analysis in R_Fall 2022/MPLS.Sorted.Rdata")

MPLS.Sorted %>% filter(grade ==5) %>% {table(.$gen, .$eth2)}


#Graphing the interaction between gender and ethnicity by grade and reading using pipes
g1<- MPLS.Sorted %>% mutate(GenxEth2 = interaction(gen, eth2)) %>%
  group_by(GenxEth2, grade) %>% summarise(Read = mean(read, na.rm = TRUE)) %>%
  ggplot(aes(x = grade, y = Read, group = GenxEth2, shape = GenxEth2)) +
  geom_point() + geom_line(aes(linetype = GenxEth2)) + scale_x_continuous(breaks = 5:8, name = "Grade") +
  scale_y_continuous(name = "Reading Score") +
  theme(legend.position = c(0.1, 0.32),
        legend.box.background = element_rect(color = "black", size = 2))

#Faceting based on static predictors
#Faceting with a single, static predictor
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read, group = subid)) + geom_line()
g2 <- g1 + stat_summary(fun = mean, aes(group = 1), geom = "line", lwd = 3)
g3 <- g2 + facet_grid(. ~gen) + scale_x_continuous(breaks = 5:8, name = "Grade") +
  scale_y_continuous(name = "Reading Score")

MPLS.Sorted$gen2 <- MPLS.Sorted$gen
levels(MPLS.Sorted$gen2) <- c("Female", "Male")
g3 <- g2 + facet_grid(. ~gen2, margins = TRUE)

#Faceting with two static predictors
levels(MPLS.Sorted$eth2) <- c("Non-White", "White")
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read, group = subid)) + geom_line()
g2 <- g1 + stat_summary(fun = mean, geom = "line", lwd = 2, aes(group = 1))
g3 <- g2 + facet_grid(eth2 ~ gen2) + scale_x_continuous(breaks = 5:8, name = "Grade") +
  scale_y_continuous(name = "Reading Score")
print(g3)


#Superimposing and faceting
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read)) +
  scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name = "Reading Score")
g2 <- g1 + stat_summary(fun = mean, aes(linetype = gen2), geom = "line")
g3 <- g2 + stat_summary(fun = mean, aes(shape = gen2), geom = "point", size = 3)
g4 <- g3 + theme(legend.position = c(0.78, 0.3), 
                 legend.box.background = element_rect(color = "black", size = 2))
g5 <- g4 + scale_linetype(name = "Gender") + scale_shape(name = "Gender", solid = FALSE)
g6 <- g5 + facet_grid(. ~ eth2)
print(g6)

#Quantitative static predictors
set.seed(123)
x <- rnorm(n = 100, mean = 100, sd = 15)
table(cut_interval(x, n = 4))
table(cut_number(x, n = 4))

with(MPLS.Sorted[MPLS.Sorted$grade ==5,], median(att))
with(MPLS.Sorted[MPLS.Sorted$grade ==5,], table(att))
with(MPLS.Sorted[MPLS.Sorted$grade ==5,], table(cut_number(att, n = 2)))

#Faceting by attendance using 2 groups
MPLS.Sorted$att2 <- cut_number(MPLS.Sorted$att, n = 2)
levels(MPLS.Sorted$att2) <- c("Low Attendance", "High Attendance")
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read, group = subid)) + geom_line()
g2 <- g1 + stat_summary(fun = mean, geom = "line", lwd = 3, aes(group = 1))
g3 <- g2 + facet_grid(. ~att2) + scale_x_continuous(breaks = 5:8, name = "Grade") +
  scale_y_continuous(name = "Reading Score")
print(g3)

#Faceting by attendance using 4 groups
MPLS.Sorted$att4 <- cut_number(MPLS.Sorted$att, n = 4)
levels(MPLS.Sorted$att4) <- c("Attend 1", "Attend 2", "Attend 3", "Attend 4")
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read, group = subid)) + geom_line()
g2 <- g1 + stat_summary(fun = mean, geom = "line", lwd = 3, aes(group = 1))
g3 <- g2 + facet_grid(. ~ att4) + scale_x_continuous(breaks = 5:5, name = "Grade") +
  scale_y_continuous(name = "Reading Score")
print(g3)


#Customizing graphs
#Limiting: Changes the values that are displayed AND changes the values used for computations. Any
#observations not seen are excluded from calculations:
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read, group = subid)) + geom_point()
g2 <- g1 + stat_summary(fun = mean, geom = "line", lwd = 3, aes(group = 1))
g3 <- g2 + scale_y_continuous(limits = c(200,220))
print(g3)

#Zooming: Changes the range of values that are displayed and all values are used in any 
#computations. Zooming includes the hidden observations in calculations.
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read, group = subid)) + geom_point()
g2 <- g1 + stat_summary(fun = mean, geom = "line", lwd = 3, aes (group = 1))
g3 <- g2 + coord_cartesian(ylim = c(200,220))
print(g3)


#Customizing facets:
#You can allow the scales of facets to vary (i.e., not be fixed). This can be accomplished by using
#the scales = argument in facet_grid() or facet_wrap():
#Allow both axes to vary by facet: scales = "free"
#Allow the x-axis to vary by facet: scales_x = "free"
#Allow the y axis to vary by facet: scales_y = "free"
#The argument space = is used with the same keywords to allow different spacing within facets

#Customizing the legend
#Color coding based on static predictor gender
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read))
g2 <- g1 + stat_summary(fun = mean, geom = "line", lwd = 3, aes(colour = gen))
g3 <- g2 + scale_colour_brewer(palette = 5)
print(g3)

#Changing the horizontal and vertical location of the legend:
#This is done using hjust = and vjust =
#The size and color of the legend title and legend labels can be changed also. The background
#color of the legend can also be changed.
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read))
g2 <- g1 + stat_summary(fun = mean, geom = "line", lwd = 3, aes(colour = gen))
g3 <- g2 + labs(color = 'GENDER') + scale_color_manual(labels = c("Female", "Male"),
                                                       values = c("blue", "red")) +
  theme(legend.background = element_rect(fill = "grey80"))
print(g3)

#You can also change the legend label and values manually using the scale_manual argument (as opposed
#to creating a new variable in the data frame)
g1 <- ggplot(data = MPLS.Sorted, aes(x = grade, y = read, shape = Risk)) + geom_point()
g2 <- g1 + stat_summary(fun = mean, aes(linetype = Risk), geom = "line") +
  scale_x_continuous(breaks = 5:8, name = "Grade") + scale_y_continuous(name= "Reading Score")
g3 <- g2 + scale_linetype_manual(name = "Risk Group", values = c(1, 2),
                                 breaks = c("Advantaged", "Disadvantaged"), labels = c("ADV", "DADV"))
g4 <- g3 + scale_shape_manual(name = "Risk Group", values = c(1, 19),
                              breaks = c("Advantaged", "Disadvantaged"), labels = c("ADV", "DADV"))
g5 <- g4 + theme(legend.position = c(0.7, 0.22), legend.background = element_rect())
print(g5)

