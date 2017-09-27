# Longitudinal Data Analysis for the Clinical Sciences. 
# Keith Lohse, 2017/09/25

# Place your cursor in code and press CTRL + R to run the code one line at a time.
# On a Mac, you will press COMMAND + Enter
# Note that code you run shows up in the console window below.
# Commented code (i.e., anything preceded by a #) does not have any effect.
# This is because "commenting" something effectively identifies it as none code.

##----------------------- Sample Script ----------------------------------------
## Setting the Directory -------------------------------------------------------
# The "get working directory" command will tell you the address of the current 
# working directory. 
getwd()
# However, we are going to want to update the directory to be your
# "LMER_Clinical_science" folder. I recommend putting this folder inside the 
# Documents folder on your computer. 

# Update the "set working directory" function below with the correct address
# for your machine. 
setwd("C:/Users/u6015231/Documents/GitHub/LMER_Clinical_Science/")

# Next, we can use the list file function to see the files in the current 
# working directory.
list.files()

# Next, create a subfolder called "data" in your LMER_Clinical_Science folder.
# Save the "data_LOHSE_EXAMPLE.csv" inside of your data subfolder.
# Next, you can list the files inside of the data folder to confirm that the 
# data_LOHSE_EXAMPLE.csv is saved there.
list.files("./data/")
# the "./" appends the additional text to the end of the current working 
# directory. Here, we are peaking into the "data" folder.

# Next, we will want to open several libraries whose functions we will use.
# Be sure to install these libraries first if you haven't already:
install.packages("ggplot2")
install.packages("lme4")
install.packages("dplyr")

library("ggplot2");library("lme4");library("dplyr")

## Importing Data and Quality Assurance ----------------------------------------
# Next, we will read in a "dummy" dataset that I created. 
# This data emulates datastructures in the Brain Recovery Core Database 
# See Lang et al. J. Neurologic Physical Therapy. 2011 and Lohse et al. Arch.
# Phys Med Rehabil. 2016.
DATA<-read.csv("./data/data_LOHSE_EXAMPLE.csv", header = TRUE) 
DATA 
# Clearly the raw data is a bit of a mess, but notice NAs in multiple rows 
# and columns. Some participants are missing data for entire assessments, 
# others are missing data for individual timepoints. 

# Using ggplot2, we can visualize some of our data over time.
# The code below will create a "faceted" plot that shows the data for each
# participant over time.
myX<-scale_x_continuous(name = "Time (Months from Outpatient Admission)")
myY<-scale_y_continuous(name = "Berg Balance Scale")
g1<-ggplot(data=DATA, aes(x = time, y = BERG, group = subID))+geom_line()
g2<-g1+geom_point()+facet_wrap(~subID)+myX+myY
g3<-g2 + stat_smooth(method=lm, se=FALSE)
plot(g3)
# Note that participants have different amounts of data collected at different 
# time points. 

# Congratulations, you just created your first plot in R!

