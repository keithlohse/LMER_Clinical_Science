##-------------------------
# Linear Mixed Effects Regression (LMER) Reading Group
# Keith Lohse, PhD
# Auburn Univeristy
# rehabinformatics@gmail.com
# 2016-06-06

The data and script files in this folder were created by Keith Lohse (me) at 
Auburn University for a summer reading group on Linear Mixed Effect 
Regression (LMER). The code builds on code originally written by Jeff Long 
at the University of Iowa and follows his book "Longitudinal Data Analysis for
the Behavioral Sciences Using R". The goal of the reading group was to work 
through this book chapter by chapter and the scripts/data in this folder are
the end result of that exercise. 

When appropriate, I have replicated and updated Dr. Long's code (i.e., showing
solutions using both plyr (original) and dplyr (more recent)) and I have tried
to thoroughly comment Dr. Long's code, explaining what the code is doing at 
each step. (The original code available from the publisher has essentially zero
comments.) 

There are also some situations were we use code and data completely separate 
from Dr. Long's book. These are original data that were collected in the 
Rehabilitation Informatics Laboratory at Auburn University. All of the 
necessary data are stored in this folder, so the new code should run smoothly
for everyone provided that you have all of the files saved to your working 
directory and that you have installed all of the necessary packages (code 
for installing these packages is included at the beginning of each script 
file).


##--------------------------
# File Structure

For all of these scripts/data, the working assumption is that you create two
folders within your working directory:
1. data
2. scripts

In my code, I try to use relative rather than absolute references when possible, 
but please note that file pathways will need to be updated to be specific to 
your computer. I do, however, try to point out when you need to do this in the
code. For instance, use the getwd() function in R to find what your current 
working directory is, and then use the setwd() function to change it. As you 
will see in my code, I always adjust my working directory to
> setwd("C:/Currant/LMER_reading_group/")

... and within the "LMER_reading_group" folder, I have separate folders called 
"data" and "scripts" in which I store all of the data files and script files.  


##--------------------------
# Progressing Through the Chapters

In general, we progress through Dr. Long's textbook chapter by chapter. The 
first two chapters are more conceptual and don't really have a lot of R code
to go along with them as such, I recommend that you read the first two
and then start working on the LMER_basics.R script file. From there, you 
can just go chapter by chapter and *hopefully* all of the updated code will 
work for you. The titles of the script files should line up with the chapters
pretty intuitively, that is, "LMER_long3.R" should be completed while reading
or after reading Chapter 3 from Dr. Long's textbook ("LMER_long4.R" goes with
Chapter 4, etc.).
  


  