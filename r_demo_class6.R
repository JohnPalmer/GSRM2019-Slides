#######################################
# Research Methods for Global Studies #
# R demo for class 6                  #
#######################################

# loading libraries
library(tidyverse)
library(haven)
library(stargazer)

# loading data directly from URL 
D = read_dta("http://data.princeton.edu/wws509/datasets/effort.dta")

# creating program effort categories
D$effort_cat <- cut(D$effort, breaks=c(min(D$effort),5,15,max(D$effort)), right=FALSE, include.lowest=TRUE, labels=c("Weak","Moderate","Strong"))

# exploring models
M1 = lm(change~setting, data=D)
M2 = lm(change~setting+effort, data=D)

stargazer(M1, M2, type="text")

