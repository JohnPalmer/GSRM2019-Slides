########################
# R Script for Class 5 #
########################

# Set working directory to wherever you have your data file
setwd("~/Google Drive File Stream/My Drive/upf_courses/GSRM/2018/slides/global_studies_research_methods_2018")

# Loading data. (Note that if your system saves csv files using semi-colons, you should use read.csv2)
D = read.csv('shoesize.csv')

# quick view of data
head(D)

# exploring height
D$Height
mean(D$Height)
sd(D$Height)
quantile(D$Height)
hist(D$Height)
plot(density(D$Height))

# Assumming this data is a random sample of the full school populatio, what is our best estimate of the mean height of all students at this university? Provide a point estimate and 95% confidence interval.


# What is the relationship between height and shoe size?

plot(D$Height, D$Size, pch=20, xlab="height (inches)", ylab="shoe size")

cor(D$Height, D$Size)

M = lm(D$Height~D$Size)
summary(M)

M = lm(D$Height~D$Size+D$Gender)
summary(M)
