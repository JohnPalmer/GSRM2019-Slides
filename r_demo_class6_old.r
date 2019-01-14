########################
# R Script for Class 5 #
########################

x <- seq(-4, 4, length=100)
hx <- dnorm(x)
hx2 = dt(x, df=99)
plot(x, hx2, type='l', xlab="t", ylab="density", col="black", lwd=2, yaxs="i")
abline(v=-2)

cord.x <- c(-4,seq(-4,-2,0.01),-2) 
cord.y <- c(0,dt(seq(-4,-2,0.01), df=99),0)
polygon(cord.x,cord.y,col='#fdae61')

cord.x <- c(2,seq(2,4,0.01),4) 
cord.y <- c(0,dt(seq(2,4,0.01), df=99),0)
polygon(cord.x,cord.y,col='#fdae61')


help(pt)



setwd("~/Google Drive File Stream/My Drive/upf_courses/GSRM/2018/slides/global_studies_research_methods_2018")
D = read.csv('shoesize.csv')
D

# exploring height
D$Height
mean(D$Height)
sd(D$Height)
quantile(D$Height)
hist(D$Height)
plot(density(D$Height))

sd(D$Height)/sqrt(408)

# Assumming this data is a random sample of the full school populatio, what is our best estimate of the mean height of all students at this university? Provide a point estimate and 95% confidence interval.


2*(pt(-2, df=99))
1-pnorm(2)


# Assumming this data is a random sample of the full school populatio, what is our best estimate of the mean shoe sizeght of all students at this university? Provide a point estimate and 95% confidence interval.

hist(D$Size)
mean(D$Size)
sd(D$Size)

mean(D$Size)+(2*(sd(D$Size)/sqrt(408)))
mean(D$Size)-(2*(sd(D$Size)/sqrt(408)))


# What is the relationship between height and shoe size?

plot(D$Height, D$Size, pch=20, xlab="height (inches)", ylab="shoe size")
M = lm(D$Size~D$Height)
curve(coef(M)[1] + coef(M)[2]*x, add=TRUE, lwd=2, col="#d7191c")


cor(D$Height, D$Size)


M = lm(D$Height~D$Size)
summary(M)

M = lm(D$Height~D$Size+D$Gender)
summary(M)

M = lm(D$Height~D$Size+D$Gender + D$Size*D$Gender )
summary(M)

# family planning data
library(foreign)
library(calibrate)

D = read.dta('effort.dta')
head(D)

plot(change~effort, data=D, ylab="CBR change", xlab="program effort", pch=20, col="#000099dd", cex=2)

M = lm(change~effort, data=D)
summary(M)

M = lm(change~effort+setting, data=D)
summary(M)