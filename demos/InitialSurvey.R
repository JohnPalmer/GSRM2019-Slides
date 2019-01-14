# clearing memory
rm(list = ls())

# setting working directory. NOTE HERE YOU WILL NEED TO CHANGE THE DIRECTORY PATH TO MATCH WHATEVER DIRECTPRY YOU HAVE 
# PUT THE DATA FILE IN. 
setwd("~/Google Drive File Stream/My Drive/upf_courses/GSRM/2018/R_demos/initial_survey")

# reading the original data from csv file exported from Aula Global 
D <- read.csv("InitialSurvey.csv", stringsAsFactors = FALSE)

# review the survey questions (which are the names of this data frame)
names(D)

# let's explore the answers to the first question
D[,1]

# notice that each response includes all of the checked choices, separated by "\n". We can split on that character combination like this:
strsplit(D[,1], "\n")

# That gives us a list of vectors. Since we don't care about which respondent said what, we could flatten that into a long vector:
unlist(strsplit(D[,1], "\n"))

# Let's assign that to a new vector
d1 = unlist(strsplit(D[,1], "\n"))

# We can now create a frequency table of each response in that vector
td1 = table(d1)

# Let's create short names for each response to make it easier to fit them in graphs.
names(td1)
shortnames = c("qual. anal.", "quant. anal.", "qual. collect", "quant collect", "library", "prim. paper", "sec. paper")

# Our first attempt at a bor plot, showing frequency of each response
barplot(td1, names.arg = shortnames, las=2, cex.names =.8)

# We can also express this in terms of proportions
td1p = td1/nrow(D)
barplot(td1p, names.arg = shortnames, las=2, cex.names =.8)

# Notice that the plot looks nicer if we order the bars by frequency
barplot(td1p[order(td1p)], names.arg = shortnames[order(td1p)], las=2, cex.names =.8)

# Let's look at the second question
d2 = unlist(strsplit(D[,2], "\n"))

td2 = table(d2)

names(td2)
shortnames = c("Alg.", "Calc.", "Desc. Stats", "GLMs", "Inf. Stats", "Lin. Alg.", "MLMs", "Reg.", "TS")
  
barplot(td2[order(td2)], names.arg = shortnames[order(td2)], las=2, cex.names =.8)

td2p = td2/nrow(D)
barplot(td2p[order(td2p)], names.arg = shortnames[order(td2p)], las=2, cex.names =.8)

# Now continuous variables
D[,6]
hist(D[,6], nclass = 30)
boxplot(D[,6])
plot(density(D[,6], na.rm=TRUE))

hist(D[,9], nclass=30)
boxplot(D[,9])
plot(density(D[,9], na.rm=TRUE))


# Instead of treating these as population, what if we treat them as a random sample and for estimating population values?

# Let's start by creating new vectors that are in the order we want, and then turning them into a date frame
td2p_o = as.numeric(td2p[order(td2p)])
sno = as.character(shortnames[order(td2p)])
math_exp = data.frame(course = sno, p=td2p_o, se=sqrt(td2p_o*(1-td2p_o)/nrow(D)))
math_exp$ci95_ul = math_exp$p+math_exp$se*2
math_exp$ci95_ll = math_exp$p-math_exp$se*2

# Now to plot it:
bp = barplot(math_exp$p, names.arg = math_exp$course, las=2, cex.names =.8)
segments(bp, math_exp$ci95_ll, bp, math_exp$ci95_ul, lwd=1.5)


