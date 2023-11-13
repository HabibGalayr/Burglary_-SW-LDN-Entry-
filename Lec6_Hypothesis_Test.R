#-----Section 01-------------------------------------------

# Set working directory
setwd(dirname(file.choose()))
getwd()

burglary.chi <- read.csv("06_burglary-chi.csv", stringsAsFactors = TRUE)
head(burglary.chi)  	# The top of the data
str(burglary.chi)     # structure of the data

#-----Section 02-------------------------------------------

# create a crosstable of district by entry point
myxtab <- xtabs(~ district + entry_pn, exclude = "", data=burglary.chi)
myxtab
# extract columns 'door' and 'window' into a new data frame
mytable <- as.data.frame(cbind(myxtab[,"Door"],myxtab[,"Window"]))
names(mytable)[1] <- "Door"
names(mytable)[2] <- "Window"
# print the table
mytable

# visualise
install.packages("DescTools")
library(DescTools)
PlotMosaic(as.matrix(mytable), main = "district ~ entry point")
PlotMosaic(t(as.matrix(mytable)), main = "entry point ~ district")
PlotCirc(t(as.matrix(mytable)), main = "entry point -> district")
# t() gives transpose of a matrix.

#-----Section 03-------------------------------------------

# Pearson's Chi-squared Test

# simple chi-squared test
chisq.test(mytable)

# simple chi-squared with additional outputs
chisq_out <- chisq.test(mytable)
chisq_out
chisq_out$observed
chisq_out$expected
chisq_out$stdres
# Standardised residuals are the difference between observed and expected (residual) divided by standard deviation of residuals.
# If your residuals are +/-2, then there is a definite difference, if +/-3 then something unusual is happening.

# chi-squared test using Monte Carlo simulation
chisq.test(mytable, correct = FALSE,
           p = rep(1/length(mytable), length(mytable)), rescale.p = FALSE,
           simulate.p.value = TRUE, B = 2000)

# calculate effect size
CramerV(mytable, conf.level = 0.95)

rm(myxtab, mytable, chisq_out)

#-----Section 04-------------------------------------------

# repeat chi-squared test for dwelling type by entry point
myxtab <- xtabs (~ dwell_ty + entry_pn, exclude = "", data=burglary.chi)
myxtab
# extract columns 'door' and 'window' into a new data frame
mytable <- as.data.frame(cbind(myxtab[,"Door"],myxtab[,"Window"]))
names(mytable)[1] <- "Door"
names(mytable)[2] <- "Window"
# print the table
mytable

# visualise
PlotMosaic(t(as.matrix(mytable)), main = "entry point ~ dwelling type")
PlotCirc(t(as.matrix(mytable)), main = "entry point -> dwelling type")

# Pearson's Chi-squared Test

# simple chi-squared test
chisq.test(mytable)

# simple chi-squared with additional outputs
chisq_out <- chisq.test(mytable)
chisq_out$observed
chisq_out$expected
chisq_out$stdres

# chi-squared test using Monte Carlo simulation
chisq.test(mytable, correct = false,
           p = rep(1/length(mytable), length(mytable)), rescale.p = FALSE,
           simulate.p.value = TRUE, B = 2000)

# calculate effect size
CramerV(mytable, conf.level = 0.95)

rm(myxtab, mytable, chisq_out)

# Note: for 2x2 tables of counts use fisher.test(mytable)

fisher.test(mytable, simulate.p.value = TRUE)

#-----Section 05-------------------------------------------

AandE <- read.csv("06_A&E.csv", stringsAsFactors = FALSE)
head(AandE)
str(AandE)
# make $in_out as factor
AandE$in_out <- factor(AandE$in_out)
str(AandE)


#-----Section 06-------------------------------------------

# Wilcoxon Rank Sum and Signed Rank Tests
# wilcox.test(x, y = NULL,
# alternative = c("two.sided", "less", "greater"),
# mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
# conf.int = FALSE, conf.level = 0.95, ...)

# dependent 2-group Wilcoxon Signed Rank Test
wtest1 <- wilcox.test(AandE$male, AandE$female, paired=TRUE) # where male and female are numeric
wtest1

# check why the result of hypothesis test
summary(AandE$male)
summary(AandE$female)

# calculate effect size
Zstat1 <- qnorm(wtest1$p.value/2)
abs(Zstat1)/sqrt(nrow(AandE))

#-----Section 07-------------------------------------------

# independent 2-group Mann-Whitney U Test
wtest2 <- wilcox.test(AandE$total ~ AandE$in_out)  # where total is numeric and in_out is A binary factor
wtest2

# check why the result of hypothesis test
mydata.inner<-AandE[AandE$in_out=="Inner",]
mydata.outer<-AandE[AandE$in_out=="Outer",]
summary(mydata.inner$total)
summary(mydata.outer$total)

# calculate effect size
#Zstat2 <- qnorm(wtest2$p.value/2)
#abs(Zstat2)/sqrt(nrow(AandE))

rm(mydata.inner, mydata.outer)

#-----Section 08-------------------------------------------

# repeat with proportional data
# create proportional variables (per thousand population)
AandE <- within (AandE, pMale <- (male / popM) * 1000)
AandE <- within (AandE, pFemale <- (female / popF) * 1000)
AandE <- within (AandE, pTotal <- (total / popT) * 1000)

# dependent 2-group Wilcoxon Signed Rank Test
wtest3 <- wilcox.test(AandE$pMale, AandE$pFemale, paired=TRUE) # where male and female are numeric
wtest3
# check why the result of hypothesis test
summary(AandE$pMale)
summary(AandE$pFemale)
# calculate effect size
#Zstat3 <- qnorm(wtest3$p.value/2)
#abs(Zstat3)/sqrt(nrow(AandE))

# independent 2-group Mann-Whitney U Test
wtest4 <- wilcox.test(AandE$pTotal ~ AandE$in_out)  # where total is numeric and in_out is A binary factor
wtest4
# check why the result of hypothesis test
mydata.inner<-AandE[AandE$in_out=="Inner",]
mydata.outer<-AandE[AandE$in_out=="Outer",]
summary(mydata.inner$pTotal)
summary(mydata.outer$pTotal)
# calculate effect size
Zstat4 <- qnorm(wtest4$p.value/2)
abs(Zstat4)/sqrt(nrow(AandE))

#-----Section 09-------------------------------------------

# remove all variables from the environment
rm(list=ls())
