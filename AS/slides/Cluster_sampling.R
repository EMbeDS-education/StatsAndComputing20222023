# Code from Chapter 5 of R Companion for Sampling: Design and Analysis by Yan Lu and Sharon L. Lohr
# All code is presented for educational purposes only and without warranty.

##### Install the R packages needed for the chapter

library(survey)
library(sampling)
library(SDAResources)

########## Estimates from One-Stage Cluster Samples ##########

##### Example 5.2
# Produce estimates of the population mean and total for GPA data.
data(gpa)
?gpa
# define one-stage cluster design (variable suite)
# wt weight 100/5=20 for every person
# note that id is suite instead of individual student as we take an SRS of suites
dgpa<-svydesign(id=~suite,weights=~wt,fpc=~rep(100,20),data=gpa) 
dgpa  
# estimate mean and se
gpamean<-svymean(~gpa,dgpa)
gpamean
degf(dgpa)
# n=5, t-approximation is suggested for CI
confint(gpamean,level=.95,df=4) # use t-approximation
# confint(gpamean,level=.95) # uses normal approximation, if desired (for large n) 
# estimate total and se (if desired)
gpatotal<-svytotal(~gpa,dgpa)
gpatotal
confint(gpatotal,level=.95,df=4)

# you can also calculate SEs by direct formula
suitesum<-tapply(gpa$gpa,gpa$suite,sum)  #sum gpa for each suite
# variability comes from among the suites
st2<-var(suitesum) 
st2
# SE of t-hat, formula (5.3) of SDA
vthat <-100^2*(1-5/100)*st2/5
sqrt(vthat) 
# SE of ybar, formula (5.6) of SDA
sqrt(vthat)/(4*100)

##### Example 5.6

data(algebra)
algebra$sampwt<-rep(187/12,299)
# define one-stage cluster design
dalg<-svydesign(id=~class,weights=~sampwt,fpc=~rep(187,299), data=algebra) 
dalg
# estimate mean and se
svymean(~score,dalg)
# n=12, t-distribution is suggested for CI
degf(dalg)
confint(svymean(~score,dalg),level=.95,df=11) #use t-approximation
# estimate total and se if desired
svytotal(~score,dalg)
confint(svytotal(~score,dalg),level=.95,df=11)
