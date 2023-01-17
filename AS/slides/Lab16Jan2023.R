

############ R LAB 16 Jan 2023
############ Simple Random Sampling & Stratified Sampling


###############################################################
##
# SURVEY SAMPLING IN R
##
##############################################################

setwd("C:/Users/Gaia/Dropbox/JM Materiale Gaia Francesco/Corso JM I SEMESTRE 20-21/R Labs/Lab30Oct2020")

# An important distinction that I want to make sure has been made before 
# we go any further is the distinction between a sample and a population:
# Population: the set of all subject of interest
# Sample: the subset of population for wich we have data

# Suppose that you want to know in Italy what is the mean height of adult males (+18)
# The population that we are dealing with in this case is all italian adult males. 
# One way to find an exact answer to this research question would be to survey the entire 
# population. 
# However, this is nearly impossible! 
# It would be much quicker and easier to measure only a subset of the population, a sample.
# If we want our sample to be an accurate reflection of the population, 
# we can't just choose any sample that we wish. 
# The way in which we collect our sample is very important. 
# Let's suppose that we were able to choose an appropriate sample
# Then, we might discuss two different means: the mean height of the sample 
# and the mean height of the population. 
# These are both descriptions, as opposed to inferences. 
# Mean Height of the Sample:
# Statistic - describes the sample. Can be known, but it changes depending on the sample.
# Mean Height of the Population:
# Parameter - describes the population
# Usually unknown - but we wish we knew it
# Our goal is to use the information we've gathered from the sample to infer, or predict, 
# something about the population. For our example, we want to predict the population mean, 
# using our knowledge of the sample. 
# The accuracy of our sample mean relies heavily upon how well our sample represents 
# the population at large. If our sample does a poor job at representing the population, 
# then any inferences that we make about the population are also going to be poor. 
# Thus, it is very important to select a good sample!


# There are 2 main kinds of sampling: Random Sampling and Non-Random Sampling

# We'll focus on random sampling, in particular: simple random sampling, systematic sampling, 
# stratified sampling, cluster sampling

# In simple random sampling, for a given sample size  n,  every set of  n  
# members of the population has the same chance to be the sample that is actually selected

# In a systematic sample, the members of the population are put in a row. 
# Then 1 out of every  k  members are selected. The starting point is randomly chosen 
# from the first  k  elements and then elements are sampled at the same location in 
# each of the subsequent segments of size  k 

# In a stratified sample, the population must first be separated into homogeneous groups, 
# or strata. 
# Each element only belongs to one stratum and the stratum consist of elements that are 
# alike in some way. 
# A simple random sample is then drawn from each stratum, which is combined to make the 
# stratified sample.

# Cluster sampling is a sampling method used when natural groups are evident in the population. 
# The clusters should all be similar each other: each cluster should be a small 
# scale representation 
# of the population. To take a cluster sample, a random sample of the clusters is chosen. 
# The elements of the randomly chosen clusters make up the sample.

##################################################################################?

# Cran Task View: Official Statistics & Survey Methodology

# A comprehensive list of packages specifically developed for various aspects of 
# survey sampling can be found at: 
# https://cran.r-project.org/web/views/OfficialStatistics.html

# We will be using self-defined routines, the sampling package, and
# the survey package, which you will need to install and load using
# the following commands:


install.packages(c('sampling','survey'))

# This packages encompass functions for dealing with the most common sampling designs:
# I) computation of inclusion probabilities
# II) sample extraction
# III) estimation of population quantities
# IV) estimation of estimators' variance.
# Some interesting datasets to exercise with are also provided.

library(sampling)

library(survey)


# Let's begin by understanding how the survey package works. 
# Go to packages, type  "survey" in the search box and then click on "survey"
# You can find some usefull example in "User guides, package vignettes and other documentation"

# To use package survey takes two step

# a) First you have to specify the sampling design of the 
#    survey using "svydesign()" function. 

?svydesign

# b) Then, the particular statistical method or model is calculated 
#    using a special function that calls on the specified survey design.  
#   These special functions all start with "svy" and include (for example):
# 1. svymean() to compute the mean 
# 2. svytotal() to compute the total
# 3. svyratio() to compute ratio estimation and estimates of totals based on 
# ratios for complex survey





# STEP A
# Let's check the basic syntax of svydesign() function from the help



?svydesign

########

# SIMPLE RANDOM SAMPLING

#######

# Simple Random Sampling

#######
# Simple random sampling (SRS) provides a natural starting point 
# for a discussion of probability sampling methods. 
# It is the simplest method and it underlies many of the more complex methods.
# Simple random sampling is a sampling scheme with the property that any of the
# possible subsets of n distinct elements, from the population of N elements, 
# is equally likely to be the chosen sample.
# Every element in the population has the same probability of being selected for the sample, 
# and the joint probabilities of sets of elements being selected are equal

# Example 1: Package "Sampling"

set.seed(1985)    # Random Number Generation: fix it to Reproducibility
?srswor   # simple random rampling without replacement
s<-srswor(n=3,N=10) 
s

# The sample is (I'm not saving anyhing)
(1:10)[s==1]


# Example 2: Sampling and Survey

# In the R language individual data sets are stored as data.frame objects, 
# allowing users to load as many tables into 
# working memory as necessary for the analysis

# We will be using the belgianmunicipalities dataset in the
# sampling package to explore routines to extract sample and
# analyze survey data.

data(belgianmunicipalities)
?belgianmunicipalities

# This data provides information about the Belgian population of July 1, 2004
# compared to that of July 1, 2003
# and some financial information about the municipality incomes at the end of 2001.

# Try to understand better our data
dim(belgianmunicipalities)

# head() and tail () Returns the first or last parts of a vector, matrix, table, 
# data frame or function.
head(belgianmunicipalities)
tail(belgianmunicipalities)
# 

length(belgianmunicipalities$Commune) #nrow

summary(belgianmunicipalities)
hist(belgianmunicipalities$medianincome)
# 
name=belgianmunicipalities$Commune


# We want to know if a municipality has a number of men on 1 July 2004 
# greater than the first quartile of the distribution. 
# To do this we create a new variable. 
# If yes, this new variable will have value 1. If no, zero.
# Men04 is the number of men on July 1, 2004
# First we need to find what the first quartile of the distribution is.
quantile(belgianmunicipalities$Men04)
# ifelse(test, yes, no)(1st Qu.:  3331)
binary<-ifelse(belgianmunicipalities$Men04>3331,1,0)  
# add variable binary to our dataset
belgianmunicipalities<-cbind(belgianmunicipalities,binary) 
# give the name "Bin" to binary variable in the dataset
names(belgianmunicipalities)[18]<-"Bin"   

n=50  # sample size  (50/589=8%)
N=nrow(belgianmunicipalities) # population size

# Select SRS sample
s=srswor(n,N)  # vector 589*1
s
sum(s)
# 1 if is selected, 0 if it is not. Each value is a municipality
# which communes are sampled (are in the sample)?
as.vector(name[s==1])  
# Save the index of the selected municipality
id<-(1:N)[s==1]   # or similary which(s==1)     
id   # the position of the sampled commune
# select data from the sampled communes. 
# Create a new data.frame (see in Global Environment)
sample.srs= belgianmunicipalities[id,]   #50 obs 18 varibales
# Add Finite Population Correction in Case of SRS
sample.srs<-cbind(sample.srs,N)
head(sample.srs)
# Add weights from a SRS without replacement:
# all the units have the same probaility of being selected
sample.srs <-cbind(sample.srs,N/n) 
head(sample.srs)
# Give names to the new variables
names(sample.srs)[19]<-"fpc"
names(sample.srs)[20]<-"pw"    
# Specify the complex survey design
# ?svydesign: check the arguments on the help
srs_design<-svydesign(id=~1,fpc=~fpc,data=sample.srs)
# id=~1 I do not want any cluster (it's a simple random sampling)
# fpc: finite population correction (see details in the help)
# The finite population correction (fpc) factor is used to adjust 
# a variance estimate for an estimated mean or total, 
# so that this variance only applies to the portion of the population 
# that is not in the sample.
# This is used when the sampling fraction 
# (the number of elements or respondents sampled relative to the population) 
# becomes large. 
# The FPC is used in the calculation of the standard error of the estimate.  
# If the value of the FPC is close to 1, it will have little impact and can
# be safely ignored. 

srs_design
summary(srs_design)
str(srs_design)

# Finite population correction (FPC) assumes simple random sampling without 
# replacement of PSUs within each stratum with no subsampling within PSUs.
# Weights must represent population totals for deff to be correct when
# using an FPC.  Note: deft is invariant to the scale of weights.


######## MEAN
# Now we want to estimate the mean income of the popuation
# We can do it using a Horvitz Thompson Estimator
# This estimator is implemented in R using the function 
# svymean
?svymean

mean_inc<-svymean(~ averageincome,srs_design)   
mean_inc

# Now we want to add the confidence interval
# mean_inc[1] value in the first position of object mean_inc
mean_inc[1]
CI_l<-mean_inc[1]-qnorm(0.975)*  mean_inc[2]
CI_u<-mean_inc[1]+qnorm(0.975)* mean_inc[2]
CI_l
CI_u

# Confidence interval can be computed also using funcion confint
?confint
confint(mean_inc)
confint(mean_inc, level=0.90)

# What is the real population mean?
mean(belgianmunicipalities$averageincome)
# This value falls in the confidence interval

# Different Approach

# Compute the mean "by hand
# remember that sample.srs is our sample
# mean income in our data 
mean(sample.srs$averageincome)  
# the same value only because we are in a simple random sampling
# standard error in our sample (without replacement from a finite population)
sqrt(((N-n)/(N))*(var(sample.srs$averageincome)/n))  
var(sample.srs$averageincome)/mean(sample.srs$averageincome) 

####TOTAL

# Suppose that we want to know the total numer of male in our population
# We can use the function svytotal
?svytotal
tot<-svytotal(~Tot04,srs_design)
tot
# Confidence interval
confint(tot)
# Real Value (please, remember that you can compute the real value only
# because this is a test exercise and you have the entire population. Usually
# this is not possible)
sum(belgianmunicipalities$Tot04)




# In the previous example we use the Finite population correction
# However we can also use the weight
?svydesign

nofpc<-svydesign(id=~1,weights=~pw,data= sample.srs)
summary(nofpc)

# Try to compute the same mean that we computed before  
mean.weight<-svymean(~ averageincome, nofpc)
mean.weight
# Compare this value with the value that we used in the previous example
# with the fpc
mean_inc
# we have the same mean but lower SE
# And about Total?
tot.weight<-svytotal(~Tot04,nofpc)
tot.weight
tot
# It depends! You have to use what you have.  

# The svyquantile() function in the survey package estimates
# quantiles by inversion of the estimated distribution function.

quantile.weight<-svyquantile(~Tot04,nofpc, c(.25,.5,.75), ci=TRUE)
quantile.weight

rm(list=ls())


##############

# ASSIGNMENT 1

# Using Data swissmunicipalities from "sampling" package:
# a) Exploring the data: Let's begin with a bit descriptive of the dataset, 
# tables and plot (see prof. Schirripa Spagnolo lecture)
# b) Using a SRS (sample units: municipality name. sample: 10% of the population) 
# compute the estimates of the proportion of men and women 
# aged between 20 and 39 and the mean of the total population 
# What you can say about these quantities? 
# Compare the survey and the "by-hand" results.

#############
##################
# ASSIGNMENT 2

# Example taken from Lehtonen and Pahkinen's Pratical Methods for Design and
# Analysis of compex Survey. page 29. Table 2.4. Estimates from a simple random sample drawn 
# without replacement. n=8, N=32

# Import the dataset from text directly into R using the read.table function and 
# the text= parameter
# specifying the entire data set. The syntax n indicates the end of one line of data

province<-read.table(text=
                       "id cluster ue91 lab91
                     1 1 4123 33786
                     2 4 760 5919
                     3 5 721 4930
                     4 15 142 675
                     5 18 187 1448
                     6 26 331 2543
                     7 30 127 1084
                     8 31 219 1330",
                     header=TRUE)

# a) add two columns to the province data.frame object. the columns (variable) 
# are named fpc and weights. What are the
# right value of these variables?
# b) Construct a "survey.design" object called "province.design" specifying a simple 
# random sampling design.
# c) View the weighted total population of this survey design, by referring to the 
# weights column.
# d) Print the mean and the standard errr of the ue91 variable 
# e) Print the weighted total and the standard error of the ue91 variable

rm(list=ls())
#################

# ASSIGNMENT 3: Together
# Kruuk et al. (1990) used a stratified random sampling design to estimate 
# the number of otter (Lutra lutra) dens or holts along a 1400km 
# coastline of the Shetland Islands.
# The coastline was divided into 237.5km habitable sections, 
# with each section classified as one of four types: 
# cliffs over 10m (89 sections), 
# agricultural (61 sections), 
# peat (40 sections), 
# and non-peat (47 sections). 
# The sample featured here was collected 
# using stratified random sampling, 
# but for this example it will be treated as if it had been collected 
# using simple random sampling. 
# The data are available in the  SDaA package. 
# If you have not done so already you will need to install 
# this package using install.packages("SDaA"). 
# To access the data load the package.

#install.packages('SDaA')
library(SDaA)
library(survey)
data(otters) # The data frame is called otters.
?otters
head(otters)
str(otters)
#Each observation includes the section number, the type of habitat, 
# and the number of holts. Although not necessary for the analysis, 
#it would be helpful to have the stratum names be more descriptive. 
#This can be done by changing habitat into a factor using the factor 
# function and assigning labels to the strata.
otters$habitat <- factor(otters$habitat, labels = c("cliffs", "agricultural", 
                                                    "peat", "non-peat"))
head(otters)

# SPECIFICATION OF THE DESIGN
# Before specifying the design it is necessary to include within the data frame 
# the population size for the finite population correction. 
# This can be done by creating a variable N.
otters$N <- 237
head(otters)
# The svydesign function in the survey package is used to create 
# a survey design object that includes information about the design and the data. 
#A simple random sampling design can be specified as follows.
mydesign <- svydesign(id = ~1, data = otters, fpc = ~N)
# The argument ids = ~1 has to do with if or how elements are grouped 
# within sampling units, such as in cluster sampling. 
# But for simple random sampling simply specify ids = ~1 
# to indicate one element per sampling unit. 
# The argument data = otters indicates the data frame. 
# The argument fpc = ~N indicates the variable that size of the population 
# from which the unit 
# was sampled. 
#  Notice that the variables are always proceeded by a tilde (~). 
# This is a convention of the survey package but is not typical of other packages in R, 
# so it is easy to forget to use it. 
# The object mydesign now includes the data as well as information about the design.


# Inferences can be made by applying special functions to the object 
# created by svydesign. 
# An estimate for the population mean ?? and its associated standard error 
# can be computed using the svymean function. 
# Recall that the estimator of ?? is simply the sample mean \bar{y}, 
# and its estimated variance is (1-n/N)(s^2/n)
svymean(~holts, design = mydesign)
# Thus we estimate that the mean number of holts per section is about 5.44 holts 
# with a bound on the error of estimation of about 2?0.6031???1.21 holts. 
# The svymean function does not report the bound on the error of estimation , 
# but it can be computed easily "by hand" 
# if we multiply the standard error by z. 
# The confint function can be applied to the result of svymean to compute 
# the confidence interval.
confint(svymean(~holts, design = mydesign))
# Thus we estimate that the mean number of holts per section is between 
# 4.26 and 6.62 holts.

# INFERENCES FOR DOMAIN MEANS

# Domain means and totals can be computed using the svyby function which
# compute survey statistics on subsets of a survey defined by factors.
?svyby

svyby(~holts, by = ~habitat, design = mydesign, FUN = svymean)

svyby(~holts, by = ~habitat, design = mydesign, FUN = svytotal)

# Here the by argument specifies the variable that defines the domains. 
# The FUN argument determines what type of parameters should be estimated.

# The domain mean estimator is a ratio so we can compute 
# the estimates for a specific domain by the following code:
otters$d <- ifelse(otters$habitat == "cliffs", 1, 0)
head(otters)
otters$dholts <- otters$holts * otters$d
head(otters)
mydesign <- svydesign(id = ~1, data = otters, fpc = ~N)
svyratio(numerator = ~dholts, denominator = ~d, design = mydesign)

rm(list=ls())
#############################################################
########## STRATIFIED SAMPLING
#############################################################
library(survey)
library(sampling)
data(swissmunicipalities)
?swissmunicipalities
head(swissmunicipalities)
n=50
N=nrow(swissmunicipalities)

nstrat <- ceiling(n/N*table(swissmunicipalities$REG))
ss <- sampling::strata(swissmunicipalities,stratanames="REG",
                       size=nstrat,
                       method="srswor")
ds <- getdata(swissmunicipalities,ss)

dstrat<-svydesign(id=~1, strata=~REG, data=ds, probs=~Prob)
dstrat
summary(dstrat)
svymean(~P00BWTOT, design = dstrat)











