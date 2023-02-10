/*analysis of WVS, WAVE 6*/

 cd "/Users/user_1/Documents/Dati/DIDATTICA/LM MISS/2021_2022/stata/value survey/"
 
 use WV6_Data_stata_v20201117, clear
 

 *variable V240 is sex (1=M, 2=F), recode in a dummy variable (0,1)
 codebook V240
 recode V240 (2=0), into (male)
 
label def malelabel 0 "Female" 1 "Male"
label value male malelabel

tab male, m
 
 *V242 is age
 codebook V242
sum V242 

histogram V242, bin(8)

 recode V242 16/35=1 36/55=2 56/75=3 76/102=4, into(agegroup)
 
tab agegroup, m

label define agelabel 1 "16-35" 2 "36-55" 3 "56-75" 4 "76+"
label value agegroup agelabel

*V248 education level
codebook V248


*V23 life satisfaction
rename V23 lifesat

tab lifesat, m

summarize lifesat, detail
histogram lifesat , discrete normal

graph bar (mean) lifesat, over(agegroup)

table agegroup, c(mean lifesat median lifesat sd lifesat min lifesat max lifesat)

*V24 trust in people
codebook V24
tab V24,m

recode V24 (1=1 "Trusting") (2=0 "Not Trusting"), gen(trust)

table trust, c(mean lifesat)

*cow country 360 Romania 255 Germany 380 Sweden 230 Spain

keep if cow==360 | cow==255 | cow==380 | cow==230

save datareduced, replace

*equality index
codebook equality
table cow, c(mean equality)

graph bar (mean) equality, by(cow)

*some analysis
table cow, c(mean trust) 

*confidence interval for trust (binomial) and equality (continuous)


ci prop trust
ci means equality

/*confidence interval we see the range of possible values the proportion/mean might 
take in the population, 
 The larger our sample the smaller our standard error and therefore the narrower 
 the confidence interval and more precise estimate of the population parameter*/
preserve
 sample 500, count
 ci means equality
 *Note that as the sample size decreases, the confidence intervals increase

restore

* comparison of means for a given variable in two groups
ttest lifesat if cow==360 | cow==255, by(cow)

*anova
oneway lifesat cow

pwcorr lifesat equality, sig

scatter equality lifesat

swilk lifesat

