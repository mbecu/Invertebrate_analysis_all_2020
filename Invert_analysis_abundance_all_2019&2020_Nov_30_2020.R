#################################################################################
#Invertebrate abundance data July and August 2019, and August 2020. Farewell, Stewart, and Tlowils lake 2019; 
#additionally Blackwater and Twaddle 2020.
#Mariella Becu, November 30, 2020
#Testing Models Using LMM and GLMM
#Total Abundance for Stream or Lake site and Disturbance of Streams

###############################################################################

library(tidyverse)
library(nlme)
library(emmeans)
library(car)
library(sjPlot)
library(lme4)
library(lmerTest)
library(effects)
library(dplyr)
library(glmm)
library(lattice)
library(visreg)
library(MASS)
library(afex)

rm(list=ls(all=TRUE))

setwd()

alldata <- read.csv("/Volumes/Mariella's Hard drive/New_Documents_andor_Changes_Harddrive/Analysis 2019&2020/Invertebrates/November 2020/Invertebrates_all_2019&2020_Nov19_2020.csv")

names(alldata)
alldata

Site.all <- (alldata$Site)
Site.all
Block.all <- (alldata$Location)
Block.all
Basket.all <- (alldata$Basket_1)
Basket.all
Month.all <- (alldata$Month)
Month.all
Year.all <- (alldata$Year)
Year.all
Location.all <- (alldata$Stream.or.Lake)
Location.all
Location.ID <- (alldata$Stream.or.Lake.ID)
Location.ID
Disturb.all <- (alldata$Disturbance)
Disturb.all
ABUND.all <- alldata$Abundance_Benthos
ABUND.all
RICH.all <- alldata$Richness_Benthos
RICH.all
Density_ABUND.all <- alldata$Density_Abundance....cm2._Benthos
Density_ABUND.all
Density_RICH.all <- alldata$Density_Richness_Benthos....cm2.
Density_RICH.all

ABUND_data <- data.frame(Block.all, Location.all, Location.ID, Site.all, Basket.all, Month.all, Year.all, Disturb.all, ABUND.all, RICH.all, Density_ABUND.all, Density_RICH.all)
ABUND_data

ABUND_data_disturb <- subset(ABUND_data, !Disturb.all == "Lake Site")
ABUND_data_disturb

names(ABUND_data)
names(ABUND_data_disturb)

alldata.mean <- read.csv("/Volumes/Mariella's Hard drive/New_Documents_andor_Changes_Harddrive/Analysis 2019&2020/Invertebrates/November 2020/Invertebrates_2019&2020_averages_Nov19_2020.csv")

Site.m <- (alldata.mean$Site)
Site.m
Block.m <- (alldata.mean$Location)
Block.m
Basket.m <- (alldata.mean$Basket_1)
Basket.m
Month.m <- (alldata.mean$Month)
Month.m
Year.m <- (alldata.mean$Year)
Year.m
Location.m <- (alldata.mean$Stream.or.Lake)
Location.m
Location.IDm <- (alldata.mean$Stream.or.Lake_ID)
Location.IDm
Disturb.m <- (alldata.mean$Disturbance)
Disturb.m
ABUND.m <- alldata.mean$Abundance_Benthos
ABUND.m
RICH.m <- alldata.mean$Richness_Benthos
RICH.m
Density_ABUND.m <- alldata.mean$Density_Abundance....cm2._Benthos
Density_ABUND.m
Density_RICH.m <- alldata.mean$Density_Richness_Benthos....cm2.
Density_RICH.m

ABUND_data_mean <- data.frame(Block.m, Location.m, Location.IDm, Site.m, Basket.m, Month.m, Year.m, Disturb.m, ABUND.m, RICH.m, Density_ABUND.m, Density_RICH.m)
ABUND_data_mean

ABUND_data_disturb_mean <- subset(ABUND_data_mean, !Disturb.m == "Lake site")
ABUND_data_disturb_mean


#####VIZUALIZING DATA##### --> May have to use either averages or raw data for some graphs that only have one average value...as it doesn't show variation in the sites
FacetPlot = ggplot(ABUND_data_mean, aes(x=(Block.m), y=Density_ABUND.m)) + geom_boxplot()+ scale_y_continuous(name = "Abundance (# of indiv. / cm2)") + scale_x_discrete(name = "Location")+ggtitle("Abundance per Lake")
FacetPlot

FacetPlot1 = ggplot(ABUND_data, aes(x=(Location.all), y=Density_ABUND.all)) + geom_boxplot() + facet_grid(~Block.all) + scale_y_continuous(name = "Abundance (# of indiv./ cm2)") + scale_x_discrete(name = "Location") + ggtitle("Abundance per Site per Lake")
FacetPlot1

FacetPlot2 = ggplot(ABUND_data, aes(x=(Location.all), y=Density_ABUND.all, fill=Month.all)) + geom_boxplot() + facet_grid(Year.all~Block.all) + scale_y_continuous(name = "Abundance (# of indiv./ cm2)") + scale_x_discrete(name = "Location")+ ggtitle("Abundance per Month per Site per Lake")
FacetPlot2

FacetPlot3 = ggplot(ABUND_data_disturb, aes(x=(Disturb.all), y=Density_ABUND.all, fill = Month.all)) + geom_boxplot() +  facet_grid(Year.all~Block.all) + scale_y_continuous(name = "Abundance (# of indiv./ cm2)") + scale_x_discrete(name = "Site")+ ggtitle("Abundance per Month per Indiv. Site per Lake")
FacetPlot3

FacetPlot4 = ggplot(ABUND_data, aes(x=(Site.all), y=Density_ABUND.all, fill = Month.all)) + geom_boxplot() + facet_grid(Year.all~Block.all) + scale_y_continuous(name = "Abundance (# of indiv./ cm2)") + scale_x_discrete(name = "Site")+ ggtitle("Abundance per Month per Indiv. Site per Lake")
FacetPlot4

FacetPlot5 = ggplot(ABUND_data, aes(x=(Location.all), y=(Density_ABUND.all), fill = Month.all)) + geom_boxplot() + facet_grid(~Year.all) + scale_y_continuous(name = "Abundance (# of indiv./ cm2)") + scale_x_discrete(name = "Site")+ ggtitle("Abundance per Year")
FacetPlot5

FacetPlot6 = ggplot(ABUND_data_disturb, aes(x=(Disturb.all), y=Density_ABUND.all, fill = Month.all)) + geom_boxplot() + facet_grid(~Year.all) + scale_y_continuous(name = "Abundance (# of indiv./ cm2)") + scale_x_discrete(name = "Site")+ ggtitle("Abundance per Year")
FacetPlot6

##################  Location ##################
################## GOOD CODE ##################

##COLINEARITY## -- Not an issue because there is only one fixed factor...

pairs(ABUND_data_mean)
vif() #Colinearity is less than 10, so it shouldn't be an issue. Plus there is only one factor, so not an issue at all.

####FITTING MODELS#####

##LMMs##
library(nlme)
A.lm <- lm(log10(ABUND.all) ~ Location.ID, data = ABUND_data)
summary(A.lm)

###Interactions between fixed and random factors###

A.lm.m <- lm(log10(ABUND.m) ~ Location.m + Block.m, data = ABUND_data_mean)
summary(A.lm.m)
anova(A.lm.m)

A.lm.m1 <- lm(log10(ABUND.m) ~ Location.m * Block.m, data = ABUND_data_mean)
summary(A.lm.m)
anova(A.lm.m, A.lm.m1)
##No Block interaction
A.lm.m2 <- lm(log10(ABUND.m) ~ Location.m + Site.m, data = ABUND_data_mean)
A.lm.m3 <- lm(log10(ABUND.m) ~ Location.m * Site.m, data = ABUND_data_mean)
anova(A.lm.m2, A.lm.m3)
#No Site interaction
A.lm.m4 <- lm(log10(ABUND.m) ~ Location.m + Month.m, data = ABUND_data_mean)
A.lm.m5 <- lm(log10(ABUND.m) ~ Location.m * Month.m, data = ABUND_data_mean)
anova(A.lm.m4, A.lm.m5)
##No Month interaction
A.lm.m6 <- lm(log10(ABUND.m) ~ Location.m + Year.m, data = ABUND_data_mean)
A.lm.m7 <- lm(log10(ABUND.m) ~ Location.m * Year.m, data = ABUND_data_mean)
anova(A.lm.m6, A.lm.m7)
##No Year interaction

A1.lmer <- lmer(log10(ABUND.all)~ Location.all + (1 | Block.all/Site.all/Basket.all) + (1|Year.all/Month.all), REML = "true", data = ABUND_data)
summary(A1.lmer)
anova(A1.lmer)
df.residual(A1.lmer)

A1.lmer.ln <- lmer(log(ABUND.all)~ Location.all + (1 | Block.all/Site.all/Basket.all) + (1|Year.all/Month.all), REML = "true", data = ABUND_data)
summary(A1.lmer.ln)
anova(A1.lmer.ln, A1.lmer)
##Log10 fits better

A1.lmer1 <- lmer(log10(ABUND.all)~ 1 + (1 | Block.all/Site.all/Basket.all) + (1|Year.all/Month.all), REML = "true",data = ABUND_data)
summary(A1.lmer1)
anova(A1.lmer1, A1.lmer)
##Location.all is significant in the model.

A1.lmer.m <- lmer(log10(ABUND.m)~ Location.m + (1 | Block.m/Site.m) + (1|Year.m/Month.m), REML = "true", data = ABUND_data_mean)
summary(A1.lmer.m)
anova(A1.lmer.m)
df.residual(A1.lmer.m)

A1.lmer.ln.m <- lmer(log(ABUND.m)~ Location.m + (1 | Block.m/Site.m) + (1|Year.m/Month.m), REML = "true", data = ABUND_data_mean)
summary(A1.lmer.ln.m)
anova(A1.lmer.ln.m, A1.lmer.m)
##Log10 is a better fit

A1.lmer.m1 <- lmer(log10(ABUND.m)~ 1 + (1 | Block.m/Site.m) + (1|Year.m/Month.m), REML = "true", data = ABUND_data_mean)
summary(A1.lmer.m1)
anova(A1.lmer.m1, A1.lmer.m)
#Location.m is significant in the model

#Testing random effects
summary(A1.1<-(lmer(log10(ABUND.m)~ Location.m + (1 | Block.m), REML = "true", data = ABUND_data_mean)))
anova(A1.1, A1.lmer.m)  
summary(A1.2<-(lmer(log10(ABUND.m)~ Location.m + (1 | Block.m/Site.m), REML = "true", data = ABUND_data_mean)))
anova(A1.2, A1.lmer.m)
summary(A1.3<-(lmer(log10(ABUND.m)~ Location.m + Year.m + Month.m + (1 | Block.m/Site.m), REML = "true", data = ABUND_data_mean)))
anova(A1.3, A1.lmer.m)
summary(A1.4<-(lmer(log10(ABUND.m)~ Location.m + Month.m + (1 | Block.m/Site.m) + (1| Year.m), REML = "true", data = ABUND_data_mean)))
anova(A1.4, A1.lmer.m) 
#Month is significant in the model...might be intersting to look at, but not a hypothesis I was going to test.
summary(A1.5<-(lmer(log10(ABUND.m)~ Location.m * Month.m + (1 | Block.m/Site.m) + (1| Year.m), REML = "true", data = ABUND_data_mean)))
anova(A1.5, A1.4) 
#There is no interaction between Location.m and Month.m 
summary(A1.6<-(lmer(log10(ABUND.m)~ Location.m + (1| Year.m/Month.m), REML = "true", data = ABUND_data_mean)))
summary(A1.7<-(lmer(log10(ABUND.m)~ Location.m + (1|Block.m) + (1| Year.m/ Month.m), REML = "true", data = ABUND_data_mean)))
anova(A1.6, A1.7)

###Testing Assumptions###
#Equal Variance
plot(resid(A1.lmer.m))          # plot of residuals against predicted values
leveneTest(log10(ABUND.m) ~ Location.m, ABUND_data_mean)
#Yes!

plot(resid(A1.lmer))          # plot of residuals against predicted values
leveneTest(log10(ABUND.all) ~ Location.all, ABUND_data)
#Yes!

#Normality
hist(resid(A1.lmer.m))      # histogram of residuals
shapiro.test(log10(ABUND.m))  #Normally distributed
qqnorm(resid(A1.lmer.m))
qqline(resid(A1.lmer.m),col=2)
#Yes to normality!

hist(resid(A1.lmer))      # histogram of residuals
shapiro.test(log10(ABUND.all))  #Normally distributed
qqnorm(resid(A1.lmer))
qqline(resid(A1.lmer),col=2)
#Yes to normality!

####Both A1.lmer and A1.lmer.m models are pretty much the same. Will use A1.lmer.m from now on.

#Get random effects
ranef(A1.lmer.m)       # 
summary(A1.lmer.m)     # variances for random effects, fit metrics
VarCorr(A1.lmer.m)     # variance components for random effects  

#Inference#

##The LRT (Likelihood ratio test) is generally preferred over Wald tests of fixed effects in mixed models. For linear 
#mixed models with little correlation among predictors, a Wald test using the approach of Kenward and Rogers (1997)
#will be quite similar to LRT test results. The SSCC does not recommend the use of Wald tests 
#for generalized models.
#####Either Satterthwaite's method or Kenward and Roger's are the most preferred and conservative tests to use

summary(A1.lmer.m) #Satterthwaite's method
anova(A1.lmer.m)   #Satterthwaite's method
Anova(A1.lmer.m)   #Type 2 Wald Chi-square test
anova(A1.lmer.m, ddf="Kenward-Roger", REML = TRUE, refit=FALSE) #Kenward-Roger
summary(A1.lmer) #Satterthwaite's method
anova(A1.lmer)   #Satterthwaite's method
Anova(A1.lmer)   #Type 2 Wald Chi-square test
anova(A1.lmer, ddf="Kenward-Roger", REML = TRUE, refit=FALSE) #Kenward-Roger
#KRmodComp. Only available for linear mixed models (does not support glmer() models.) 
#An F test of nested models with an estimated degrees of freedom. The KRmodcomp() function estimates which 
#F-test distribution is the best distribution from the family of F distributions. This function addresses 
#the degrees of freedom concern.
library(pbkrtest)
KRmodcomp(A1.lmer, A1.lmer1)     #Kenward-Roger
KRmodcomp(A1.lmer.m, A1.lmer.m1) #Kenward-Roger

#Profiled confidence interval. While not a formal test, an interval which does not contain zero
#indicates the parameter is significant.
confint(A1.lmer.m, level = 0.95)
confint(A1.lmer, level = 0.95) 
##Calculating the confidence intervals of the difference between the means
##(the slope coefficient = Stream estimate in Summary)
##Generally, a slope confidence interval which contains zero means that if we repeated the 
##experiment we might find the reverse trend as presented in our boxplot.

###Don't really need to find the least-square means. Only if the variance attributed to the random variables was super large. 
###Otherwise, just using whatever mean the model spits out is fine.
A1.lmer.m.em <- emmeans(A1.lmer.m, pairwise~Location.m, adjust = "bonferroni")
A1.lmer.m.em
#Back-transforming data
plot(A1.lmer.m.em, type = "response")
summary(A1.lmer.m.em, infer = TRUE, type = "response") ##Don't think I have to look at p-values because 2 levels
##The means are similar to the model outputs above using Satterthwaite's method and Kenward-Roger.

# uses kenward-roger degrees of freedom method
emmeans(A1.lmer.m, "Location.m", infer = TRUE, data = ABUND_data_mean, type = 'response')
# uses satterthwaite degrees of freedom method
emmeans(A1.lmer.m, "Location.m", infer = TRUE, data = ABUND_data_mean, lmer.df = "satterthwaite", type = 'response') 

FacetPlot = ggplot(ABUND_data_mean, aes(x=(Location.m), y=(Density_ABUND.m), fill = Month.m)) + geom_boxplot() + facet_grid(~Year.m) + scale_y_continuous(name = "Abundance (# of indiv./ cm2)") + scale_x_discrete(name = "Site")+ ggtitle("Abundance per Year")
FacetPlot

A1.lmer.em <- emmeans(A1.lmer, pairwise~Location.all, adjust = "bonferroni")
A1.lmer.em
#Back-transforming data
plot(A1.lmer.em, type = "response")
summary(A1.lmer.em, infer = TRUE, type = "response") ##Don't think I have to look at p-values because 2 levels
##The means are similar to the model outputs above using Satterthwaite's method and Kenward-Roger.

# uses kenward-roger degrees of freedom method
emmeans(A1.lmer, "Location.all", infer = TRUE, data = ABUND_data, type = 'response')
# uses satterthwaite degrees of freedom method
emmeans(A1.lmer, "Location.all", infer = TRUE, data = ABUND_data, lmer.df = "satterthwaite", type = 'response') 


###Effect size####
#Unstandardized effect size
Location.abund.m <- data.frame(ABUND.m, Location.m)
Location.abund.m
Stream.abund.m <- subset(Location.abund.m, !Location.m == "Lake")
Stream.abund.m
Stream_yhat.abund.m <- mean(Stream.abund.m$ABUND.m)
Stream_yhat.abund.m
Lake.abund.m <- subset(Location.abund.m, !Location.m == "Stream")
Lake.abund.m
Lake_yhat.abund.m <- mean(Lake.abund.m$ABUND.m)
Lake_yhat.abund.m

#Unstandardized mean difference
USMD.m <- Stream_yhat.abund.m - Lake_yhat.abund.m
USMD.m

#Emmeans back-transformed data standardized mean difference from model
summary(A1.lmer.em, infer = TRUE, type = "response")
#Location.all response    SE   df lower.CL upper.CL 
#Lake              165  74.6 2.76     36.4      749
#Stream            289 130.5 2.76     63.8     1310  
USMD1 <- (289 - 165)
USMD1

library(effectsize)
allEffects(A1.lmer)
allEffects(A1.lmer.m)

plot(allEffects(A1.lmer))
plot(allEffects(A1.lmer.m))

#Cohen's d is the appropriate effect size measure if two groups have similar standard deviations and are of the same size. 
#Glass's delta, which uses only the standard deviation of the control group, is an alternative measure if each group has a 
#different standard deviation. Hedges' g, which provides a measure of effect size weighted according to the relative size of 
#each sample, is an alternative where there are different sample sizes. (This is important! If you've got different sample sizes 
#then you should use Hedges' g.)

##To calculate the standardized mean difference between two groups, 
#subtract the mean of one group from the other (M1 – M2) and divide the result by the standard deviation (SD) 
#of the population from which the groups were sampled (pooled SD).

#Standardized mean difference
sd(Lake.abund.m$ABUND.m)
sd(Stream.abund.m$ABUND.m)
(sd_pooled(ABUND.m ~ Location.m, data = Location.abund.m))
SD_PLD <- sqrt(((sd(Stream.abund.m$ABUND.m))^2+(sd(Lake.abund.m$ABUND.m))^2)/2)
SD_PLD

#Cohen's D
SMD <- (Stream_yhat.abund.m-Lake_yhat.abund.m)/(sd_pooled(ABUND.m ~ Location.m, data = Location.abund.m))
SMD
SMD1 <-(Stream_yhat.abund.m-Lake_yhat.abund.m)/(SD_PLD)
SMD1

#Are standard deviations between Lake and Stream significantly different? Have to use the control as the denominator...
#Is Lake really the control?
#Glass's delta
SMD2 <- (Stream_yhat.abund.m-Lake_yhat.abund.m)/(sd(Lake.abund.m$ABUND.m))
SMD2



################# Disturbance #################
################## GOOD CODE ##################

##COLINEARITY## -- Not an issue because there is only one fixed factor...

pairs(ABUND_data_disturb_mean)
vif() #Colinearity is less than 10, so it shouldn't be an issue. Plus there is only one factor, so not an issue at all.

####FITTING MODELS#####

##LMMs##
library(nlme)
A.lm.d <- lm(log10(ABUND.all) ~ Disturb.all, data = ABUND_data_disturb)
summary(A.lm.d)

###Interactions between fixed and random factors###

A.lm.m.d <- lm(log10(ABUND.m) ~ Disturb.m + Block.m, data = ABUND_data_disturb_mean)
summary(A.lm.m.d)
anova(A.lm.m.d)

A.lm.m1.d <- lm(log10(ABUND.m) ~ Disturb.m * Block.m, data = ABUND_data_disturb_mean)
summary(A.lm.m.d)
anova(A.lm.m.d, A.lm.m1.d)
##No Block interaction
A.lm.m2.d <- lm(log10(ABUND.m) ~ Disturb.m + Site.m, data = ABUND_data_disturb_mean)
A.lm.m3.d <- lm(log10(ABUND.m) ~ Disturb.m * Site.m, data = ABUND_data_disturb_mean)
anova(A.lm.m2.d, A.lm.m3.d)
#No Site interaction
A.lm.m4.d <- lm(log10(ABUND.m) ~ Disturb.m + Month.m, data = ABUND_data_disturb_mean)
A.lm.m5.d <- lm(log10(ABUND.m) ~ Disturb.m * Month.m, data = ABUND_data_disturb_mean)
anova(A.lm.m4.d, A.lm.m5.d)
##No Month interaction
A.lm.m6.d <- lm(log10(ABUND.m) ~ Disturb.m + Year.m, data = ABUND_data_disturb_mean)
A.lm.m7.d <- lm(log10(ABUND.m) ~ Disturb.m * Year.m, data = ABUND_data_disturb_mean)
anova(A.lm.m6.d, A.lm.m7.d)
##No Year interaction

A1.lmer.d1 <- lmer((ABUND.all)~ Disturb.all + (1 | Block.all/Site.all/Basket.all) + (1|Year.all/Month.all), REML = "true", data = ABUND_data_disturb)
summary(A1.lmer.d1)
anova(A1.lmer.d1)
df.residual(A1.lmer.d1)

A1.lmer.d <- lmer(log10(ABUND.all)~ Disturb.all + (1 | Block.all/Site.all/Basket.all) + (1|Year.all/Month.all), REML = "true", data = ABUND_data_disturb)
summary(A1.lmer.d)
anova(A1.lmer.d)
df.residual(A1.lmer.d)

anova(A1.lmer.d1, A1.lmer.d)
##Log fit is way better than response

A1.lmer.ln.d <- lmer(log(ABUND.all)~ Disturb.all + (1 | Block.all/Site.all/Basket.all) + (1|Year.all/Month.all), REML = "true", data = ABUND_data_disturb)
summary(A1.lmer.ln.d)
anova(A1.lmer.ln.d, A1.lmer.d)
##Log10 fits better

A1.lmer1.d <- lmer(log10(ABUND.all)~ 1 + (1 | Block.all/Site.all/Basket.all) + (1|Year.all/Month.all), REML = "true",data = ABUND_data_disturb)
summary(A1.lmer1.d)
anova(A1.lmer1.d, A1.lmer.d)
##Disturb.all is significant in the model.

A1.lmer.m.d <- lmer(log10(ABUND.m)~ Disturb.m + (1 | Block.m/Site.m) + (1|Year.m/Month.m), REML = "true", data = ABUND_data_disturb_mean)
summary(A1.lmer.m.d)
anova(A1.lmer.m.d)
df.residual(A1.lmer.m.d)

A1.lmer.ln.m.d <- lmer(log(ABUND.m)~ Disturb.m + (1 | Block.m/Site.m) + (1|Year.m/Month.m), REML = "true", data = ABUND_data_disturb_mean)
summary(A1.lmer.ln.m.d)
anova(A1.lmer.ln.m.d, A1.lmer.m.d)
##Log10 is a better fit

A1.lmer.m1.d <- lmer(log10(ABUND.m)~ 1 + (1 | Block.m/Site.m) + (1|Year.m/Month.m), REML = "true", data = ABUND_data_disturb_mean)
summary(A1.lmer.m1.d)
anova(A1.lmer.m1.d, A1.lmer.m.d)
#Disturb.m is significant in the model

#Testing random effects
summary(A1.1.d<-(lmer(log10(ABUND.m)~ Disturb.m + (1 | Block.m), REML = "true", data = ABUND_data_disturb_mean)))
anova(A1.1.d, A1.lmer.m.d)  
summary(A1.2.d<-(lmer(log10(ABUND.m)~ Disturb.m + (1 | Block.m/Site.m), REML = "true", data = ABUND_data_disturb_mean)))
anova(A1.2.d, A1.lmer.m.d)
summary(A1.3.d<-(lmer(log10(ABUND.m)~ Disturb.m + Year.m + Month.m + (1 | Block.m/Site.m), REML = "true", data = ABUND_data_disturb_mean)))
anova(A1.3.d, A1.lmer.m.d)
##Model with Year and Month as fixed factors is a better fit -- but not what I am testing.
summary(A1.4.d<-(lmer(log10(ABUND.m)~ Disturb.m + Month.m + (1 | Block.m/Site.m) + (1| Year.m), REML = "true", data = ABUND_data_disturb_mean)))
anova(A1.4.d, A1.lmer.m.d) 
#Month is significant in the model...might be intersting to look at, but not a hypothesis I was going to test.
summary(A1.5.d<-(lmer(log10(ABUND.m)~ Disturb.m * Month.m + (1 | Block.m/Site.m) + (1| Year.m), REML = "true", data = ABUND_data_disturb_mean)))
anova(A1.5.d, A1.4.d) 
#There is no interaction between Disturb.m and Month.m 
summary(A1.6.d<-(lmer(log10(ABUND.m)~ Disturb.m + (1| Year.m/Month.m), REML = "true", data = ABUND_data_disturb_mean)))
summary(A1.7.d<-(lmer(log10(ABUND.m)~ Disturb.m + (1|Block.m) + (1| Year.m/ Month.m), REML = "true", data = ABUND_data_disturb_mean)))
anova(A1.6.d, A1.7.d)

###Testing Assumptions###
#Equal Variance
plot(resid(A1.lmer.m.d))          # plot of residuals against predicted values
leveneTest(log10(ABUND.m) ~ Disturb.m, ABUND_data_disturb_mean)
#Yes!

plot(resid(A1.lmer.d))          # plot of residuals against predicted values
leveneTest(log10(ABUND.all) ~ Disturb.all, ABUND_data_disturb)
#Yes!

#Normality
hist(resid(A1.lmer.m.d))      # histogram of residuals
shapiro.test(log10(ABUND.m))  #Normally distributed
qqnorm(resid(A1.lmer.m.d))
qqline(resid(A1.lmer.m.d),col=2)
#Yes to normality!

hist(resid(A1.lmer.d))      # histogram of residuals
shapiro.test(log10(ABUND.all))  #Normally distributed
qqnorm(resid(A1.lmer.d))
qqline(resid(A1.lmer.d),col=2)
#Yes to normality!

####Both A1.lmer.d and A1.lmer.m.d models are pretty much the same. Will use A1.lmer.m.d from now on.

#Get random effects
ranef(A1.lmer.m.d)       # 
summary(A1.lmer.m.d)     # variances for random effects, fit metrics
VarCorr(A1.lmer.m.d)     # variance components for random effects  

#Inference#

##The LRT (Likelihood ratio test) is generally preferred over Wald tests of fixed effects in mixed models. For linear 
#mixed models with little correlation among predictors, a Wald test using the approach of Kenward and Rogers (1997)
#will be quite similar to LRT test results. The SSCC does not recommend the use of Wald tests 
#for generalized models.
#####Either Satterthwaite's method or Kenward and Roger's are the most preferred and conservative tests to use

summary(A1.lmer.m.d) #Satterthwaite's method
anova(A1.lmer.m.d)   #Satterthwaite's method
Anova(A1.lmer.m.d)   #Type 2 Wald Chi-square test
anova(A1.lmer.m.d, ddf="Kenward-Roger", REML = TRUE, refit=FALSE) #Kenward-Roger
summary(A1.lmer.d) #Satterthwaite's method
anova(A1.lmer.d)   #Satterthwaite's method
Anova(A1.lmer.d)   #Type 2 Wald Chi-square test
anova(A1.lmer.d, ddf="Kenward-Roger", REML = TRUE, refit=FALSE) #Kenward-Roger
#KRmodComp. Only available for linear mixed models (does not support glmer() models.) 
#An F test of nested models with an estimated degrees of freedom. The KRmodcomp() function estimates which 
#F-test distribution is the best distribution from the family of F distributions. This function addresses 
#the degrees of freedom concern.
library(pbkrtest)
KRmodcomp(A1.lmer.d, A1.lmer1.d)     #Kenward-Roger
KRmodcomp(A1.lmer.m.d, A1.lmer.m1.d) #Kenward-Roger

#Profiled confidence interval. While not a formal test, an interval which does not contain zero
#indicates the parameter is significant.
confint(A1.lmer.m.d, level = 0.95)
confint(A1.lmer.d, level = 0.95) 
##Calculating the confidence intervals of the difference between the means
##(the slope coefficient = Harvested Stream estimate in Summary)
##Generally, a slope confidence interval which contains zero means that if we repeated the 
##experiment we might find the reverse trend as presented in our boxplot.

###Don't really need to find the least-square means. Only if the variance attributed to the random variables was super large. 
###Otherwise, just using whatever mean the model spits out is fine.
A1.lmer.m.em.d <- emmeans(A1.lmer.m.d, pairwise~Disturb.m, adjust = "bonferroni")
A1.lmer.m.em.d
#Back-transforming data
plot(A1.lmer.m.em.d, type = "response")
summary(A1.lmer.m.em.d, infer = TRUE, type = "response") ##Don't think I have to look at p-values because 2 levels
##The means are similar to the model outputs above using Satterthwaite's method and Kenward-Roger.

# uses kenward-roger degrees of freedom method
emmeans(A1.lmer.m.d, "Disturb.m", infer = TRUE, data = ABUND_data_disturb_mean, type = 'response')
# uses satterthwaite degrees of freedom method
emmeans(A1.lmer.m.d, "Disturb.m", infer = TRUE, data = ABUND_data_disturb_mean, lmer.df = "satterthwaite", type = 'response') 

FacetPlot = ggplot(ABUND_data_disturb_mean, aes(x=(Disturb.m), y=(Density_ABUND.m), fill = Month.m)) + geom_boxplot() + facet_grid(~Year.m) + scale_y_continuous(name = "Abundance (# of indiv./ cm2)") + scale_x_discrete(name = "Site")+ ggtitle("Abundance per Year")
FacetPlot

A1.lmer.em.d <- emmeans(A1.lmer.d, pairwise~Disturb.all, adjust = "bonferroni")
A1.lmer.em.d
#Back-transforming data
plot(A1.lmer.em.d, type = "response")
summary(A1.lmer.em.d, infer = TRUE, type = "response") ##Don't think I have to look at p-values because 2 levels
##The means are similar to the model outputs above using Satterthwaite's method and Kenward-Roger.

# uses kenward-roger degrees of freedom method
emmeans(A1.lmer.d, "Disturb.all", infer = TRUE, data = ABUND_data_disturb, type = 'response')
# uses satterthwaite degrees of freedom method
emmeans(A1.lmer.d, "Disturb.all", infer = TRUE, data = ABUND_data_disturb, lmer.df = "satterthwaite", type = 'response') 


###Effect size####
#Unstandardized effect size
Harvested.abund.m <- subset(ABUND_data_disturb_mean, !Disturb.m == "Unharvested")
Harvested.abund.m
Harvested_yhat.abund.m <- mean(Harvested.abund.m$ABUND.m)
Harvested_yhat.abund.m
Unharvested.abund.m <- subset(ABUND_data_disturb_mean, !Disturb.m == "Harvested")
Unharvested.abund.m
Unharvested_yhat.abund.m <- mean(Unharvested.abund.m$ABUND.m)
Unharvested_yhat.abund.m

#Unstandardized mean difference
USMD.m.d <- Unharvested_yhat.abund.m - Harvested_yhat.abund.m
USMD.m.d

#Emmeans back-transformed data standardized mean difference from model
summary(A1.lmer.em.d, infer = TRUE, type = "response")
#Disturb.all response  SE   df lower.CL upper.CL t.ratio p.value
#Harvested        195 106 1.76     13.6     2800  9.748  0.0155 
#Unharvested      455 246 1.76     31.7     6513 11.308  0.0120 
USMD1.d <- (455-195)
USMD1.d

library(effectsize)
allEffects(A1.lmer.d)
allEffects(A1.lmer.m.d)

plot(allEffects(A1.lmer.d))
plot(allEffects(A1.lmer.m.d))

#Cohen's d is the appropriate effect size measure if two groups have similar standard deviations and are of the same size. 
#Glass's delta, which uses only the standard deviation of the control group, is an alternative measure if each group has a 
#different standard deviation. Hedges' g, which provides a measure of effect size weighted according to the relative size of 
#each sample, is an alternative where there are different sample sizes. (This is important! If you've got different sample sizes 
#then you should use Hedges' g.)

##To calculate the standardized mean difference between two groups, 
#subtract the mean of one group from the other (M1 – M2) and divide the result by the standard deviation (SD) 
#of the population from which the groups were sampled (pooled SD).

#Standardized mean difference
sd(Unharvested.abund.m$ABUND.m)
sd(Harvested.abund.m$ABUND.m)
(sd_pooled(ABUND.m ~ Disturb.m, data = ABUND_data_disturb_mean))
SD_PLD.d <- sqrt(((sd(Harvested.abund.m$ABUND.m))^2+(sd(Unharvested.abund.m$ABUND.m))^2)/2)
SD_PLD.d

#Cohen's D
SMD.d <- (Unharvested_yhat.abund.m-Harvested_yhat.abund.m)/(sd_pooled(ABUND.m ~ Disturb.m, data = ABUND_data_disturb_mean))
SMD.d
SMD1.d <-(Unharvested_yhat.abund.m-Harvested_yhat.abund.m)/(SD_PLD.d)
SMD1.d

#Are standard deviations between Unharvested and Harvested significantly different? Have to use the control as the denominator...
#Is Unharvested really the control?
#Glass's delta
SMD2.d <- (Unharvested_yhat.abund.m-Harvested_yhat.abund.m)/(sd(Unharvested.abund.m$ABUND.m))
SMD2.d

###############################################
###############################################
###############################################
###############Fitting GLMMs###################

A1.glmer <- glmer(ABUND.all~ Location.all + (1 | Block.all/Site.all/Basket.all) + (1|Year.all/Month.all), data = ABUND_data, family = poisson(link = "log"))
summary(A1.glmer)
Anova(A1.glmer)
##Singular fit --> Overfitted model##

anova(A1.lmer, A1.glmer)

A1.glmer.a <- glmer(ABUND.all~ Location.all + (1 | Block.all/Site.all/Basket.all) + (1|Year.all/Month.all), data = ABUND_data, family = gaussian(link = "log"))
summary(A1.glmer.a)
Anova(A1.glmer.a)

anova(A1.glmer, A1.glmer.a)

overdisp_fun <- function(A1.glmer) {
  rdf <- df.residual(A1.glmer)
  rp <- residuals(A1.glmer,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)}
overdisp_fun(A1.glmer)
overdisp_fun(A1.glmer.a)
##Overdispersed##

A2.glmer <- glmer.nb(ABUND.all~Location.all + (1 | Block.all/Site.all/Basket.all) + (1|Year.all/Month.all), data = ABUND_data)
summary(A2.glmer)
anova(A2.glmer)
Anova(A2.glmer, Type = 3)

overdisp_fun(A2.glmer)
##Not overdispersed## 

anova(A1.glmer, A2.glmer)
##A2.glmer is a way better fit##

anova(A1.lmer, A2.glmer)
#Don't know if I can compare these, but A1.lmer is a better fit.

#GLMM with averaged data##
A1.glmer.m <- glmer(round(ABUND.m)~ Location.m + (1 | Block.m/Site.m) + (1|Year.m/Month.m), data = ABUND_data_mean, family = poisson(link = "log"))
summary(A1.glmer.m)
anova(A1.glmer.m)
Anova(A1.glmer.m)

overdisp_fun(A1.glmer.m)
##Overdispersed## 

A2.glmer.m <- glmer.nb(round(ABUND.m)~ Location.m + (1 | Block.m/Site.m) + (1|Year.m/Month.m), data = ABUND_data_mean)
summary(A2.glmer.m)
anova(A2.glmer.m)
Anova(A2.glmer.m)
confint(A2.glmer.m)

overdisp_fun(A2.glmer.m)
##Not Overdispersed## 

anova(A1.glmer.m, A2.glmer.m)
##A2.glmer.m is a better fit##
A3.glmer <- glmer.nb(round(ABUND.all)~ 1 + (1 | Block.all/Site.all/Basket.all) + (1|Year.all/Month.all), data = ABUND_data)
A3.glmer.m <- glmer.nb(round(ABUND.m)~ 1 + (1 | Block.m/Site.m) + (1|Year.m/Month.m), data = ABUND_data_mean)
summary(A3.glmer.m)
anova(A3.glmer.m, A2.glmer.m)
##Location.m is significant in the model

anova(A2.glmer.m, A1.lmer.m)
#Don't know if I can compare these, but A1.lmer.m is a better fit --> same as last time.

##Testing Assumptions ~ Model Diagnostics##
#Linearity, Normality, Independance

##The plot() function will produce a residual plot for a glmm model that is similar to the plot for lmer models. 
##The plot() function plots the Pearson residuals, residuals scaled by variance function, verses the fitted values 
##on the response scale. For generalized models it is often more useful to examine the residuals plotted on the link scale, η, 
##instead of the response scale. The following code demonstrate producing a residual plot on the link scale.
ggplot(data.frame(eta=predict(A2.glmer.m,type="link"),pearson=residuals(A2.glmer.m,type="pearson")),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()
#Looks pretty good.
plot(fitted(A2.glmer.m), resid(A2.glmer.m, type = "pearson")) #residuals vs fitted
abline(h=0)

#Normality?
par(mfrow=c(2,2))
qqnorm(resid(A2.glmer.m, type = "pearson"), main="normal qq-plot, residuals")
qqline(resid(A2.glmer.m, type = "pearson"))
hist(resid(A2.glmer.m, type = "pearson"))

#Generalized linear models are freed from the assumption that residuals are normally distributed with equal variance, 
#but the method nevertheless makes important assumptions that should be checked. Unfortunately there is no plot(z) method for 
#glm() model objects. If you enter plot(z), the resulting plots will look strange because the data are discrete, 
#and they won’t be easy to interpret. (R is using the plot() function for lm() model objects, which aren’t valid for glm() objects.) 
#Use a loess curve to check if the fitted curve is reasonably close to the fitted logistic or log-linear regression curves.

ggplot(ABUND_data_mean, aes(Location.m, ABUND.m)) + 
  geom_point(size = 2, col = "firebrick") + 
  geom_smooth(method = "loess", size = 1, col = "black", span = 0.8) +
  theme_classic()

#Sensitivity to some data?
#The following code determines which of the observations have the highest leverage and displays these observations. 
#The code also generates a new model without these observations and then compares the coefficients for the will all observations
#to this new model with some observations removed.
ggplot(data.frame(lev=hatvalues(A2.glmer.m),pearson=residuals(A2.glmer.m,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()

#Use visreg() to visualize the model fit on the transformed scale (the function uses predict(z) to generate the result) 
#and “working values”.
# Visualize fit on the original scale
visreg(A2.glmer.m, xvar = "Location.m", ylim = range(ABUND.m), rug = 2, scale = "response")
# Visualize fit on the transformed scale (showing working values)
visreg(A2.glmer.m, xvar = "Location.m") 
visreg(A2.glmer, xvar = "Location.all")

#In base R, use fitted(z) to plot the predicted values corresponding to your data points on the original scale of the data.
plot(jitter(ABUND.m, amount = 0.02) ~ Location.m, data = ABUND_data_mean)
yhat <- fitted(A2.glmer.m)
lines(yhat[order(Location.m)] ~ Location.m[order(Location.m)])


#Use summary to view the parameter estimates (slope and intercept) on the logit or log scale, 
#along with standard errors. Note that P-values in the summary() output are based on a normal 
#approximation and are not accurate for small to moderate sample sizes – use the log likelihood ratio test instead 
#(see anova() below).
summary(A2.glmer.m)
A2.glmer.m.em
anova(A2.glmer.m, A3.glmer.m) #Location.m is significant ~ 0.015

#A method for calculating likelihood based confidence intervals for parameters is available in the MASS library.
confint(A2.glmer.m, level = 0.95)

##MODEL VALIDATION##

# To obtain estimates of the fixed effects and variance components, with confidence intervals#
summary(A2.glmer.m)
VarCorr(A2.glmer.m)
confint(A2.glmer.m) 

####Both A2.glmer and A2.glmer.m models are pretty much the same. Will use A2.glmer.m from now on.

#Get random effects
ranef(A2.glmer.m)     
summary(A2.glmer.m)    # variances for random effects, fit metrics
VarCorr(A2.glmer.m)     # variance components for random effects  

#Inference#

##The LRT (Likelihood ratio test) is generally preferred over Wald tests of fixed effects in mixed models. For linear 
#mixed models with little correlation among predictors, a Wald test using the approach of Kenward and Rogers (1997)
#will be quite similar to LRT test results. The SSCC does not recommend the use of Wald tests 
#for generalized models.
#####Either Satterthwaite's method or Kenward and Roger's are the most preferred and conservative tests to use

summary(A2.glmer.m) #Maximum likelihood (Laplace Approximation)
anova(A2.glmer.m)   #
Anova(A2.glmer.m)   #Type 2 Wald Chi-square test
summary(A2.glmer)   #Maximum likelihood (Laplace Approximation)
anova(A2.glmer)     #
Anova(A2.glmer)     #Type 2 Wald Chi-square test
anova(A1.lmer, ddf="Kenward-Roger", REML = TRUE, refit=FALSE) #Kenward-Roger
#KRmodComp. Only available for linear mixed models (does not support glmer() models.) 

#I was in contact with Ben Bolker, the author of the lme4 package. F tests don't really work for GLM(M)s. 
#(F tests are based on the uncertainty of the estimate of the residual variance; typical (binomial & Poisson) 
#GLMs don't estimate a residual variance at all. There is a theory of "Bartlett corrections", which are finite-size
#corrections for GLMs, but they're not widely used.). To calculate the P-value associated with a variable we need 
#to create a model without this variable and compare both models as follows using LRT test. 
#The Chi-sqaure and P-value of such comparison is the P-value associated to the variable
anova(A2.glmer.m, A3.glmer.m)
anova(A2.glmer, A3.glmer)

##Confint not working for GLMER
#Profiled confidence interval. While not a formal test, an interval which does not contain zero
#indicates the parameter is significant. 
confint(A2.glmer.m, level = 0.95)
confint(A2.glmer, level = 0.95) 
##Calculating the confidence intervals of the difference between the means
##(the slope coefficient = Stream estimate in Summary)
##Generally, a slope confidence interval which contains zero means that if we repeated the 
##experiment we might find the reverse trend as presented in our boxplot.

###Don't really need to find the least-square means. Only if the variance attributed to the random variables was super large. 
###Otherwise, just using whatever mean the model spits out is fine.
A2.glmer.m.em <- emmeans(A2.glmer.m, pairwise~Location.m, adjust = "bonferroni")
A2.glmer.m.em
#Back-transforming data
plot(A2.glmer.m.em, type = "response")
summary(A2.glmer.m.em, infer = TRUE, type = "response") ##Don't think I have to look at p-values because 2 levels

#####Don't think this is working
# uses kenward-roger degrees of freedom method <---Not sure if it is actually doing the kenward-roger
emmeans(A2.glmer.m, "Location.m", infer = TRUE, data = ABUND_data_mean, type = 'response')
# uses satterthwaite degrees of freedom method
emmeans(A2.glmer.m, "Location.m", infer = TRUE, data = ABUND_data_mean, lmer.df = "satterthwaite", type = 'response') 

FacetPlot = ggplot(ABUND_data_mean, aes(x=(Location.m), y=(Density_ABUND.m), fill = Month.m)) + geom_boxplot() + facet_grid(~Year.m) + scale_y_continuous(name = "Abundance (# of indiv./ cm2)") + scale_x_discrete(name = "Site")+ ggtitle("Abundance per Year")
FacetPlot

A2.glmer.em <- emmeans(A2.glmer, pairwise~Location.all, adjust = "bonferroni")
A2.glmer.em
#Back-transforming data
plot(A2.glmer.em, type = "response")
summary(A2.glmer.em, infer = TRUE, type = "response") ##Don't think I have to look at p-values because 2 levels
##The means are similar to the model outputs above using Satterthwaite's method and Kenward-Roger.

#####Don't think this is working for glmer
# uses kenward-roger degrees of freedom method
emmeans(A1.lmer, "Location.all", infer = TRUE, data = ABUND_data, type = 'response')
# uses satterthwaite degrees of freedom method
emmeans(A1.lmer, "Location.all", infer = TRUE, data = ABUND_data, lmer.df = "satterthwaite", type = 'response') 


###Effect size####
#Unstandardized effect size
Location.abund.m <- data.frame(ABUND.m, Location.m)
Location.abund.m
Stream.abund.m <- subset(Location.abund.m, !Location.m == "Lake")
Stream.abund.m
Stream_yhat.abund.m <- mean(Stream.abund.m$ABUND.m)
Stream_yhat.abund.m
Lake.abund.m <- subset(Location.abund.m, !Location.m == "Stream")
Lake.abund.m
Lake_yhat.abund.m <- mean(Lake.abund.m$ABUND.m)
Lake_yhat.abund.m

#Unstandardized mean difference
USMD.m <- Stream_yhat.abund.m - Lake_yhat.abund.m
USMD.m

#Emmeans back-transformed data standardized mean difference from model
summary(A2.glmer.em, infer = TRUE, type = "response")
#Location.all response    SE  df asymp.LCL asymp.UCL
#Lake              178  62.8 Inf      89.5       356 
#Stream            309 108.6 Inf     154.8       615 
USMD2 <- (309 - 178)
USMD2

library(effectsize)
allEffects(A2.glmer)
allEffects(A2.glmer.m)

plot(allEffects(A2.glmer), multiline = FALSE)
plot(allEffects(A2.glmer.m), multiline = FALSE)

#Cohen's d is the appropriate effect size measure if two groups have similar standard deviations and are of the same size. 
#Glass's delta, which uses only the standard deviation of the control group, is an alternative measure if each group has a 
#different standard deviation. Hedges' g, which provides a measure of effect size weighted according to the relative size of 
#each sample, is an alternative where there are different sample sizes. (This is important! If you've got different sample sizes 
#then you should use Hedges' g.)

##To calculate the standardized mean difference between two groups, 
#subtract the mean of one group from the other (M1 – M2) and divide the result by the standard deviation (SD) 
#of the population from which the groups were sampled (pooled SD).

#Standardized mean difference
sd(Lake.abund.m$ABUND.m)
sd(Stream.abund.m$ABUND.m)
(sd_pooled(ABUND.m ~ Location.m, data = Location.abund.m))
SD_PLD <- sqrt(((sd(Stream.abund.m$ABUND.m))^2+(sd(Lake.abund.m$ABUND.m))^2)/2)
SD_PLD

#Cohen's D
SMD <- (Stream_yhat.abund.m-Lake_yhat.abund.m)/(sd_pooled(ABUND.m ~ Location.m, data = Location.abund.m))
SMD
SMD1 <-(Stream_yhat.abund.m-Lake_yhat.abund.m)/(SD_PLD)
SMD1

#Are standard deviations between Lake and Stream significantly different? Have to use the control as the denominator...
#Is Lake really the control?
#Glass's delta
SMD2 <- (Stream_yhat.abund.m-Lake_yhat.abund.m)/(sd(Lake.abund.m$ABUND.m))
SMD2

######Fitting GLMMs###### --> Have to change to disturbance and update order of nesting (don't even need to include Disturb in nesting)

A1.glmer <- glmer(ABUND.all~ Location.all + (1 | Block.all/Location.ID/Site.all/Basket.all) + (1|Year.all/Month.all), data = ABUND_data, family = poisson)
summary(A1.glmer)
Anova(A1.glmer)
##Singular fit --> Overfitted model##

anova(A1.lmer, A1.glmer)

overdisp_fun <- function(A1.glmer) {
  rdf <- df.residual(A1.glmer)
  rp <- residuals(A1.glmer,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)}
overdisp_fun(A1.glmer)
##Overdispersed##

A2.glmer <- glmer.nb(ABUND.all~Location.all + (1 | Block.all/Location.ID/Site.all/Basket.all) + (1|Year.all/Month.all), family = quasipoisson() , data = ABUND_data)
summary(A2.glmer)
anova(A2.glmer)
Anova(A2.glmer, Type = 3)

overdisp_fun(A2.glmer)
##Not overdispersed## 

anova(A1.glmer, A2.glmer)
##A2.glmer is a way better fit##

anova(A1.lmer, A2.glmer)
#Don't know if I can compare these, but A1.lmer is a better fit.

#GLMM with averaged data##
A1.glmer.m <- glmer(round(ABUND.m)~ Location.m + (1 | Block.m/Location.m/Site.m) + (1|Year.m/Month.m), data = ABUND_data_mean, family = poisson())
summary(A1.glmer.m)
anova(A1.glmer.m)
Anova(A1.glmer.m)

overdisp_fun(A1.glmer.m)
##Overdispersed## 

A2.glmer.m <- glmer.nb(round(ABUND.m)~ Location.m + (1 | Block.m/Location.IDm/Site.m) + (1|Year.m/Month.m), data = ABUND_data_mean, family = quassipoisson())
summary(A2.glmer.m)
anova(A2.glmer.m)
Anova(A2.glmer.m)

overdisp_fun(A2.glmer.m)
##Not Overdispersed## 

anova(A1.glmer.m, A2.glmer.m)
##A2.glmer.m is a better fit##

A3.glmer.m <- glmer.nb(round(ABUND.m)~ 1 + (1 | Block.m/Location.IDm/Site.m) + (1|Year.m/Month.m), data = ABUND_data_mean, family = quassipoisson())
summary(A3.glmer.m)
anova(A3.glmer.m, A2.glmer.m)
##Location.m is significant in the model

anova(A2.glmer.m, A1.lmer.m)
#Don't know if I can compare these, but A1.lmer.m is a better fit --> same as last time.

A2.glmer.m.em <- emmeans(A2.glmer.m, pairwise~Location.m, adjust = "tukey")
A2.glmer.m.em ##Don't think I have to look at p-values because 2 levels
plot(A2.glmer.m.em, type = "response")
summary(A2.glmer.m.em, infer = TRUE) ##Don't think I have to look at p-values because 2 levels
#Back-transforming data
summary(A2.glmer.m.em, infer = TRUE, type = "response") ##Don't think I have to look at p-values because 2 levels

A2.glmer.em <- emmeans(A2.glmer, pairwise~Location.all, adjust = "bonferroni")

plot(A2.glmer.em, type = "response")
summary(A2.glmer.em, infer = TRUE) ##Don't think I have to look at p-values because 2 levels
#Back-transforming data
summary(A2.glmer.em, infer = TRUE, type = "response") ##Don't think I have to look at p-values because 2 levels

