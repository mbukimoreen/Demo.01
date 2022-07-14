# EXERCISES SESSION 2: REGRESSION ANALYSIS
# EXERCISE 1
# -	Run a simple regression for the pubs.dat data provided, 
#  predicting mortality from number of pubs. 
# -	Try repeating the analysis but bootstrapping the regression parameters. 

# Does the number of pubs significantly predict mortality?
# NB: When using the robust method, the predictor has a significant effect 
# on response if the bootstrap confidence limits for the predictor do not cross zero,.

# SOLUTION
# Setting the work directory

setwd("C:/Users/admin/Desktop/INTER SESSIONS/SESSION 2")

#Load the required packages
library(QuantPsyc)
library(car)

# Load the required data

library(haven)
pubs <- read_sav("pubs.sav")
View(pubs)

pubsdat<- as.data.frame(pubs)
pubsdat
summary(pubsdat) 

# Fitting simple regression model
pubmodel <- lm(mortality ~pubs , data = pubsdat)
summary(pubmodel)
lm.beta(pubmodel)
#From the output we can say that B0= 3351.955 this implies that when (pubs =0) there 3351.955
# no of mortality
#
# printing the residuals for the model there are three unstandardized
#, studentized and standardized 

resid(pubmodel)
rstudent(pubmodel)
rstandard(pubmodel)
 
# to obtain the confidence interval and correlation
confint(pubmodel)
Anova(pubmodel)
anova(pubmodel)

# test for autocorrealotion between the errors
durbinWatsonTest(pubmodel)

#BOOTSRAPPING METHOD
# we bootstrap the regresiion model in 1000 reprications


bootReg<-function(formula, data, i)
{
 d <- data[i,]
 fit <- lm(formula, data = d)
 return(coef(fit))
}

bootsoutput <- boot(statistic = bootReg, formula =mortality ~pubs , data = pubsdat, R = 1000)
bootsoutput

# to obtain the confidence interval of the bootstrap
boot.ci(bootsoutput, type = "bca", index = 1)
boot.ci(bootsoutput, type = "bca", index = 2)
# EXERCISE 2
#A fashion student was interested in factors that predicted the salaries of catwalk models. 
# She collected data from 231 models. For each model she asked them their salary per day on days
# when they were working (salary), their age (age), how many years they had worked as a model
# (years), and then got a panel of experts from modelling agencies to rate the attractiveness
# of each model as a percentage, with 100% being perfectly attractive (beauty). The data are 
# in the file Supermodel.dat. Unfortunately, this fashion student bought some substandard 
# statistics text and so doesn't know how to analyse her data. Can you help her out by 
# conducting # a multiple regression to see which factors predict a model's salary? How valid 
#is the  regression model?
 
# Hints: 
# -	Interpret the R2 and the adjusted R2.
# -	Obtain and interpret the standardized beta estimates.
# -	Obtain and store the case wise diagnostics. Use them to list standardized residuals
#  greater than 2 and create a variable called large.residual, which is TRUE (or 1) 
# if the residual is greater than 2 or less than -2.
#-	Make plots to interpret residuals, normality of errors, homoscedasticity and
# independence of errors. 
# -	Check and interpret the Durbin-Watson and VIF.

# From the assessment of the several assumptions, is this model reliable?

# SOLUTION 

# Loading the required data

library(haven)
Supermodel <- read_sav("Supermodel.sav")
View(Supermodel)

Supermodel1<- as.data.frame(Supermodel)
summary(Supermodel1) 

# Fitting the multiple regression model
supmod <- lm(SALARY ~ AGE + YEARS + BEAUTY, data = Supermodel1)
supmod
summary(supmod)
lm.beta(supmod)

#
# obtaining the residuals
resid(supmod)
rstudent(supmod)
rstandard(supmod)

# Case wise diagnostics
# this involves obtain residual, standardized , unstandardized and even studentized
# cook distance
Supermodel1$residual <- resid(supmod)
Supermodel1$studentized.residual <- rstudent(supmod)
Supermodel1$standardized.residual <- rstandard(supmod)
Supermodel1$cooks.distant <- cooks.distance(supmod)
Supermodel1$dfbeta <- dfbeta(supmod)
Supermodel1$dffit <- dffits(supmod)
Supermodel1$leverage <- hatvalues(supmod)
Supermodel1$covariance.ratio <- covratio(supmod)

Supermodel1
round(Supermodel1, digits = 3)

#to obtain the large residual
#--Create a variable called large.residual, which is TRUE (or 1) if the residual is 
#greater than 2, or less than -2.----------
Supermodel1$large.residual <-Supermodel1$standardized.residual > 2|Supermodel1$standardized.residual < -2

# -Count the number of large residuals
sum(Supermodel1$large.residual)

# plots to interpret residuals, normality of errors, homoscedasticity and
# independence of errors
# this test for the assumptions for multiple regression
plot(supmod$fitted.values,rstandard(supmod))
abline(0,0)

# alternatively
# plot for normality, homoscedasticity and independence of variance
plot(supmod)


## the Durbin-Watson and VIF 
#the durbin-watson test for the independence of residual 
durbinWatsonTest(supmod)

#vif  test for Multi-collinearity ;
vif(supmod)

# mean 
mean(vif(supmod))

#reciprocal for the mean
1/mean(vif(supmod))

# EXERCISE 3
#A study was carried out to explore the relationship between aggression and several
# potential predicting factors in 666 children who had an older sibling. 
#Variables measured were Parenting_Style (high score = bad parenting practices), 
#Computer_Games (high score = more time spent playing computer games), Television 
#(high score = more time spent watching television), Diet (high score = the child has
#a good diet low in additives), and Sibling_Aggression (high score = more aggression 
#seen in their older sibling). Past research indicated that parenting style and sibling 
#aggression were good predictors of the level of aggression in the younger child.
#All other variables were treated in an exploratory fashion. The data are in the file
#Child Aggression.dat. Analyse them with multiple regression.

#We need to conduct this analysis hierarchically, entering parenting style and sibling
#aggression in the first step (forced entry) and the remaining variables in a second step.
#Compare the model on both model objects) with parenting style plus sibling   aggression 
#and that with the remaining variables (anova(model1, model2)). Interpret and assess the
#reliability of the chosen model.

# SOLUTION 
# Loading the required data
library(haven)
Child_Aggression <- read_sav("Child Aggression.sav")
View(Child_Aggression)
summary(Child_Aggression) 
names(Child_Aggression)

# fitting models
model1 <- lm(Aggression ~ Parenting_Style + Sibling_Aggression, data =Child_Aggression )
model2 <- lm(Aggression ~ Television + Computer_Games + Diet, data =Child_Aggression )
summary(model1)
summary(model2)
lm.beta(model1)
lm.beta(model2)

# compare the R2 in two models, use the ANOVA command
# comparing the anova models 
anova(model1,model2)

