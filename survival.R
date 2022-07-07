##################################################################################
# SURVIVSL ANALYSIS FOR HYPERTENSION AMONG ADULTS AGED 18 YEARS AND ABOVE
# 
# This script consist of hypertension data set for the patients from Kerugoya 
# referral hospital 
# It will be used to determine time taken to hypertension among adults aged 18yrs 
# and above
#
# BY MOREEN MBUKI     DATE:10/4/2022
#################################################################################
#REQUIRED PACKAGES
library(survival)
library(survminer)
library(readr)
library(readxl)
#load the dataset
hypertension <-read.csv(file.choose(), header = T)
hypertension
View(hypertension)
head(hypertension)
tail(hypertension)
names(hypertension)
str(hypertension)
summary(hypertension)


# CREATING THE SURVIVAL FUCTIONS AND KAPLAN MEIER CURVES
#survival function for hypertension stage
s1 <-Surv(hypertension$Controltime,hypertension$Status == 1)
s1
fit1 <-surv_fit(s1~Hypertn.stage, data = hypertension)
fit1
summary(fit1)

#plot the kaplan meier curve
km1<-ggsurvplot(fit1, data =hypertension,
                
                surv.median.line="hv",
                ylab = 'Control probability',
                xlab ='Time (in weeks)',
                xlim=c(5, 150),
                legend.title ="KM Curve",
                pval = T,
                conf.int = F,
                risk.table = T,
                tables.height = 0.3,
                tables.theme = theme_cleantable()
)

print(km1)
# A log rank test on the variable hypertn stage
survdiff(s1~Hypertn.stage, data = hypertension)
# survival function for sex
s2 <-Surv(hypertension$Controltime,hypertension$Status==1)
s2
fit2 <-surv_fit(s1~Sex, data = hypertension)
fit2
summary(fit2)


#plot the kaplan meier curve
km2<-ggsurvplot(fit2, data =hypertension,
                
                surv.median.line="hv",
                ylab = 'Control probability',
                xlab ='Time (in weeks)',
                xlim=c(5, 150),
                legend.title ="KM Curve for Sex",
                pval = T,
                conf.int = F,
                risk.table = T,
                tables.height = 0.3,
                tables.theme = theme_cleantable(),
                palette = c("maroon","blue")
)

print(km2)
# A log rank test on the variable sex
survdiff(s1~Sex, data = hypertension)

#survival function for the diabetes
s3 <-Surv(hypertension$Controltime,hypertension$Status==1)
s3
fit3 <-surv_fit(s1~Diabetes, data = hypertension)
fit3
summary(fit3)

# plotting the kaplan meier curve
km3<-ggsurvplot(fit3, data =hypertension,
                
                surv.median.line="hv",
                ylab = 'Control probability',
                xlab ='Time (in weeks)',
                xlim=c(5, 150),
                legend.title ="KM Curve Diabetes",
                pval = T,
                conf.int = F,
                risk.table = T,
                tables.height = 0.3,
                tables.theme = theme_cleantable(),
                palette = c("black","purple")
)

print(km3)

# A log rank test on the variable diabetes status
survdiff(s1~Diabetes, data = hypertension)
  
# COX PROPORTIONAL HAZARD FUNCTIONS
# UNIVARIATE COX 
# hypertension stage
mod1 <- coxph(s1~Hypertn.stage, data = hypertension, method = "breslow")
mod1
summary(mod1)

# sex
mod2 <- coxph(s1~Sex, data = hypertension,method = "breslow")
mod2
summary(mod2)

# diabetes
mod3 <- coxph(s1~Diabetes, data = hypertension,method = "breslow")
mod3
summary(mod3)

# ages
mod4 <- coxph(s1~Age, data = hypertension,method = "breslow")
mod4
summary(mod4)


# hypertension and Diabetes
mod5 <- coxph(s1~Hypertn.stage + Diabetes  , data = hypertension)
mod5
summary(mod5)


#Hazard Function
#hazards ratio
ggforest(mod5, data = hypertension)

# checking for the assumption of the cox 
# how to check for linearity use of martingale residuals
plot(predict(mod5),residuals(mod5, type = "martingale"),
     xlab = "fitted value",ylab = "Martingale Residual",
     main = " Residual plot",las=1)
abline(h=0)
lines(smooth.spline(predict(mod5),residuals(mod5, type = "martingale")),
      col="red")

#checking for linearily by used of deviance
plot(predict(mod5),residuals(mod5, type = "deviance"),
     xlab = "fitted value",ylab = "deviance Residual",
     main = " Residual plot",las=1)
abline(h=0)
lines(smooth.spline(predict(mod5),residuals(mod5, type = "deviance")),
      col="red")

## checking for proportional hazards assumptions
#this can be done by use of Schoenfield test the hypothesis
# Ho: HAZARD are proportional
# Ha: HAZARD are NOT proportional
# this return for each x and for the overall model
cox.zph(mod5)
# test IF coef for variable(x) changes over time
# if it changes over time > non.prop
# (HR changes over time)
# PLOT A GRAPH FOR THIS 
par(mfrow=c(4,1))
plot(cox.zph(mod5))
#plot the four figure
#now we need on to check and interpret
par(mfrow=c(1,1))
plot(cox.zph(mod5)[1])
abline(h=0,col=2)

par(mfrow=c(1,1))
plot(cox.zph(mod5)[2])
abline(h=0,col=2)


