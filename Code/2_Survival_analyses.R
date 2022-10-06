##################################################################
##################################################################
######Survival analyses for C. hansenae, Poo et al. ##############
####Code by Anne Devan-Song. Bend, OR. 2022#######################
#########Poo et al.###############################################
##################################################################
##################################################################

rm(list=ls())
graphics.off()
setwd("~/Dropbox/EmergenceHatching/Data") #set working directory

library(ggplot2)
library(devtools)
library(survival)
library(KMsurv)
library(OIsurv)
library(survminer)



df <-read.csv("time_to_event_data.csv")

#remove 2013 data 
#df <- subset(df, date <20130101)
str(df)

df120 <- subset(df, CritHPO == 120) #subset those that survived to 120 HPO
df120$delta<- rep(c(0),times=nrow(df120 )) 
#replace with 0 for plotting purposes only, since they didn't die at 120 HPO 

dfother <- subset(df, CritHPO < 120)
dfother$delta<- rep(c(1),times=nrow(dfother))
newdf <- rbind(dfother, df120) #new dataframe with format for survival analysis

my.surv <- Surv(newdf$CritHPO, newdf$delta)
survfit(my.surv ~ 1)
my.fit <- survfit(my.surv~ 1)
summary(my.fit)$surv
summary(my.fit)$time
summary(my.fit)$n.risk
summary(my.fit)$n.event  # {d_i}
summary(my.fit)$std.err  # standard error of the K-M estimate at {t_i}
summary(my.fit)$lower
str(my.fit)
str(summary(my.fit))

plot(my.fit, main="Kaplan-Meier estimate with 95% confidence bounds",
     xlab="Hours Post Oviposition (HPO)", ylab="Unsubmerged")


survdf <- as.data.frame(cbind(my.fit$time, my.fit$surv, my.fit$lower, my.fit$upper, my.fit$std.err))
colnames(survdf) <- c("HPO", "Survival", "LowerCI", "UpperCI", "SDE")
survdf$SD <- survdf$SDE*(sqrt(468))

#write.csv(newdf, file="survival_estimates.csv", row.names=FALSE)
write.csv(survdf, file="survival_estimates.csv", row.names=FALSE)


#Analysis for difference in survival between sites nad years
#Reload survival with correct HPO (120 instead of 0)
df <-read.csv("time_to_event_data.csv")
df$delta<- rep(c(1),times=nrow(df))
df$date <- as.character(df$date)
#df$date <- as.Date(df$date, format = "%Y%m%d", origin="1970-01-01")

sub11 <- subset(df, date < "2011-12-01")
sub11$year <- "2011"
sub12 <- subset(df, date >"2011-12-01" & date <"2012-12-01")
sub12$year <- "2012"

combined2 <- rbind(sub11, sub12)

my.surv   <- Surv(combined2$CritHPO, combined2$delta)
coxph.fit <- coxph(my.surv ~ combined2$site*combined2$year, method="breslow")
coxph.fit <- coxph(my.surv ~ combined2$site*combined2$year)

coxph.fit
summary(coxph.fit)

#No difference in survival between site and year. 
#Report Wald test
#3.88 on 3df, p=0.3
#No effect of site (0.351), year (0.657) or site*year (0.453) on survial



