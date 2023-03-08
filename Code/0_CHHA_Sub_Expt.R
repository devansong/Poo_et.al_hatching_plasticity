############################################
# CHHA Sumbergence-induced hatching 2013
##Code written by S. Poo##################
############################################ 
library(multcomp)
library("glmmTMB")
library("car", lib.loc="~/R/win-library/3.6")
stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))


#---------------------------------------------------------------------------------------
# I. Hatching Success: Percent of Clutch Hatched After Submergence (48-96 HPO)
#---------------------------------------------------------------------------------------

Hatch_48to96 <- read.csv("C:/Dropbox/NUS/Working file/CHHA Submergence hatching/2022 CHHA Data & Analyses/CHHA Percent Hatched (All) 2022.08.20.csv", header=T)
summary (Hatch_48to96)


############################################
# Summary Stats  
############################################

# Survival 1 hr post-submergene 
aggregate(Hatch_48to96[,"PercentTotSurvive1HR"], 
          by=list(Hatch_48to96$Treatment), mean, na.rm=TRUE)
aggregate(Hatch_48to96[,"PercentTotSurvive1HR"], 
          by=list(Hatch_48to96$Treatment), stderr)

# Survival 24 hrs post-submergence  
aggregate(Hatch_48to96[,"PercentTotSurvive"], 
          by=list(Hatch_48to96$Treatment), mean, na.rm=TRUE)
aggregate(Hatch_48to96[,"PercentTotSurvive"], 
          by=list(Hatch_48to96$Treatment), stderr)

############################################
# GLM: Percent Hatch ~ Submergence Time
############################################
# Survival 1 hr post-submergene 

Hatch_48to96$Survive_1hr_Yes <- round(Hatch_48to96$PercentTotSurvive1HR)
Hatch_48to96$Survive_1hr_No <- 100-Hatch_48to96$Survive_1hr_Yes

mod1_48to96 <- glm(cbind(round(Survive_1hr_Yes), round(Survive_1hr_No)) ~ Treatment, family=quasibinomial, data=Hatch_48to96)
summary(mod1_48to96)
anova(mod1_48to96,test="F") 

#           Df Deviance Resid. Df Resid. Dev     F    Pr(>F)    
# NULL                         28     2351.3                    
# Treatment  4   1631.2        24      720.1 12.96 9.283e-06 ***
  

summary(  glht(mod1_48to96, linfct = mcp(Treatment = "Tukey"))  )

#                      Estimate Std. Error z value Pr(>|z|)  
# SUB-84 - SUB-48 == 0 2.109e+01  3.916e+03   0.005  1.00000   
# SUB-96 - SUB-48 == 0 2.122e+01  3.916e+03   0.005  1.00000   
# SUB-72 - SUB-60 == 0 1.851e+01  3.574e+03   0.005  1.00000   
# SUB-84 - SUB-60 == 0 2.109e+01  3.574e+03   0.006  1.00000   
# SUB-96 - SUB-60 == 0 2.122e+01  3.574e+03   0.006  1.00000   
# SUB-84 - SUB-72 == 0 2.585e+00  8.139e-01   3.176  0.00824 **
# SUB-96 - SUB-72 == 0 2.715e+00  7.916e-01   3.430  0.00339 **
# SUB-96 - SUB-84 == 0 1.301e-01  7.407e-01   0.176  0.99970   


#####
# Survival 24 hrs post-submergence 

Hatch_48to96$Survive_24hr_Yes <- round(Hatch_48to96$PercentTotSurvive)
Hatch_48to96$Survive_24hr_No <- 100-Hatch_48to96$Survive_24hr_Yes

mod2_48to96 <- glm(cbind(round(Survive_24hr_Yes), round(Survive_24hr_No)) ~ Treatment, family=quasibinomial, data=Hatch_48to96)
summary(mod2_48to96)
anova(mod2_48to96,test="F") 

#           Df Deviance Resid. Df Resid. Dev      F    Pr(>F)    
# NULL                         28     3291.7                     
# Treatment  4   2728.4        24      563.2 34.874 1.125e-09 ***
  
summary(  glht(mod2_48to96, linfct = mcp(Treatment = "Tukey"))  )

#                        Estimate Std. Error z value Pr(>|z|)  
# SUB-60 - SUB-48 == 0 7.705e-11  6.892e+03   0.000  1.00000   
# SUB-72 - SUB-48 == 0 2.118e+01  5.090e+03   0.004  1.00000   
# SUB-84 - SUB-48 == 0 2.434e+01  5.090e+03   0.005  1.00000   
# SUB-96 - SUB-48 == 0 2.505e+01  5.090e+03   0.005  1.00000   
# SUB-72 - SUB-60 == 0 2.118e+01  4.646e+03   0.005  1.00000   
# SUB-84 - SUB-60 == 0 2.434e+01  4.646e+03   0.005  1.00000   
# SUB-96 - SUB-60 == 0 2.505e+01  4.646e+03   0.005  1.00000   
# SUB-84 - SUB-72 == 0 3.164e+00  1.002e+00   3.158  0.00856 **
# SUB-96 - SUB-72 == 0 3.866e+00  1.242e+00   3.112  0.00995 **
# SUB-96 - SUB-84 == 0 7.022e-01  1.524e+00   0.461  0.98673   



#---------------------------------------------------------------------------------------
# II. Tadpole Length: Hatchling     
#---------------------------------------------------------------------------------------

Hatchling_72to96 <- read.csv("C:/Dropbox/NUS/Working file/CHHA Submergence hatching/2022 CHHA Data & Analyses/CHHA Hatchling (72-96HPO) 2022.01.29.csv", header=T)
summary (Hatchling_72to96)

############################################
# Summary Stats  
############################################

aggregate(Hatchling_72to96[,"TotLength"], 
          by=list(Hatchling_72to96$SubTreat), mean, na.rm=TRUE)
aggregate(Hatchling_72to96[,"TotLength"], 
          by=list(Hatchling_72to96$SubTreat), stderr)

############################################
# GLM: Length ~ Treatment 
############################################
ht1 <- glmmTMB(log(TotLength)~SubTreat, 
               family=gaussian(link="identity"), data=Hatchling_72to96)
summary(ht1)
Anova(ht1) 

summary(  glht(ht1, linfct = mcp(SubTreat = "Tukey"))  )


#                       Estimate Std. Error z value Pr(>|z|)    
#  SUB-84 - SUB-72 == 0  0.11659    0.01861   6.264   <1e-08 ***
#  SUB-96 - SUB-72 == 0  0.23839    0.01879  12.689   <1e-08 ***
#  SUB-96 - SUB-84 == 0  0.12181    0.01797   6.779   <1e-08 ***


#---------------------------------------------------------------------------------------
# III. Tadpole Length: 144 HPO    
#---------------------------------------------------------------------------------------

Tad144 <- read.csv("C:/Dropbox/NUS/Working file/CHHA Submergence hatching/2022 CHHA Data & Analyses/CHHA 144HPO 10tads 2022.01.29.csv", header=T)
summary (Tad144)

############################################
# Summary Stats  
############################################

aggregate(Tad144[,"TotLength"], 
          by=list(Tad144$SubTreat), mean, na.rm=TRUE)
aggregate(Tad144[,"TotLength"], 
          by=list(Tad144$SubTreat), stderr)

############################################
# GLM: Lenght ~ Treatment 
############################################

tad1 <- glmmTMB(log(TotLength)~SubTreat, 
                family=gaussian(link="identity"), data=Tad144)
summary(tad1)
Anova(tad1) 

summary(  glht(tad1, linfct = mcp(SubTreat = "Tukey"))  )


#                        Estimate Std. Error z value Pr(>|z|)    
#  SUB-72 - Control == 0 -0.07594    0.02009  -3.781   <0.001 ***
#  SUB-84 - Control == 0  0.02152    0.02114   1.018    0.739    
#  SUB-96 - Control == 0  0.03482    0.02009   1.733    0.306    
#  SUB-84 - SUB-72 == 0   0.09746    0.02186   4.458   <0.001 ***
#  SUB-96 - SUB-72 == 0   0.11076    0.02084   5.314   <0.001 ***
#  SUB-96 - SUB-84 == 0   0.01330    0.02186   0.608    0.929    


