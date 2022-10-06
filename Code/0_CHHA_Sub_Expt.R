############################################
# CHHA Sumbergence-induced hatching 2013
############################################ 
library(multcomp)
library("glmmTMB")
stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))



#---------------------------------------------------------------------------------------
# I. Hatching Success: Percent of Clutch Hatched After Submergence 
#---------------------------------------------------------------------------------------

Hatch_72to96 <- read.csv("C:/Dropbox/NUS/Working file/CHHA Submergence hatching/2022 CHHA Data & Analyses/CHHA Percent Hatched (72-96HPO) 2022.01.30.csv", header=T)
summary (Hatch_72to96)

############################################
# Summary Stats  
############################################

# Survival 1 hr post-submergene 
aggregate(Hatch_72to96[,"PercentTotSurvive1HR"], 
          by=list(Hatch_72to96$Treatment), mean, na.rm=TRUE)
aggregate(Hatch_72to96[,"PercentTotSurvive1HR"], 
          by=list(Hatch_72to96$Treatment), stderr)

# Survival 24 hrs post-submergence 
aggregate(Hatch_72to96[,"PercentTotSurvive"], 
          by=list(Hatch_72to96$Treatment), mean, na.rm=TRUE)
aggregate(Hatch_72to96[,"PercentTotSurvive"], 
          by=list(Hatch_72to96$Treatment), stderr)

############################################
# GLM: Percent Hatch ~ Submergence Time
############################################
# Survival 1 hr post-submergene 

Hatch_72to96$Survive_1hr_Yes <- round(Hatch_72to96$PercentTotSurvive1HR)
Hatch_72to96$Survive_1hr_No <- 100-Hatch_72to96$Survive_1hr_Yes

mod1 <- glm(cbind(round(Survive_1hr_Yes), round(Survive_1hr_No)) ~ Treatment, family=quasibinomial, data=Hatch_72to96)
summary(mod1)

summary(  glht(mod1, linfct = mcp(Treatment = "Tukey"))  )

#                      Estimate Std. Error z value Pr(>|z|)  
#   SUB-84 - SUB-72 == 0   2.5850     1.0295   2.511   0.0321 *
#   SUB-96 - SUB-72 == 0   2.7151     1.0013   2.711   0.0184 *
#   SUB-96 - SUB-84 == 0   0.1301     0.9369   0.139   0.9894  

#####
# Survival 24 hrs post-submergence 

Hatch_72to96$Survive_24hr_Yes <- round(Hatch_72to96$PercentTotSurvive)
Hatch_72to96$Survive_24hr_No <- 100-Hatch_72to96$Survive_24hr_Yes

mod2 <- glm(cbind(round(Survive_24hr_Yes), round(Survive_24hr_No)) ~ Treatment, family=quasibinomial, data=Hatch_72to96)
summary(mod2)

summary(  glht(mod2, linfct = mcp(Treatment = "Tukey"))  )

#                        Estimate Std. Error z value Pr(>|z|)  
#  SUB-84 - SUB-72 == 0   3.1636     1.2672   2.497   0.0321 *
#  SUB-96 - SUB-72 == 0   3.8659     1.5711   2.461   0.0348 *
#  SUB-96 - SUB-84 == 0   0.7022     1.9274   0.364   0.9269  

# SUB-72 a 
# SUB-84 b 
# SUB-96 b 

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

summary(  glht(tad1, linfct = mcp(SubTreat = "Tukey"))  )


#                        Estimate Std. Error z value Pr(>|z|)    
#  SUB-72 - Control == 0 -0.07594    0.02009  -3.781   <0.001 ***
#  SUB-84 - Control == 0  0.02152    0.02114   1.018    0.739    
#  SUB-96 - Control == 0  0.03482    0.02009   1.733    0.306    
#  SUB-84 - SUB-72 == 0   0.09746    0.02186   4.458   <0.001 ***
#  SUB-96 - SUB-72 == 0   0.11076    0.02084   5.314   <0.001 ***
#  SUB-96 - SUB-84 == 0   0.01330    0.02186   0.608    0.929    

# SUB-72      a
# SUB-84      b
# SUB-96      b
# SUB-Contral b
