########################################################################
############Mar2nd 2022, Code written by Anne Devan-Song################
######Simulating egg survival###########################################
############Bend, OR, USA###############################################
########################################################################

rm(list=ls())
graphics.off()

library(viridis)
library(tidyverse)
library(truncnorm)

setwd("~/Dropbox/EmergenceHatching/Data") #set a working directory

proj <- read.csv("24H_Survival_projections_for_simulation.csv")

#Extract vectors of all survial summaries 
Survival <- proj$Survival
SD <- proj$SD
LowerCI <- proj$Lower
SDE <-proj$SDE

ClutchSurv <- proj$ClutchSurvival
ClutchSD <- proj$ClutchSD

str(proj)


df <- data.frame("Iteration" = numeric(0), 
                 "HPO" = numeric(0),
                 "surival" = numeric(0))
HPO <- c(0, 12, 24, 36, 48, 60, 72,84, 96, 108, 120)


#LowerCI <- c(rep((0.7), times=10))

for (i in c(1:1000)) {
  
  start <- 1 
  
  Lower1 <- rtruncnorm(1, a=0, b=1, mean=LowerCI[1], sd=0.01)
  subsur12 <- rtruncnorm(1, a=Lower1, b=1, mean=Survival[1], sd=SD[1])
  number12 <- subsur12
  
  Lower2 <- rtruncnorm(1, a=0, b=number12, mean=LowerCI[2], sd=0.01)
  subsur24 <- rtruncnorm(1, a=Lower2, b=number12, mean=Survival[2], sd=SD[2])
  number24 <- subsur24
  
  Lower3 <- rtruncnorm(1, a=0, b=number24, mean=LowerCI[3], sd=0.01)
  subsur36 <- rtruncnorm(1, a=Lower3, b=number24, mean=Survival[3], sd=SD[3])
  number36 <- subsur36
  
  Lower4 <- rtruncnorm(1, a=0, b=number36, mean=LowerCI[4], sd=0.01)
  subsur48 <- rtruncnorm(1, a=Lower4, b=number36, mean=Survival[4], sd=SD[4])
  number48 <- subsur48
  
  Lower5 <- rtruncnorm(1, a=0, b=number48, mean=LowerCI[5], sd=0.01)
  subsur60 <- rtruncnorm(1, a=Lower5, b=number48, mean=Survival[5], sd=SD[5])
  number60 <- subsur60
  
  ###
  
  Lower6 <- rtruncnorm(1, a=0, b=number60, mean=LowerCI[6], sd=0.01)
  subsur72 <- rtruncnorm(1, a=Lower6, b=number60, mean=Survival[6], sd=SD[6])
  clutchsur72 <- rtruncnorm(1, a=0, b=1, mean=ClutchSurv[6], sd=ClutchSD[6])
  #subsur72 are those that remain unsubmerged 
  #calculate those that got submerged between then and now
  submerged72 <- subsur60-subsur72
  survivehatch72 <- submerged72*clutchsur72
  number72 <- subsur72 + survivehatch72
  
  ###
  
  Lower7 <- rtruncnorm(1, a=0, b=number72, mean=LowerCI[7], sd=0.01)
  subsur84 <- rtruncnorm(1, a=Lower7, b=number72, mean=Survival[7], sd=SD[7]) 
  clutchsur84 <- rtruncnorm(1, a=0, b=1, mean=ClutchSurv[7], sd=ClutchSD[7])
  submerged84 <- number72-subsur84
  survivehatch84 <- submerged84*clutchsur84
  number84 <- subsur84 + survivehatch84

  ###
  
  Lower8 <- rtruncnorm(1, a=0, b=number84, mean=LowerCI[8], sd=0.01)
  subsur96  <- rtruncnorm(1, a=Lower8, b=number84, mean=Survival[8], sd=SD[8]) 
  clutchsur96  <- rtruncnorm(1, a=0, b=1, mean=ClutchSurv[8], sd=ClutchSD[8])
  submerged96  <- number84-subsur96 
  survivehatch96  <- submerged96 *clutchsur96 
  number96  <- subsur96  + survivehatch96 
  
  ###
  
  Lower9 <- rtruncnorm(1, a=0, b=number96, mean=LowerCI[9], sd=0.01)
  subsur108  <- rtruncnorm(1, a=Lower9, b=number96, mean=Survival[9], sd=SD[9]) 
  clutchsur108  <- rtruncnorm(1, a=0, b=1,mean=ClutchSurv[9], sd=ClutchSD[9])
  submerged108  <- number96-subsur108
  survivehatch108  <- submerged108 *clutchsur108 
  number108  <- subsur108  + survivehatch108
  
  ###
  
  Lower10 <- rtruncnorm(1, a=0, b=number108, mean=LowerCI[10], sd=0.01)
  subsur120  <- rtruncnorm(1, a=Lower10, b=number108, mean=Survival[10], sd=SD[10]) 
  clutchsur120  <- 1
  submerged120  <- number108-subsur120
  survivehatch120  <- submerged120 *clutchsur120 
  number120  <- subsur120  + survivehatch120

  
  
  
  finalrow <- rbind(start, number12, number24, number36, number48, number60, 
                    number72, number84, number96, number108, number120)
  finalrow <- as.data.frame(finalrow)
  finalrow$HPO <- HPO
  finalrow$iteration <- i
  
  df <- rbind(df, finalrow)
  
  
}

df$iteration <- as.factor(df$iteration)
ggplot(df, aes(x=HPO, y=V1, group=iteration)) +
  geom_line(color="grey") +
  scale_color_viridis(discrete = TRUE) + 
  theme(legend.position = "none")+
  theme_classic()+ 
  ylim(0.7, 1)



rnorm(1, mean=, sd=)



newdf <- data.frame("Iteration" = numeric(0), 
                 "HPO" = numeric(0),
                 "surival" = numeric(0))

#set.seed(456)
for (i in c(1:1000)) {
  
  start <- 1 
  
  Lower1 <- rtruncnorm(1, a=0, b=1, mean=LowerCI[1], sd=0.01)
  subsur12 <- rtruncnorm(1, a=Lower1, b=1, mean=Survival[1], sd=SD[1])
  number12 <- subsur12
  
  Lower2 <- rtruncnorm(1, a=0, b=number12, mean=LowerCI[2], sd=0.01)
  subsur24 <- rtruncnorm(1, a=Lower2, b=number12, mean=Survival[2], sd=SD[2])
  number24 <- subsur24
  
  Lower3 <- rtruncnorm(1, a=0, b=number24, mean=LowerCI[3], sd=0.01)
  subsur36 <- rtruncnorm(1, a=Lower3, b=number24, mean=Survival[3], sd=SD[3])
  number36 <- subsur36
  
  Lower4 <- rtruncnorm(1, a=0, b=number36, mean=LowerCI[4], sd=0.01)
  subsur48 <- rtruncnorm(1, a=Lower4, b=number36, mean=Survival[4], sd=SD[4])
  number48 <- subsur48
  
  Lower5 <- rtruncnorm(1, a=0, b=number48, mean=LowerCI[5], sd=0.01)
  subsur60 <- rtruncnorm(1, a=Lower5, b=number48, mean=Survival[5], sd=SD[5])
  number60 <- subsur60
  
  ###
  
  Lower6 <- rtruncnorm(1, a=0, b=number60, mean=LowerCI[6], sd=0.01)
  subsur72 <- rtruncnorm(1, a=Lower6, b=number60, mean=Survival[6], sd=SD[6])
  number72 <- subsur72
  
  ###
  
  Lower7 <- rtruncnorm(1, a=0, b=number72, mean=LowerCI[7], sd=0.01)
  subsur84 <- rtruncnorm(1, a=Lower7, b=number72, mean=Survival[7], sd=SD[7]) 
  number84 <- subsur84
  
  ###
  
  Lower8 <- rtruncnorm(1, a=0, b=number84, mean=LowerCI[8], sd=0.01)
  subsur96  <- rtruncnorm(1, a=Lower8, b=number84, mean=Survival[8], sd=SD[8]) 
  number96  <- subsur96 
  
  ###
  
  Lower9 <- rtruncnorm(1, a=0, b=number96, mean=LowerCI[9], sd=0.01)
  subsur108  <- rtruncnorm(1, a=Lower9, b=number96, mean=Survival[9], sd=SD[9]) 
  number108  <- subsur108 
  
  ###
  
  Lower10 <- rtruncnorm(1, a=0, b=number108, mean=LowerCI[10], sd=0.01)
  subsur120  <- rtruncnorm(1, a=Lower10, b=number108, mean=Survival[10], sd=SD[10]) 
  number120  <- subsur120  
  
  
  
  finalrow <- rbind(start, number12, number24, number36, number48, number60, 
                  number72, number84, number96, number108, number120)
  finalrow <- as.data.frame(finalrow)
  finalrow$HPO <- HPO
  finalrow$iteration <- i
  newdf <- rbind(newdf, finalrow)


}

newdf$iteration <- as.factor(newdf$iteration)

plot <- ggplot() +
  geom_rect(mapping=aes(xmin=72, xmax=120, ymin=0.83, ymax=1), fill='grey', alpha=0.5)+
  geom_line(aes(x=newdf$HPO, y=newdf$V1, group=df$iteration), color="black", size=0.1, alpha=1) +
  geom_line(aes(x=df$HPO, y=df$V1, group=df$iteration), color="#488f31", size=0.15, alpha=0.3) +
  #scale_color_viridis(discrete = TRUE) + 
  annotate("text", x = 87, y = 0.987, label = "With hatching plasticity", color="#488f31")+
  annotate("text", x = 63, y = 0.870, label = "Without hatching plasticity", color="black")+
  scale_x_continuous(breaks = round(seq(min(0), max(120), by = 12),1), limits=c(0,120)) +
  scale_y_continuous(breaks = round(seq(min(0.85), max(1), by = 0.05), 2), 
                     limits=c(0.83,1))+
  xlab("Hours Post Ovipoisiton (h)")+
  ylab("Survival") +
  theme_classic() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position = "none")

plot
setwd("~/Dropbox/EmergenceHatching/Figures")
png("Simulation.png", units="in", width=4, height=3, res=300)
plot
dev.off()


ggplot() +
  geom_line(aes(x=df$HPO, y=df$V1, group=df$iteration), color="grey")+
  theme(legend.position = "none")




#Other Trial; ignore. 




mean(df$number120)
sd(df$number120)

#write.csv(df, file="simulatedeggsurvival.csv", row.names=FALSE)

write.csv(df, file="simulatedeggsurvival_1000.csv", row.names=FALSE)

#df <- read.csv("simulatedeggsurvival.csv")

sd(df$number12)
mean(df$number12)
#df$number120 = "0"

df$plasticity <- "NO"
newdf$plasticity <- "YES"

combined <- rbind(df, newdf)

summaries <- data.frame("HPO" = numeric(0), 
                        "Plasticity" = character(0),
                        "MeanSurv" = numeric(0),
                        "SD" = numeric(0),
                        "SE" = numeric(0),"UpperCI" = numeric(0),
                        "LowerCI" = numeric(0))

n <- 1000

for (i in c(0, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120)) {
  HPO <- i
  subset <- subset(df, HPO == i)
  MeanSurv <- mean(subset$V1)
  SD <- sd(subset$V1)
  SE <- SD / sqrt(999)
  UpperCI <- MeanSurv + qt((1-0.95)/2, 999) * SE
  LowerCI <- MeanSurv - qt((1-0.95)/2, 999) * SE
  row <- cbind(i, "No", MeanSurv, SD, SE, UpperCI, LowerCI)
  row <- as.data.frame(row)
  summaries <- rbind(summaries, row)
}


for (i in c(0, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120)) {
  HPO <- i
  subset <- subset(newdf, HPO == i)
  MeanSurv <- mean(subset$V1)
  SD <- sd(subset$V1)
  SE <- SD / sqrt(999)
  UpperCI <- MeanSurv + qt((1-0.95)/2, 999) * SE
  LowerCI <- MeanSurv - qt((1-0.95)/2, 999) * SE
  row <- cbind(i, "Yes", MeanSurv, SD, SE, UpperCI, LowerCI)
  row <- as.data.frame(row)
  summaries <- rbind(summaries, row)
}


str(summaries)
summaries$i <- as.numeric(as.character(summaries$i))

summaries$MeanSurv <- as.numeric(as.character(summaries$MeanSurv))
summaries$LowerCI <- as.numeric(as.character(summaries$LowerCI ))
summaries$UpperCI <- as.numeric(as.character(summaries$UpperCI))
summaries$i <- as.factor(summaries$i)

str(summaries)

setwd("~/Dropbox/EmergenceHatching/Data")
write.csv(summaries, file="summary_simulation_differences.csv")

plot <- ggplot(summaries, aes(x=i, y=MeanSurv, color=V2)) +
  #geom_ribbon(aes(ymin=LowerCI,ymax=UpperCI, color=V2), alpha=0.9, show.legend=FALSE) + 
  geom_line(aes(x=i, y=MeanSurv, color=V2), size=0.01, alpha =0.00001) +
  ylab("Survival")+ 
  xlab("Hours Post Oviposition")+ 
  #scale_x_continuous(breaks = round(seq(min(0), max(120), by = 12),1), limits=c(0,120)) +
  theme(
    strip.background =  element_rect(fill = NA, colour = NA), 
    panel.background =  element_rect(fill = "white"), 
    panel.border =      element_rect(fill = NA, colour="black"), 
    panel.grid.major =  element_blank(),
    panel.grid.minor =  element_blank(),
    #panel.margin =      unit(1, "lines")
  )

plot

ggplot(summaries, aes(x=i, y=MeanSurv)) +
  #geom_ribbon(aes(ymin=LowerCI,ymax=UpperCI, color=V2), alpha=0.9, show.legend=FALSE) + 
  geom_line()


