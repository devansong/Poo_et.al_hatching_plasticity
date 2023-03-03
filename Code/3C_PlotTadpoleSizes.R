########################################################################
############Mar2nd 2022, Code written by Anne Devan-Song################
######Plotting Sheila's tadpole size data###############################
############Bend, OR, USA###############################################
########################################################################

rm(list=ls())
graphics.off()

#library(viridis)
library(tidyverse)
#install.packages("stringr")          # Install stringr package
library("stringr")  
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
} #call multiplot function 

setwd("~/Dropbox/EmergenceHatching/Data") #set a working directory

df <- read.csv("CHHA 144HPO 2022.01.30.csv")
df$SubTreat <- factor(df$SubTreat,     # Reorder factor levels
                         c("SUB-72", "SUB-84", "SUB-96", "Control"))

library(ggbeeswarm)
library(ggpubr)
plotB <- ggplot(data = df,aes(x = SubTreat, y = TotLength, fill = SubTreat))+
  scale_fill_manual(values = c("#bc5090", "#003f5c","#ffa600", "gray")) +
  scale_x_discrete(labels=c("72", "84", "96", "Control"))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA, show.legend=F) +
  # geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+
  ggbeeswarm::geom_quasirandom(shape = 21,size=1.4, dodge.width = .75, color="black",alpha=0.7
                               ,show.legend = F)+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=0.5, alpha = 0,show.legend = F)+
  ylab("Tadpole Length at 144 HPO (mm)")+ 
  xlab("Submergence Time (HPO)")+ 
  #scale_x_continuous(breaks=seq(1,15))+
  #facet_wrap(~variable, scales="free", ncol=1)+
  #scale_x_discrete(labels=c("SUB-72" = "72", "SUB-84" = "84",
  # "SUB-96" = "96", "Control" = "Control"))+
  theme(strip.background =  element_rect(fill = NA, colour = NA), 
        panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill = NA, colour="black"), 
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank())+
  ggtitle("(b)")+
  ylim(0, 8.6)+
  #scale_y_continuous(breaks = round(seq(min(0), max(100), by = 20),1), limits=c(0,10))+
  annotate("text", x = c(1, 2, 3, 4), y=8.6, label = c("*", "^", "^", "^"), size=6)
#  annotate("text", x = c(2.5), y=2.5, label = c("72 HPO - 84 HPO, z = 4.46, p < 0.01"), size=2.8)+
#  annotate("text", x = c(2.5), y=2, label = c("72 HPO - 96 HPO, z = 5.31, p < 0.01"), size=2.8)+
#  annotate("text", x = c(2.5), y=1.5, label = c("72 HPO - Control, z = -3.78, p < 0.01"), size=2.8)+
#  annotate("text", x = c(2.5), y=1, label = c("84 HPO - 96 HPO, z = 0.61, p = 0.93"), size=2.8)+
#  annotate("text", x = c(2.5), y=0.5, label = c("84 HPO - Control, z = 1.02, p = 0.74"), size=2.8)+
#  annotate("text", x = c(2.5), y=0, label = c("96 HPO - Control, z = 1.73, p = 0.31"), size=2.8)


plotB
setwd("~/Dropbox/EmergenceHatching/Figures")
png("TadpoleSizeAt144HPO.png", units="in", width=4.2, height=3.2, res=600)
plotB
dev.off()


setwd("~/Dropbox/EmergenceHatching/Data") #set a working directory
df2 <- read.csv("CHHA Hatchling (72-96HPO) 2022.01.30.csv")

plotA <- ggplot(data = df2,aes(x = SubTreat, y = TotLength, fill = SubTreat))+
  scale_fill_manual(values = c("#bc5090", "#003f5c","#ffa600")) +
  scale_x_discrete(labels=c("72", "84", "96"))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA, show.legend=F) +
  # geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+
  ggbeeswarm::geom_quasirandom(shape = 21,size=1.4, dodge.width = .75, color="black",alpha=0.7
                               ,show.legend = F)+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=0.5, alpha = 0,show.legend = F)+
  #labs(y = paste0("Tadpole Length at 1 h", "\n", "Post-submergence Experiment (mm)"))+ 
  labs(y = paste0("Tadpole Length at 1 h Post-submergence Experiment (mm)"))+ 
  xlab("Submergence Time (HPO)")+ 
  theme(strip.background =  element_rect(fill = NA, colour = NA), 
        panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill = NA, colour="black"), 
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank())+
  ggtitle("(a)")+
  ylim(0, 8.6)+
  annotate("text", x = c(1, 2, 3), y=8.6, label = c("*", "^", "~"), size=6)
  
  #scale_y_continuous(breaks = round(seq(min(0), max(100), by = 20),1), limits=c(0,10))+
  #annotate("text", x = c(1, 2, 3), y=8.5, label = c("(a)", "(b)", "(c)"))
#  annotate("text", x = c(2), y=2.5, label = c("72 HPO - 84 HPO, z = 6.26, p < 0.01"), size=2.8)+
#  annotate("text", x = c(2), y=2, label = c("72 HPO - 96 HPO, z = 12.69, p < 0.01"), size=2.8)+
#annotate("text", x = c(2), y=1.5, label = c("84 HPO - 96 HPO, z = 6.78, p < 0.01"), size=2.8)

setwd("~/Dropbox/EmergenceHatching/Figures")
png("TadpoleLengths_Fig3.png", units="in", width=5, height=4.7, res=600)
multiplot(plotA, plotB, cols=2)
dev.off()


plotA

setwd("~/Dropbox/EmergenceHatching/Figures")
png("TadpoleSizeAt1HPostSub.png", units="in", width=3.2, height=3.2, res=600)
plotA
dev.off()

setwd("~/Dropbox/EmergenceHatching/Figures")
png("TadpoleLengths_Fig3.png", units="in", width=5, height=4.7, res=600)
multiplot(plotA, plotB, cols=2)
dev.off()





#Do the same buyt for hatchlings

setwd("~/Dropbox/EmergenceHatching/Data") #set a working directory
df <- read.csv("CHHA Percent Hatched (72-96HPO) 2022.01.30.csv")

plot <- ggplot(df, aes(x=Treatment, y=PercentTotSurvive1HR, color=Treatment)) +
  scale_x_discrete(labels=c("72", "84", "96"))+
  geom_point(alpha = 0.9, size=3)+
  scale_color_manual(values = c("#bc5090", "#003f5c","#ffa600"))+
  geom_boxplot(alpha = 0.2, color = "black", width=0.4) +
  ylab("% of clutch Hatched After 60mins")+ 
  xlab("Embryo Age at Submergence (HPO)")+ 
  theme(strip.background =  element_rect(fill = NA, colour = NA), 
        panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill = NA, colour="black"), 
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank(), 
        legend.position = "none")

plot
setwd("~/Dropbox/EmergenceHatching/Figures")
png("PercentHatchedAfter60mins.png", units="in", width=3, height=3, res=600)
plot
dev.off()



setwd("~/Dropbox/EmergenceHatching/Data") #set a working directory
df <- read.csv("CHHA Percent Hatched (72-96HPO) 2022.01.30.csv")

subs <- subset(df, Treatment=="SUB-72")
mean(subs$PercentTotSurvive1HR)
sd(subs$PercentTotSurvive1HR)/(sqrt(7))



