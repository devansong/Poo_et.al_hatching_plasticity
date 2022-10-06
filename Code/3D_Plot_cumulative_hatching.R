########################################################################
############Mar2nd 2022, Code written by Anne Devan-Song################
######Plotting hatching hr#####################################################
############Bend, OR, USA###############################################
########################################################################

rm(list=ls())
graphics.off()
library(tidyverse)
library(dplyr)
library(ggpubr)
setwd("~/Dropbox/EmergenceHatching/Data") #set a working directory
dataset <- read.csv("CHHA Sub Over Time 2022.03.02.csv")

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

create_metric_df <- function(metric) {
  df_tidy_summary <- dataset %>%
    filter(!is.na(!!as.symbol(metric))) %>%
    group_by(MinSinceSub, Treatment) %>%
    summarise(n = n(),
              mean = mean(!!as.symbol(metric)),
              median = median(!!as.symbol(metric)),
              sd = sd(!!as.symbol(metric))) %>%
    mutate(sem = sd / sqrt(n - 1),
           CI_lower = mean + qt((1-0.95)/2, n - 1) * sem,
           CI_upper = mean - qt((1-0.95)/2, n - 1) * sem)
  
  df_tidy_summary <- as.data.frame(df_tidy_summary)
  df_tidy_summary$metric <- metric
  return(df_tidy_summary)
}

metrics <- c("PercentHatch",
             "CummulativePercentHatch")

df <- data.frame("MinSinceSub" = numeric(0),
                "Treatment"       = numeric(0),
                 "n"        = numeric(0),
                 "mean"     = numeric(0),
                 "median"   = numeric(0),
                 "sd"       = numeric(0),
                 "sem"      = numeric(0),
                 "CI_lower" = numeric(0),
                 "CI_upper" = numeric(0),
                 "metric"= character(0),
                 stringsAsFactors=FALSE) 

for (metric in metrics) {
  tempdf <- create_metric_df(metric)
  df <- rbind(df, tempdf)
}

subdf <- subset(df, metric == "CummulativePercentHatch")

plotA <- ggplot(subdf, aes(x=MinSinceSub, y=mean, color=Treatment)) +
  geom_ribbon(aes(ymin=CI_lower,ymax=CI_upper,fill=Treatment),color="black",alpha=0.1, show.legend=FALSE, size=0.1) + 
  geom_line(aes(x=MinSinceSub, y=mean, color=Treatment), size=1.5) +
  scale_fill_manual(values = c("red","red", "#bc5090", "#003f5c","#ffa600"), name = "Treatment", labels = c("48 HPO", "60 HPO", "72 HPO", "84 HPO", "96 HPO"))+
  scale_color_manual(values = c("red","red", "#bc5090", "#003f5c","#ffa600"), name = "Treatment", labels = c("48 HPO", "60 HPO", "72 HPO", "84 HPO", "96 HPO")) +
  ylab("Cumulative % of Clutch Hatched")+ 
  xlab("Time Since Submergence (min)")+ 
  scale_x_continuous(breaks = round(seq(min(0), max(60), by = 5),1), limits=c(5,60))+
  scale_y_continuous(breaks = round(seq(min(0), max(100), by = 20),1), limits=c(-25,117))+
  theme(
    strip.background =  element_rect(fill = NA, colour = NA), 
    panel.background =  element_rect(fill = "white"), 
    panel.border =      element_rect(fill = NA, colour="black"), 
    panel.grid.major =  element_blank(),
    panel.grid.minor =  element_blank(),
    legend.position = c(0.1,0.77)
    #panel.margin =      unit(1, "lines")
  )+
  ggtitle("A")


plotA

setwd("~/Dropbox/EmergenceHatching/Figures")
png("Cumulative Hatching.png", units="in", width=6.2, height=4, res=600)
plotA
dev.off()

###TRy with error bars instead 
subdf$upperSE <- subdf$mean + subdf$sem
subdf$lowerSE <- subdf$mean - subdf$sem
subdf <- subdf %>% 
  mutate(lowerSE_Mod = if_else(lowerSE < 0, 0, lowerSE))

plotTry <- ggplot(subdf, aes(x=MinSinceSub, y=mean, color=Treatment)) +
  geom_errorbar(aes(ymin=lowerSE_Mod,ymax=upperSE,color=Treatment),alpha=0.4, 
                show.legend=FALSE, size=0.5, 
                position=position_dodge(width=-2)) + 
  geom_line(aes(x=MinSinceSub, y=mean, color=Treatment), size=1.5) +
  scale_fill_manual(values = c("red","red", "#bc5090", "#003f5c","#ffa600"), name = "Treatment", labels = c("48 HPO", "60 HPO", "72 HPO", "84 HPO", "96 HPO"))+
  scale_color_manual(values = c("red","red", "#bc5090", "#003f5c","#ffa600"), name = "Treatment", labels = c("48 HPO", "60 HPO", "72 HPO", "84 HPO", "96 HPO")) +
  labs(y = paste0("Cumulative Proportion of", "\n", "Clutch Hatched (%)"))+ 
  xlab("Time Since Submergence (min)")+ 
  scale_x_continuous(breaks = round(seq(min(0), max(60), by = 5),1), limits=c(5,63))+
  scale_y_continuous(breaks = round(seq(min(0), max(80), by = 20),1), limits=c(0,120))+
  theme(
    strip.background =  element_rect(fill = NA, colour = NA), 
    panel.background =  element_rect(fill = "white"), 
    panel.border =      element_rect(fill = NA, colour="black"), 
    panel.grid.major =  element_blank(),
    panel.grid.minor =  element_blank(),
    legend.position = c(0.18, 0.75), 
    legend.key.size = unit(0.5, 'cm'), #change legend key size
    legend.key.height = unit(0.35, 'cm'), #change legend key height
    legend.key.width = unit(0.5, 'cm'), #change legend key width
    legend.title = element_text(size=9), #change legend title font size
    legend.text = element_text(size=7.5)
    #panel.margin =      unit(1, "lines")
  )+
  ggtitle("(a)")


plotTry




#Do the same buyt for hatchlings

setwd("~/Dropbox/EmergenceHatching/Data") #set a working directory
df <- read.csv("CHHA Percent Hatched (72-96HPO) 2022.01.30.csv")

plotB <- ggplot(df, aes(x=Treatment, y=PercentTotSurvive1HR, color=Treatment)) +
  scale_x_discrete(labels=c("72", "84", "96"))+
  geom_point(alpha = 0.9, size=3)+
  scale_color_manual(values = c("#bc5090", "#003f5c","#ffa600"))+
  geom_boxplot(alpha = 0.2, color = "black", width=0.4) +
  labs(y = paste0("Proportion of Clutch Hatched", "\n", "After 60 mins (%)"))+ 
  labs(x = paste0("Embryo Age at", '\n', "Submergence (HPO)"))+ 
  theme(strip.background =  element_rect(fill = NA, colour = NA), 
        panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill = NA, colour="black"), 
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank(), 
        legend.position = "none") +
  ggtitle("(b)")+
  scale_y_continuous(breaks = round(seq(min(0), max(100), by = 20),1), limits=c(0,120))+
  annotate("text", x = c(1, 2, 3), y=115, label = c("*", "^", "^"), size=6)

plotB
setwd("~/Dropbox/EmergenceHatching/Figures")
png("PercentHatchedAfter60mins.png", units="in", width=3, height=3, res=600)
plotB
dev.off()

multiplot(plotTry, plotB, cols=2)

setwd("~/Dropbox/EmergenceHatching/Figures")
png("Hatching_Fig2.png", units="in", width=5.5, height=3, res=300)
ggarrange(plotTry, plotB, widths = c(1.3,1))
dev.off()
ggarrange(plotTry, plotB, widths = c(1.3,1))

setwd("~/Dropbox/EmergenceHatching/Data") #set a working directory
df <- read.csv("CHHA Percent Hatched (72-96HPO) 2022.01.30.csv")

subs <- subset(df, Treatment=="SUB-72")
mean(subs$PercentTotSurvive1HR)
sd(subs$PercentTotSurvive1HR)/(sqrt(7))








