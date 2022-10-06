########################################################################
############Mar2nd 2022, Code written by Anne Devan-Song################
######Plotting data#####################################################
############Bend, OR, USA###############################################
########################################################################

rm(list=ls())
graphics.off()

library(viridis)
library(tidyverse)

setwd("~/Dropbox/EmergenceHatching/Data") #set a working directory

df <- read.csv("expanded.csv")
df$Date <- as.character(df$Date)
df$Date <- as.Date(df$Date, format = "%m%d%y", origin="1/1/1970")

#"#3ad8cf"
plot <- ggplot(df, aes(x = HPO, y = New_Height_cm, color = Unique.ID, shape=FALSE)) +
  geom_line(alpha = 0.4) +
  scale_color_manual(values=c(rep("#FF7F50", 476))) + 
  ylim(c(-100, 350))  + 
  xlim(c(0, 120))+
  geom_hline(yintercept=0,linetype= 'dashed', col = "blue", size=1)+
  annotate("text", x = 60, y = -18, label = "Below pond water level", col="blue")+
  xlab("Hours Post Oviposition (h)")+
  ylab("Clutch Height Above Water (cm)") +
  scale_x_continuous(breaks = round(seq(min(0), max(120), by = 12),1), limits=c(0,120))+
  theme() + 
  #theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
   #     panel.grid.minor = element_blank(), axis.line = element_line(colour = #"black"), legend.position = "none") + 
  theme(
    strip.background =  element_rect(fill = NA, colour = NA), 
    panel.background =  element_rect(fill = "white"), 
    panel.border =      element_rect(fill = NA, colour="black"), 
    panel.grid.major =  element_blank(),
    panel.grid.minor =  element_blank(),
    legend.position = "none"
    #panel.margin =      unit(1, "lines")
  ) +
  ggtitle("(a)")
  #scale_color_viridis(discrete = TRUE, breaks = round(seq(min(0), max(120), by = 12),1)) 

plot

setwd("~/Dropbox/EmergenceHatching/Figures")
png("Clutchheightovertime.png", units="in", width=3.3, height=2.8, res=300)
plot
dev.off()

setwd("~/Dropbox/EmergenceHatching/Data") #set a working directory
df2 <- read.csv("survival_estimates.csv")

row <- cbind(0, 1, 1, 1, 0, 0)
row <- as.data.frame(row)

colnames(row) <- colnames(df2)

newdf <- rbind(df2, row)
#newdf[10, 3] <- 0
#newdf[10, 4] <- 0




plot2 <- ggplot(newdf, aes(x=HPO, y=Survival)) +
  geom_ribbon(aes(ymin=LowerCI,ymax=UpperCI),color="lightgray",alpha=0.2, show.legend=FALSE) + 
  geom_line(aes(x=HPO, y=Survival), size=0.7, color= "maroon") +
  geom_point(size=2, shape=3, color="red")+
  ylab("Proportion Unsubmerged")+ 
  xlab("Hours Post Oviposition (h)")+ 
  scale_x_continuous(breaks = round(seq(min(0), max(120), by = 12),1), limits=c(0,120)) +
  ylim(0.7, 1)+
  theme(
    strip.background =  element_rect(fill = NA, colour = NA), 
    panel.background =  element_rect(fill = "white"), 
    panel.border =      element_rect(fill = NA, colour="black"), 
    panel.grid.major =  element_blank(),
    panel.grid.minor =  element_blank()
    #panel.margin =      unit(1, "lines")
  )+
  ggtitle("(b)")
  

plot2
setwd("~/Dropbox/EmergenceHatching/Figures")
png("ClutchSubmergenceCurve.png", units="in", width=3.5, height=2.8, res=300)
plot2
dev.off()

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

plot3 <- multiplot(plot, plot2, cols=2)


plot3
setwd("~/Dropbox/EmergenceHatching/Figures")
png("Combined_Submerged_Fig1.png", units="in", width=6, height=2.8, res=300)
multiplot(plot, plot2, cols=2)
dev.off()

