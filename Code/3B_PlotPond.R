########################################################################
############Mar2nd 2022, Code written by Anne Devan-Song################
######Plotting data#####################################################
############Bend, OR, USA###############################################
########################################################################

rm(list=ls())
graphics.off()

library(viridis)
library(tidyverse)

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

df <- read.csv("Pond_data.csv")
df$Date <- as.character(df$Date)
df$Date <- as.Date(df$Date, format = "%Y%m%d", origin="1970-01-01")


sub11 <- subset(df, Date < "2011-12-01")
sub11$year <- "2011"
sub12 <- subset(df, Date >"2011-12-01" & Date <"2012-12-01")
sub12$year <- "2012"
sub13 <- subset(df, Date >"2013-04-01")
sub13$year <- "2013"

#don't inclure 2013 pond data anymore
combined <- rbind(sub11, sub12)


plot <- ggplot(combined, aes(x = Date, y = Orig_WaterLevel_corr, color=Location)) +
  geom_line(size=1.3) +
  scale_color_manual(values=c("#58508d", "#ff6361")) + 
  facet_wrap(~ year, scale = "free") +
  #ylim(c(0, 170))  + 
  #xlim(c(0, 120))+
  #geom_hline(yintercept=0,linetype= 'dashed', col = 'maroon', size=1.3)+
  #annotate("text", x = 60, y = -10, label = "Below pond water level")+
  scale_y_continuous(breaks = round(seq(min(0), max(150), by = 50),1), limits=c(0, 150))+
  xlab("Date")+
  ylab("Pond Water Level (cm)") +
  theme() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "top")
#scale_color_viridis(discrete = TRUE, breaks = round(seq(min(0), max(120), by = 12),1)) 
plot


setwd("~/Dropbox/EmergenceHatching/Figures")
png("Pondheightoverdate.png", units="in", width=5, height=2.8, res=300)
plot
dev.off()











