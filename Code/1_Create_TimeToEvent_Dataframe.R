########################################################################
############Feb 28th 2022, Code written by Anne Devan-Song##############
######Creating submergence time to event################################
############Bend, OR, USA###############################################
########################################################################

rm(list = ls()) #clear workspace
graphics.off() #clear plots 

library(tidyverse) #load all the libraries you need for this code

setwd("~/Dropbox/EmergenceHatching/Data") #set a working directory

clutch <- read.csv("Clutch_data.csv")
clutch$Date <- as.character(clutch$Date)
clutch$Date <- as.Date(clutch$Date, format = "%Y%m%d")

pond <- read.csv("Pond_data.csv")
pond$Date <- as.character(pond$Date)
pond$Date <- as.Date(pond$Date, format = "%Y%m%d")

clutch <- clutch[, c("Date", 
                     "Location", 
                     "TimeOfDay", 
                     "Unique.ID", 
                     "Height_cm")]

pond <- pond[, c("Date", 
                     "Location", 
                     "TimeOfDay", 
                    "Orig_WaterLevel", 
                     "Orig_WaterLevel_corr")]

merged <- merge(clutch, pond, by = c("Date", "Location", "TimeOfDay"), all.x=TRUE)

colnames(merged) <- c("Date", 
                      "Location", 
                      "TimeOfDay", 
                      "Unique.ID", 
                      "Orig_Height_cm",
                      "Orig_WaterLevel", 
                      "Orig_WaterLevel_corr")



#### Now turn the above into a forloop to do it for every unique clutch

df <- data.frame("Date" = numeric(0), 
                 "Location" = numeric(0), 
                 "TimeOfDay" = numeric(0), 
                 "Unique.ID" = numeric(0), 
                 "Orig_Height_cm" = numeric(0), 
                 "Orig_WaterLevel" = numeric(0), 
                 "HPO" = numeric(0))

uniqueID <- unique(merged$Unique.ID)

for (i in uniqueID) {
  baserow <- subset(merged, Unique.ID == i)
  row1 <- baserow
  row1$HPO <- 0
  
  row2 <-baserow
  row2$Date <- baserow$Date #change to new date 
  row2$TimeOfDay <- "dusk" #alternate dawn and dusk
  row2$HPO <- 12
  
  row3 <-baserow
  row3$Date <- baserow$Date + 1 #change to new date 
  row3$TimeOfDay <- "dawn" #alternate dawn and dusk
  row3$HPO <- 24
  
  row4 <-baserow
  row4$Date <- baserow$Date + 1#change to new date 
  row4$TimeOfDay <- "dusk" #alternate dawn and dusk
  row4$HPO <- 36
  
  row5 <-baserow
  row5$Date <- baserow$Date +2#change to new date 
  row5$TimeOfDay <- "dawn" #alternate dawn and dusk
  row5$HPO <- 48
  
  row6 <-baserow
  row6$Date <- baserow$Date +2#change to new date 
  row6$TimeOfDay <- "dusk" #alternate dawn and dusk
  row6$HPO <- 60
  
  
  row7 <-baserow
  row7$Date <- baserow$Date +3#change to new date 
  row7$TimeOfDay <- "dawn" #alternate dawn and dusk
  row7$HPO <- 72
  
  row8 <-baserow
  row8$Date <- baserow$Date+3 #change to new date 
  row8$TimeOfDay <- "dusk" #alternate dawn and dusk
  row8$HPO <- 84
  
  row9 <-baserow
  row9$Date <- baserow$Date +4 #change to new date 
  row9$TimeOfDay <- "dawn" #alternate dawn and dusk
  row9$HPO <- 96
  
  row10 <-baserow
  row10$Date <- baserow$Date +4 #change to new date 
  row10$TimeOfDay <- "dusk" #alternate dawn and dusk
  row10$HPO <- 108
  
  row11 <-baserow
  row11$Date <- baserow$Date +5  #change to new date 
  row11$TimeOfDay <- "dawn" #alternate dawn and dusk
  row11$HPO <- 120
  
  finalrow <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10, row11)
  finalrow <- as.data.frame(finalrow)
  
  df <- rbind(df, finalrow)

}

colnames(pond)
colnames(df)
colnames(pond) <- c("Date", 
                      "Location", 
                      "TimeOfDay", 
                      "New_WaterLevel", 
                    "New_WaterLevel_corr")

#rename colnames of pond to merge with new dataframe 

mergeddf <- merge(df, pond, by = c("Date", "Location", "TimeOfDay"), all.x=TRUE)


sub <- mergeddf[is.na(mergeddf$New_WaterLevel_corr), ]   


write.csv(mergeddf, file="expanded2.csv")


mergeddf$New_Height_cm <- mergeddf$Orig_Height_cm - 
  (mergeddf$New_WaterLevel_corr - (mergeddf$Orig_WaterLevel_corr))

#mergeddf$submerged <- ""

mergeddf$submerged <- ifelse(mergeddf$New_Height_cm < 1, "1", 
                             ifelse(mergeddf$New_Height_cm > 0, "0", "NA"))


write.csv(mergeddf, file="expanded.csv")



#Now, replace all 120 HPO with submerged = 1

HPO120 <- subset(mergeddf, HPO==120)
HPO_Other <-subset(mergeddf, HPO <120)

HPO120$submerged <- as.numeric(HPO120$submerged)
HPO120$submerged <- rep(c(1),times=nrow(HPO120))

newmergeddf <- rbind(HPO_Other, HPO120)

newmergeddf$submerged <- as.numeric(newmergeddf$submerged)

setwd("~/Dropbox/EmergenceHatching/Data") 
write.csv(newmergeddf, file="expanded.csv")

#Create a dataframe with unique clutch ID, date, location, and earliest HPO at submergence 
survivaldf <- data.frame("Unique.ID" = character(0), 
                 "HPO_when_submerged" = numeric(0), 
                 "site" = character(0), 
                 "date" = character(0))

uniqueID <- unique(newmergeddf$Unique.ID)

newmergeddf$Date <- as.character(newmergeddf$Date)

#i <- "LDPCHL022011"

for (i in uniqueID) {
  individual <- subset(newmergeddf, Unique.ID == i)
  subindividual <- subset(individual, submerged == 1)
  CritHPO <- min(subindividual$HPO)
  site <- individual[1,2]
  date <- individual[1, 1]

  finalrow <- cbind(i, CritHPO, site, date)
  finalrow <- as.data.frame(finalrow)
  
  survivaldf <- rbind(survivaldf, finalrow)
}

survivaldf$CritHPO <- as.numeric(as.character(survivaldf$CritHPO))

hist(survivaldf$CritHPO)

nrow(subset(survivaldf, CritHPO<120))
43/476


write.csv(survivaldf, file="time_to_event_data.csv", row.names=FALSE)

