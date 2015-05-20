#load libraries
library(ggplot2)
library(plyr)
library(dplyr)
#install.packages("gridExtra")
library(gridExtra) 

# Get system specs
sessionInfo()

# Download file
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","Reproducible_Research/RepData_PeerAssessment2/repdata_data_FStormData.csv.bz2")

# Read file
data <- read.csv(bzfile("Reproducible_Research/RepData_PeerAssessment2/repdata_data_FStormData.csv.bz2"))

dim(data)

head(data)

names(data) <- make.names(names(data)) 

str(data)

View(head(data))

df <- data.frame(data[,c("BGN_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","CROPDMG")])
df$date <- as.Date(df$BGN_DATE, format = "%m/%d/%Y")
df$year <- as.numeric(format(df$date,"%Y"))
#df$HEALTH <- df$FATALITIES + df$INJURIES
#df$ECONOMY <- df$PROPDMG + df$CROPDMG

hist(df$year, breaks = 30, col="blue", main="Data collection growth in U.S. from 1995 to 2011", xlab="Year")

# ----- Health and injuries-----
df.fatalities.bytype <- aggregate(df[, 'FATALITIES'], by=list(df$EVTYPE), FUN=sum, na.rm=TRUE)
df.injuries.bytype <- aggregate(df[, 'INJURIES'], by=list(df$EVTYPE), FUN=sum, na.rm=TRUE)
names(df.fatalities.bytype) <- c("EVTYPE","VALUE")
names(df.injuries.bytype) <- c("EVTYPE","VALUE")
df.fatalities.bytype.10 <- head(arrange(df.fatalities.bytype[which(df.fatalities.bytype$VALUE >0),], desc(VALUE)),10)
df.injuries.bytype.10   <- head(arrange(df.injuries.bytype[which(df.injuries.bytype$VALUE >0),], desc(VALUE)),10) 

# ---- Property and crop damage
df.property.bytype <- aggregate(df[, 'PROPDMG'], by=list(df$EVTYPE), FUN=sum, na.rm=TRUE)
df.crop.bytype <- aggregate(df[, 'CROPDMG'], by=list(df$EVTYPE), FUN=sum, na.rm=TRUE)
names(df.property.bytype) <- c("EVTYPE","VALUE")
names(df.crop.bytype) <- c("EVTYPE","VALUE")
df.property.bytype.10 <- head(arrange(df.property.bytype[which(df.property.bytype$VALUE >0),], desc(VALUE)),10) 
df.crop.bytype.10     <- head(arrange(df.crop.bytype[which(df.crop.bytype$VALUE >0),], desc(VALUE)),10) 


#-----Total all
df.health.bytype <- aggregate(df[, 'HEALTH'], by=list(df$EVTYPE), FUN=sum, na.rm=TRUE)
df.economy.bytype <- aggregate(df[, 'ECONOMY'], by=list(df$EVTYPE), FUN=sum, na.rm=TRUE)
names(df.health.bytype) <- c("EVTYPE","HEALTH")
names(df.economy.bytype) <- c("EVTYPE","HEALTH")
df.health.bytype.10 <- head(arrange(df.health.bytype[which(df.health.bytype$HEALTH >0),], desc(HEALTH)),10) 
df.economy.bytype.10     <- head(arrange(df.economy.bytype[which(df.economy.bytype$HEALTH >0),], desc(HEALTH)),10) 


#ggplot(df.health.bytype.20, aes(y = HEALTH, x = EVTYPE)) + stat_summary(fun.y="sum", geom="line") + ggtitle("Vehicle combustion-related PM2.5 emissions from all sources in Baltimore")  + ylab("Emissions level") +xlab("Year")  + theme(panel.background = element_rect(fill = 'white', colour = 'red')) + theme(plot.background = element_rect(fill = 'whitesmoke', colour = 'red'))

#--- Barplots health and injuries
par(mfrow=c(2,1))
barplot(df.fatalities.bytype.10$HEALTH, names.arg=df.health.bytype.10$EVTYPE, 
        main="Top 10 causes of death (summary)",las=2,cex.axis=0.7, cex.names=0.6, 
        xlab='', ylab="")

barplot(df.injuries.bytype.10$HEALTH, names.arg=df.health.bytype.10$EVTYPE, 
        main="Top 10 causes of injuries (summary)",las=2,cex.axis=0.7, cex.names=0.6,
        xlab='', ylab="")

# --- Barplots property and crops damage
par(mfrow=c(2,1))
barplot(df.property.bytype.10$HEALTH, names.arg=df.property.bytype.10$EVTYPE, 
        main="Top 10 causes of property damage (summary)",las=2,cex.axis=0.7, cex.names=0.6, 
        xlab='', ylab="")

barplot(df.crop.bytype.10$HEALTH, names.arg=df.crop.bytype.10$EVTYPE, 
        main="Top 10 causes of crop damage (summary)",las=2,cex.axis=0.7, cex.names=0.6,
        xlab='', ylab="")

###--------------
aes(x=area, y=sale, fill=area.color)

df.fatalities.bytype.10 <- transform(df.fatalities.bytype.10, EVTYPE = reorder(EVTYPE, order(VALUE, decreasing = TRUE)))

ggplot(data = df.fatalities.bytype.10) +        
        geom_bar(data = df.fatalities.bytype.10,colour="blue", stat="identity", fill="blue", aes(y=VALUE,x=EVTYPE)) +
        theme(axis.text.x = element_text(angle = 45, 
                                         hjust = 1)) + xlab("Weather Type") + 
        ggtitle("Top 10 causes of death\n in the U.S. from 1995 - 2011")

df.fatalities.bytype.10 <- transform(df.fatalities.bytype.10, EVTYPE = reorder(EVTYPE, order(VALUE, decreasing = TRUE)))
df.injuries.bytype.10 <- transform(df.injuries.bytype.10, EVTYPE = reorder(EVTYPE, order(VALUE, decreasing = TRUE)))

df.property.bytype.10 <- transform(df.property.bytype.10, EVTYPE = reorder(EVTYPE, order(VALUE, decreasing = TRUE)))
df.crop.bytype.10 <- transform(df.crop.bytype.10, EVTYPE = reorder(EVTYPE, order(VALUE, decreasing = TRUE)))

fp <- qplot(EVTYPE, data = df.fatalities.bytype.10, weight = VALUE, geom = "bar", binwidth = 1) + 
        scale_y_continuous("Fatalities") + 
        theme(axis.text.x = element_text(angle = 45, 
                                         hjust = 1)) + xlab("Weather Type") + 
        ggtitle("Top 10 causes of death\n in the U.S. from 1995 - 2011")
ip <- qplot(EVTYPE, data = df.injuries.bytype.10, weight = VALUE, geom = "bar", binwidth = 1) + 
        scale_y_continuous("Injuries") + 
        theme(axis.text.x = element_text(angle = 45, 
                                         hjust = 1)) + xlab("Weather Type") + 
        ggtitle("Top 10 causes of injuries\n in the U.S. from 1995 to 2011")
grid.arrange(fp, ip, ncol = 2, main="hah")

###--------------

pp <- qplot(EVTYPE, data = df.property.bytype.10, weight = VALUE, geom = "bar", binwidth = 1) + 
        scale_y_continuous("Property Damage") + 
        theme(axis.text.x = element_text(angle = 45, 
                                         hjust = 1)) + xlab("Weather Type") + 
        ggtitle("Top 10 causes of property damage\n in the U.S. from 1995 - 2011")
cp <- qplot(EVTYPE, data = df.crop.bytype.10, weight = VALUE, geom = "bar", binwidth = 1) + 
        scale_y_continuous("Crop Damage") + 
        theme(axis.text.x = element_text(angle = 45, 
                                         hjust = 1)) + xlab("Weather Type") + 
        ggtitle("Top 10 causes of crop damage\n in the U.S. from 1995 - 2011")
grid.arrange(pp, cp, ncol = 2)


?qplot

