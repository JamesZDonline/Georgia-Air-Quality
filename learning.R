#Packages
library(plyr)
library(dplyr)
library(zoo)

#Setup the working directory
setwd("/media/jameszd/StorageDrive/MomProjects/")

#Read in the data
Oz2005 <- read.csv("AMP501_1292949-0.txt",sep="|",header=TRUE,skip=0,na.strings="")

#Data comes with two headers. Remove the Second Header
Oz2005 <- Oz2005[-1,]

#Data comes with a comment at the end saying how many
#rows were written, remove that too
lastrow<-nrow(Oz2005)
Oz2005 <- Oz2005[-lastrow,]

#Maybe clear out some columns? wait to get the coding for it

#Sample.Value thinks it is a factor, change it to a character
#so it stays the correct number, then change to numeric
Oz2005$Sample.Value <- as.numeric(as.character(Oz2005$Sample.Value))

#Paste together Date and hour to get DateTime and make it a POSIXct type
Oz2005$DateTime<-paste(Oz2005$Date,Oz2005$Start.Time,sep=":")
Oz2005$DateTime<-as.POSIXct(Oz2005$DateTime,format="%Y%m%d:%R")

#Make factor for 8 hour averages
Oz2005$CutTime<-cut(Oz2005$DateTime,breaks="8 hours")




