#Packages
library(plyr)
library(dplyr)
library(zoo)
library(XML)


# Load data into R --------------------------------------------------------

#Read in Data file names from "Data" folder
files <- dir("Data")

#Clear OzData if it already exists to start with a blank slate
if(exists("OzData")){rm(OzData)}

#for every data file, read it in and add it to OzData.
for (file in files){
   
   #Read in the data files, pasting Data and the filename together for the path name
   OzData_tmp <- read.csv(paste("Data",file,sep="/"),sep="|",header=TRUE,skip=0,na.strings="")
   
   #Data comes with two headers. Remove the Second Header
   OzData_tmp <- OzData_tmp[-1,]
   
   #Data comes with a comment at the end saying how many
   #rows were written, remove that too
   lastrow<-nrow(OzData_tmp)
   OzData_tmp <- OzData_tmp[-lastrow,]
   
   #Check if OzData exists. If it doesn't, use tmp data to make OzData. If it does, add tmp data to OzData.
   if (exists("OzData")){
      OzData <- rbind(OzData,OzData_tmp)
   }else{
      OzData <- OzData_tmp
   }
   print(paste(file,"loaded",sep=" "))
}

rm(OzData_tmp)

# Do Some Cleaning -------------------------------------------------------------

#Sample.Value thinks it is a factor, change it to a character
#so it stays the correct number, then change to numeric
OzData$Sample.Value <- as.numeric(as.character(OzData$Sample.Value))

#Paste together Date and hour to get DateTime and make it a POSIXct type
OzData$DateTime<-paste(OzData$Date,OzData$Start.Time,sep=":")
OzData$DateTime<-as.POSIXct(OzData$DateTime,format="%Y%m%d:%R")

#Change County.Code to CountyName by Reading in fips table, and Removing the County.Code
fips<-read.csv("FIPS.csv",header=FALSE,colClasses=c("NULL","NULL","factor","factor","NULL"),
               col.names=c("State","StateCode","CountyCode","CountyName","ClassCode"))

# merging it with the OzData
OzData<-merge(OzData,fips,by.x="County.Code",by.y="CountyCode")
OzData$CountyName <- factor(OzData$CountyName)

# #and Removing the County.Code
# OzData$County.Code<-NULL

#I want to do the same thing with site codes 
# doc<-htmlTreeParse("http://www.air.dnr.state.ga.us/amp/data_descrip.html#BLEDSOE",useInternal=TRUE)

# places <- xpathSApply(doc,"//h2",xmlValue)

#Remove a bunch of extra columns
OzData$Uncertainty <- NULL
OzData$OzData$Qualifier...9 <- NULL
OzData$OzData$Qualifier...10 <- NULL


rm(fips)






   
