#Packages
library(plyr)
library(dplyr)
library(zoo)
library(XML)
library(ggplot2)


# Load data into R --------------------------------------------------------
cat("Do you want to re-load the QData from the files? y/n \n
    Note! If the answer is n, the program will try loading it from a presaved RData file. If you are unsure, type y. \n
    Further Note! If you answer with y or an invalid response, the
    program will go ahead and load from files.")
runthis<-readLines(con=stdin(),n=1)
if(runthis!="n"){
   #Read in Data file names from "Data" folder
   files <- dir("Data",pattern=".txt")
   
   #Clear QData if it already exists to start with a blank slate
   if(exists("QData")){rm(QData)}
   
   #for every data file, read it in and add it to QData.
   for (file in files){
      
      #Read in the data files, pasting Data and the filename together for the path name
      QData_tmp <- read.csv(paste("Data",file,sep="/"),sep="|",header=TRUE,skip=0,na.strings="")
      
      #Data comes with two headers. Remove the Second Header
      QData_tmp <- QData_tmp[-1,]
      
      #Data comes with a comment at the end saying how many
      #rows were written, remove that too
      lastrow<-nrow(QData_tmp)
      QData_tmp <- QData_tmp[-lastrow,]
      
      #Check if QData exists. If it doesn't, use tmp data to make QData. If it does, add tmp data to QData.
      if (exists("QData")){
         QData <- rbind(QData,QData_tmp)
      }else{
         QData <- QData_tmp
      }
      print(paste(file,"loaded",sep=" "))
   }
   save(QData,file="QData.RData")
   
   rm(QData_tmp)
}
# Do Some Cleaning -------------------------------------------------------------
if(!exists("QData")){load("QData.RData")}
#Remove a bunch of extra columns

QData$Uncertainty <- NULL
QData$Qualifier...2 <- NULL
QData$Qualifier...3 <- NULL
QData$Qualifier...4 <- NULL
QData$Qualifier...5 <- NULL
QData$Qualifier...6 <- NULL
QData$Qualifier...7 <- NULL
QData$Qualifier...8<- NULL
QData$Qualifier...9 <- NULL
QData$Qualifier...10 <- NULL
QData$Monitor.Protocol..MP..ID <- NULL
QData$Sampling.Frequency<-NULL
QData$Action.Code<-NULL
QData$X..RD<-NULL

#Sample.Value thinks it is a factor, change it to a character
#so it stays the correct number, then change to numeric
QData$Sample.Value <- as.numeric(as.character(QData$Sample.Value))


#Create Year factor
QData$year<-factor(substr(as.character(QData$Date),1,4))

#Create Month Factor
QData$month<-factor(substr(as.character(QData$Date),5,6))

#Paste together Date and hour to get DateTime and make it a POSIXct type
QData$DateTime<-paste(QData$Date,QData$Start.Time,sep=":")
QData$DateTime<-as.POSIXct(QData$DateTime,format="%Y%m%d:%R")

#Paste together State.Code, County.Code, and Site.ID to create the full Site ID and save it as a factor back in Site ID
QData$Site.ID<-factor(paste(QData$State.Code,QData$County.Code,QData$Site.ID,sep=""))
QData$State.Code <- NULL

#Create Common.Name from Lookup table in SiteID.txt
SiteLookup <- read.csv("SiteID.txt",sep=",",header=T)

QData<-merge(QData,SiteLookup,by.x="Site.ID",by.y="Site.ID",all.x=TRUE)


#Change County.Code to CountyName by Reading in fips table, and Removing the County.Code
fips<-read.csv("FIPS.csv",header=FALSE,colClasses=c("NULL","NULL","factor","factor","NULL"),
               col.names=c("State","StateCode","CountyCode","CountyName","ClassCode"))

# merging it with the QData
QData<-merge(QData,fips,by.x="County.Code",by.y="CountyCode",all.x=TRUE)
QData$CountyName <- factor(QData$CountyName)

# #and Removing the County.Code
# QData$County.Code<-NULL

#Change 008 Data Units to 007 data units
QData$Sample.Value[QData$Unit=="008"] <-QData$Sample.Value[QData$Unit=="008"]/1000

#Remove Georgia DOT Data
GDOTdata<-which(QData$Common.Name=="Georgia DOT")
QData<-QData[-GDOTdata,]


#Remove 2 POC data
POC2<-which(QData$POC==2)
QData<-QData[-POC2,]

#Remove Winter Months
nov<-which(QData$month=="11")
dec<-which(QData$month=="12")
jan<-which(QData$month=="01")
feb<-which(QData$month=='02')


QData<-QData[-jan,]
QData<-QData[-feb,]
QData<-QData[-nov,]
QData<-QData[-dec,]

#Some data starts with 2004, others with 2005, remove 2004
tooearly<-which(QData$year=="2004")
QData<-QData[-tooearly,]


rm(fips,SiteLookup,GDOTdata,POC2,dec,feb,jan,nov,tooearly)

#Create Data Frame with the daily maximum value
OzMax<-ddply(QData, .(Date,Common.Name),summarize,Sample.Max=max(Sample.Value,na.rm=TRUE),parallel=TRUE)

# Use OzMax Date to get year and month
OzMax$year <- factor(substr(as.character(OzMax$Date),1,4))
OzMax$month <- factor(substr(as.character(OzMax$Date),5,6))

#Calculate average by year
OzYearMean <- ddply(OzMax,.(year,Common.Name),summarize, yearavg=mean(Sample.Max,na.rm=TRUE),
                    perc10=quantile(Sample.Max,probs=.1,na.rm=TRUE),
                    perc90=quantile(Sample.Max,probs=.9,na.rm=TRUE))



#Plot Generation

# Subdivide by place and year
#YearSummary<-ddply(QData, .(Common.Name,year),summarize, yearly.mean=mean(Sample.Value,na.rm=TRUE), 
 #                  perc10=quantile(Sample.Value,probs=.1,na.rm=TRUE),
  #                 perc90=quantile(Sample.Value,probs=.9,na.rm=TRUE))

#YearSummary2<-ddply(QData, .(year),summarize, yearly.mean=mean(Sample.Value,na.rm=TRUE),
 #                   perc10=quantile(Sample.Value,probs=.1,na.rm=TRUE),
  #                  perc90=quantile(Sample.Value,probs=.9,na.rm=TRUE))

#Quick Plot Yearly Average and Yearly Average by Name
s<-qplot(as.Date(year,format="%Y"),yearavg,data=OzYearMean, color=Common.Name, geom=c("line","point"),xlab="Year",
      ylab="Yearly Mean of Daily Max Ozone concentration (ppm)", main="Yearly Trend in Georgia Ozone")

plot(s)

s2<-s+scale_y_continuous(limits=c(.02,.08),breaks=seq(.02,.1,.01))+
      geom_abline(intercept=.075,slope=0,linetype="dotdash")+
      theme(panel.background=element_rect(fill="white"))

plot(s2)
#p2+geom_smooth(aes(ymin=perc10,ymax=perc90),data=OzYearMean,stat="identity")

AllYearMean <- ddply(OzMax,.(year),summarize, yearavg=mean(Sample.Max,na.rm=TRUE),
                    perc10=quantile(Sample.Max,probs=.1,na.rm=TRUE),
                    perc90=quantile(Sample.Max,probs=.9,na.rm=TRUE))

p<-qplot(as.Date(year,format="%Y"),yearavg,data=AllYearMean, geom=c("line","point"),xlab="Year",
         ylab="Yearly Mean of Daily Max Ozone concentration (ppm)", main="Yearly Trend in Georgia Ozone")

p2 <- p+scale_y_continuous(limits=c(.02,.09),breaks=seq(.02,.1,.01))+
      geom_abline(intercept=.075,slope=0,linetype="dotdash")+
      geom_smooth(aes(ymin=perc10,ymax=perc90),data=AllYearMean,stat="identity")+
      theme(panel.background=element_rect(fill="white"),panel.grid.major=element_line(color="gray"))

plot(p2)

