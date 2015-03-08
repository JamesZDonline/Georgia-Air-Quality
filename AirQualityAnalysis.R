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
   files <- dir("Data")
   
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

#Some of the data is composite data. Such data is labeled as RC in the X..RD column and should have different headers.
#One way to fix this is to subset the data out, fix it and then merge it back in.
RCData<-which(QData$X..RD=="RC")
QDataRC<-QData[RCData,]

#Remove the RCData from the original Dataset
QData<-QData[-RCData,]

#Change headers for RC data to correct headers
RCheaders<-c("X..RD","Action.Code","State.Code","County.Code","Site.ID","Parameter",
             "POC","Unit","Method","Year","Month","Number.of.Samples","Composite.Type",
             "Sample.Value","Monitor.Protocol..MP..ID","Qualifier...1","Qualifier...2",
             "Qualifier...3","Qualifier...4","Qualifier...5","Qualifier...6","Qualifier...7",
             "Qualifier...8","Qualifier...9","Qualifier...10","Alternate.Method.Detectable.Limit",
             "Uncertainty","ExtraColumn")

names(QDataRC)<-RCheaders

#Create Date Variable for RC Data. I decided to set the day of the month to be the first for each month. I can change this later
#if I find a need
QDataRC$Date<-paste(QDataRC$Year,QDataRC$Month,"01",sep="")

#Likewise arbitrarily set the start time to be midnight
QDataRC$Start.Time<-"00:00"

#Add null data code to be na
QDataRC$Null.Data.Code<-NA

#Composite Type is all Monthly so I can just remove that variable
QDataRC$Composite.Type<-NULL

QDataRC$ExtraColumn<-NULL
QDataRC$Year<-NULL
QDataRC$Month<-NULL




#Remove a bunch of extra columns

QData$Action.Code<-NULL

QData$Parameter<-factor(QData$Parameter)
QData$POC<-factor(QData$POC)
QData$Sample.Duration<-factor(QData$Sample.Duration)
QData$Unit<-factor(QData$Unit)
QData$Method<-factor(QData$Method)
QData$Sampling.Frequency<-factor(QData$Sampling.Frequency)
