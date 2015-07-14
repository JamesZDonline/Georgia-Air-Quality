#Packages
require(plyr)
require(zoo)
require(XML)
require(ggplot2)


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

#Function for combining together taken from https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/

rbind.all.columns <- function(x, y) {
   
   x.diff <- setdiff(colnames(x), colnames(y))
   y.diff <- setdiff(colnames(y), colnames(x))
   
   x[, c(as.character(y.diff))] <- NA
   
   y[, c(as.character(x.diff))] <- NA
   
   return(rbind(x, y))
}

#Recombine QData and QDataRC

QData<-rbind.all.columns(QData,QDataRC)
rm(QDataRC)
#Remove a bunch of extra columns
#Action code is I for all data. No need to keep this
QData$Action.Code<-NULL

#Uncertainty, AMDL, and these qualifiers are NA for all data
QData$Uncertainty<-NULL
QData$Alternate.Method.Detectable.Limit<-NULL
QData$Qualifier...10<-NULL
QData$Qualifier...9<-NULL
QData$Qualifier...8<-NULL
QData$Qualifier...7<-NULL
QData$Qualifier...6<-NULL
QData$Qualifier...5<-NULL
QData$Qualifier...4<-NULL
QData$Qualifier...3<-NULL

#Some of the data is of the wrong type, correct this
QData$X..RD<-factor(QData$X..RD)
QData$Parameter<-factor(QData$Parameter)
QData$Sample.Value<-as.numeric(as.character(QData$Sample.Value))
QData$POC<-factor(QData$POC)
QData$Sample.Duration<-factor(QData$Sample.Duration)
QData$Unit<-factor(QData$Unit)
QData$Method<-factor(QData$Method)
QData$Null.Data.Code<-factor(QData$Null.Data.Code)
QData$Sampling.Frequency<-factor(QData$Sampling.Frequency)
QData$Number.of.Samples<-factor(QData$Number.of.Samples)

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

#Create Readable county names
fips<-read.csv("FIPS.csv",header=FALSE,colClasses=c("NULL","NULL","factor","factor","NULL"),
               col.names=c("State","StateCode","CountyCode","CountyName","ClassCode"))

# merging it with the Data
QData<-merge(QData,fips,by.x="County.Code",by.y="CountyCode",all.x=TRUE)
QData$CountyName <- factor(QData$CountyName)

#Add Sitenames and Location type
SiteLookup<-read.csv("Sites2.csv",sep=",",header=T)
QData<-merge(QData,SiteLookup,by.x="Site.ID",by.y="Site.ID",all.x=TRUE)
QData$MetroAtlanta<-gsub("no",QData$MetroAtlanta,ignore.case=T,replacement="State")
QData$MetroAtlanta<-gsub("yes",QData$MetroAtlanta,ignore.case=T,replacement="Metro-Atlanta")
save(QData,file="CleanedQData.RData")

#Create separate files for each criteria pollutant
#Lead has two parameter codes '12128' and '14129' Extract them both and save in LeadData
Lead<-QData[which(QData$Parameter=='12128'|QData$Parameter=='14129'),]
save(Lead,file="LeadData.RData")

PM2.5<-QData[which(QData$Parameter=='88101'),]
save(PM2.5,file="PM25.RData")

SO2<-QData[which(QData$Parameter=='42401'),]
save(SO2,file="SO2.RData")

NO2<-QData[which(QData$Parameter=='42602'),]
save(NO2,file="NO2.RData")

O3<-QData[which(QData$Parameter=='44201'),]
save(O3,file="O3.RData")

rm(QData)

#Run analysis for each criteria
source("O3Analysis.R")
source("LeadAnalysis.R")
source("NO2Analysis.R")
source("SO2Analysis.R")
source("PM25Analysis.R")
source("SO2Long.R")
