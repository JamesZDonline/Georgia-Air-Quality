#Packages
library(plyr)
library(dplyr)
library(zoo)


# Load data into R --------------------------------------------------------

#Read in Data file names from "Data" folder
files <- dir("Data")

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
}


# Do Analysis -------------------------------------------------------------





   
