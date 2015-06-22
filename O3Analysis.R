# Load Packages and Data --------------------------------------------------
require(plyr)
require(foreach)
require(ggplot2)

load("O3.RData")


# Clean The data ----------------------------------------------------------

#Remove months November, December, January and February
nov<-which(O3$month=="11")
dec<-which(O3$month=="12")
jan<-which(O3$month=="01")
feb<-which(O3$month=='02')

O3<-O3[-c(jan,feb,nov,dec),]


#Also remove the 2004 Data
tooearly<-which(O3$year=="2004")
O3<-O3[-tooearly,]

#Fix an issue with units
O3$Sample.Value[O3$Unit=="008"] <-O3$Sample.Value[O3$Unit=="008"]/1000

#Now done prior to dividing up pollutants so no longer necessary here
      #Create readable site names
      # SiteLookup <- read.csv("Sites2.csv",sep=",",header=T)
      # O3<-merge(O3,SiteLookup,by.x="Site.ID",by.y="Site.ID",all.x=TRUE)

#Remove Georgia DOT
O3<-O3[-which(O3$Common.Name=="Georgia DOT-Fayetteville"),]


O3$year<-factor(O3$year)
O3$month<-factor(O3$month)

# Analysis ----------------------------------------------------------------

#In order to examine trends, the EPA tracks the 
#Annual 4th maximum of daily max 8-hour averages

#In order to replicate this, it is first necessary to 
#produce 8 hour averages
fil<-rep(1/8,8)

O3<-ddply(O3,.(year),mutate, hr8mean=as.numeric(filter(Sample.Value,fil,sides=1)))

#Then Compute the daily max of those 8-hour averages
O3DailyMax<-ddply(O3, .(Date,Common.Name,MetroAtlanta),.parallel=TRUE,summarize,Daily.Max=max(Sample.Value,na.rm=TRUE))

O3DailyMax$year<-factor(substr(as.character(O3DailyMax$Date),1,4))

#This function finds the n-th maximum in a vector
nmax<-function(data,n){
   len<-length(data)
   sort(data,partial=len-n)[len-n+1]
}

#Now we find the 4th highest annual daily maximum
O3Standard<-ddply(O3DailyMax,.(year,Common.Name,MetroAtlanta),summarize,standard=nmax(Daily.Max,4))

AverageStandard<-ddply(O3Standard,.(year),summarize,avg=mean(standard,na.rm=TRUE),perc10=quantile(standard,probs=.1,na.rm=TRUE),
                       perc90=quantile(standard,probs=.9,na.rm=TRUE))

# Plots -------------------------------------------------------------------

FullPlot<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=O3Standard, color=Common.Name, geom=c("line","point"),xlab="Year",
         ylab="Ozone concentration (ppm) Standard", main="Yearly Trend in Georgia Ozone")+
   scale_y_continuous(limits=c(.00,.15),breaks=seq(.0,.15,.01))+
   geom_abline(intercept=.075,slope=0,linetype="dotdash",size=1)+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")

plot(FullPlot)

Metro<-O3Standard[O3Standard$MetroAtlanta=="Metro-Atlanta",]
State<-O3Standard[O3Standard$MetroAtlanta=="State",]

SplitMetro<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=Metro, color=Common.Name,geom=c("line","point"),xlab="Year",
                  ylab="Ozone concentration (ppm) Standard", main="Yearly Trend in Metro-Atlanta Ozone")+geom_abline(intercept=35,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(.00,.15),breaks=seq(.0,.15,.01))+
   geom_abline(intercept=.075,slope=0,linetype="dotdash",size=1)+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")

plot(SplitMetro)

SplitState<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=State, color=Common.Name, geom=c("line","point"),xlab="Year",
                  ylab="Ozone concentration (ppm) Standard", main="Yearly Trend in Statewide Ozone")+
   scale_y_continuous(limits=c(.00,.15),breaks=seq(.0,.15,.01))+
   geom_abline(intercept=.075,slope=0,linetype="dotdash",size=1)+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")

plot(SplitState)

svg("Plots/O3FullPlot.svg",width=8, height=8)
plot(FullPlot)
dev.off()

svg("Plots/O3MetroPlot.svg",width=8, height=8)
plot(SplitMetro)
dev.off()

svg("Plots/O3StatePlot.svg",width=8, height=8)
plot(SplitState)
dev.off()

SmoothPlot<-qplot(as.Date(year,format="%Y"),avg,data=AverageStandard, geom=c("line","point"),xlab="Year",
          ylab="Yearly Mean of Daily Max Ozone concentration (ppm)", main="Yearly Trend in Georgia Ozone")+
   scale_y_continuous(limits=c(0,.15),breaks=seq(.0,.15,.01))+
   geom_abline(intercept=.075,slope=0,linetype="dotdash")+
   geom_smooth(aes(ymin=perc10,ymax=perc90),data=AverageStandard,stat="identity",fill="orange")+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))

plot(SmoothPlot)

svg("Plots/O3Smooth.svg",width=8, height=8)
plot(SmoothPlot)
dev.off()

O3Standard$year<-as.numeric(as.character(O3Standard$year))

percentChange<-ddply(O3Standard,.(Common.Name),summarize,percentChange=(standard[year==max(year)]-standard[year==min(year)])/standard[year==max(year)],StartYear=min(year),EndYear=max(year)+1)
write.table(percentChange,file="PercentChange/percentChangeOzone.csv",sep=",",row.names=F)
# rm(list=ls())

SiteLookup<-read.csv("Sites2.csv",sep=",",header=T)
mapData<-merge(SiteLookup,O3Standard,by="Common.Name")
mapData$MetroAtlanta.x<-NULL
mapData$MetroAtlanta.y<-NULL
write.table(mapData[mapData$year==2014,],file="MapFiles/OzoneMapData2014.csv",sep=",",row.names=F)
write.table(mapData[mapData$year==2005,],file="MapFiles/OzoneMapData2005.csv",sep=",",row.names=F)
