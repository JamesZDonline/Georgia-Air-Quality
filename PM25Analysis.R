# Load packages and Data --------------------------------------------------
require(plyr)
require(ggplot2)
load("PM25.RData")


# Clean The Data ----------------------------------------------------------

#98th percentile of average daily maximum concentrations, averaged over 3 years
PM2.5$Site.ID<-factor(PM2.5$Site.ID)
PM2.5$Sample.Duration<-factor(PM2.5$Sample.Duration)
PM2.5$Start.Time<-factor(PM2.5$Start.Time)

#Fix an issue with units
PM2.5$Sample.Value[PM2.5$Unit=="007"] <-PM2.5$Sample.Value[PM2.5$Unit=="007"]*1000


# Calculate Standards Measurements ----------------------------------------

PM2.5DailyMean<-ddply(PM2.5, .(Date,Common.Name,MetroAtlanta),.parallel=TRUE,summarize,Daily.Mean=mean(Sample.Value,na.rm=TRUE))
PM2.5DailyMean$year<-factor(substr(as.character(PM2.5DailyMean$Date),1,4))

PM2.5Yearly<-ddply(PM2.5DailyMean,.(year,Common.Name,MetroAtlanta),summarize,Yearly=quantile(Daily.Mean,.98,na.rm=T))

f3<-rep(1/3,3)

PM2.5Yearly<-PM2.5Yearly[-which(PM2.5Yearly$Common.Name=="University of Georgia"),]

PM2.5Standard<-ddply(PM2.5Yearly,.(Common.Name),transform,standard=as.numeric(filter(Yearly,f3,sides=1)))

PM2.5Standard<-PM2.5Standard[complete.cases(PM2.5Standard),]


# Plot Data ---------------------------------------------------------------

FullPlot<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=PM2.5Standard, color=Common.Name, geom=c("line","point"),xlab="Year",
         ylab=bquote("PM2.5 Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Georgia PM2.5")+geom_abline(intercept=35,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,70),breaks=seq(0,70,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")

plot(FullPlot)


Metro<-PM2.5Standard[PM2.5Standard$MetroAtlanta=="Metro-Atlanta",]
State<-PM2.5Standard[PM2.5Standard$MetroAtlanta=="State",]

SplitMetro<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=Metro, color=Common.Name,geom=c("line","point"),xlab="Year",
                  ylab=bquote("PM2.5 Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Metro-Atlanta PM2.5")+geom_abline(intercept=35,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,70),breaks=seq(0,70,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")

plot(SplitMetro)

SplitState<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=State, color=Common.Name, geom=c("line","point"),xlab="Year",
                ylab=bquote("PM2.5 Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Statewide PM2.5")+
   geom_abline(intercept=35,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,70),breaks=seq(0,70,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")

plot(SplitState)

svg("Plots/PM25FullPlot.svg",width=8, height=8)
plot(FullPlot)
dev.off()

svg("Plots/PM25SplitMetro.svg",width=8, height=8)
plot(SplitMetro)
dev.off()

svg("Plots/PM25SplitState.svg",width=8, height=8)
plot(SplitState)
dev.off()

PM2.5Summary<-ddply(PM2.5Standard,.(year),summarize,average=mean(standard,na.rm=TRUE),perc10=quantile(standard,probs=.1,na.rm=TRUE),
                    perc90=quantile(standard,probs=.9,na.rm=TRUE))

p1<-qplot(as.Date(paste(year,"01","01",sep="-")),average,data=PM2.5Summary, geom=c("line","point"),xlab="Year",
          ylab=bquote("PM2.5 Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Georgia PM2.5")+
   geom_abline(intercept=35,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,70),breaks=seq(0,70,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   geom_smooth(aes(ymin=perc10,ymax=perc90),data=PM2.5Summary,stat="identity",fill="orange")
plot(p1)


svg("Plots/PM25Smooth.svg",width=8, height=8)
plot(p1)
dev.off()

PM2.5Standard$year<-as.numeric(as.character(PM2.5Standard$year))
percentChange<-ddply(PM2.5Standard,.(Common.Name),summarize,percentChange=(standard[year==max(year)]-standard[year==min(year)])/standard[year==max(year)],StartYear=min(year),EndYear=max(year)+1)
write.table(percentChange,file="PercentChange/percentChangePM25.csv",sep=",",row.names=F)


SiteLookup<-read.csv("Sites2.csv",sep=",",header=T)
mapData<-merge(SiteLookup,PM2.5Standard,by="Common.Name")
mapData$MetroAtlanta.x<-NULL
mapData$MetroAtlanta.y<-NULL
minyear<-PM2.5Standard$year<-min(as.numeric(as.character(PM2.5Standard$year)))
write.table(mapData[mapData$year==2014,],file="MapFiles/PM25MapData2014.csv",sep=",",row.names=F)
write.table(mapData[mapData$year==minyear,],file=paste("MapFiles/PM25MapData",minyear,".csv",sep=""),sep=",",row.names=F)
#rm(list=ls())
