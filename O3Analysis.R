# Load Packages and Data --------------------------------------------------
require(plyr)
require(reshape)
require(foreach)
require(RColorBrewer)
require(scales)
require(ggplot2)

load("O3.RData")


# Clean The data ----------------------------------------------------------

#Remove months November, December, January and February
nov<-which(O3$month=="11")
dec<-which(O3$month=="12")
jan<-which(O3$month=="01")
feb<-which(O3$month=='02')

O3<-O3[-c(jan,feb,nov,dec),]


#Also remove the 2004 and 2003 Data
tooearly<-which(O3$year=="2004"|O3$year=="2003")
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
O3Standard$year<-as.Date(paste(O3Standard$year,"01","01",sep="-"))
# Plots -------------------------------------------------------------------
cbbPalette<-c("#999999","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
family="Ariel"
legendrows=8
legendcols=3
yaxisLimits=c(.05,.14)
AllMyOpts<-theme(plot.title=element_text(family=family,face="bold"),
                       legend.title=element_text(family=family,face="bold"),
                       legend.text=element_text(family=family,face="plain"),
                       axis.text=element_text(family=family,face="plain",colour="black"),
                       axis.title=(element_text(family=family,face="bold",colour="black")),
                       axis.title.y=(element_text(vjust = .75)),
                       legend.position="bottom",
                       panel.background=element_rect(fill="white"),
                       panel.grid.major=element_line(colour="grey85"))

FullPlot<-ggplot(O3Standard,aes(x=year,y=standard,col=Common.Name,linetype=Common.Name))+geom_line(lwd=1.2)+
   geom_point(size=2.75)+ggtitle("8-Hour Ozone Annual Trend")+xlab("Year")+ylab("Ozone concentration (ppm)\n8-Hour Standard")+AllMyOpts+
   scale_linetype_manual(values=c(rep("solid",8),rep("dashed",8),rep("dotted",7)),name="")+
   scale_color_manual(values=c(cbbPalette,cbbPalette[1:8],cbbPalette[1:7]),name="")+
   scale_x_date(breaks=date_breaks(width="1 year"),labels=date_format("%Y"))+
   scale_y_continuous(limits=yaxisLimits,breaks=seq(.0,.15,.01))+
   geom_abline(intercept=.075,slope=0,linetype="dotdash",size=1)+
   stat_summary(fun.y=mean,color="black",geom="line",size=2,linetype="dashed")+
   guides(colour=guide_legend(ncol=legendcols),linetype=guide_legend(ncol=legendcols))
   

plot(FullPlot)

svg("Plots/O3FullPlot.svg",width=6.5, height=6)
plot(FullPlot)
dev.off()

Metro<-O3Standard[O3Standard$MetroAtlanta=="Metro-Atlanta",]
State<-O3Standard[O3Standard$MetroAtlanta=="State",]

# SplitMetro<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=Metro, color=Common.Name,geom=c("line","point"),xlab="Year",
#                   ylab="Ozone concentration (ppm) Standard", main="Yearly Trend in Metro-Atlanta Ozone")+geom_abline(intercept=35,slope=0,linetype="dotdash")+
#    scale_y_continuous(limits=c(.00,.15),breaks=seq(.0,.15,.01))+
#    geom_abline(intercept=.075,slope=0,linetype="dotdash",size=1)+
#    theme(panel.background=element_rect(fill="white"))+
#    theme(panel.grid.major=element_line(colour="grey85"))+
#    stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")

SplitMetro<-ggplot(Metro,aes(x=year,y=standard,col=Common.Name,linetype=Common.Name))+geom_line(lwd=1.2)+geom_point(size=2.75)+
   ggtitle("8-Hour Ozone Annual Trend (Metro-Atlanta)")+xlab("Year")+ylab("Ozone concentration (ppm)\n8-Hour Standard")+AllMyOpts+
   scale_linetype_manual(values=c(rep("solid",7),rep("dashed",6)),name="")+
   scale_color_manual(values=c(cbbPalette,cbbPalette[1:5]),name="")+
   scale_x_date(breaks=date_breaks(width="1 year"),labels=date_format("%Y"))+
   scale_y_continuous(limits=yaxisLimits,breaks=seq(.0,.15,.01))+
   geom_abline(intercept=.075,slope=0,linetype="dotdash",size=1)+
   stat_summary(fun.y=mean,color="black",geom="line",size=2,linetype="dashed")+
   guides(colour=guide_legend(ncol=legendcols),linetype=guide_legend(ncol=legendcols))

plot(SplitMetro)

SplitState<-ggplot(State,aes(x=year,y=standard,col=Common.Name,linetype=Common.Name))+geom_line(lwd=1.2)+geom_point(size=2.75)+
   ggtitle("8-Hour Ozone Annual Trend (Non-Metro-Atlanta)")+xlab("Year")+ylab("Ozone concentration (ppm)\n8-Hour Standard")+AllMyOpts+
   scale_linetype_manual(values=c(rep("solid",7),rep("dashed",5)),name="")+
   scale_color_manual(values=c(cbbPalette,cbbPalette),name="")+
   scale_x_date(breaks=date_breaks(width="1 year"),labels=date_format("%Y"))+
   scale_y_continuous(limits=yaxisLimits,breaks=seq(.0,.15,.01))+
   geom_abline(intercept=.075,slope=0,linetype="dotdash",size=1)+
   stat_summary(fun.y=mean,color="black",geom="line",size=2,linetype="dashed")+
   guides(colour=guide_legend(ncol=legendcols),linetype=guide_legend(ncol=legendcols))

plot(SplitState)

svg("Plots/O3MetroPlot.svg",width=6.5, height=4)
plot(SplitMetro)
dev.off()

svg("Plots/O3StatePlot.svg",width=6.5, height=4)
plot(SplitState)
dev.off()

SmoothPlot<-qplot(as.Date(year,format="%Y"),avg,data=AverageStandard, geom=c("line","point"),xlab="Year",
    ylab="Ozone concentration (ppm)\n 8-Hour Standard", main="8-Hour Ozone Annual Trend")+AllMyOpts+
   AllMyOpts+scale_y_continuous(limits=c(0,.15),breaks=seq(.0,.15,.01))+
   geom_abline(intercept=.075,slope=0,linetype="dotdash")+
   scale_x_date(breaks=date_breaks(width="1 year"),labels=date_format("%Y"))+
   geom_smooth(aes(ymin=perc10,ymax=perc90),data=AverageStandard,stat="identity",fill="orange")

plot(SmoothPlot)

svg("Plots/O3Smooth.svg",width=6.5, height=4)
plot(SmoothPlot)
dev.off()

startYear=min(O3Standard$year)
endYear=max(O3Standard$year)

O3StandMelt<-melt(O3Standard,id.vars = c("year","Common.Name","MetroAtlanta"))
O3StandCast<-cast(O3StandMelt,year+Common.Name~MetroAtlanta)
names(O3StandCast)<-c("year","Common.Name","Metro","State")
O3StandAvg<-ddply(O3StandCast,.(year),summarize,Metroaverage=mean(Metro,na.rm=T),Stateaverage=mean(State,na.rm=T))
O3StandAvg$Full<-AverageStandard$avg
O3PercChange<-data.frame((O3StandAvg[which(O3StandAvg$year==startYear),2:4]-O3StandAvg[O3StandAvg$year==endYear,2:4])/O3StandAvg[O3StandAvg$year==startYear,2:4])
O3PercChange$Pollutant<-"Ozone"
O3PercChange$startYear=startYear
O3PercChange$endYear=endYear

write.table(O3PercChange,file="PercentChange/percentchange.csv",sep=",",row.names=F)

# rm(list=ls())

SiteLookup<-read.csv("Sites2.csv",sep=",",header=T)
mapData<-merge(SiteLookup,O3Standard,by="Common.Name")
mapDataMelt<-melt(mapData,id.vars = c("Common.Name","Site.ID","year"))
mapData<-cast(mapData,Common.Name+Site.ID~year)

write.table(mapData,file="MapFiles/OzoneMapData.csv",sep=",",row.names=F)

