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

O3<-droplevels(O3)
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
family="Arial"
legendrows=4
yaxisLimits=c(.05,.14)

# FullPlot<-ggplot(O3Standard,aes(x=year,y=standard,col=Common.Name,linetype=Common.Name))+geom_line(lwd=1.2)+
#    geom_point(size=2.75)+ggtitle("8-Hour Ozone Annual Trend")+xlab("Year")+ylab("Ozone concentration (ppm) 8-Hour Standard")+AllMyOpts+
#    scale_linetype_manual(values=c(rep("solid",8),rep("dashed",8),rep("dotted",7)),name="")+
#    scale_color_manual(values=c(cbbPalette,cbbPalette[1:8],cbbPalette[1:7]),name="")+
#    scale_x_date(breaks=date_breaks(width="1 year"),labels=date_format("%Y"))+
#    scale_y_continuous(limits=yaxisLimits,breaks=seq(.0,.15,.01))+
#    geom_abline(intercept=.075,slope=0,linetype="dotdash",size=1)+
#    stat_summary(fun.y=mean,color="black",geom="line",size=2,linetype="dashed")+
#    guides(colour=guide_legend(nrow=legendrows),linetype=guide_legend(nrow=legendrows))
#    
# 
# plot(FullPlot)

# Metro<-O3Standard[O3Standard$MetroAtlanta=="Metro-Atlanta",]
# State<-O3Standard[O3Standard$MetroAtlanta=="State",]
# 
# # SplitMetro<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=Metro, color=Common.Name,geom=c("line","point"),xlab="Year",
# #                   ylab="Ozone concentration (ppm) Standard", main="Yearly Trend in Metro-Atlanta Ozone")+geom_abline(intercept=35,slope=0,linetype="dotdash")+
# #    scale_y_continuous(limits=c(.00,.15),breaks=seq(.0,.15,.01))+
# #    geom_abline(intercept=.075,slope=0,linetype="dotdash",size=1)+
# #    theme(panel.background=element_rect(fill="white"))+
# #    theme(panel.grid.major=element_line(colour="grey85"))+
# #    stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
# 
# SplitMetro<-ggplot(Metro,aes(x=year,y=standard,col=Common.Name,linetype=Common.Name))+geom_line(lwd=1.2)+geom_point(size=2.75)+
#    ggtitle("8-Hour Ozone Annual Trend (Metro-Atlanta)")+xlab("Year")+ylab("Ozone concentration (ppm) 8-Hour Standard")+AllMyOpts+
#    scale_linetype_manual(values=c(rep("solid",6),rep("dashed",6)),name="")+
#    scale_color_manual(values=c(cbbPalette,cbbPalette[1:4]),name="")+
#    scale_x_date(breaks=date_breaks(width="1 year"),labels=date_format("%Y"))+
#    scale_y_continuous(limits=yaxisLimits,breaks=seq(.0,.15,.01))+
#    geom_abline(intercept=.075,slope=0,linetype="dotdash",size=1)+
#    stat_summary(fun.y=mean,color="black",geom="line",size=2,linetype="dashed")+
#    guides(colour=guide_legend(nrow=legendrows),linetype=guide_legend(nrow=legendrows))
# 
# plot(SplitMetro)
# 
# SplitState<-ggplot(State,aes(x=year,y=standard,col=Common.Name,linetype=Common.Name))+geom_line(lwd=1.2)+geom_point(size=2.75)+
#    ggtitle("8-Hour Ozone Annual Trend (Non-Metro-Atlanta)")+xlab("Year")+ylab("Ozone concentration (ppm) 8-Hour Standard")+AllMyOpts+
#    scale_linetype_manual(values=c(rep("solid",6),rep("dashed",5)),name="")+
#    scale_color_manual(values=c(cbbPalette,cbbPalette),name="")+
#    scale_x_date(breaks=date_breaks(width="1 year"),labels=date_format("%Y"))+
#    scale_y_continuous(limits=yaxisLimits,breaks=seq(.0,.15,.01))+
#    geom_abline(intercept=.075,slope=0,linetype="dotdash",size=1)+
#    stat_summary(fun.y=mean,color="black",geom="line",size=2,linetype="dashed")+
#    guides(colour=guide_legend(nrow=legendrows),linetype=guide_legend(nrow=legendrows))
# 
# plot(SplitState)
# 
# jpeg("Plots/O3FullPlot.jpg",width=1600,height=1600)
# plot(FullPlot)
# dev.off()
# 
# svg("Plots/O3FullPlot.svg",width=8, height=8)
# plot(FullPlot)
# dev.off()
# 
# svg("Plots/O3MetroPlot.svg",width=8, height=8)
# plot(SplitMetro)
# dev.off()
# 
# svg("Plots/O3StatePlot.svg",width=8, height=8)
# plot(SplitState)
# dev.off()


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


AllMyOpts<-theme(plot.title=element_text(family=family,face="bold"),
                 legend.title=element_text(family=family,face="bold"),
                 legend.text=element_text(family=family,face="plain"),
                 axis.text=element_text(family=family,face="plain",colour="black"),
                 axis.title=(element_text(family=family,face="bold",colour="black")),
                 axis.title.y=(element_text(vjust = .75)),
                 legend.position="bottom",
                 panel.background=element_rect(fill="white"),
                 panel.grid.major=element_line(colour="grey85"))



xlabel<-paste("Year 2005-2014: ",as.character(100*round(O3PercChange$Full,digits = 2)),"% decrease",sep="")

SmoothPlot<-ggplot(AverageStandard,aes(x=as.Date(paste(year,"01","01",sep="-")),y=avg))+geom_line(lwd=1.2)+geom_point(size=2.75)+AllMyOpts+
   xlab(xlabel)+ylab("Concentration (ppm)")+ggtitle("State of Georgia Annual Trend: Ozone")+
   scale_y_continuous(limits=c(0,.15),breaks=seq(.0,.15,.02))+
   geom_abline(intercept=.075,slope=0,linetype="dotdash")+
   scale_x_date(breaks=date_breaks(width="1 year"),labels=date_format("%Y"))+
   geom_smooth(aes(ymin=perc10,ymax=perc90),data=AverageStandard,stat="identity",fill="orange",colour="black")+
   annotate("text",x=as.Date("2013-01-01"),y=.1425,label=paste(as.character(length(levels(O3$Site.ID))),"sites"),family=family,size=3)+
   annotate("text",x=as.Date("2007-01-15"),y=.072,label="Current National Standard",family=family,size=3)

plot(SmoothPlot)

tiff("Plots/O3Smooth.tiff",width=5.5, height=4,units="in",res=300,family=family,pointsize=9)
plot(SmoothPlot)
dev.off()


xlabelMetro<-paste("Year 2005-2014: ",as.character(100*round(O3PercChange$Metroaverage,digits = 2)),"% decrease",sep="")

Metro<-O3Standard[O3Standard$MetroAtlanta=="Metro-Atlanta",]
AverageMetroStandard<-ddply(Metro,.(year),summarize,avg=mean(standard,na.rm=TRUE),perc10=quantile(standard,probs=.1,na.rm=TRUE),
                       perc90=quantile(standard,probs=.9,na.rm=TRUE))

State<-O3Standard[O3Standard$MetroAtlanta=="State",]
AverageStateStandard<-ddply(O3Standard,.(year),summarize,avg=mean(standard,na.rm=TRUE),perc10=quantile(standard,probs=.1,na.rm=TRUE),
                       perc90=quantile(standard,probs=.9,na.rm=TRUE))


SmoothMetro<-ggplot(AverageMetroStandard,aes(x=as.Date(paste(year,"01","01",sep="-")),y=avg))+geom_line(lwd=1.2)+geom_point(size=2.75)+AllMyOpts+
   ggtitle("Metro-Atlanta Annual Trend: Ozone")+xlab(xlabelMetro)+ylab("Concentration (ppm)")+
   scale_y_continuous(limits=c(0,.15),breaks=seq(.0,.15,.02))+
   geom_abline(intercept=.075,slope=0,linetype="dotdash")+
   scale_x_date(breaks=date_breaks(width="1 year"),labels=date_format("%Y"))+
   geom_smooth(aes(ymin=perc10,ymax=perc90),data=AverageMetroStandard,stat="identity",fill="orange",colour="black")+
   annotate("text",x=as.Date("2013-01-01"),y=.1425,label=paste(as.character(length(levels(droplevels(O3$Site.ID[O3$MetroAtlanta!="State"])))),"sites"),family=family,size=3)+
   annotate("text",x=as.Date("2007-01-15"),y=.072,label="Current National Standard",family=family,size=3)




plot(SmoothMetro)

tiff("Plots/O3SmoothMetro.tiff",width=5.5, height=4,units="in",res=300,family=family,pointsize=9)
plot(SmoothMetro)
dev.off()

xlabelState<-paste("Year 2005-2014: ",as.character(100*round(O3PercChange$Stateaverage,digits = 2)),"% decrease",sep="")

SmoothState<-ggplot(AverageStateStandard,aes(x=as.Date(paste(year,"01","01",sep="-")),y=avg))+geom_line(lwd=1.2)+geom_point(size=2.75)+AllMyOpts+
   ggtitle("Non-Metro-Atlanta Annual Trend: Ozone")+xlab(xlabelState)+ylab("Concentration (ppm)")+
   scale_y_continuous(limits=c(0,.15),breaks=seq(.0,.15,.02))+
   geom_abline(intercept=.075,slope=0,linetype="dotdash")+
   scale_x_date(breaks=date_breaks(width="1 year"),labels=date_format("%Y"))+
   geom_smooth(aes(ymin=perc10,ymax=perc90),data=AverageStateStandard,stat="identity",fill="orange",colour="black")+
   annotate("text",x=as.Date("2013-01-01"),y=.1425,label=paste(as.character(length(levels(droplevels(O3$Site.ID[O3$MetroAtlanta=="State"])))),"sites"),family=family,size=3)+
   annotate("text",x=as.Date("2007-01-15"),y=.072,label="Current National Standard",family=family,size=3)

plot(SmoothState)

tiff("Plots/O3SmoothState.tiff",width=5.5, height=4,units="in",res=300,family=family,pointsize=9)
plot(SmoothState)
dev.off()

write.table(O3PercChange,file="PercentChange/percentchange.csv",sep=",",row.names=F)

# rm(list=ls())

SiteLookup<-read.csv("Sites2.csv",sep=",",header=T)
mapData<-merge(SiteLookup,O3Standard,by="Common.Name")
mapDataMelt<-melt(mapData,id.vars = c("Common.Name","Site.ID","year"))
mapData<-cast(mapData,Common.Name+Site.ID~year)

write.table(mapData,file="MapFiles/OzoneMapData.csv",sep=",",row.names=F)

