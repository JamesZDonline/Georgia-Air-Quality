require(ggplot2)
require(plyr)
load("SO2.RData")
SO2<-SO2[as.numeric(as.character(SO2$year))>=2005,]
#Annual 99th percentile of daily max 1-hour average 
SO2$Site.ID<-factor(SO2$Site.ID)
SO2$Sample.Duration<-factor(SO2$Sample.Duration)
SO2$Start.Time<-factor(SO2$Start.Time)

#Remove Baldwin County Airport
SO2<-SO2[which(SO2$Common.Name!="Baldwin Co Airport"),]

#The one with only one data point is Stilesboro Remove it too?
SO2<-SO2[which(!SO2$Common.Name=="Stilesboro"),]

#Fort Mountain data is all NA so that can be removed
SO2<-SO2[which(!SO2$Common.Name=="Ft Mountain"),]

#There is another site with an unknown name that is also all NA
SO2<-SO2[which(!SO2$Common.Name==""),]

#Fix an issue with units
SO2$Sample.Value[SO2$Unit=="007"] <-SO2$Sample.Value[SO2$Unit=="007"]*1000
SO2$Unit[SO2$Unit=="007"] <-"008"

SO2$Sample.Value[SO2$Unit=="001"]<-SO2$Sample.Value[SO2$Unit=="001"]/2.62
SO2$Unit[SO2$Unit=="001"] <-"008"

SO2<-droplevels(SO2)

SO2DailyMax<-ddply(SO2, .(Date,Common.Name,MetroAtlanta),.parallel=TRUE,summarize,Daily.Max=max(Sample.Value,na.rm=TRUE))
SO2DailyMax$year<-factor(substr(as.character(SO2DailyMax$Date),1,4))

SO2Standard<-ddply(SO2DailyMax,.(year,Common.Name,MetroAtlanta),summarize,standard=quantile(Daily.Max,.98))

SO2Standard<-SO2Standard[-which(is.infinite(SO2Standard$standard)),]

AverageStandard<-ddply(SO2Standard,.(year),summarize,avg=mean(standard,na.rm=T),perc10=quantile(standard,probs=.1,na.rm=TRUE),
                       perc90=quantile(standard,probs=.9,na.rm=TRUE))

SO2Standard$year<-as.Date(paste(SO2Standard$year,"01","01",sep="-"))

# Plots -------------------------------------------------------------------
cbbPalette<-c("#999999","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
family="Arial"
AllMyOpts<-theme(plot.title=element_text(family=family,face="bold"),
                 legend.title=element_text(family=family,face="bold"),
                 legend.text=element_text(family=family,face="plain"),
                 legend.position="bottom",
                 axis.text=element_text(family=family,face="plain",colour="black"),
                 axis.title=(element_text(family=family,face="bold",colour="black")),
                 axis.title.y=(element_text(vjust = .75)),
                 panel.background=element_rect(fill="white"),
                 panel.grid.major=element_line(colour="grey85"))


FullPlot<-ggplot(SO2Standard,aes(x=year,y=standard,col=Common.Name,linetype=Common.Name))+geom_line(lwd=1.2)+geom_point(size=2.75)+
   ggtitle("Yearly Trend in Georgia SO2")+xlab("Year")+ylab("SO2 Concentration (ppb) Standard")+AllMyOpts+
   scale_linetype_manual(values=c(rep("solid",8),rep("dashed",8),rep("dotted",8),rep("twodash",5)),name="Common Name")+
   scale_color_manual(values=c(cbbPalette,cbbPalette,cbbPalette,cbbPalette),name="Common Name")+
   geom_abline(intercept=75,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,120),breaks=seq(0,120,10))+
   stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")+
   guides(colour=guide_legend(nrow=legendrows),linetype=guide_legend(nrow=legendrows))
   

plot(FullPlot)

# 
# 
# Metro<-SO2Standard[SO2Standard$MetroAtlanta=="Metro-Atlanta",]
# State<-SO2Standard[SO2Standard$MetroAtlanta=="State",]
# 
# SplitMetro<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=Metro, color=Common.Name,geom=c("line","point"),xlab="Year",
#                   ylab="SO2 Concentration (ppb) Standard", main="Yearly Trend in Metro-Atlanta SO2")+
#    geom_abline(intercept=75,slope=0,linetype="dotdash")+
#    scale_y_continuous(limits=c(0,120),breaks=seq(0,120,10))+
#    theme(panel.background=element_rect(fill="white"))+
#    theme(panel.grid.major=element_line(colour="grey85"))+
#    stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
# 
# plot(SplitMetro)
# 
# SplitState<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=State, color=Common.Name, geom=c("line","point"),xlab="Year",
#                   ylab="SO2 Concentration (ppb) Standard", main="Yearly Trend in Statewide SO2")+
#    geom_abline(intercept=75,slope=0,linetype="dotdash")+
#    scale_y_continuous(limits=c(0,120),breaks=seq(0,120,10))+
#    theme(panel.background=element_rect(fill="white"))+
#    theme(panel.grid.major=element_line(colour="grey85"))+
#    stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
# 
# plot(SplitState)
# 
# svg("Plots/SO2FullPlot.svg",width=8, height=8)
# plot(FullPlot)
# dev.off()
# 
# svg("Plots/SO2SplitMetro.svg",width=8, height=8)
# plot(SplitMetro)
# dev.off()
# 
# svg("Plots/SO2SplitState.svg",width=8, height=8)
# plot(SplitState)
# dev.off()
# 
# SO2Summary<-ddply(SO2Standard,.(year),summarize,average=mean(standard,na.rm=TRUE),perc10=quantile(standard,probs=.1,na.rm=TRUE),
#                   perc90=quantile(standard,probs=.9,na.rm=TRUE))
# 
# FullPlot<-qplot(as.Date(paste(year,"01","01",sep="-")),average,data=SO2Summary, geom=c("line","point"),xlab="Year",
#           ylab="SO2 Concentration (ppb) Standard", main="Yearly Trend in Georgia SO2")+
#    geom_abline(intercept=75,slope=0,linetype="dotdash")+
#    scale_y_continuous(limits=c(0,120),breaks=seq(0,120,10))+
#    theme(panel.background=element_rect(fill="white"))+
#    theme(panel.grid.major=element_line(colour="grey85"))
# 
# plot(FullPlot)
# 
# 
# 
# SmoothPlot<-FullPlot+geom_smooth(aes(ymin=perc10,ymax=perc90),data=SO2Summary,stat="identity",fill="orange")
# plot(SmoothPlot)
# 
# svg("Plots/SO2Smooth.svg",width=8, height=8)
# plot(SmoothPlot)
# dev.off()


startYear=min(SO2Standard$year)
endYear=max(SO2Standard$year)

SO2StandMelt<-melt(SO2Standard,id.vars = c("year","Common.Name","MetroAtlanta"))
SO2StandCast<-cast(SO2StandMelt,year+Common.Name~MetroAtlanta)
names(SO2StandCast)<-c("year","Common.Name","Metro","State")
SO2StandAvg<-ddply(SO2StandCast,.(year),summarize,Metroaverage=mean(Metro,na.rm=T),Stateaverage=mean(State,na.rm=T))
SO2StandAvg$Full<-AverageStandard$avg
SO2PercChange<-data.frame((SO2StandAvg[which(SO2StandAvg$year==startYear),2:4]-SO2StandAvg[SO2StandAvg$year==endYear,2:4])/SO2StandAvg[SO2StandAvg$year==startYear,2:4])
SO2PercChange$Pollutant<-"SO2"
SO2PercChange$startYear=startYear
SO2PercChange$endYear=endYear

xlabel<-paste("Year 2005-2014: ",as.character(100*round(SO2PercChange$Full,digits = 2)),"% decrease",sep="")

SmoothPlot<-ggplot(AverageStandard,aes(x=as.Date(paste(year,"01","01",sep="-")),y=avg))+geom_line(lwd=1.2)+geom_point(size=2.75)+AllMyOpts+
   xlab(xlabel)+ylab("Concentration (ppm)")+ggtitle("State of Georgia Annual Trend: SO2")+
   geom_abline(intercept=75,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,120),breaks=seq(0,120,10))+
   scale_x_date(breaks=date_breaks(width="1 year"),labels=date_format("%Y"))+
   geom_smooth(aes(ymin=perc10,ymax=perc90),data=AverageStandard,stat="identity",fill="orange",colour="black")+
   annotate("text",x=as.Date("2013-01-01"),y=115,label=paste(as.character(length(levels(factor(SO2Standard$Common.Name)))),"sites"),family=family,size=3)+
   annotate("text",x=as.Date("2007-01-15"),y=77,label="Current National Standard",family=family,size=3)

plot(SmoothPlot)

tiff("Plots/SO2Smooth.tiff",width=5.5, height=4,units="in",res=300,family=family,pointsize=9)
plot(SmoothPlot)
dev.off()


write.table(SO2PercChange,file="PercentChange/percentchange.csv",sep=",",append=T,row.names=F,col.names=F)
# 
# rm(list=ls())


