require(ggplot2)
require(plyr)
load("SO2.RData")
#Annual 99th percentile of daily max 1-hour average 
SO2$Site.ID<-factor(SO2$Site.ID)
SO2$Sample.Duration<-factor(SO2$Sample.Duration)
SO2$Start.Time<-factor(SO2$Start.Time)

#Remove Baldwin County Airport
SO2<-SO2[-which(SO2$Common.Name=="Baldwin Co Airport"),]

#The one with only one data point is Stilesboro Remove it too?
SO2<-SO2[-which(SO2$Common.Name=="Stilesboro"),]

#Fix an issue with units
SO2$Sample.Value[SO2$Unit=="007"] <-SO2$Sample.Value[SO2$Unit=="007"]*1000

SO2DailyMax<-ddply(SO2, .(Date,Common.Name,MetroAtlanta),.parallel=TRUE,summarize,Daily.Max=max(Sample.Value,na.rm=TRUE))
SO2DailyMax$year<-factor(substr(as.character(SO2DailyMax$Date),1,4))

SO2Standard<-ddply(SO2DailyMax,.(year,Common.Name,MetroAtlanta),summarize,standard=quantile(Daily.Max,.98))

SO2Standard<-SO2Standard[-which(is.infinite(SO2Standard$standard)),]

FullPlot<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=SO2Standard, color=Common.Name, geom=c("line","point"),xlab="Year",
         ylab="SO2 Concentration (ppb) Standard", main="Yearly Trend in Georgia SO2")+
   geom_abline(intercept=75,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
plot(FullPlot)



Metro<-SO2Standard[SO2Standard$MetroAtlanta=="Metro-Atlanta",]
State<-SO2Standard[SO2Standard$MetroAtlanta=="State",]

SplitMetro<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=Metro, color=Common.Name,geom=c("line","point"),xlab="Year",
                  ylab="SO2 Concentration (ppb) Standard", main="Yearly Trend in Metro-Atlanta SO2")+
   geom_abline(intercept=75,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")

plot(SplitMetro)

SplitState<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=State, color=Common.Name, geom=c("line","point"),xlab="Year",
                  ylab="SO2 Concentration (ppb) Standard", main="Yearly Trend in Statewide SO2")+
   geom_abline(intercept=75,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")

plot(SplitState)

svg("Plots/SO2FullPlot.svg",width=8, height=8)
plot(FullPlot)
dev.off()

svg("Plots/SO2SplitMetro.svg",width=8, height=8)
plot(SplitMetro)
dev.off()

svg("Plots/SO2SplitState.svg",width=8, height=8)
plot(SplitState)
dev.off()

SO2Summary<-ddply(SO2Standard,.(year),summarize,average=mean(standard,na.rm=TRUE),perc10=quantile(standard,probs=.1,na.rm=TRUE),
                  perc90=quantile(standard,probs=.9,na.rm=TRUE))

FullPlot<-qplot(as.Date(paste(year,"01","01",sep="-")),average,data=SO2Summary, geom=c("line","point"),xlab="Year",
          ylab="SO2 Concentration (ppb) Standard", main="Yearly Trend in Georgia SO2")+
   geom_abline(intercept=75,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))

plot(FullPlot)



SmoothPlot<-FullPlot+geom_smooth(aes(ymin=perc10,ymax=perc90),data=SO2Summary,stat="identity",fill="orange")
plot(SmoothPlot)

svg("Plots/SO2Smooth.svg",width=8, height=8)
plot(SmoothPlot)
dev.off()

SO2Standard$year<-as.numeric(as.character(SO2Standard$year))

percentChange<-ddply(SO2Standard,.(Common.Name),summarize,percentChange=(standard[year==max(year)]-standard[year==min(year)])/standard[year==max(year)],StartYear=min(year),EndYear=max(year)+1)
write.table(percentChange,file="PercentChange/percentChangeSO2.csv",sep=",",row.names=F)
# 
# rm(list=ls())
