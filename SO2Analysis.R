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

s<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=SO2Standard, color=Common.Name, geom=c("line","point"),xlab="Year",
         ylab="SO2 Concentration (ppb) Standard", main="Yearly Trend in Georgia SO2")
plot(s)
s2<-s+geom_abline(intercept=75,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))
#+   theme(panel.grid.minor=element_line(colour="gray70"))
plot(s2)

s3<-s2+stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
plot(s3)

s4<-s3+facet_grid(MetroAtlanta~.)
plot(s4)

jpeg("SO2FullPlot.jpg")
plot(s3)
dev.off()

jpeg("SO2SplitPlot.jpg")
plot(s4)
dev.off()

SO2Summary<-ddply(SO2Standard,.(year),summarize,average=mean(standard,na.rm=TRUE),perc10=quantile(standard,probs=.1,na.rm=TRUE),
                  perc90=quantile(standard,probs=.9,na.rm=TRUE))

p1<-qplot(as.Date(paste(year,"01","01",sep="-")),average,data=SO2Summary, geom=c("line","point"),xlab="Year",
          ylab="SO2 Concentration (ppb) Standard", main="Yearly Trend in Georgia SO2")
plot(p1)

p2<-p1+geom_abline(intercept=75,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))
plot(p2)

p3<-p2+geom_smooth(aes(ymin=perc10,ymax=perc90),data=SO2Summary,stat="identity")
plot(p3)

jpeg("SO2Smooth.jpg")
plot(p3)
dev.off()

SO2Standard$year<-as.numeric(as.character(SO2Standard$year))

percentChange<-ddply(SO2Standard,.(Common.Name),summarize,percentChange=(standard[year==max(year)]-standard[year==min(year)])/standard[year==max(year)],numYears=max(year)-min(year)+1)
write.table(percentChange,file="percentChangeSO2.csv",sep=";")
# 
# rm(list=ls())