# Load Libraries and Data -------------------------------------------------
require(ggplot2)
require(plyr)
load("NO2.RData")


# Do some data cleaning ---------------------------------------------------

#Annual 98th percentile of daily max 1-hour average 
NO2$Site.ID<-factor(NO2$Site.ID)
NO2$Sample.Duration<-factor(NO2$Sample.Duration)
NO2$Start.Time<-factor(NO2$Start.Time)

#Remove Roadside site (Name=="")
NO2<-NO2[-which(NO2$Common.Name==""),]

#Fix an issue with units
NO2$Sample.Value[NO2$Unit=="007"] <-NO2$Sample.Value[NO2$Unit=="007"]*1000

NO2DailyMax<-ddply(NO2, .(Date,Common.Name,MetroAtlanta),.parallel=TRUE,summarize,Daily.Max=max(Sample.Value,na.rm=TRUE))
NO2DailyMax$year<-factor(substr(as.character(NO2DailyMax$Date),1,4))

NO2Standard<-ddply(NO2DailyMax,.(year,Common.Name,MetroAtlanta),summarize,standard=quantile(Daily.Max,.98))


# Create Plots ------------------------------------------------------------
FullPlot<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=NO2Standard, color=Common.Name, geom=c("line","point"),xlab="Year",
         ylab="NO2 Concentration (ppb) Standard", main="Yearly Trend in Georgia NO2")+
   geom_abline(intercept=100,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
plot(FullPlot)

svg("Plots/NO2FullPlot.svg",width=8, height=8)
plot(FullPlot)
dev.off()



NO2Summary<-ddply(NO2Standard,.(year),summarize,average=mean(standard,na.rm=TRUE),perc10=quantile(standard,probs=.1,na.rm=TRUE),
                    perc90=quantile(standard,probs=.9,na.rm=TRUE))

p1<-qplot(as.Date(paste(year,"01","01",sep="-")),average,data=NO2Summary, geom=c("line","point"),xlab="Year",
          ylab="NO2 Concentration (ppb) Standard", main="Yearly Trend in Georgia NO2")
plot(p1)

p2<-p1+geom_abline(intercept=100,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))
plot(p2)

p3<-p2+geom_smooth(aes(ymin=perc10,ymax=perc90),data=NO2Summary,stat="identity",fill="orange")
plot(p3)

svg("Plots/NO2Smooth.svg",width=8, height=8)
plot(p3)
dev.off()

NO2Standard$year<-as.numeric(as.character(NO2Standard$year))

percentChange<-ddply(NO2Standard,.(Common.Name),summarize,percentChange=(standard[year==max(year)]-standard[year==min(year)])/standard[year==max(year)],StartYear=min(year),EndYear=max(year)+1)
write.table(percentChange,file="PercentChange/percentChangeNO2.csv",sep=",",row.names=F)

rm(list=ls())

