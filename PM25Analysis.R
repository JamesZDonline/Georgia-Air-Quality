require(plyr)
load("PM25.RData")
#98th percentile of 1-hour daily maximum concentrations, averaged over 3 years
PM2.5$Site.ID<-factor(PM2.5$Site.ID)
PM2.5$Sample.Duration<-factor(PM2.5$Sample.Duration)
PM2.5$Start.Time<-factor(PM2.5$Start.Time)

#Fix an issue with units
PM2.5$Sample.Value[PM2.5$Unit=="007"] <-PM2.5$Sample.Value[PM2.5$Unit=="007"]*1000

PM2.5DailyMax<-ddply(PM2.5, .(Date,Common.Name,MetroAtlanta),.parallel=TRUE,summarize,Daily.Max=max(Sample.Value,na.rm=TRUE))
PM2.5DailyMax$year<-factor(substr(as.character(PM2.5DailyMax$Date),1,4))

PM2.5Yearly<-ddply(PM2.5DailyMax,.(year,Common.Name,MetroAtlanta),summarize,Yearly=quantile(Daily.Max,.98))

f3<-rep(1/3,3)

PM2.5Yearly<-PM2.5Yearly[-which(PM2.5Yearly$Common.Name=="University of Georgia"),]

PM2.5Standard<-ddply(PM2.5Yearly,.(Common.Name),transform,standard=as.numeric(filter(Yearly,f3,sides=1)))

PM2.5Standard<-PM2.5Standard[complete.cases(PM2.5Standard),]


s<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=PM2.5Standard, color=Common.Name, geom=c("line","point"),xlab="Year",
         ylab=bquote("PM2.5 Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Georgia PM2.5")
plot(s)
s2<-s+geom_abline(intercept=35,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,70),breaks=seq(0,70,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))

plot(s2)

s3<-s2+stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
plot(s3)

s4<-s3+facet_grid(MetroAtlanta~.)
plot(s4)

jpeg("PM25FullPlot.jpg")
plot(s3)
dev.off()

jpeg("PM25SplitPlot.jpg")
plot(s4)
dev.off()


PM2.5Summary<-ddply(PM2.5Standard,.(year),summarize,average=mean(standard,na.rm=TRUE),perc10=quantile(standard,probs=.1,na.rm=TRUE),
                    perc90=quantile(standard,probs=.9,na.rm=TRUE))

p1<-qplot(as.Date(paste(year,"01","01",sep="-")),average,data=PM2.5Summary, geom=c("line","point"),xlab="Year",
          ylab=bquote("PM2.5 Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Georgia PM2.5")
plot(p1)

p2<-p1+geom_abline(intercept=35,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,70),breaks=seq(0,70,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))
plot(p2)

p3<-p2+geom_smooth(aes(ymin=perc10,ymax=perc90),data=PM2.5Summary,stat="identity")
plot(p3)

jpeg("PM25Smooth.jpg")
plot(p3)
dev.off()

rm(list=ls())
