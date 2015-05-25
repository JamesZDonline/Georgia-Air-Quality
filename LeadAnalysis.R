require(plyr)
require(ggplot2)
load("LeadData.RData")
#Annual Maximum 3 month rolling average
Lead$Site.ID<-factor(Lead$Site.ID)
Lead$Sample.Duration<-factor(Lead$Sample.Duration)
Lead$Start.Time<-factor(Lead$Start.Time)

#Fix an issue with units
Lead$Sample.Value[Lead$Unit=="007"] <-Lead$Sample.Value[Lead$Unit=="007"]*1000

 
#Lead Method code 110 started in 2009

#Lead Method code 089 was in 2009

#Lead Method code 092 was from 2004 to 2009
Lead092<-Lead[Lead$Method=="092",]
Lead110<-Lead[Lead$Method=="110",]
LeadMonthAvg<-ddply(Lead110,.(Common.Name,year,month,MetroAtlanta),summarize,monthavg=mean(Sample.Value,na.rm=T))

f3<-rep(1/3,3)
LeadStandard<-ddply(LeadMonthAvg,.(Common.Name),transform,standard=as.numeric(filter(monthavg,f3,sides=1)))
LeadSummary<-ddply(LeadStandard,.(year,month),summarize,average=mean(standard,na.rm=TRUE),perc10=quantile(standard,probs=.1,na.rm=TRUE),
                    perc90=quantile(standard,probs=.9,na.rm=TRUE))


LeadStandard<-LeadStandard[complete.cases(LeadStandard),]

s<-qplot(as.Date(paste(year,month,"01",sep="-")),standard,data=LeadStandard, color=Common.Name, geom=c("line","point"),xlab="Year",
         ylab=bquote("Lead Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Georgia Lead")
plot(s)
s2<-s+geom_abline(intercept=.15,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,.2),breaks=seq(0,.2,.01))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))
plot(s2)

s3<-s2+stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
plot(s3)

s4<-s3+facet_grid(MetroAtlanta~.)
plot(s4)

jpeg("LeadFullPlot.jpg")
plot(s3)
dev.off()

jpeg("LeadSplitPlot.jpg")
plot(s4)
dev.off()

p1<-qplot(as.Date(paste(year,month,"01",sep="-")),average,data=LeadSummary, geom=c("line","point"),xlab="Year",
         ylab=bquote("Lead Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Georgia Lead")
plot(p1)

p2<-p1+geom_abline(intercept=.15,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,.21),breaks=seq(0,.21,.01))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))
plot(p2)

p3<-p2+geom_smooth(aes(ymin=perc10,ymax=perc90),data=LeadSummary,stat="identity")
plot(p3)

jpeg("LeadSmooth.jpg")
plot(p3)
dev.off()

rm(list=ls())

