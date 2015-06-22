# Load Packages and Data --------------------------------------------------

require(plyr)
require(ggplot2)
load("LeadData.RData")


# Data Cleaning -----------------------------------------------------------

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


# Create Standard ---------------------------------------------------------
LeadMonthAvg<-ddply(Lead110,.(Common.Name,year,month,MetroAtlanta),summarize,monthavg=mean(Sample.Value,na.rm=T))

f3<-rep(1/3,3)
LeadStandard<-ddply(LeadMonthAvg,.(Common.Name),transform,standard=as.numeric(filter(monthavg,f3,sides=1)))
LeadSummary<-ddply(LeadStandard,.(year,month),summarize,average=mean(standard,na.rm=TRUE),perc10=quantile(standard,probs=.1,na.rm=TRUE),
                    perc90=quantile(standard,probs=.9,na.rm=TRUE))


LeadStandard<-LeadStandard[complete.cases(LeadStandard),]


# Plots -------------------------------------------------------------------

FullPlot<-qplot(as.Date(paste(year,month,"01",sep="-")),standard,data=LeadStandard, color=Common.Name, geom=c("line","point"),xlab="Year",
         ylab=bquote("Lead Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Georgia Lead")+
   geom_abline(intercept=.15,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,.2),breaks=seq(0,.2,.01))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")

plot(FullPlot)

Metro<-LeadStandard[LeadStandard$MetroAtlanta=="Metro-Atlanta",]
State<-LeadStandard[LeadStandard$MetroAtlanta=="State",]

MetroSplit<-qplot(as.Date(paste(year,month,"01",sep="-")),standard,data=Metro, color=Common.Name, geom=c("line","point"),xlab="Year",
                ylab=bquote("Lead Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Metro-Atlanta Lead")+
   geom_abline(intercept=.15,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,.2),breaks=seq(0,.2,.01))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")

StateSplit<-qplot(as.Date(paste(year,month,"01",sep="-")),standard,data=State, color=Common.Name, geom=c("line","point"),xlab="Year",
                 ylab=bquote("Lead Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Statewide Lead")+
   geom_abline(intercept=.15,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,.2),breaks=seq(0,.2,.01))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")


plot(MetroSplit)

plot(StateSplit)

svg("Plots/LeadFullPlot.svg",width=8)
plot(FullPlot)
dev.off()

svg("Plots/LeadMetroSplit.svg",width=8)
plot(MetroSplit)
dev.off()

svg("Plots/LeadStateSplit.svg",width=8)
plot(StateSplit)
dev.off()

p1<-qplot(as.Date(paste(year,month,"01",sep="-")),average,data=LeadSummary, geom=c("line","point"),xlab="Year",
         ylab=bquote("Lead Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Georgia Lead")
plot(p1)

p2<-p1+geom_abline(intercept=.15,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,.21),breaks=seq(0,.21,.01))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))
plot(p2)

p3<-p2+geom_smooth(aes(ymin=perc10,ymax=perc90),data=LeadSummary,stat="identity",fill="orange")
plot(p3)

svg("Plots/LeadSmooth.svg",width=8)
plot(p3)
dev.off()


LeadStandard$year<-as.numeric(as.character(LeadStandard$year))
LeadStandard$month<-as.numeric(as.character(LeadStandard$month))
LeadStandard$date<-as.Date(paste(LeadStandard$year,LeadStandard$month,"01",sep="/"))

percentChange<-ddply(LeadStandard,.(Common.Name),summarize,percentChange=(standard[date==max(date)]-standard[date==min(date)])/standard[date==max(date)],StartYear=min(as.numeric(format(date,"%Y"))),EndYear=max(as.numeric(format(date,"%Y"))))
write.table(percentChange,file="PercentChange/percentChangeLead.csv",sep=",",row.names=F)

#rm(list=ls())

