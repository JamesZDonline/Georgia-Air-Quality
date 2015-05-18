require(plyr)
require(ggplot2)
load("LeadData.RData")
#Annual Maximum 3 month rolling average
Lead$Site.ID<-factor(Lead$Site.ID)
Lead$Sample.Duration<-factor(Lead$Sample.Duration)
Lead$Start.Time<-factor(Lead$Start.Time)

#Fix an issue with units
Lead$Sample.Value[Lead$Unit=="007"] <-Lead$Sample.Value[Lead$Unit=="007"]*1000

LeadMonthAvg<-ddply(Lead110,.(Common.Name,year,month,MetroAtlanta),summarize,monthavg=mean(Sample.Value,na.rm=T))

f3<-rep(1/3,3)
LeadStandard<-ddply(LeadMonthAvg,.(Common.Name),transform,standard=as.numeric(filter(monthavg,f3,sides=1)))


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
