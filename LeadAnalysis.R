# Load Packages and Data --------------------------------------------------
require(reshape)
require(plyr)
require(ggplot2)
load("LeadData.RData")


# Data Cleaning -----------------------------------------------------------


#Annual Maximum 3 month rolling average
Lead$Site.ID<-factor(Lead$Site.ID)
Lead$Sample.Duration<-factor(Lead$Sample.Duration)
Lead$Start.Time<-factor(Lead$Start.Time)

#Fix an issue with units

 
#Lead Method code 110 started in 2009

#Lead Method code 089 was in 2009

#Lead Method code 092 was from 2004 to 2009
Lead092<-Lead[Lead$Method=="092", c("Common.Name", "Date", "MetroAtlanta", "Sample.Value")]
names(Lead092)<-c("Common.Name", "Date", "MetroAtlanta", "monthavg")
Lead092$Date<-as.character(Lead092$Date)
Lead092$year<-substr(Lead092$Date,1,4)
Lead092$month<-substr(Lead092$Date,5,nchar(Lead092$Date)-2)
Lead092$month[nchar(Lead092$month)==1]<-paste("0",Lead092$month[nchar(Lead092$month)==1],sep="")
Lead092$Date<-NULL

Lead110<-Lead[Lead$Method=="110",]


# Create Standard ---------------------------------------------------------
LeadMonthAvg<-ddply(Lead110,.(Common.Name,year,month,MetroAtlanta),summarize,monthavg=mean(Sample.Value,na.rm=T))
LeadMonthAvg<-rbind(LeadMonthAvg,Lead092)
LeadMonthAvg<-droplevels(LeadMonthAvg)
f3<-rep(1/3,3)
LeadStandard<-ddply(LeadMonthAvg,.(Common.Name),transform,standard=as.numeric(filter(monthavg,f3,sides=1)))
LeadSummary<-ddply(LeadStandard,.(year,month),summarize,average=mean(standard,na.rm=TRUE),perc10=quantile(standard,probs=.1,na.rm=TRUE),
                    perc90=quantile(standard,probs=.9,na.rm=TRUE))


LeadStandard<-LeadStandard[complete.cases(LeadStandard),]


# Plots -------------------------------------------------------------------


cbbPalette<-c("#999999","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
family="Arial"
legendrows=4
AllMyOpts<-theme(plot.title=element_text(family=family,face="bold"),
                 legend.title=element_text(family=family,face="bold"),
                 legend.text=element_text(family=family,face="plain"),
                 axis.text=element_text(family=family,face="plain",colour="black"),
                 axis.title=(element_text(family=family,face="bold",colour="black")),
                 axis.title.y=(element_text(vjust = .75)),
                 legend.position="bottom",
                 panel.background=element_rect(fill="white"),
                 panel.grid.major=element_line(colour="grey85"))

# FullPlot<-qplot(as.Date(paste(year,month,"01",sep="-")),standard,data=LeadStandard, color=Common.Name, geom=c("line","point"),xlab="Year",
#          ylab=bquote("Lead Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Georgia Lead")+
#    geom_abline(intercept=.15,slope=0,linetype="dotdash")+
#    scale_y_continuous(limits=c(0,.24),breaks=seq(0,.24,.01))+
#    theme(panel.background=element_rect(fill="white"))+
#    theme(panel.grid.major=element_line(colour="grey85"))+
#    stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
# 
# plot(FullPlot)
# 
# Metro<-LeadStandard[LeadStandard$MetroAtlanta=="Metro-Atlanta",]
# State<-LeadStandard[LeadStandard$MetroAtlanta=="State",]
# 
# MetroSplit<-qplot(as.Date(paste(year,month,"01",sep="-")),standard,data=Metro, color=Common.Name, geom=c("line","point"),xlab="Year",
#                 ylab=bquote("Lead Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Metro-Atlanta Lead")+
#    geom_abline(intercept=.15,slope=0,linetype="dotdash")+
#    scale_y_continuous(limits=c(0,.24),breaks=seq(0,.24,.01))+
#    theme(panel.background=element_rect(fill="white"))+
#    theme(panel.grid.major=element_line(colour="grey85"))+
#    stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
# 
# StateSplit<-qplot(as.Date(paste(year,month,"01",sep="-")),standard,data=State, color=Common.Name, geom=c("line","point"),xlab="Year",
#                  ylab=bquote("Lead Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Statewide Lead")+
#    geom_abline(intercept=.15,slope=0,linetype="dotdash")+
#    scale_y_continuous(limits=c(0,.24),breaks=seq(0,.24,.01))+
#    theme(panel.background=element_rect(fill="white"))+
#    theme(panel.grid.major=element_line(colour="grey85"))+
#    stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
# 
# 
# plot(MetroSplit)
# 
# plot(StateSplit)
# 
# svg("Plots/LeadFullPlot.svg",width=8)
# plot(FullPlot)
# dev.off()
# 
# svg("Plots/LeadMetroSplit.svg",width=8)
# plot(MetroSplit)
# dev.off()
# 
# svg("Plots/LeadStateSplit.svg",width=8)
# plot(StateSplit)
# dev.off()
# 
# p1<-qplot(as.Date(paste(year,month,"01",sep="-")),average,data=LeadSummary, geom=c("line","point"),xlab="Year",
#          ylab=bquote("Lead Concentration (μg/"~m^3~") Standard"), main="Yearly Trend in Georgia Lead")
# plot(p1)
# 
# p2<-p1+geom_abline(intercept=.15,slope=0,linetype="dotdash")+
#    scale_y_continuous(limits=c(0,.24),breaks=seq(0,.24,.01))+
#    theme(panel.background=element_rect(fill="white"))+
#    theme(panel.grid.major=element_line(colour="grey85"))
# plot(p2)
# 
# p3<-p2+geom_smooth(aes(ymin=perc10,ymax=perc90),data=LeadSummary,stat="identity",fill="orange")
# plot(p3)
# 
# svg("Plots/LeadSmooth.svg",width=8)
# plot(p3)
# dev.off()


 LeadStandard$year<-as.numeric(as.character(LeadStandard$year))
# LeadStandard$month<-as.numeric(as.character(LeadStandard$month))
# LeadStandard$date<-as.Date(paste(LeadStandard$year,LeadStandard$month,"01",sep="/"))

LeadSummary$year<-as.numeric(as.character(LeadSummary$year))
LeadSummary$month<-as.numeric(as.character(LeadSummary$month))
LeadSummary$date<-as.Date(paste(LeadSummary$year,LeadSummary$month,"01",sep="/"))



# Percent Change Calculation ----------------------------------------------
LeadAverage<-ddply(LeadStandard,.(year),summarize,avg=mean(standard,na.rm=T))
AverageLeadStandard<-ddply(LeadStandard,.(year,MetroAtlanta,Common.Name),summarize,avg=mean(standard,na.rm=T))

# LeadStandard$year<-NULL
startyear=min(LeadStandard$year)
endyear=max(LeadStandard$year)

LeadStandMelt<-melt(AverageLeadStandard,id.vars = c("year","Common.Name","MetroAtlanta"))
LeadStandCast<-cast(LeadStandMelt,year+Common.Name~MetroAtlanta)
names(LeadStandCast)<-c("year","Common.Name","Metro","State")
LeadStandAvg<-ddply(LeadStandCast,.(year),summarize,Metroaverage=mean(Metro,na.rm=T),Stateaverage=mean(State,na.rm=T))
LeadStandAvg$Full<-LeadAverage$avg
LeadPercChange<-data.frame((LeadStandAvg[which(LeadStandAvg$year==startyear),2:4]-LeadStandAvg[LeadStandAvg$year==endyear,2:4])/LeadStandAvg[LeadStandAvg$year==startyear,2:4])
LeadPercChange$Pollutant<-"Lead"
LeadPercChange$startYear=startyear
LeadPercChange$endYear=endyear

xlabel<-paste("Year 2005-2014: ",as.character(100*round(LeadPercChange$Full,digits = 2)),"% decrease",sep="")

SmoothPlot<-ggplot(LeadSummary,aes(x=as.Date(paste(year,month,"01",sep="-")),y=average))+geom_line(lwd=1.2)+geom_point(size=2.75)+AllMyOpts+
   xlab(xlabel)+ylab(expression(bold("Concentration (μg/"~m^3~")")))+ggtitle("State of Georgia Annual Trend: Lead ")+
   scale_y_continuous(limits=c(0,.24),breaks=seq(0,.24,.01))+
   geom_abline(intercept=.15,slope=0,linetype="dotdash")+
   scale_x_date(breaks=date_breaks(width="1 year"),labels=date_format("%Y"))+
   geom_smooth(aes(ymin=perc10,ymax=perc90),data=LeadSummary,stat="identity",fill="orange",colour="black")+
   annotate("text",x=as.Date("2013-01-01"),y=.235,label=paste(as.character(length(levels(Lead$Site.ID))),"sites"),family=family,size=3)+
   annotate("text",x=as.Date("2007-01-15"),y=.155,label="Current National Standard",family=family,size=3)
   

plot(SmoothPlot)

tiff("Plots/LeadSmooth.tiff",width=5.5, height=4, units="in",res=300,family=family,pointsize=9)
plot(SmoothPlot)
dev.off()



write.table(LeadPercChange,file="PercentChange/percentchange.csv",sep=",",append=T,row.names=F,col.names=F)

#rm(list=ls())

