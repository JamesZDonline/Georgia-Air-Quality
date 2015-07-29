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
NO2$Unit[NO2$Unit=="007"] <-"008"


NO2DailyMax<-ddply(NO2, .(Date,Common.Name,MetroAtlanta),.parallel=TRUE,summarize,Daily.Max=max(Sample.Value,na.rm=TRUE))
NO2DailyMax$year<-factor(substr(as.character(NO2DailyMax$Date),1,4))

NO2Standard<-ddply(NO2DailyMax,.(year,Common.Name,MetroAtlanta),summarize,standard=quantile(Daily.Max,.98))

NO2Standard$year<-as.Date(paste(NO2Standard$year,"01","01",sep="-"))
NO2<-droplevels(NO2)
# Create Plots ------------------------------------------------------------
cbbPalette<-c("#999999","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
family="Arial"
AllMyOpts<-theme(plot.title=element_text(family=family,face="bold"),
                 legend.title=element_text(family=family,face="bold"),
                 legend.text=element_text(family=family,face="plain"),
                 axis.text=element_text(family=family,face="plain",colour="black"),
                 axis.title=(element_text(family=family,face="bold",colour="black")),
                 axis.title.y=(element_text(vjust = .75)),
                 panel.background=element_rect(fill="white"),
                 panel.grid.major=element_line(colour="grey85"))

# FullPlot<-ggplot(NO2Standard,aes(x=year,y=standard,col=Common.Name))+geom_line(lwd=1.2)+geom_point(size=2.75)+
#    ggtitle("Yearly Trend in Georgia NO2")+xlab("Year")+ylab("NO2 Concentration (ppb) Standard")+AllMyOpts+
#    scale_color_manual(values=c(cbbPalette,cbbPalette[1:7],cbbPalette[1:7]),name="Common Name")+
#    geom_abline(intercept=100,slope=0,linetype="dotdash")+
#    scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
#    stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
# plot(FullPlot)
# 
# svg("Plots/NO2FullPlot.svg",width=8, height=8)
# plot(FullPlot)
# dev.off()
# 
# 
# 
NO2Summary<-ddply(NO2Standard,.(year),summarize,average=mean(standard,na.rm=TRUE),perc10=quantile(standard,probs=.1,na.rm=TRUE),
                    perc90=quantile(standard,probs=.9,na.rm=TRUE))
# 
# p3<-qplot(as.Date(paste(year,"01","01",sep="-")),average,data=NO2Summary, geom=c("line","point"),xlab="Year",
#           ylab="NO2 Concentration (ppb) Standard", main="Yearly Trend in Georgia NO2")+AllMyOpts+
#           geom_abline(intercept=100,slope=0,linetype="dotdash")+
#           scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
#           geom_smooth(aes(ymin=perc10,ymax=perc90),data=NO2Summary,stat="identity",fill="orange")
# plot(p3)
# 
# svg("Plots/NO2Smooth.svg",width=8, height=8)
# plot(p3)
# dev.off()


# Calculate Pecent Change -------------------------------------------------


startYear=min(NO2Standard$year)
endYear=max(NO2Standard$year)

NO2StandMelt<-melt(NO2Standard,id.vars = c("year","Common.Name","MetroAtlanta"))
NO2StandCast<-cast(NO2StandMelt,year+Common.Name~MetroAtlanta)
names(NO2StandCast)<-c("year","Common.Name","Metro")
NO2StandAvg<-ddply(NO2StandCast,.(year),summarize,Metroaverage=mean(Metro,na.rm=T))
NO2StandAvg$State<-NA
NO2StandAvg$Full<-NO2Summary$average

NO2PercChange<-data.frame((NO2StandAvg[which(NO2StandAvg$year==startYear),2:4]-NO2StandAvg[NO2StandAvg$year==endYear,2:4])/NO2StandAvg[NO2StandAvg$year==startYear,2:4])

NO2PercChange$Pollutant<-"NO2"
NO2PercChange$startYear=startYear
NO2PercChange$endYear=endYear


xlabel<-paste("Year 2005-2014: ",as.character(100*round(NO2PercChange$Full,digits = 2)),"% decrease",sep="")

SmoothPlot<-ggplot(NO2Summary,aes(x=as.Date(paste(year,"01","01",sep="-")),y=average))+geom_line(lwd=1.2)+geom_point(size=2.75)+AllMyOpts+
   xlab(xlabel)+ylab("Concentration (ppm)")+ggtitle("State of Georgia Annual Trend: NO2 ")+
   geom_abline(intercept=100,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
   scale_x_date(breaks=date_breaks(width="1 year"),labels=date_format("%Y"))+
   geom_smooth(aes(ymin=perc10,ymax=perc90),data=NO2Summary,stat="identity",fill="orange",colour="black")+
   annotate("text",x=as.Date("2013-01-01"),y=90,label=paste(as.character(length(levels(NO2$Site.ID))),"sites"),family=family,size=3)+
   annotate("text",x=as.Date("2007-01-15"),y=98,label="Current National Standard",family=family,size=3)

plot(SmoothPlot)

tiff("Plots/NO2Smooth.tiff",width=5.5, height=4,unit="in",res=300,family=family,pointsize=9)
plot(SmoothPlot)
dev.off()

# 
# write.table(NO2PercChange,file="PercentChange/percentchange.csv",sep=",",append=T,row.names=F,col.names=F)
# 
# rm(list=ls())

