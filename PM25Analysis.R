# Load packages and Data --------------------------------------------------
require(plyr)
require(ggplot2)
require(reshape)
load("PM25.RData")


# Clean The Data ----------------------------------------------------------

#98th percentile of average daily maximum concentrations, averaged over 3 years
PM2.5$Site.ID<-factor(PM2.5$Site.ID)
PM2.5$Sample.Duration<-factor(PM2.5$Sample.Duration)
PM2.5$Start.Time<-factor(PM2.5$Start.Time)

#Fix an issue with units
PM2.5$Sample.Value[PM2.5$Unit=="007"] <-PM2.5$Sample.Value[PM2.5$Unit=="007"]*1000
PM2.5$Unit[PM2.5$Unit=="007"] <-"008"


#Merge Coosa Elementary and High 
PM2.5$Common.Name[PM2.5$Common.Name=="Coosa High school "]<-"Coosa Elementary"

PM2.5<-droplevels(PM2.5)
# Calculate Standards Measurements ----------------------------------------

PM2.5DailyMean<-ddply(PM2.5, .(Date,Common.Name,MetroAtlanta),.parallel=TRUE,summarize,Daily.Mean=mean(Sample.Value,na.rm=TRUE))
PM2.5DailyMean$year<-factor(substr(as.character(PM2.5DailyMean$Date),1,4))

PM2.5Yearly<-ddply(PM2.5DailyMean,.(year,Common.Name,MetroAtlanta),summarize,Yearly=quantile(Daily.Mean,.98,na.rm=T))

f3<-rep(1/3,3)

PM2.5Yearly<-PM2.5Yearly[-which(PM2.5Yearly$Common.Name=="University of Georgia"),]

PM2.5Standard<-ddply(PM2.5Yearly,.(Common.Name),transform,standard=as.numeric(filter(Yearly,f3,sides=1)))

PM2.5Standard<-PM2.5Standard[complete.cases(PM2.5Standard),]

PM2.5Standard$year<-as.Date(paste(PM2.5Standard$year,"01","01",sep="-"))

# Plot Data ---------------------------------------------------------------
cbbPalette<-c("#999999","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
family="Arial"
legendrows=4
AllMyOpts<-theme(plot.title=element_text(family=family,face="bold"),
                 legend.title=element_text(family=family,face="bold"),
                 legend.text=element_text(family=family,face="plain"),
                 axis.text=element_text(family=family,face="plain",colour="black"),
                 axis.title=(element_text(family=family,face="bold",colour="black")),
                 legend.position="bottom",
                 axis.title.y=(element_text(vjust = .75,face="bold")),
                 panel.background=element_rect(fill="white"),
                 panel.grid.major=element_line(colour="grey85"))

# FullPlot<-ggplot(PM2.5Standard,aes(x=year,y=standard,col=Common.Name,linetype=Common.Name))+geom_line(lwd=1.2)+geom_point(size=2.75)+
#    ggtitle("Yearly Trend in Georgia PM2.5")+xlab("Year")+ylab(bquote("PM2.5 Concentration (μg/"~m^3~") Standard"))+AllMyOpts+
#    scale_linetype_manual(values=c(rep("solid",8),rep("dashed",8),rep("dotted",8),rep("twodash",5)),name="Common Name")+
#    scale_color_manual(values=c(cbbPalette,cbbPalette,cbbPalette,cbbPalette),name="Common Name")+
#    geom_abline(intercept=35,slope=0,linetype="dotdash")+
#    scale_y_continuous(limits=c(0,50),breaks=seq(0,50,10))+
#    stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")+
#    guides(colour=guide_legend(nrow=legendrows),linetype=guide_legend(nrow=legendrows))
# 
# plot(FullPlot)
# 
# 
# 
# Metro<-PM2.5Standard[PM2.5Standard$MetroAtlanta=="Metro-Atlanta",]
# State<-PM2.5Standard[PM2.5Standard$MetroAtlanta=="State",]
# 
# SplitMetro<-ggplot(Metro,aes(x=year,y=standard,col=Common.Name,linetype=Common.Name))+geom_line(lwd=1.2)+geom_point(size=2.75)+
#    ggtitle("Yearly Trend in Metro-Atlanta PM2.5")+xlab("Year")+ylab(bquote("PM2.5 Concentration (μg/"~m^3~") Standard"))+AllMyOpts+
#    scale_linetype_manual(values=c(rep("solid",8),rep("dashed",8),rep("dotted",8),rep("twodash",5)),name="Common Name")+
#    scale_color_manual(values=c(cbbPalette,cbbPalette,cbbPalette,cbbPalette),name="Common Name")+
#    geom_abline(intercept=35,slope=0,linetype="dotdash")+
#    scale_y_continuous(limits=c(0,50),breaks=seq(0,50,10))+
#    stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
#    
# 
# plot(SplitMetro)
# 
# SplitState<-ggplot(State,aes(x=year,y=standard,col=Common.Name,linetype=Common.Name))+geom_line(lwd=1.2)+geom_point(size=2.75)+
#    ggtitle("Yearly Trend in Statewide PM2.5")+xlab("Year")+ylab(bquote("PM2.5 Concentration (μg/"~m^3~") Standard"))+AllMyOpts+
#    scale_linetype_manual(values=c(rep("solid",8),rep("dashed",8),rep("dotted",8),rep("twodash",5)),name="Common Name")+
#    scale_color_manual(values=c(cbbPalette,cbbPalette,cbbPalette,cbbPalette),name="Common Name")+
#    geom_abline(intercept=35,slope=0,linetype="dotdash")+
#    scale_y_continuous(limits=c(0,50),breaks=seq(0,50,10))+
#    stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
# 
# plot(SplitState)
# 
# svg("Plots/PM25FullPlot.svg",width=8, height=8)
# plot(FullPlot)
# dev.off()
# 
# svg("Plots/PM25SplitMetro.svg",width=8, height=8)
# plot(SplitMetro)
# dev.off()
# 
# svg("Plots/PM25SplitState.svg",width=8, height=8)
# plot(SplitState)
# dev.off()

PM2.5Summary<-ddply(PM2.5Standard,.(year),summarize,average=mean(standard,na.rm=TRUE),perc10=quantile(standard,probs=.1,na.rm=TRUE),
                    perc90=quantile(standard,probs=.9,na.rm=TRUE))






# Percent Change Calculation ----------------------------------------------
PM2.5Standard$Yearly<-NULL
startYear=min(PM2.5Standard$year)
endYear=max(PM2.5Standard$year)

PM2.5StandMelt<-melt(PM2.5Standard,id.vars = c("year","Common.Name","MetroAtlanta"))
PM2.5StandCast<-cast(PM2.5StandMelt,year+Common.Name~MetroAtlanta)
names(PM2.5StandCast)<-c("year","Common.Name","Metro","State")
PM2.5StandAvg<-ddply(PM2.5StandCast,.(year),summarize,Metroaverage=mean(Metro,na.rm=T),Stateaverage=mean(State,na.rm=T))
PM2.5StandAvg$Full<-PM2.5Summary$average
PM2.5PercChange<-data.frame((PM2.5StandAvg[which(PM2.5StandAvg$year==startYear),2:4]-PM2.5StandAvg[PM2.5StandAvg$year==endYear,2:4])/PM2.5StandAvg[PM2.5StandAvg$year==startYear,2:4])
PM2.5PercChange$Pollutant<-"PM2.5"
PM2.5PercChange$startYear=startYear
PM2.5PercChange$endYear=endYear


xlabel<-paste("Year 2005-2014: ",as.character(100*round(PM2.5PercChange$Full,digits = 2)),"% decrease",sep="")

SmoothPlot<-ggplot(PM2.5Summary,aes(x=year,y=average))+geom_line(lwd=1.2)+geom_point(size=2.75)+AllMyOpts+
   xlab(xlabel)+ylab(bquote(bold("Concentration (μg/"~m^3~")")))+ggtitle(bquote(bold("State of Georgia Annual Trend:"~PM[2.5])))+
   scale_y_continuous(limits=c(0,50),breaks=seq(0,50,5))+
   geom_abline(intercept=35,slope=0,linetype="dotdash")+
   scale_x_date(breaks=date_breaks(width="1 year"),labels=date_format("%Y"))+
   geom_smooth(aes(ymin=perc10,ymax=perc90),data=PM2.5Summary,stat="identity",fill="orange",colour="black")+
   annotate("text",x=as.Date("2013-01-01"),y=48,label=paste(as.character(length(levels(PM2.5$Site.ID))),"sites"),family=family,size=3)+
   annotate("text",x=as.Date("2007-01-15"),y=36,label="Current National Standard",family=family,size=3)

plot(SmoothPlot)


tiff("Plots/PM25Smooth.tiff",width=5.5, height=4,unit="in",res=300,family=family, pointsize=9)
plot(SmoothPlot)
dev.off()


#write.table(largestPercent,file="PercentChange/percentchange.csv",sep=",",append=T,row.names=F,col.names=F)



# 
# SiteLookup<-read.csv("Sites2.csv",sep=",",header=T)
# mapData<-merge(SiteLookup,PM2.5Standard,by="Common.Name")
# mapDataMelt<-melt(mapData,id.vars = c("Common.Name","Site.ID","year"))
# mapData<-cast(mapData,Common.Name+Site.ID~year)
# 
# write.table(mapData,file="MapFiles/PM25MapData.csv",sep=",",row.names=F)
