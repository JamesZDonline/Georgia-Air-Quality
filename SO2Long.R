require(plyr)
require(ggplot2)
load("SO2.RData")

#Annual 99th percentile of daily max 1-hour average 
SO2$Site.ID<-factor(SO2$Site.ID)
SO2$Sample.Duration<-factor(SO2$Sample.Duration)
SO2$Start.Time<-factor(SO2$Start.Time)

#Remove missing Sample Values
SO2<-SO2[-which(is.na(SO2$Sample.Value)),]

#Remove Baldwin County Airport
SO2<-SO2[which(SO2$Common.Name!="Baldwin Co Airport"),]

#The one with only one data point is Stilesboro Remove it too?
SO2<-SO2[which(SO2$Common.Name!="Stilesboro"),]

#Fort Mountain has no Measurements (All NA) so remove it too
SO2<-SO2[SO2$Common.Name!="Ft Mountain",]

#There is also a site in Charleton County with no measurements So I can remove that too. I don't have its name
SO2<-SO2[SO2$Common.Name!="",]
# 
# #Remove Coosa
# SO2<-SO2[SO2$Common.Name!="Coosa Elementary",]
# 
# # 
# # #Remove South DeKalb
# SO2<-SO2[SO2$Common.Name!="South DeKalb",]

#Fix an issue with units
SO2$Sample.Value[SO2$Unit=="007"] <-SO2$Sample.Value[SO2$Unit=="007"]*1000
SO2$Unit[SO2$Unit=="007"] <-"008"

SO2$Sample.Value[SO2$Unit=="001"]<-SO2$Sample.Value[SO2$Unit=="001"]/2.62
SO2$Unit[SO2$Unit=="001"] <-"008"

#Extract Metro Atlanta locations
SO2Atlanta<-SO2[SO2$MetroAtlanta=="Metro-Atlanta",]

#Extract Savannah Locations
SO2Savannah<-SO2[SO2$CountyName=="Chatham County",]

SO2AS<-rbind(SO2Atlanta,SO2Savannah)

SO2DailyMax<-ddply(SO2AS, .(Date,Common.Name,MetroAtlanta),.parallel=TRUE,summarize,Daily.Max=max(Sample.Value,na.rm=TRUE))

SO2DailyMax$year<-factor(substr(as.character(SO2DailyMax$Date),1,4))

SO2Standard<-ddply(SO2DailyMax,.(year,Common.Name,MetroAtlanta),summarize,standard=quantile(Daily.Max,.98))
AverageStandard<-ddply(SO2Standard,.(year),summarize,avg=mean(standard,na.rm=T))

SO2Standard<-SO2Standard[!is.infinite(SO2Standard$standard),]

# Plots -------------------------------------------------------------------
cbbPalette<-c("#999999","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
family="Ariel"
legendcols=3
yaxisLimits=c(.05,.14)
AllMyOpts<-theme(plot.title=element_text(family=family,face="bold"),
                 legend.title=element_text(family=family,face="bold"),
                 legend.text=element_text(family=family,face="plain"),
                 axis.text=element_text(family=family,face="plain",colour="black"),
                 axis.title=(element_text(family=family,face="bold",colour="black")),
                 axis.title.y=(element_text(vjust = .75)),
                 legend.position="bottom",
                 panel.background=element_rect(fill="white"),
                 panel.grid.major=element_line(colour="grey85"))


FullPlot<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=SO2Standard, color=Common.Name,linetype=MetroAtlanta, geom=c("line","point"),xlab="Year",
                ylab="Concentration (ppb)", main="Long Term Time Series (1985-2014) SO2 Trend")+AllMyOpts+
   geom_abline(intercept=75,slope=0,linetype="dotdash")+
   
   scale_color_manual(values=c(cbbPalette[c(1,2,4,6,7,8)],cbbPalette[1:7]),name="")+
   scale_linetype_manual(values = c("solid","dashed"),name="",labels=c("Metro-Atlanta","Savannah"))+
   scale_y_continuous(limits=c(0,150),breaks=seq(0,150,20))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))+
   #stat_summary(fun.y=mean,color="black",geom="line",size=1,linetype="dashed")+
   guides(colour=guide_legend(ncol=legendcols),linetype=guide_legend(ncol=legendcols))+
   annotate("text",x=as.Date("1991-04-15"),y=77.2,label="Current National Standard",family=family)

plot(FullPlot)
tiff("Plots/SO2Long.tiff",width=6.5, height=4.5,units="in",family=family,pointsize=10,res=300)
plot(FullPlot)
dev.off()

# 
# # Calculate Percent Change ------------------------------------------------
# 
# 
# 
# SO2Standard$year<-as.Date(paste(SO2Standard$year,"01","01",sep="-"))
# 
# 
# 
# 
# SO2StandMelt<-melt(SO2Standard,id.vars = c("year","Common.Name","MetroAtlanta"))
# SO2StandCast<-cast(SO2StandMelt,year+Common.Name~MetroAtlanta)
# names(SO2StandCast)<-c("year","Common.Name","Metro","State")
# SO2StandAvg<-ddply(SO2StandCast,.(year),summarize,Metroaverage=mean(Metro,na.rm=T),Stateaverage=mean(State,na.rm=T))
# SO2StandAvg$Full<-AverageStandard$avg
# 
# 
# MetroFrame<-SO2StandAvg[!is.na(SO2StandAvg$Metroaverage),]
# MetroFrame<-MetroFrame[complete.cases(MetroFrame$Metroaverage),]
# 
# StateFrame<-SO2StandAvg[!is.na(SO2StandAvg$Stateaverage),]
# StateFrame<-StateFrame[complete.cases(StateFrame$Stateaverage),]
# 
# startYearMetro=min(MetroFrame$year)
# endYearMetro=max(MetroFrame$year)
# 
# MetroPercChange<-data.frame((MetroFrame[which(MetroFrame$year==startYearMetro),c(2,4)]-MetroFrame[MetroFrame$year==endYearMetro,c(2,4)])/MetroFrame[MetroFrame$year==startYearMetro,c(2,4)])
# MetroPercChange$Stateaverage<-NA
# MetroPercChange$Pollutant<-"SO2Long"
# MetroPercChange$startYear<-startYearMetro
# MetroPercChange$endYear<-endYearMetro
# 
# startYearState=min(StateFrame$year)
# endYearState=max(StateFrame$year)
# StatePercChange<-data.frame((StateFrame$Stateaverage[which(StateFrame$year==startYearState)]-StateFrame$Stateaverage[StateFrame$year==endYearState])/StateFrame$Stateaverage[StateFrame$year==startYearState])
# names(StatePercChange)<-"Stateaverage"
# StatePercChange$Metroaverage<-NA
# StatePercChange$Pollutant<-"SO2Long"
# StatePercChange$Full<-MetroPercChange$Full
# StatePercChange$startYear<-startYearState
# StatePercChange$endYear<-endYearState
# 
# SO2PercChange<-rbind(MetroPercChange,StatePercChange)
# SO2PercChange<-SO2PercChange[,c(1,3,2,4,5,6)]


# percentChange<-ddply(SO2Standard,.(Common.Name),summarize,percentChange=(standard[year==max(year)]-standard[year==min(year)])/standard[year==max(year)],StartYear=min(year),EndYear=max(year)+1)
# # largestPercent<-percentChange[which(abs(percentChange$percentChange)==max(abs(percentChange$percentChange))),]
# # largestPercent$Pollutant<-"SO2Long"
# 
# write.table(SO2PercChange,file="PercentChange/percentchange.csv",sep=",",append=T,row.names=F,col.names=F)
