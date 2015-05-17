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

PM2.5Standard<-ddply(PM2.5Yearly,.(year,Common.Name,MetroAtlanta),summarize,standard=filter(Yearly,f3,sides=1))


s<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=PM2.5Standard, color=Common.Name, geom=c("line","point"),xlab="Year",
         ylab="PM2.5 Concentration (μg/m^3) Standard", main="Yearly Trend in Georgia PM2.5")
plot(s)
s2<-s+geom_abline(intercept=35,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
   theme(panel.background=element_rect(fill="white"))
plot(s2)

s3<-s2+stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
plot(s3)

s4<-s3+facet_grid(MetroAtlanta~.)
plot(s4)