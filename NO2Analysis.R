load("NO2.RData")
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

s<-qplot(as.Date(paste(year,"01","01",sep="-")),standard,data=NO2Standard, color=Common.Name, geom=c("line","point"),xlab="Year",
         ylab="NO2 Concentration (ppb) Standard", main="Yearly Trend in Georgia NO2")
plot(s)
s2<-s+geom_abline(intercept=100,slope=0,linetype="dotdash")+
   scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
   theme(panel.background=element_rect(fill="white"))+
   theme(panel.grid.major=element_line(colour="grey85"))

plot(s2)

s3<-s2+stat_summary(fun.y=mean,color="black",geom="line",size=1.5,linetype="dashed")
plot(s3)


jpeg("NO2FullPlot.jpg")
plot(s3)
dev.off()

