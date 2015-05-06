load("LeadData.RData")
#Annual Maximum 3 month rolling average
Lead$Site.ID<-factor(Lead$Site.ID)
Lead$Sample.Duration<-factor(Lead$Sample.Duration)
Lead$Start.Time<-factor(Lead$Start.Time)

#Fix an issue with units
Lead$Sample.Value[Lead$Unit=="007"] <-Lead$Sample.Value[Lead$Unit=="007"]*1000
