

##for internet use
setInternet2(use = TRUE)

##setting the directory
mainDir <- getwd()
subDir <- "Reproductibleassign2"
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))

##download the file
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, destfile = "storm_data.csv")
storm<-read.csv("storm_data.csv")

#transforming the dates
library(lubridate)
storm$year<-year(mdy_hms(storm$BGN_DATE))

#subsetting info from quantile 10%
hist(storm$year)
##justification for years of choice

#grouping in time period
storm$group<-""
storm$group[storm$year>=1990 & storm$year<1995]<-"[1990-1995["
storm$group[storm$year>=1995 & storm$year<2000]<- "[1995-2000["
storm$group[storm$year>=2000 & storm$year<2005]<- "[2000-2005["
storm$group[storm$year>=2005 & storm$year<2010]<- "[2005-2010["
storm$group[storm$year>2010 ]<- "[2005,+++["
storm$group<-as.factor(storm$group)

#TOTAL DAMAGE
storm$total_damage<-storm$PROPDMG + storm$CROPDMG

#subset only info from 1999 & relevant data
sub_storm<-storm[storm$group!="",]
sub_storm<-sub_storm[sub_storm$total_damage > 10 | sub_storm$FATALITIES > 0 | sub_storm$INJURIES > 0 ,]

# subsetting the data
events<-unique(sub_storm$EVTYPE)

### after analysing the events we came up with this methodology for grep the categories without losing much information
for_grep<-c('ASTRONOMICAL LOW TIDE', 'AVALANCHE', 'AVALANCHE', 'BLIZZARD', 'COASTAL FLOOD', 'COASTAL FLOOD', 
      'COLD/WIND CHILL', 'DEBRIS FLOW', 'DENSE FOG', 'DENSE SMOKE', 'DROUGHT', 'DROUGHT', 'DUST DEVIL', 
      'DUST DEVIL', 'DUST STORM', 'EXCESSIVE HEAT', 'EXCESSIVE HEAT', 'EXTREME COLD/WIND CHILL', 
      'EXTREME COLD/WIND CHILL', 'EXTREME COLD/WIND CHILL', 'EXTREME COLD/WIND CHILL', 'EXTREME COLD/WIND CHILL', 
      'FLASH FLOOD', 'FLOOD', 'FREEZING FOG', 'FROST/FREEZE', 'FROST/FREEZE', 'FROST/FREEZE', 'FROST/FREEZE', 
      'FROST/FREEZE', 'FUNNEL CLOUD', 'HAIL', 'HEAT', 'HEAVY RAIN', 'HEAVY RAIN', 'HEAVY RAIN', 'HEAVY RAIN', 
      'HEAVY RAIN', 'HEAVY SNOW', 'HEAVY SNOW', 'HIGH SURF', 'HIGH SURF', 'HIGH SURF', 'HIGH SURF', 'HIGH SURF', 
      'HIGH WIND', 'HIGH WIND', 'HIGH WIND', 'HIGH WIND', 'HIGH WIND', 'HURRICANE (TYPHOON)', 'HURRICANE (TYPHOON)', 
      'ICE STORM', 'ICE STORM', 'ICE STORM', 'LAKE-EFFECT SNOW', 'LAKE-EFFECT SNOW', 'LAKESHORE FLOOD', 'LIGHTNING', 
      'MARINE HAIL', 'MARINE HIGH WIND', 'MARINE HIGH WIND', 'MARINE STRONG WIND', 'MARINE THUNDERSTORM WIND', 
      'MARINE THUNDERSTORM WIND', 'MARINE THUNDERSTORM WIND', 'MARINE THUNDERSTORM WIND', 'RIP CURRENT', 'SEICHE', 
      'SLEET', 'STORM SURGE/TIDE', 'STORM SURGE/TIDE', 'STORM SURGE/TIDE', 'STRONG WIND', 'STRONG WIND',    
      'THUNDERSTORM WIND', 'THUNDERSTORM WIND', 'TORNADO', 'TROPICAL DEPRESSION', 'TROPICAL STORM', 'TSUNAMI', 
      'VOLCANIC ASH', 'WATERSPOUT', 'WILDFIRE', 'WILDFIRE', 'WINTER STORM', 'WINTER WEATHER', 'FLOOD')

for_grep2<-c('ASTRONOMICAL', 'AVALANCHE', 'SLIDE', 'BLIZZARD', 'COASTAL', 'CSTL', 'COLD', 'DEBRIS FLOW', '^FOG', 
             'DENSE SMOKE', 'DROUGHT', 'DROWNING', 'DUST DEVIL', 'dust$', 'DUST STORM', 'EXCESSIVE HEAT', 'WARM', 
             'EXTREME COLD/WIND CHILL', 'EXTREME WIND', 'COLD', 'HYPOTHERMIA', 'HYPERTHERMIA', 'FLASH FLOOD', 'FLOOD', 
             'FOG AND COLD TEMPERATURES', 'FREEZ', 'GLAZE', 'FROST', 'BLACK ICE', 'LOW TEMPERATURE', 'FUNNEL CLOUD', 
             'HAIL', '^HEAT', 'HEAVY RAIN', 'PRECIP', 'WET', 'RAIN', 'EXCESSIVE WETNESS', 'HEAVY SNOW', 'SNOW', 
             'HIGH SURF', 'SURF', 'SWELLS', 'HIGH SEAS', 'ROUGH SEAS', 'HIGH WIND', '^WIND', 'gradient wind', 
             'GUSTNADO', 'DRY MICROBURST', 'HURRICANE', 'TYPHOON', 'ICE STORM', '^ICE', 'ICY ROADS', 
             'LAKE-EFFECT SNOW', 'LAKE', 'LAKESHORE FLOOD', 'LIGHTNING', 'MARINE HAIL', 'MARINE HIGH WIND', 
             '^MARINE', 'MARINE STRONG WIND', 'THUDERSTORM', 'THUNDERSNOW', 'TSTM', 'TUNDERSTORM WIND', 
             'RIP CURRENT', 'SEICHE', 'SLEET', 'STORM SURGE/TIDE', 'storm', 'surge', 'STRONG WIND', 'Microburst', 
             'THUNDERSTORM WIND', 'SEVERE THUNDERSTORM', 'TORNADO', 'TROPICAL DEPRESSION', 'TROPICAL STORM', 
             'TSUNAMI', 'VOLCANIC ASH', 'WATERSPOUT', 'WILDFIRE', 'fire', 'WINTER STORM', 'WINTER WEATHER', 
             'URBAN/SML STREAM FLD')

			 ### for the loop
long<-length(for_grep)
sub_storm$event_group <- ""

###will search each grep function and assign to the "for_event" column	
for (i in 1:long) {	
	sss<-grep( for_grep2[i] , events,ignore.case=TRUE,value=TRUE)
	sub_storm$for_event<-FALSE
	sub_storm$for_event<-sub_storm$EVTYPE %in% sss
	sub_storm$event_group[sub_storm$for_event == TRUE] <- for_grep[i]	
}	
sub_storm$for_event<-NULL

#build and arange the occurrences vector
library(tidyr)
occurences<-data.frame(table(sub_storm$event_group,sub_storm$group))
names(occurences)<-c("event_group","period","value")
occurences <- spread(occurences, period, value)
occurences <- occurences[,-2]
occurences$average <- apply(occurences[,2:6],1,mean,na.rm=TRUE)
occurences<-occurences[ order(-occurences$average), ]
occurences$ranking<-rank(-occurences$average)
head(occurences,10)

#statistics for people
##build and arange the fatalities vector
stat_fatalities<-data.frame(tapply(sub_storm$FATALITIES,list(sub_storm$event_group,sub_storm$group),sum))
names(stat_fatalities)<-c("event_group","[1990-1995[","[1995-2000[","[2000-2005[","[2005-2010[", "[2005,+++[")
stat_fatalities$event_group<-rownames(stat_fatalities)
stat_fatalities$average <- apply(stat_fatalities[,2:5],1,mean,na.rm=TRUE)
stat_fatalities<-stat_fatalities[ order(-stat_fatalities$average), ]
stat_fatalities$ranking<-rank(-stat_fatalities$average)
head(stat_fatalities,10)

##build and arange the injuries vector
stat_injuries<-data.frame(tapply(sub_storm$INJURIES,list(sub_storm$event_group,sub_storm$group),sum))
names(stat_injuries)<-c("event_group","[1990-1995[","[1995-2000[","[2000-2005[","[2005-2010[", "[2005,+++[")
stat_injuries$event_group<-rownames(stat_injuries)
stat_injuries$average <- apply(stat_injuries[,2:5],1,mean,na.rm=TRUE)
stat_injuries<-stat_injuries[ order(-stat_injuries$average), ]
stat_injuries$ranking<-rank(-stat_injuries$average)
head(stat_injuries,10)

##build and arange the costs vector
stat_cost<-data.frame(tapply(sub_storm$total_damage,list(sub_storm$event_group,sub_storm$group),sum))
names(stat_cost)<-c("event_group","[1990-1995[","[1995-2000[","[2000-2005[","[2005-2010[", "[2005,+++[")
stat_cost$event_group<-rownames(stat_cost)
stat_cost$average <- apply(stat_cost[,2:5],1,mean,na.rm=TRUE)
stat_cost<-stat_cost[ order(-stat_cost$average), ]
stat_cost$ranking<-rank(-stat_cost$average)
head(stat_cost,10)


#matrix of ranking for each category as a summary
ranking<-data.frame(Ranking=1:10)
ranking$Occurences<-occurences$event_group[match(ranking$Ranking,occurences$ranking)]
ranking$Fatalities<-stat_fatalities$event_group[match(ranking$Ranking,stat_fatalities$ranking)]
ranking$Injuries<-stat_injuries$event_group[match(ranking$Ranking,stat_injuries$ranking)]
ranking$Cost<-stat_cost$event_group[match(ranking$Ranking,stat_cost$ranking)]
ranking
