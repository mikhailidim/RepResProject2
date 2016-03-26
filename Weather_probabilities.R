## Reproducible Research 
## Project 2
## Michael Mikhailidi
## March 2016

# Source data  is not available in work directory.
#Libraries required 
require(ggplot2)
require(dplyr)


# Source data  is not available in work directory.
if (!file.exists("weather.csv.bz2")) {
  message("Downloading data.") 
  dmethod <- 
    download.file(url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                  destfile="weather.csv.bz2", quiet = TRUE,
                  method = switch(Sys.info()["sysname"], Linux="curl", Windows="wininet"))
}
# Load Soruce data.
# As son as object is quite heavy make sure it's not loaded.
if ( ! (is.object("weather") && 
        (nrow(weather) == 902297 && length(weather == 37)))) { 
  message("Reading data.")   
  weather<-read.csv("weather.csv.bz2",stringsAsFactors = FALSE )
  
}


str(weather)
# Data manipulations 

require(ggplot2)
require(dplyr)
require(knitr)
events<-group_by(weather,EVTYPE) %>% summarize(total.injures=sum(INJURIES+FATALITIES),occurs=n()) 
byharm<-events[head(order(events$total.injures,decreasing = TRUE),10),]
bycount<-events[head(order(events$occurs,decreasing = TRUE),10),]

#Exponents fro properties and crops
unique(weather$PROPDMGEXP)
unique(weather$CROPDMGEXP)

# Variables with unclear exponents
length(grep("[+-?]",weather$CROPDMGEXP)) + length(grep("[+-?]",weather$PROPDMGEXP))
round((length(grep("[+-?]",weather$CROPDMGEXP)) + length(grep("[+-?]",weather$PROPDMGEXP))) / nrow(weather)*100,4)

# Property and crops damages
pdamage<-select(weather, EVTYPE,contains("ropdmg")) %>% mutate(PROPDMGEXP=toupper(PROPDMGEXP),CROPDMGEXP=toupper(CROPDMGEXP))
# Table of  of exponents
exponents<-as.data.frame(cbind(exp.name=c("1st","Skip","Skip","Skip","1st","1st","2nd","3rd","4th","5th","6th","7th","8th","Billion","Hundred","Thousand","Million"),
                                DMGEXP=sort(unique(toupper(weather$PROPDMGEXP))),
                               exp.value=c(1,0,0,0,1,1,100,1000,10000,100000,1000000,10000000,100000000,1000000000,100,1000,1000000)
                         ))


# Plot 1
fills <- factor(findInterval(byharm$occurs,quantile(byharm$occurs,c(0,.5,1))),labels=c("rare","occasional","often"))

g<-ggplot(data=byharm,aes(x = EVTYPE,y=total.injures,fill=fills))
g<-g+geom_bar(stat="identity",position = position_dodge())
g<-g+theme(legend.title=element_blank())+scale_fill_brewer(palette="Set1",direction = -1)
g<-g+ggtitle("People injured by weather event type")+xlab("Weather Events")+ylab("People injured")
g<-g+theme(plot.title = element_text(lineheight=.9, face="bold"),axis.text.x=element_text(angle=45,hjust=1))
print(g)

#Top of the top
bycount<-events[head(order(events$occurs,decreasing = TRUE),10),]
topinjures <- dplyr::intersect(bycount,byharm)
kable(topinjures,col.names = c("Event type","Injured","Total events"),caption = "Table 1. The most harmful and ommon weather events.")


