# Analysis of Effects of Severe Weather Events in United Sates (1950 ~ 2011)
DL  
October 24, 2015  
## 1. Synopsis
The goal of this analysis is to explore the NOAA StormData Database and address two questions:

Across the United States,

1. Which type of sever weather events are most harmful with respect to population?
2. Which type of events have the greatest economic consequeces?

The results of the analysis show that **TORNADO** is the most harmful weather event with respect to population health and safety, which led to the majority of the falities and injuries caused by sever weather events.
**FLOOD** is the weather event has the greatest economic consequeces with respect to property damage, while **DROUGHT** is the weahter event has the greatest economic consequences regarding the crop damage.

## 2. Data Processing

```r
library(knitr)
library(ggplot2)
library(lubridate)
setwd("/Users/PAN/Documents/R/RepData_PeerAssessment2")
StormData <- read.csv("repdata-data-StormData.csv")
#head(StormData)
#summary(StormData)
#dim(StormData)
#str(StormData)

#change to upper case to aggregate data within the same category
levels(StormData$EVTYPE) <- toupper(levels(StormData$EVTYPE))

StormData$EVTYPE <- factor(StormData$EVTYPE)
levels(StormData$PROPDMGEXP)
```

```
##  [1] ""  "-" "?" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8" "B" "h" "H" "K"
## [18] "m" "M"
```

```r
deaths <- aggregate(FATALITIES ~ EVTYPE, StormData, sum, na.rm=TRUE)
injuries <- aggregate(INJURIES ~ EVTYPE, StormData, sum, na.rm=TRUE)

# Sort out the top 10 weather events based on the sum of fatalities and injuries respectively
top10_deaths <- deaths[order(-deaths$FATALITIES), ][1:10, ]
top10_injuries <- injuries[order(-injuries$INJURIES), ][1:10, ]

#change to upper case to aggregate data within the same category
levels(StormData$PROPDMGEXP) <- toupper(levels(StormData$PROPDMGEXP))
levels(StormData$CROPDMGEXP) <- toupper(levels(StormData$CROPDMGEXP))
levels(StormData$PROPDMGEXP) <- gsub('\\d|[[:punct:]]', '', levels(StormData$PROPDMGEXP))
levels(StormData$CROPDMGEXP) <- gsub('\\d|[[:punct:]]', '', levels(StormData$CROPDMGEXP))

#transform symbol to multiplier 
StormData$PROPEXP[StormData$PROPDMGEXP == ""] <- 0
StormData$PROPEXP[StormData$PROPDMGEXP == "0"] <- 1
StormData$PROPEXP[StormData$PROPDMGEXP == "1"] <- 10
StormData$PROPEXP[StormData$PROPDMGEXP == "2"] <- 100
StormData$PROPEXP[StormData$PROPDMGEXP == "3"] <- 1000
StormData$PROPEXP[StormData$PROPDMGEXP == "4"] <- 10000
StormData$PROPEXP[StormData$PROPDMGEXP == "5"] <- 1e+05
StormData$PROPEXP[StormData$PROPDMGEXP == "6"] <- 1e+06
StormData$PROPEXP[StormData$PROPDMGEXP == "7"] <- 1e+07
StormData$PROPEXP[StormData$PROPDMGEXP == "8"] <- 1e+08
StormData$PROPEXP[(StormData$PROPDMGEXP=="H")] <- 100
StormData$PROPEXP[(StormData$PROPDMGEXP=="K")] <- 1000
StormData$PROPEXP[(StormData$PROPDMGEXP=="M")] <- 1e+06
StormData$PROPEXP[(StormData$PROPDMGEXP=="B")] <- 1e+09
StormData$CROPEXP[(StormData$CROPDMGEXP=="H")] <- 100
StormData$CROPEXP[(StormData$CROPDMGEXP=="K")] <- 1000
StormData$CROPEXP[(StormData$CROPDMGEXP=="M")] <- 1e+06
StormData$CROPEXP[(StormData$CROPDMGEXP=="B")] <- 1e+09

# Caluclate monetary cost of the damage
# Create two additional columns to store the data
StormData$propDamage <- StormData$PROPDMG * StormData$PROPEXP
StormData$cropDamage <- StormData$CROPDMG * StormData$CROPEXP
PROPDamage <- aggregate(propDamage ~ EVTYPE, data = StormData, FUN = sum)
CROPDamage <- aggregate(cropDamage ~ EVTYPE, data = StormData, FUN = sum)

# Sort out the top 10 weather events based on the sum of damage cost
top10_propDamage <- PROPDamage[order(-PROPDamage$propDamage), ][1:10, ]
top10_cropDamage <- CROPDamage[order(-CROPDamage$cropDamage), ][1:10, ]
```

## 3. Results
### A. Health/Safety Effects 

```r
library(ggplot2)
ggplot(data=top10_injuries, aes(x=reorder(EVTYPE, -INJURIES), y=INJURIES, fill=EVTYPE)) +
    geom_bar(stat="identity") +labs(title='Top 10 Harmful Weather Events (Cause Injuries)',
                                        x="Weather Event Type", y="Injuries") + 
        geom_text(data=top10_injuries, 
                        aes(EVTYPE, INJURIES, label=top10_injuries$INJURIES),
                        size=4) +
        theme(legend.title = element_text(colour="black", size=10, face="bold")) +
        scale_fill_discrete(name="Event Type") +
        theme(plot.title = element_text(size=15, face="bold", vjust=2),
              axis.text=element_text(size=8),
              axis.text.x = element_text(angle = 90,size=8, hjust = 1),
              axis.title.x = element_text(size=12, color="black",face="bold", vjust=-0.35),
                axis.title.y = element_text(size=12, color="black", face="bold",vjust=0.35) )
```

![](PA2_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
ggplot(data=top10_deaths, aes(x=reorder(EVTYPE, -FATALITIES), y=FATALITIES, fill=EVTYPE)) +
    geom_bar(stat="identity") +labs(title='Top 10 Harmful Weather Events (Cause Deaths)',
                                        x="Weather Event Type", y="Fatalities") + 
        geom_text(data=top10_deaths, 
                        aes(EVTYPE, FATALITIES, label=top10_deaths$FATALITIES),
                        size=4) +
        theme(legend.title = element_text(colour="black", size=10, face="bold")) +
        scale_fill_discrete(name="Event Type") +
        theme(plot.title = element_text(size=15, face="bold", vjust=2),
              axis.text=element_text(size=8),
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title.x = element_text(size=12, color="black",face="bold", vjust=-0.35),
                axis.title.y = element_text(size=12, color="black",face="bold", vjust=0.35) )
```

![](PA2_template_files/figure-html/unnamed-chunk-2-2.png) 

### B. Economic Effects

```r
PROPDamage <- aggregate(propDamage ~ EVTYPE, data = StormData, FUN = sum)
CROPDamage <- aggregate(cropDamage ~ EVTYPE, data = StormData, FUN = sum)
top10_propDamage <- PROPDamage[order(-PROPDamage$propDamage), ][1:10, ]
top10_cropDamage <- CROPDamage[order(-CROPDamage$cropDamage), ][1:10, ]

library(scales)
library(ggplot2)
ggplot(data=top10_propDamage, aes(x=reorder(EVTYPE, -propDamage), y=propDamage, fill=EVTYPE)) +
    geom_bar(stat="identity") +labs(title='Top 10 Harmful Weather Events (Property Damage)',
                                        x="Weather Event Type", y="Estimated Costs ($)") + 
        geom_text(data=top10_propDamage, 
                        aes(EVTYPE, propDamage, label=top10_propDamage$propDamage),
                        size=2) +
        theme(legend.title = element_text(colour="black", size=10, face="bold")) +
        scale_fill_discrete(name="Event Type") +
        theme(plot.title = element_text(size=15, face="bold", vjust=2),
              axis.text=element_text(size=10),
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title.x = element_text(size=10, color="black",face="bold",  vjust=-0.35),
                axis.title.y = element_text(size=10, color="black", face="bold", vjust=0.35) )
```

![](PA2_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
ggplot(data=top10_cropDamage, aes(x=reorder(EVTYPE, -cropDamage), y=cropDamage, fill=EVTYPE)) +
    geom_bar(stat="identity") +labs(title='Top 10 Harmful Weather Events (Crop Damage)',
                                        x="Weather Event Type", y="Estimated Costs ($)") + 
        geom_text(data=top10_cropDamage, 
                        aes(EVTYPE, cropDamage, label=top10_cropDamage$cropDamage),
                        size=2) +
        theme(legend.title = element_text(colour="black", size=10, face="bold")) +
        scale_fill_discrete(name="Event Type") +
        theme(plot.title = element_text(size=15, face="bold", vjust=2),
              axis.text=element_text(size=10),
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title.x = element_text(size=10, color="black", face="bold", vjust=-0.35),
                axis.title.y = element_text(size=10, color="black",face="bold",  vjust=0.35) )
```

![](PA2_template_files/figure-html/unnamed-chunk-3-2.png) 

## 4. Coclusion
The results of the analysis show that **TORNADO** is the most harmful weather event with respect to population's health and safety, which led to the majority of the falities as well as injuries caused by sever weather events.
**FLOOD** is the weather event has the greatest economic consequeces with respect to property damage, while **DROUGHT** is the weahter event has the greatest economic consequences regarding the crop damage.
