Executive Summary
=================

This analysis seeks to provide insight into which weather events results
in the worst population health and the worst economic outcomes. The
dataset we used is from the NOAA’s storm database. Tornados result in
both the worst population health outcomes and the worst economic
outcomes.

Data processing
===============

First, we load in the data and the libraries we need for this analysis.
The `read.csv` function is able to decompress the .csv.bz2 file, so that
is all we need to load in the data. Since this is a very large dataset,
we can cache the dataset to ensure we only need to load it once. We’ll
also reduce the dataframe to only the columns we’ll care about in this
analysis to improve performance elsewhere.

    #load in the libraries
    library(ggplot2)
    library(dplyr)
    library(stringdist)

    #load in the data
    data <- read.csv(".\\repdata_data_StormData.csv.bz2",header=TRUE)

    #Get names of columns
    names(data)

    ##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"     "COUNTYNAME" "STATE"     
    ##  [8] "EVTYPE"     "BGN_RANGE"  "BGN_AZI"    "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END"
    ## [15] "COUNTYENDN" "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"      "F"         
    ## [22] "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"    "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP"
    ## [29] "WFO"        "STATEOFFIC" "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
    ## [36] "REMARKS"    "REFNUM"

After some looking at the names of the columns in our dataset, it looks
like the columns we are interested in are `EVTYPE`, `FATALITIES`,
`INJURIES`, `PROPDMG`, `CROPDMG`, `PROPDMGEXP`, and `CROPDMGEXP`. Lets
extract that into a smaller dataset.

    reducedData <- data[c("BGN_DATE","EVTYPE","FATALITIES","INJURIES",
                          "PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]

Data for all events is only logged starting in January of 1996, so we’re
going to restrict the dataset to observations from 1996 or later.

    reducedData$BGN_DATE <- as.Date(data$BGN_DATE, "%m/%d/%Y")
    reducedData <- reducedData[reducedData$BGN_DATE>=1996,]

We want to sum the health impacts and economic impacts so we need to
ensure all variables are numeric. We’ll also break these into two
separate dataframes for health and economic outcomes. This will make
cleaning up the data a bit easier. We’ll also sum up the health outcomes
directly now so we don’t have to worry about that later. We can also
eliminate all observations with zero combined injuries and fatalities
because they have no impact on the cumulative health impact.

    reducedData$FATALITIES <- as.numeric(reducedData$FATALITIES)
    reducedData$INJURIES <- as.numeric(reducedData$INJURIES)
    reducedData$PROPDMG <- as.numeric(reducedData$PROPDMG)
    reducedData$CROPDMG <- as.numeric(reducedData$CROPDMG)


    healthData <- reducedData[c("EVTYPE","FATALITIES","INJURIES")]
    economicData <- reducedData[c("EVTYPE","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]

    healthData["HEALTH.OUTCOMES"] <- healthData$FATALITIES + healthData$INJURIES
    healthData <- healthData[healthData$HEALTH.OUTCOMES > 0,]
    healthData <- healthData[c("EVTYPE","HEALTH.OUTCOMES")]

Now we’ll clean up the damage values by working through the exponents.
Per the analysis in "[How to Handle Exponent Value of PROPDMGEXP and
CROPDMGEXP"](https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html),
we know that B and b stands for billions, M and m stand for millions, K
and k stand for thousands, and H and h stand for hundreds. We also know
that the integers 0 through 8 all correspond to a multiple of 10. We
will remove the rows where we don’t know the exponent for either the
property damage or the crop damage. We’ll also remove the observations
with no property or crop damage because they do not contribute to the
overall economic impacts for a given weather event. We’ll also convert
the damage to value in billions for consistency.

    economicData <- subset(economicData, (PROPDMGEXP %in% c("B","b","M","m","K","k","H","h","0",
                                                            "1","2","3","4","5","6","7","8") & 
                                              CROPDMGEXP %in% c("B","b","M","m","K","k","H","h","0",
                                                            "1","2","3","4","5","6","7","8")))

    economicData <- economicData[(economicData$PROPDMG + economicData$CROPDMG)>0,]

    #Adjust property damage value
    for(i in 1:nrow(economicData)){
        if(economicData$PROPDMGEXP[i] %in% c("0","1","2","3","4","5","6","7","8")){
            economicData$PROPDMG[i] <- economicData$PROPDMG[i]/100000000 #dividing by 100 million since
                                                                         #counted in tens
        }
        else if(economicData$PROPDMGEXP[i] %in% c("M","m")){
            economicData$PROPDMG[i] <- economicData$PROPDMG[i]/1000 #dividing by 1000 since 
                                                                    #counted in millions
        }
        else if (economicData$PROPDMGEXP[i] %in% c("K","k")){
            economicData$PROPDMG[i] <- economicData$PROPDMG[i]/1000000 #dividing by 1 million since
                                                                       #counted in thousands
        }
        
        else {
            economicData$PROPDMG[i] <- economicData$PROPDMG[i]/10000000 #dividing by 10 million since
                                                                        #counted in hundreds
        }
    }

    #Adjust crop damage data
    for(i in 1:nrow(economicData)){
        if(economicData$CROPDMGEXP[i] %in% c("0","1","2","3","4","5","6","7","8")){
            economicData$CROPDMG[i] <- economicData$CROPDMG[i]/100000000 #dividing by 100 million since
                                                                         #counted in tens
        }
        else if(economicData$CROPDMGEXP[i] %in% c("M","m")){
            economicData$CROPDMG[i] <- economicData$CROPDMG[i]/1000 #dividing by 1000 since 
                                                                    #counted in millions
        }
        else if (economicData$CROPDMGEXP[i] %in% c("K","k")){
            economicData$CROPDMG[i] <- economicData$CROPDMG[i]/1000000 #dividing by 1 million since
                                                                       #counted in thousands
        }
        else {
            economicData$PROPDMG[i] <- economicData$CROPDMG[i]/10000000 #dividing by 10 million since
                                                                        #counted in hundreds
        }
    }

Now that we’ve standardized damage to the value in billions, we can add
the values together. We’ll also reduce the dataframe to the only columns
we’re interested in.

    economicData["ECONOMIC.OUTCOMES"] <- economicData$PROPDMG + economicData$CROPDMG

    economicData <- economicData[c("EVTYPE","ECONOMIC.OUTCOMES")]

Finally, we need to consolidate the weather events together so we’re
only looking at the 48 unique types of events. From the National Weather
Service’s documentation, we see the 48 unique types of events (stored
directly in a list) are:

    events <- c("Astronomical Low Tide",
                "Avalanche",
                "Blizzard",
                "Coastal Flood",
                "Cold/Wind Chill",
                "Debris Flow",
                "Dense Fog",
                "Dense Smoke",
                "Drought",
                "Dust Devil",
                "Dust Storm",
                "Excessive Heat",
                "Extreme Cold/Wind Chill",
                "Flash Flood",
                "Flood",
                "Frost/Freeze",
                "Funnel Cloud",
                "Freezing Fog",
                "Hail",
                "Heat",
                "Heavy Rain",
                "Heavy Snow",
                "High Surf",
                "High Wind",
                "Hurricane (Typhoon)",
                "Ice Storm",
                "Lake-Effect Snow",
                "Lakeshore Flood",
                "Lightning",
                "Marine Hail",
                "Marine High Wind",
                "Marine Strong Wind",
                "Marine Thunderstorm Wind",
                "Rip Current",
                "Seiche",
                "Sleet",
                "Storm Surge/Tide",
                "Strong Wind",
                "Thunderstorm Wind",
                "Tornado",
                "Tropical Depression",
                "Tropical Storm",
                "Tsunami",
                "Volcanic Ash",
                "Waterspout",
                "Wildfire",
                "Winter Storm",
                "Winter Weather")

Now we’ll do approximate character matches to classify each event. We’ll
confirm that we’ve reduced the events in our datasets to events
specified by the NOAA.

    healthData <- mutate(healthData, EVENT = events[amatch(healthData$EVTYPE,
                                                           events,
                                                           maxDist = 25)])

    economicData <- mutate(economicData, EVENT= events[amatch(economicData$EVTYPE,
                                                              events,
                                                              maxDist = 25)])

    unique(healthData$EVENT)

    ##  [1] "Tornado"                 "High Wind"               "Hail"                   
    ##  [4] "Coastal Flood"           "Ice Storm"               "Marine High Wind"       
    ##  [7] "Dense Fog"               "Rip Current"             "Heavy Rain"             
    ## [10] "Flood"                   "Flash Flood"             "Debris Flow"            
    ## [13] "Heavy Snow"              "High Surf"               "Dust Storm"             
    ## [16] "Sleet"                   "Dense Smoke"             "Excessive Heat"         
    ## [19] "Funnel Cloud"            "Strong Wind"             "Blizzard"               
    ## [22] "Storm Surge/Tide"        "Avalanche"               NA                       
    ## [25] "Dust Devil"              "Marine Hail"             "Freezing Fog"           
    ## [28] "Winter Weather"          "Lake-Effect Snow"        "Drought"                
    ## [31] "Cold/Wind Chill"         "Hurricane (Typhoon)"     "Wildfire"               
    ## [34] "Heat"                    "Waterspout"              "Extreme Cold/Wind Chill"
    ## [37] "Marine Strong Wind"      "Seiche"

    unique(economicData$EVENT)

    ##  [1] "Marine High Wind"        "High Wind"               "Heavy Rain"             
    ##  [4] "Tornado"                 "Dense Smoke"             "Flash Flood"            
    ##  [7] "Flood"                   "Dense Fog"               "Hail"                   
    ## [10] "Ice Storm"               "Storm Surge/Tide"        "Heavy Snow"             
    ## [13] "Debris Flow"             "Blizzard"                "Seiche"                 
    ## [16] "Coastal Flood"           "Dust Devil"              "Dust Storm"             
    ## [19] NA                        "Drought"                 "High Surf"              
    ## [22] "Avalanche"               "Frost/Freeze"            "Rip Current"            
    ## [25] "Strong Wind"             "Wildfire"                "Astronomical Low Tide"  
    ## [28] "Excessive Heat"          "Winter Weather"          "Lake-Effect Snow"       
    ## [31] "Lakeshore Flood"         "Marine Strong Wind"      "Cold/Wind Chill"        
    ## [34] "Extreme Cold/Wind Chill" "Funnel Cloud"            "Marine Hail"            
    ## [37] "Freezing Fog"

Results
=======

We want to exam both the health impacts and the economic impacts of
these events. We’ll look at the population health outcomes first
followed by the economic outcomes afterwards.

### Health Results

First, we want to identify which types of events (using the EVTYPE
variable) are most harmful with respect to population health. To do
this, we will use bar graphs to compare the population health effects
for each event type. We’ll look at only the top 10 most dangerous
weather events.

    healthData %>%
        group_by(EVENT) %>%
        summarize(dmg=sum(HEALTH.OUTCOMES)) %>%
        arrange(desc(dmg)) %>%
        top_n(10,dmg) %>% #Only grab top 10 most dangerous weather events
        ggplot(aes(x=reorder(EVENT,-dmg), #ensures we sort in descending order
                   y=dmg,
                   fill=EVENT)) +
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle=90,
                                             vjust=0.5,
                                             hjust=1)) +
            labs(x="Weather Event",
                 y="Total Fatalities and Injuries",
                 title="Top 10 Most Dangerous Weather Events") +
            scale_fill_discrete(name="Event Type")

![](Analysis_files/figure-markdown_strict/unnamed-chunk-135-1.png)

We can clearly see that tornados have been the most dangerous weather
event in terms of fatalities and injuries since the NOAA began
collecting this data on all events in 1996.

### Economic Impacts

Next, we want to identify which types of events have the greatest
economic consequences. To find this, we will us a bar plot to plot the
total property damage and crop damage by event type. Once again, we’ll
look at only the top 10 most damaging weather events.

    economicData %>%
        group_by(EVENT) %>%
        summarize(dmg=sum(ECONOMIC.OUTCOMES)) %>%
        arrange(desc(dmg)) %>%
        top_n(10,dmg) %>% #Only grab top 10 most economically damaging weather events
        ggplot(aes(x=reorder(EVENT,-dmg),y=dmg,fill=EVENT)) + #ensures we sort in descending order
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle=90,
                                             vjust=0.5,
                                             hjust=1)) +
            labs(x="Weather Event",
                 y="Total Crop and Property Damage",
                 title="Top 10 Most Financially Damaging Weather Events") +
            scale_fill_discrete(name="Event Type")

![](Analysis_files/figure-markdown_strict/unnamed-chunk-136-1.png)

We see that floods are the most economically damaging events in our data
set since the NOAA collected data on all events starting in 1996.
