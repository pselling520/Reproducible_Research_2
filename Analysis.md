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
`INJURIES`, `PROPDMG`, and `CROPDMG`. Lets extract that into a smaller
dataset.

    reducedData <- data[c("EVTYPE","FATALITIES","INJURIES","PROPDMG","CROPDMG")]

We want to sum the health impacts and economic impacts so we need to
ensure all variables are numeric. Then we’ll sum the impacts by event
type. We’ll also break these into two separate dataframes, but that
isn’t imperative. This will make it easier to build our graphs later on.

    reducedData$FATALITIES <- as.numeric(reducedData$FATALITIES)
    reducedData$INJURIES <- as.numeric(reducedData$INJURIES)
    reducedData$PROPDMG <- as.numeric(reducedData$PROPDMG)
    reducedData$CROPDMG <- as.numeric(reducedData$CROPDMG)

    reducedData["HEALTH.OUTCOMES"] <- reducedData$FATALITIES + reducedData$INJURIES
    reducedData["ECONOMIC.OUTCOMES"] <- reducedData$PROPDMG + reducedData$CROPDMG

    healthData <- reducedData[c("EVTYPE","HEALTH.OUTCOMES")]
    economicData <- reducedData[c("EVTYPE","ECONOMIC.OUTCOMES")]

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
        group_by(EVTYPE) %>%
        summarize(dmg=sum(HEALTH.OUTCOMES)) %>%
        arrange(desc(dmg)) %>%
        top_n(10,dmg) %>% #Only grab top 10 most dangerous weather events
        ggplot(aes(x=reorder(EVTYPE,-dmg), #ensures we sort in descending order
                   y=dmg,
                   fill=EVTYPE)) +
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle=90,
                                             vjust=0.5,
                                             hjust=1)) +
            labs(x="Weather Event",
                 y="Total Fatalities and Injuries",
                 title="Top 10 Most Dangerous Weather Events") +
            scale_fill_discrete(name="Event Type")

![](Analysis_files/figure-markdown_strict/unnamed-chunk-4-1.png)

We can clearly see that tornados have been the most dangerous weather
event in terms of fatalities and injuries since the NOAA began
collecting this data.

### Economic Impacts

Next, we want to identify which types of events have the greatest
economic consequences. To find this, we will us a bar plot to plot the
total property damage and crop damage by event type. Once again, we’ll
look at only the top 10 most damaging weather events.

    economicData %>%
        group_by(EVTYPE) %>%
        summarize(dmg=sum(ECONOMIC.OUTCOMES)) %>%
        arrange(desc(dmg)) %>%
        top_n(10,dmg) %>% #Only grab top 10 most economically damaging weather events
        ggplot(aes(x=reorder(EVTYPE,-dmg),y=dmg,fill=EVTYPE)) + #ensures we sort in descending order
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle=90,
                                             vjust=0.5,
                                             hjust=1)) +
            labs(x="Weather Event",
                 y="Total Crop and Property Damage",
                 title="Top 10 Most Financially Damaging Weather Events") +
            scale_fill_discrete(name="Event Type")

![](Analysis_files/figure-markdown_strict/unnamed-chunk-5-1.png)

Once again, we see that tornados are the most economically damaging
events in our data set.
