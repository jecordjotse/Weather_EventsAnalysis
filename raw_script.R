library(reshape2);
data <- read.csv('./data/repdata_data_StormData.csv.bz2',header = TRUE, sep = ",")
nrow(data)
names(data)
source('init.R')
loadData()
head(data)
rm(dataSet)
source('init.R')
loadData()
names(dataSet)
head(dataSet,5)
tail(dataSet,5)
str(dataSet)
View(head(select(dataSet, names(dataSet)[c(1,4,5,6,7,8)]),200))
levels(dataSet['EVTYPE'])
levels(dataSet[,8])
dataSet$EVTYPE <- tolower(dataSet$EVTYPE)
View(dataSet)
as.factor(dataSet[,8])
dataSub <- dataSet[,names(dataSet)[c(1,5,6,7,8,23,24,25,26,27,28)]]
dataSub$EVTYPE <-gsub("(^[[:space:]]+|[[:space:]]+$)", "", dataSub$EVTYPE)
dataSub$EVTYPE <-gsub("\\s+", " ", dataSub$EVTYPE)
#cleaning EVTYPE
dataSub$EVTYPE <- gsub("tstm","thunderstorm",dataSub$EVTYPE, fixed = TRUE)
dataSub$EVTYPE <- gsub("thun[a-z]+m","thunderstorm",dataSub$EVTYPE, fixed = TRUE)
dataSub$EVTYPE <- gsub("thunderstormwinds","thunderstorm w",dataSub$EVTYPE, fixed = TRUE)
dataSub$EVTYPE <- gsub("thunderstormw","thunderstorm w",dataSub$EVTYPE, fixed = TRUE)
dataSub$EVTYPE <- gsub("wnd","wind",dataSub$EVTYPE, fixed = TRUE)
dataSub$EVTYPE <- gsub("winds","wind",dataSub$EVTYPE, fixed = TRUE)
dataSub$EVTYPE <- gsub("wins","wind",dataSub$EVTYPE, fixed = TRUE)
# replace events with a group name
dataSub[grepl("thunderstorm wind",dataSub$EVTYPE, fixed = TRUE),]$EVTYPE <- "thunderstorm w"
dataSub[grepl("thunderstorm w inds",dataSub$EVTYPE),]$EVTYPE <- "thunderstorm w"
dataSub[grepl("thunderstorm w",dataSub$EVTYPE, fixed = TRUE),]$EVTYPE <- "thunderstorm w"
dataSub[grepl("thun.+ wind",dataSub$EVTYPE),]$EVTYPE <- "thunderstorm w"
dataSub[grepl("tornado",dataSub$EVTYPE),]$EVTYPE <- "tornado"
dataSub[grepl("torndao",dataSub$EVTYPE),]$EVTYPE <- "tornado"
dataSub[grepl("avalance",dataSub$EVTYPE),]$EVTYPE <- "avalanche"
dataSub[grepl("wind", dataSub$EVTYPE),]$EVTYPE <- "other wind"
dataSub[grepl(".*flo*d.*",dataSub$EVTYPE),]$EVTYPE <- "flood"
dataSub[grepl(".*cold.*|.*snow.*|.*ice.*|.*winter.*|.*freez.*|.*hail.*|.*blizz.*|.*frost.*",dataSub$EVTYPE),]$EVTYPE <- "snow related"
dataSub[grepl(".*heat.*|.*hypo.*",dataSub$EVTYPE),]$EVTYPE <- "heat related"
dataSub[grepl(".*rain.*",dataSub$EVTYPE),]$EVTYPE <- "rains"
dataSub[grepl(".*lightning.*",dataSub$EVTYPE),]$EVTYPE <- "lightning"
dataSub[grepl("rip current",dataSub$EVTYPE),]$EVTYPE <- "rip currents"
dataSub[grepl(".*fire.*",dataSub$EVTYPE),]$EVTYPE <- "fire related"
dataSub[grepl(".*hurricane.*",dataSub$EVTYPE),]$EVTYPE <- "hurricane"
dataSub[grepl(".*surf.*",dataSub$EVTYPE),]$EVTYPE <- "surf related"

dataSub$EVTYPE <- as.factor(dataSub$EVTYPE)
#dataSub$EVTYPE <- gsub("win","winds",dataSub$EVTYPE, fixed = TRUE)
#tstm = thunderstorm

dataSub[grepl("tstm",dataSub$EVTYPE, fixed = TRUE)]<-"thunderstorm wind"

fatalitiesByType <-aggregate(FATALITIES ~ EVTYPE, dataSub, sum)
filter(fatalitiesByType, FATALITIES >0)
fatalitiesNonZero <- fatalitiesByType %>% filter(FATALITIES >0) %>% arrange(desc(FATALITIES))
injuriesByType <-aggregate(INJURIES ~ EVTYPE, dataSub, sum)
injuriesByType %>% filter(INJURIES >0) %>% arrange(desc(INJURIES))
injuriesNonZero <- injuriesByType %>% filter(INJURIES >0) %>% arrange(desc(INJURIES))
#names(dataSet)
healthIssuesByType <- merge(fatalitiesByType,injuriesByType,by='EVTYPE')
healthIssuesByType <- healthIssuesByType %>% mutate(HEALTHISSUES = FATALITIES + INJURIES) %>% filter(HEALTHISSUES >0) %>% arrange(desc(HEALTHISSUES))
temp <- head(healthIssuesByType,20)
meltedIssuesByType <- melt(temp, measure.vars = c("FATALITIES","INJURIES"), id = "EVTYPE")

library(ggplot2)
# Basic barplot
par(mfrow=c(2,1))

p<-ggplot(data=arrange(meltedIssuesByType,desc(value)), aes(x=reorder(EVTYPE, -value), y=value,fill=variable)) +
geom_bar(stat="identity")

p + coord_flip()

fatalitiesByType <-aggregate(FATALITIES ~ EVTYPE, dataSub, sum)
fatalitiesNonZero <- fatalitiesByType %>% filter(FATALITIES >0) %>% arrange(desc(FATALITIES))
injuriesByType <-aggregate(INJURIES ~ EVTYPE, dataSub, sum)
injuriesNonZero <- injuriesByType %>% filter(INJURIES >0) %>% arrange(desc(INJURIES))
#names(dataSet)
healthIssuesByType <- merge(fatalitiesByType,injuriesByType,by='EVTYPE')
healthIssuesByType <- healthIssuesByType %>% mutate(HEALTHISSUES = FATALITIES + INJURIES) %>% filter(HEALTHISSUES >0) %>% arrange(desc(HEALTHISSUES))
temp <- head(healthIssuesByType,20)
meltedIssuesByType <- melt(temp, measure.vars = c("FATALITIES","INJURIES"), id = "EVTYPE")

p<-ggplot(data=arrange(meltedIssuesByType,desc(value)), aes(x=reorder(EVTYPE, -value), y=value,fill=variable)) +
    geom_bar(stat="identity")

p + coord_flip()

healthIssuesByType <- head(healthIssuesByType, 100)


propertyDMGByType <-aggregate(PROPDMG ~ EVTYPE, dataSub, sum)
nrow(filter(propertyDMGByType, PROPDMG >0))
filter(propertyDMGByType, PROPDMG >0)
propertyDMGNonZero <- propertyDMGByType %>% filter(PROPDMG >0) %>% arrange(desc(PROPDMG))
cropDMGByType <-aggregate(CROPDMG ~ EVTYPE, dataSub, sum)
nrow(filter(cropDMGByType, CROPDMG > 0))
filter(cropDMGByType, CROPDMG > 0)
cropDMGNonZero <- cropDMGByType %>% filter(CROPDMG >0) %>% arrange(desc(CROPDMG))

#names(dataSet)
econDMGByType <- merge(propertyDMGByType,cropDMGByType,by='EVTYPE')
temp <- head(econDMGByType,20)
meltedDMGByType <- melt(temp, measure.vars = c("CROPDMG","PROPDMG"), id = "EVTYPE")
econDMGByType <- econDMGByType %>% mutate(ECONDMG = CROPDMG + PROPDMG) %>% filter(ECONDMG >0) %>% arrange(desc(ECONDMG))

p<-ggplot(data=arrange(meltedDMGByType,desc(value)), aes(x=reorder(EVTYPE, -value), y=value,fill=variable)) +
geom_bar(stat="identity")
p + coord_flip()


dataSub$EVTYPE <-gsub("(^[[:space:]]+|[[:space:]]+$)", "", dataSub$EVTYPE)
dataSub$EVTYPE <-gsub("\\s+", " ", dataSub$EVTYPE)

# rename all winds & wrongly spelt wind to wind
dataSub$EVTYPE <- gsub("wnd","wind",dataSub$EVTYPE, fixed = TRUE)
dataSub$EVTYPE <- gsub("winds","wind",dataSub$EVTYPE, fixed = TRUE)
dataSub$EVTYPE <- gsub("wins","wind",dataSub$EVTYPE, fixed = TRUE)

# rename all tstm & wrongly spelt thunderstorm to thunderstorm
dataSub$EVTYPE <- gsub("tstm","thunderstorm",dataSub$EVTYPE, fixed = TRUE)
dataSub$EVTYPE <- gsub("thun[a-z]+m","thunderstorm",dataSub$EVTYPE, fixed = TRUE)
dataSub$EVTYPE <- gsub("thunderstormwinds","thunderstorm w",dataSub$EVTYPE, fixed = TRUE)
dataSub$EVTYPE <- gsub("thunderstormw","thunderstorm w",dataSub$EVTYPE, fixed = TRUE)
# replace events with a group name
#dataSub[grepl("thunderstorm wind",dataSub$EVTYPE, fixed = TRUE),]$EVTYPE <- "thunderstorm w"
dataSub[grepl("thun.+ wind",dataSub$EVTYPE),]$EVTYPE <- "thunderstorm w"
dataSub[grepl("thunderstorm w inds",dataSub$EVTYPE),]$EVTYPE <- "thunderstorm w"
dataSub[grepl("tornado",dataSub$EVTYPE),]$EVTYPE <- "tornado"
dataSub[grepl("torndao",dataSub$EVTYPE),]$EVTYPE <- "tornado"
dataSub[grepl("avalance",dataSub$EVTYPE),]$EVTYPE <- "avalanche"
dataSub[grepl("wind", dataSub$EVTYPE),]$EVTYPE <- "other wind"
dataSub[grepl(".*flo*d.*",dataSub$EVTYPE),]$EVTYPE <- "flood"
dataSub[grepl(".*cold.*|.*snow.*|.*ice.*|.*winter.*|.*freez.*|.*hail.*|.*blizz.*|.*frost.*",dataSub$EVTYPE),]$EVTYPE <- "snow related"
dataSub[grepl(".*heat.*|.*hypo.*",dataSub$EVTYPE),]$EVTYPE <- "heat related"
dataSub[grepl(".*rain.*",dataSub$EVTYPE),]$EVTYPE <- "rains"
dataSub[grepl(".*lightning.*",dataSub$EVTYPE),]$EVTYPE <- "lightning"
dataSub[grepl("rip current",dataSub$EVTYPE),]$EVTYPE <- "rip currents"
dataSub[grepl(".*fire.*",dataSub$EVTYPE),]$EVTYPE <- "fire related"
dataSub[grepl(".*hurricane.*",dataSub$EVTYPE),]$EVTYPE <- "hurricane"
dataSub[grepl(".*surf.*",dataSub$EVTYPE),]$EVTYPE <- "surf related"

dataSub$EVTYPE <- as.factor(dataSub$EVTYPE)
# all other winds