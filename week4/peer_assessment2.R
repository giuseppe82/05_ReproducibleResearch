# Let's set working directory -- absolute path
data.dir <- "." #"/Users/joedibernardo/Projects/DATASCIENCE/ReproducibleResearch/week4/RepData_PeerAssessment2"
setwd <- data.dir

# 1. Check if file exists, and download file 
filename <- "repdata-data-StormData.csv"

if(!file.exists(filename)){
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(fileUrl, destfile = filename, method = "curl")
  dateDownloaded <- date() # record the download date
   if(!file.exists(data.dir)){
    unzip(filename, exdir = ".")  
    # UCI HAR Dataset is - actually - the unzipped downloaded data set
  }
  data.dir
}

## Loading and preprocessing the data
#### 1. Load the data (i.e. read.csv())
#```{r, echo=TRUE, warning=FALSE, message=FALSE}
if(!exists("repdata-data-StormData.csv")){
  NOAA_rawdata <- read.csv(file = 'repdata-data-StormData.csv', 
                           head = TRUE, sep = ",")
}
#```
#### a quick look through the data
head(NOAA_rawdata)

#### 2. 
Fatalities_byEvent <- aggregate(FATALITIES ~ EVTYPE, NOAA_rawdata, sum, na.rm = TRUE)
Injuries_byEvent <- aggregate(INJURIES ~ EVTYPE, NOAA_rawdata, sum, na.rm = TRUE)
Harmeful_to_Health <- cbind(Fatalities_byEvent, Injuries_byEvent$INJURIES)
names(Harmeful_to_Health) <- c("Type of Events", "Fatality Cases", "Injury Cases")
Harmeful_to_Health <- Harmeful_to_Health[order(Harmeful_to_Health$`Fatality Cases`, 
                                               Harmeful_to_Health$`Injury Cases`, 
                                               decreasing = TRUE),]
HealthImpact_top10 <- Harmeful_to_Health[1:10, ]

#### 3. Let's visualize our results for the top 10 cases
library(ggplot2)
g <- ggplot(data = HealthImpact_top10, aes(x = reorder(`Type of Events`, 
                                                       -`Fatality Cases`), 
                                           y = `Fatality Cases`, 
                                           fill = `Fatality Cases`)) 

g <- g + geom_bar(stat = "identity")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g <- g + xlab("Natural Weather Events") 
g <- g + ylab('Total number of Fatalities cases across USA')
print(g)

HealthImpact_top10[1,]$`Fatality Cases` + HealthImpact_top10[1,]$`Injury Cases`
HealthImpact_top10[3,]$`Fatality Cases` + HealthImpact_top10[3,]$`Injury Cases`

library(ggplot2)
p <- ggplot(data = HealthImpact_top10, aes(x = reorder(`Type of Events`, 
                                                       -`Injury Cases`), 
                                           y = `Injury Cases`, 
                                           fill = `Injury Cases`)) 

p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + xlab("Natural Weather Events") 
p <- p + ylab('Total number of Injuries cases across USA')
print(p)

#### 4. 
Power_of_Ten <- function(x){
  
  if (is.numeric(x)) {
    x <- x
  }
  
  else if (grepl("h", x, ignore.case = TRUE)) {
    x <- 2
  }
  
  else if (grepl("k", x, ignore.case = TRUE)) {
    x <- 3
  }
  
  else if (grepl("m", x, ignore.case = TRUE)) {
    x <- 6
  }
  
  else if (grepl("b", x, ignore.case = TRUE)) {
    x <- 9
  }
  
  else if (x == "" || x == " ") {
    x <- 0
  }
  
  else {
    x <- NA
  }
  
  x

}

Raise_to_Power_of_Ten <- function(value, exp) {
  
  pow <- Power_of_Ten(exp)
  if(is.numeric(value)) {
    value <- value * (10^pow)
  }
  
  if(!is.numeric(value)) {
    value <- 0
  }
  
  value
  
}

NOAA_rawdata$PROPDMG_new <- mapply(Raise_to_Power_of_Ten, NOAA_rawdata$PROPDMG, 
                                   NOAA_rawdata$PROPDMGEXP)
NOAA_rawdata$CROPDMG_new <- mapply(Raise_to_Power_of_Ten, NOAA_rawdata$CROPDMG, 
                                   NOAA_rawdata$CROPDMGEXP)

#NOAA_rawdata$PROPDMG_new <- NOAA_rawdata$PROPDMG * 10^Power_of_Ten(NOAA_rawdata$PROPDMGEXP)  
#head(NOAA_rawdata$PROPDMG_new)
PropertyDamage_byEvent <- aggregate(PROPDMG_new ~ EVTYPE, NOAA_rawdata, sum, na.rm = TRUE)
CropDamage_byEvent <- aggregate(CROPDMG_new ~ EVTYPE, NOAA_rawdata, sum, na.rm = TRUE)
Damage_to_Economy <- cbind(PropertyDamage_byEvent, CropDamage_byEvent$CROPDMG)
names(Damage_to_Economy) <- c("Type of Events", "Property Damages", "Crop Damages")
Damage_to_Economy <- Damage_to_Economy[order(Damage_to_Economy$`Property Damages`, 
                                               Damage_to_Economy$`Crop Damages`, 
                                               decreasing = TRUE),]

Damage_to_Economy_top10 <- Damage_to_Economy[1:10, ]

g <- ggplot(data = Damage_to_Economy_top10, aes(x = reorder(`Type of Events`, 
                                                       -`Property Damages`), 
                                                y = `Property Damages`/1.e9,
                                                fill = `Property Damages`/1.e9))

g <- g + scale_fill_continuous(guide = guide_legend(title = "Billions of $"))
g <- g + geom_bar(stat = "identity")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g <- g + xlab("Natural Weather Events") 
g <- g + ylab('Total amount of damages to Properties, across USA, in billions $')
print(g)

p <- ggplot(data = Damage_to_Economy_top10, aes(x = reorder(`Type of Events`, 
                                                       -`Crop Damages`), 
                                                y = `Crop Damages`/1.e9,  
                                                fill = `Crop Damages`/1.e9)) 

p <- p + scale_fill_continuous((guide = guide_legend(title = "Billions of $")))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + xlab("Natural Weather Events") 
p <- p + ylab('Total amount of damages to Crops, across USA, in billions $')
print(p)

p <- ggplot(data = Damage_to_Economy_top10, aes(x = reorder(`Type of Events`, 
                                                    -(`Crop Damages` + `Property Damages`)), 
                                                y = (`Crop Damages` + `Property Damages`)/1.e9,
                                                fill = (`Crop Damages` + `Property Damages`)/1.e9)) 

p <- p + scale_fill_continuous((guide = guide_legend(title = "Billions of $")))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + xlab("Natural Weather Events") 
p <- p + ylab('Total amount of damages, to Properties and Crops, across USA, in $')
print(p)



