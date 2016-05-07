setwd(dir = "/Users/joedibernardo/Projects/DATASCIENCE/ReproducibleResearch/week1/")
library(ggplot2)
if(!exists('activityData')){
  activityData <- read.csv(file = 'activity.csv', head = TRUE, sep = ",")
}
#str(activityData)

# let's split data into groups, and apply the sum function to data within each subgroup
# If TRUE (the default), then if FUN always returns a scalar, tapply returns an array 
# with the mode of the scalar. 
stepsXday <- tapply(activityData$steps, activityData$date, sum, na.rm = TRUE)

qplot(stepsXday, geom = 'histogram', xlab = 'Day', ylab = 'Total number of steps per day', binwidth = 500)
Mean <- mean(stepsXday)
Median <- median(stepsXday)
Mean
Median

StepsXfive <- aggregate(steps ~ interval, activityData, mean, na.rm = TRUE)

g <- ggplot(data = StepsXfive, aes(x = StepsXfive$interval, y = StepsXfive$steps)) 
g <- g + geom_line()
g <- g + xlab("Five minutes interval") 
g <- g + ylab('Total number of steps averaged across all days')
print(g)
#StepsXfive
#class(StepsXfive)

library(plyr) 
library(Hmisc)
#ddply(StepsXfive, ~interval, function(x){x[which.max(x$steps), ]})
MaxNrofSteps <- which.max(StepsXfive$steps)
TimeMaxSteps <- StepsXfive$interval[MaxNrofSteps]
#TimeMaxSteps
NA_tot <- length(which(is.na(activityData[,]) == TRUE))
NA_tot

activityData_fill <- activityData
activityData_fill[is.na(activityData_fill)] <- StepsXfive$steps
str(activityData_fill)
#str(activityData)
activityDataImputed <- activityData
activityDataImputed$steps <- impute(activityData$steps, fun=mean)
str(activityDataImputed)
head(activityData_fill)
head(StepsXfive$steps)

stepsXday_fill <- tapply(activityData_fill$steps, activityData_fill$date, sum, na.rm = TRUE)
qplot(stepsXday_fill, geom = 'histogram', xlab = 'Day', ylab = 'Total number of steps per day', binwidth = 500)
Mean_fill <- mean(stepsXday)
Median_fill <- median(stepsXday)
Mean_fill
Median_fill

activityData_fill$dateType <- as.Date(activityData_fill$date)
str(activityData_fill)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activityData_fill$wDay <- c('weekend', 'weekday')[(weekdays(activityData_fill$dateType) %in% weekdays1)+1L]


StepsXfive_fill <- aggregate(steps ~ interval + wDay, activityData_fill, mean, na.rm = TRUE)

g <- ggplot(data = StepsXfive_fill, aes(x = StepsXfive_fill$interval, y = StepsXfive_fill$steps)) 
g <- g + geom_line()
g <- g + facet_grid(wDay ~.) # to put the plots vertically each other
g <- g + xlab("Five minutes interval") 
g <- g + ylab('Total number of steps averaged across all days')
print(g)