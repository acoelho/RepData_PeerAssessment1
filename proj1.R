library(chron)
library(lubridate)
library(dplyr)
library(ggplot2)

if (!file.exists("./activity.csv")) {
  if (!file.exists("./repdata-data-activity.zip")) {
    fURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fURL,destfile="repdata-data-activity.zip")
  }
  unzip("./repdata-data-activity.zip")
}

#Loading and preprocessing the data
data <- read.csv("activity.csv")

#What is mean total number of steps taken per day?
totalStepsPerDay <- aggregate(steps ~ date, data, sum)
hist(totalStepsPerDay$steps, labels=TRUE, main = "Total Steps per Day", xlab = "Steps")
mean(totalStepsPerDay$steps)
median(totalStepsPerDay$steps)

#What is the average daily activity pattern?
avgStepsByInterval <- aggregate(steps ~ interval, data, mean)
plot(avgStepsByInterval$interval, avgStepsByInterval$steps, type = "l" , xlab="Interval", ylab="Avg. steps")
maxStepsInterval <- avgStepsByInterval[avgStepsByInterval$steps == max(avgStepsByInterval$steps),][,"interval"]

#Imputing missing values
incompleteCases <- data[!complete.cases(data),]
length(incompleteCases$steps)

filleData <- data
#replacing NAs with mean for that interval
for (i in 1:length(filleData$steps)) {
    if (is.na(filleData[i,"steps"])) {
      filleData[i,"steps"] <- avgStepsByInterval[avgStepsByInterval$interval == filleData[i,"interval"],][,"steps"]
    }
    
}

totalStepsPerDayFil <- aggregate(steps ~ date, filleData, sum)
hist(totalStepsPerDayFil$steps, labels=TRUE, main = "Total Steps per Day", xlab = "Steps")
mean(totalStepsPerDayFil$steps)
median(totalStepsPerDayFil$steps)

#Are there differences in activity patterns between weekdays and weekends?
wkndData <- mutate(filleData, IsWeekend = gsub("FALSE", "weekday", gsub("TRUE", "weekend", is.weekend(date))))

avgStepsByIntAndWknd <- aggregate(steps ~ interval + IsWeekend, wkndData, mean)

print(qplot(interval, steps, data = avgStepsByIntAndWknd, facets = . ~ IsWeekend, geom=c("point", "line"), color = IsWeekend) +
  labs(title="Avg Steps by Interval and Weekend Statusr", x="Interval", y="Steps") +
  facet_wrap(facets=~IsWeekend, nrow = 2))

