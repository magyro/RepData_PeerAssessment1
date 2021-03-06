
---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true

---


```r
rm(list=ls())
options(stringsAsFactors = FALSE)
options(digits=2)

Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
if (!is.element("ggplot2",installed.packages()) ) {install.packages("ggplot2")}
library(ggplot2)
```

## Loading and preprocessing the data


```r
if (!file.exists("activity.csv")) {unzip("repdata_data_activity.zip")}
dataAct <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
##      The total number of steps taken per day

dailyData <-data.frame(date=character(length(levels(as.factor( dataAct$date)))))
dailyData$date<-unique(dataAct$date)
dailyData$tot<-tapply(dataAct$steps,dataAct$date,sum,na.rm=TRUE)

##      Histogram of the total number of steps taken each day

#png(file="stepsPerDay.png", width = 480, height = 480)
q <- qplot(dailyData$tot, xlab="Total Number of Steps")
print(q)
```

![plot of chunk Mean per Day](figure/Mean per Day-1.png) 

```r
#dev.off()

##      The mean and median of the total number of steps taken per day     

dailyDataMean<-mean(dailyData$tot)

dailyDataMedian<-median(dailyData$tot)
```
The mean of the number of steps taken per day is 
9354.23  
The median of the number of steps taken per day is 
10395  

## What is the average daily activity pattern?


```r
##    plot of the 5-minute interval (x-axis) 
##    and the average number of steps taken, averaged across all days (y-axis)

avInterval<-tapply(dataAct$steps,as.factor(dataAct$interval),mean,na.rm=TRUE)
tmp<-cbind(levels(factor(dataAct$interval)),avInterval)

#png(file="dailyAverageSteps.png", width = 480, height = 480)
p<-plot (tmp[,1],tmp[,2],type="l",
         main="Daily Average No. of Steps per Time Interval",
         ylab="Av. no. of Steps",xlab="Time Interval")
```

![plot of chunk Average Daily](figure/Average Daily-1.png) 

```r
#print (p)
#dev.off()
maxNoSteps<-max(as.numeric(tmp[,2]))
intervalofMax<-tmp[match(maxNoSteps,tmp[,2]),1]
```
The 5-minute interval, on average across all the days in the dataset, containning the maximum number of steps is 835  

## Imputing missing values 


```r
naNum <- sum(!complete.cases(dataAct))
```
total number of missing cases is 2304  

Filling the missing values in the dataset is performed based on the average for that day, unless all the values of the day are NA's, than it is replaced by zero.


```r
##      replicate the data into a clean data frame
cleanDataAct<-dataAct

##      Calculate daily average per interval
dailyData$av <- tapply(dataAct$steps,as.factor(dataAct$date),mean,na.rm=TRUE)
##      if the average column contains NA's it is because the input to 
##      the mean function is all NA's.
##      In that case it should be 0 as the total function
dailyData$av[is.na(dailyData$av)]<-0

##      Calculate the indices of NA values in steps vector
naInd<-which(is.na(cleanDataAct$steps))
cleanDataAct$steps[naInd]<-dailyData$av[cleanDataAct$date[naInd]]

##     Plot a histogram of the total number of steps taken each day
dailyData$totClean<-tapply(cleanDataAct$steps,as.factor(cleanDataAct$date),sum)
#png(file="totalNoOfSteps.png", width = 480, height = 480)
r<- qplot (dailyData$totClean, xlab="Total Number of Daily Steps",
           binwidth = 2200, fill=I("red"))
print(r)
```

![plot of chunk Filling of  missing values](figure/Filling of  missing values-1.png) 

```r
#dev.off()

##     The mean and median total number of steps taken per day

dailyDataMeanClean<-mean(dailyData$totClean)
dailyDataMedianClean<-median(dailyData$totClean)
```

The mean of the clean data is 9354.23  
The median of the clean data is 1.04 &times; 10<sup>4</sup>     


```r
##     Do these values differ from the estimates from the first part of the 
##     assignment? 
##     ______________________________________________________________________

dailyDataMeanDIff<- dailyDataMeanClean - dailyDataMean
dailyDataMedianDiff<- dailyDataMedianClean - dailyDataMedian
```
The difference of the mean without NA's is 0   
The difference of the median without NA's is 0



```r
##     What is the impact of imputing missing data on the 
##     estimates of the total daily number of steps? 
dailyDataTotDIff<- sum(dailyData$totClean) - sum(dailyData$tot)
```
The impact of inputing missing data on the total daily estimates, by the method of using daily average when exists and 0 when missing for that day,  is 
0




## Are there differences in activity patterns between weekdays and weekends?


```r
##      Create a new factor variable in the dataset with two levels
##       � �weekday� and �weekend� indicating whether a given date is a weekday
##        or weekend day.
wkedays<-c("Saturday","Sunday")
cleanDataAct$dayType<-"weekday"
wkeDaysInd<-
       weekdays(as.Date(cleanDataAct$date)) %in% wkedays
cleanDataAct$dayType[wkeDaysInd]<-"weekend"
cleanDataAct$dayType <- as.factor(cleanDataAct$dayType)
l<-levels(cleanDataAct$dayType)
```
New factor variable in the dataset with two levels: weekday, weekend


```r
## Make a panel plot containing a time series plot (i.e. type = "l") 
## of the 5-minute interval (x-axis) and the average number of steps taken, 
## averaged across all weekday days or weekend days (y-axis).


#prepare dataframe cleanAV for plotting 
#
## first cleanAv has the average number of steps taken, 
## averaged across all weekday days 
## and cleanAve has the average number of steps taken, 
## averaged across  all weekend days.
## Than they are combined in cleanAv
#
cleanAv<-data.frame(interval=integer
                    (length(levels(as.factor( cleanDataAct$interval)))))
cleanAv$interval<-levels(factor(cleanDataAct$interval))
cleanAv$wType<-as.factor("weekday")
cleanAve<-data.frame(interval=integer
                     (length(levels(as.factor( cleanDataAct$interval)))))
cleanAve$interval<-levels(factor(cleanDataAct$interval))
cleanAve$wType<-as.factor("weekend")

#prepare weekday data for ploting in cleanAv dataframe
df<-data.frame(wSteps=
                       numeric(length(which(cleanDataAct$dayType == "weekday"))))
df$wSteps<-cleanDataAct$steps[which(cleanDataAct$dayType=="weekday")]
df$wInterval<-cleanDataAct$interval[which(cleanDataAct$dayType=="weekday")]

cleanAv$av<-tapply(df$wSteps,as.factor(df$wInterval),mean,na.rm=TRUE)

#prepare weekend data for ploting in auxiliary
de<-data.frame(wSteps=
                       numeric(length(which(cleanDataAct$dayType == "weekend"))))
de$wSteps<-cleanDataAct$steps[which(cleanDataAct$dayType=="weekend")]
de$wInterval<-cleanDataAct$interval[which(cleanDataAct$dayType=="weekend")]

cleanAve$av<-tapply(de$wSteps,as.factor(de$wInterval),mean,na.rm=TRUE)

cleanAv<-rbind(cleanAv, cleanAve)
cleanAv$interval<-as.numeric(cleanAv$interval)


# plot
#png(file="difWeekdaysWeekends.png", width = 480, height = 480)
#par(mar = c(4, 4, 4, 4)) 

f<-ggplot(cleanAv,aes(interval, av ))
f<-f+geom_line(aes(group=wType, color = wType))
        
f<-f+facet_grid(wType~.)
f<-f+labs(y="Number of Steps")
      
print (f)
```

![plot of chunk difference wkdays and wkends 2](figure/difference wkdays and wkends 2-1.png) 

```r
#dev.off()
```

