
Reproducible Research Assignment 1
====================================
### Introduction
This file consist multiple parts for Activity Monitoring Data which is about personal movement using activity monitoring device.This device collects data at 5 minute intervals through out the day.The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


```{r loading}
library(knitr)
opts_knit$set(root.dir=normalizePath('../'))
opts_chunk$set(fig.path = "../figures/")
data=read.csv("activity.csv")
Sys.setlocale("LC_TIME", "English")
library(lattice)
str(data)
```
###Calculating  mean total number of steps taken per day

```{r mean,fig.height=4}
sums=aggregate(steps~ date,data,sum)
hist(sums$steps,xlab=" ",main="Sum of steps taken per day",col="blue")
data.frame(mean=mean(sums$steps),median=median(sums$steps))
```
 
###The average daily activity pattern
```{r,fig.height=4}
 averagedata=aggregate(steps~ interval,data,mean)
xyplot(steps~interval,data=averagedata,type="l",ylab="averaged across all days",xlab="5-minute interval")

averagedata$interval[which.max(averagedata$steps)]
```
###Imputing missing values

```{r missing,fig.height=4}
sum(sapply(data$steps,is.na))

means=aggregate(steps~ date,data,mean)
means$date=as.character(means$date)
datafilled=data

for (i in 1:length(means$steps))
{ 
  if((sum(sapply(datafilled$steps[datafilled$date==means$date[i]],is.na)))!=0) {
    datafilled$steps[datafilled$date==means$date[i]][is.na(datafilled$steps[datafilled$date==means$date[i]])]=means$steps[i]}
  
}
datafilled$steps[is.na(datafilled$steps)]=0
sums1=aggregate(steps~ date,datafilled,sum)
hist(sums1$steps,xlab=" ",main="Sum of steps taken per day for filled data",col="blue")
data.frame(mean=mean(sums1$steps),median=median(sums1$steps))
```
Mean and meadian values found from filled data set are different from their previous values.Since  missing values in each day  have been replaced with mean for that day and zero for missing days, the mean of the total number of steps have decreased ( missing values have been ignored for previous calculations of mean and median therefore missing days have been ignored).

###Differences in activity patterns between weekdays and weekends

```{r weeks}
datafilled$date=as.character(datafilled$date)
datafilled$date=as.Date(datafilled$date)
day=weekdays(datafilled$date)
days=ifelse(day=="Saturday" | day=="Sunday",TRUE,FALSE)
days[days=="TRUE"]="weekend"
days[days=="FALSE"]="weekday"
datafilled$days=as.factor(days)
averaged1=aggregate(steps~ interval,subset(datafilled,days=="weekday"),mean)
averaged2=aggregate(steps~ interval,subset(datafilled,days=="weekend"),mean)
```

###plotting filled data set 

```{r plots,fig.height=4}
new=rbind(averaged1,averaged2)
new$day=rep(c("weekday","weekend"),each=288)
new$day=as.factor(new$day)
xyplot(steps ~ interval | day,data=new,type="l",layout=c(1,2),xlab="Interval",ylab="Number of steps")
```






