setwd("C:/Users/asus/Desktop/30.10.2014/coursera")
Sys.setlocale("LC_TIME", "English")
data=read.csv("activity.csv",stringsAsFactors=TRUE)
# Calculating  mean total number of steps taken per day
sums=aggregate(steps~ date,data,sum) 
hist(sums$steps,xlab=" ",main="Sum of steps taken per day",col="blue")
data.frame(mean=mean(sums$steps),median=median(sums$steps))
#the average daily activity pattern
averagedata=aggregate(steps~ interval,data,mean)
xyplot(steps~interval,data=averagedata,type="l",ylab="averaged across all days",xlab="5-minute interval")
averagedata$interval[which.max(averagedata$steps)]
#Imputing missing values
sum(sapply(data$steps,is.na))#total NA in the data set
means=aggregate(steps~ date,data,mean)# aggregate func. automatically ignore missing values 
means$date=as.character(means$date)
datafilled=data
# NA's in each day  have been replaced with mean for that day
for (i in 1:length(means$steps))
{ 
  if((sum(sapply(datafilled$steps[datafilled$date==means$date[i]],is.na)))!=0) {
    datafilled$steps[datafilled$date==means$date[i]][is.na(datafilled$steps[datafilled$date==means$date[i]])]=means$steps[i]}
  
}
# zero assigned for missing dates
datafilled$steps[is.na(datafilled$steps)]=0
sums1=aggregate(steps~ date,datafilled,sum)
hist(sums1$steps,xlab=" ",main="Sum of steps taken per day for filled data",col="blue")
data.frame(mean=mean(sums1$steps),median=median(sums1$steps))

#differences in activity patterns between weekdays and weekends
datafilled$date=as.character(datafilled$date)
datafilled$date=as.Date(datafilled$date)
day=weekdays(datafilled$date)
days=ifelse(day=="Saturday" | day=="Sunday",TRUE,FALSE)
days[days=="TRUE"]="weekend";days[days=="FALSE"]="weekday";
datafilled$days=as.factor(days)
averaged1=aggregate(steps~ interval,subset(datafilled,days=="weekday"),mean)
averaged2=aggregate(steps~ interval,subset(datafilled,days=="weekend"),mean)
new=rbind(averaged1,averaged2)
new$day=rep(c("weekday","weekend"),each=288)
new$day=as.factor(new$day)
xyplot(steps ~ interval | day,data=new,type="l",layout=c(1,2),xlab="Interval",ylab="Number of steps")
