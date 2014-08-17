# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
act=read.csv("activity.csv")
S=with(act,tapply(steps,date,sum,na.rm=T))
hist(S,main="",xlab="total number of steps",ylab="",col="yellow")

## What is mean total number of steps taken per day?
print(c("The mean is",round(mean(S))))
## [1] "The mean is" "9354"

print(c("The median is",median(S)))
## [1] "The median is" "10395"


## What is the average daily activity pattern?

T=with(act,tapply(steps,interval,mean,na.rm=T))
plot(names(T),T,type="l",xlab="time interval",ylab ="number of steps")  

which(T==max(T))
## 835 
## 104


T[which(T==max(T))]
##   835 
## 206.2


summary(act)


## Imputing missing values
act2 = act
for (k in 1:17568)
{if (is.na(act[k,"steps"])) act2[k,"steps"]= T[[as.character(act[k,"interval"])]]
}

S2=with(act2,tapply(steps,date,sum,na.rm=T))
hist(S,main="",xlab="total number of steps",ylab="",col="yellow")

print(c("The mean is",round(mean(S2))))
## [1] "The mean is" "10766"

print(c("The median is",round(median(S2))))
## [1] "The median is" "10766"


## Are there differences in activity patterns between weekdays and weekends?
act2$day=weekdays(as.Date(act2$date))
act2wday =subset(act2, ! day %in% c("Saturday","Sunday"))
act2wkend =subset(act2,  day %in% c("Saturday","Sunday"))


par(mfrow=c(2,1))
Tday=with(act2wday,tapply(steps,interval,mean))
plot(names(Tday),Tday,type="l",xlab="Weekday Activity",ylab ="number of steps")
Tend=with(act2wkend,tapply(steps,interval,mean))
plot(names(Tend),Tend,type="l",xlab="Weekend Activity",ylab ="number of steps")
