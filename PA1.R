

library(data.table)
library(dplyr)
library(stats)
library(lattice)
library(knitr)
fileUrl <- 
        "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, "dataset.zip")
unzip("dataset.zip")
data <- read.csv("./activity.csv", header = TRUE,na.strings = c("NA"),
        colClasses = c("numeric", "Date", "numeric"))


##remove NA, get sum of steps per day
data2 <- na.omit(data)
byday <- data2 %>% group_by(date) %>% summarize(sum = sum(steps))
##histogram of sum per day
ggplot(byday, aes(sum)) + geom_histogram() + 
        labs(title="Total number of steps taken each day", 
                x="total number of steps per day")

##Calculate and report the mean and median of the total number of steps 
##taken per day
mean(byday$sum)
#[1] 10766.19
median(byday$sum)
#[1] 10765

#What is the average daily activity pattern?
##Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval
##(x-axis) and the average number of steps taken, averaged across 
##all days (y-axis)
avgsteps <- data2 %>% group_by(interval) %>% summarize(avgsteps = mean(steps))
plot(avgsteps$interval,avgsteps$avgsteps, type="l", col="blue",lwd=2,
     main="Average daily activity pattern at each 5 min interval",
     xlab="5 min interval",
     ylab="Average steps")
dev.copy(png,file="avgdailyactivity.png")
dev.off()

##Which 5-minute interval, on average across all the days in the dataset, 
##contains the maximum number of steps?
x <- which(avgsteps$avgsteps==max(avgsteps$avgsteps))
avgsteps[x,]

#Imputing missing values
#Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.
#1. Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows with 𝙽𝙰s)
length(data[is.na(data)])

#2. Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the 
#mean/median for that day, or the mean for that 5-minute interval, etc.
#Strategy:
#Use mean of entire data set for single imputation of missing values of steps

#3. Create a new dataset that is equal to the original dataset but with the 
#missing data filled in.
data3 <- data
temp <- data3$steps
temp[is.na(temp)] <- mean(temp[!is.na(temp)])
data3$steps <- temp

#4. Make a histogram of the total number of steps taken each day and 
#Calculate and report the mean and median total number of steps taken per day. 
#Do these values differ from the estimates from the first part of the 
#assignment? What is the impact of imputing missing data on the estimates of 
#the total daily number of steps?

byday2 <- data3 %>% group_by(date) %>% summarize(sum = sum(steps))
##histogram of sum per day with single mean imputation
ggplot(byday2, aes(sum)) + geom_histogram() + 
        labs(title="Total number of steps taken each day with mean imputation", 
             x="total number of steps per day")

##Calculate and report the mean and median of the total number of steps 
##taken per day
mean(byday2$sum)
#[1] 10766.19
median(byday2$sum)
#[1] 10766.19

# Since we used the mean for the imputation strategy, the mean did not change,
# but the median increased from 10765 to 10766.19.
mean(byday$sum)
#[1] 10766.19
median(byday$sum)
#[1] 10765



#Are there differences in activity patterns between weekdays and weekends?

#For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. 
#Use the dataset with the filled-in missing values for this part.

#1. Create a new factor variable in the dataset with two levels – 
#“weekday” and “weekend” indicating whether a given date is a weekday 
#or weekend day.
weekend <- c('Saturday', 'Sunday')
data3$day <- factor((weekdays(data3$date) %in% weekend), 
                   levels=c(TRUE, FALSE), labels=c('weekend', 'weekday'))

#2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕")
#of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all weekday days or weekend days (y-axis). 
#See the README file in the GitHub repository to see an example of what this
#plot should look like using simulated data.

avgsteps2 <- data3 %>% group_by(day,interval) %>% summarize(avgsteps = mean(steps))
xyplot(avgsteps~interval | day,data=avgsteps2,layout=c(1,2), type="l",
       main="Average daily activity pattern at each 5 min interval",
       xlab="5 min interval",
       ylab="Average steps"
       )
#dev.copy(png,file="weekdayendplot.png")
#dev.off()

#Commit containing full submission

#Code for reading in the dataset and/or processing the data
#Histogram of the total number of steps taken each day
#Mean and median number of steps taken each day
#Time series plot of the average number of steps taken
#The 5-minute interval that, on average, contains the maximum number of steps
#Code to describe and show a strategy for imputing missing data
#Histogram of the total number of steps taken each day after missing values are 
#imputed
#Panel plot comparing the average number of steps taken per 5-minute interval 
#across weekdays and weekends
#All of the R code needed to reproduce the results (numbers, plots, etc.) in 
#the report.

## Directions for submission
# knit .Rmd file to HTML
# commit .Rmd and .html file to master branch
# commit .md file to master branch

