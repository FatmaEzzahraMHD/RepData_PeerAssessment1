#reproductible research 
#week 2 project 
# get data
#1-Code for reading in the dataset and/or processing the data
data <- read.csv(unzip("./repdata_data_activity.zip"))

#2-Histogram of the total number of steps taken each day
library(dplyr)
library(ggplot2)
steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(steps,main = "total number of steps taken each day",  xlab = "number of step" 
      , ylab = "count"  )
dev.copy(png,'plot1.png')
dev.off()
#3-Mean and median number of steps taken each day

mean_steps <- mean(steps)
median_steps <- median(steps)
#4-Time series plot of the average number of steps taken
avg  <- aggregate(x=list(steps_number=data$steps), by=list(interval=data$interval),
                             FUN=mean, na.rm=TRUE)
ggplot(avg, aes(x=interval, y=steps_number))+ geom_line()
dev.copy(png,'plot2.png')
dev.off()

#5-The 5-minute interval that, on average, contains the maximum number of steps
max_interval <- avg[which.max(avg$steps_number),]

#6-Code to describe and show a strategy for imputing missing data
missing <- is.na(data$steps)
# How many missing
table(missing)
replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
complete_data <- data%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(complete_data)

#7-Histogram of the total number of steps taken each day after missing values are imputed
steps_with_no_missings <- tapply(complete_data$steps, complete_data$date, FUN=sum)
qplot(steps_with_no_missings,binwidth = 600, xlab="total number of steps taken each day")
dev.copy(png,'plot3.png')
dev.off()
mean2 <- mean(steps_with_no_missings)
median2 <- median(steps_with_no_missings)
#8-Panel plot comparing the average number of steps taken per 5-minute interval
#across weekdays and weekends
which_day <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
complete_data$date<- as.Date(complete_data$date)
complete_data$day <- sapply(complete_data$date, FUN=which_day)

averages <- aggregate(steps ~ interval + day, data=complete_data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps") +
  ggtitle("Comparison of Average Number of Steps in Each Interval")
dev.copy(png,'plot4.png')
dev.off()
