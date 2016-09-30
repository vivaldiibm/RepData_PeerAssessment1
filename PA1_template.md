PEER REVIEW ASSIGNMENT 1 - REPRODUCIBLE RESEARCH.
===================================================

### Set working directory and read the data.
```{r}
setwd("E:/coursera course/Course5 - Reproducible Research/Wk2")
data <- read.csv("activity.csv")
data$date <- as.Date(as.character(data$date, format = "%d/%m/%Y"))
library(ggplot2)
```

## Question 1: What is mean total number of steps taken per day?
```{r}
# What is mean total number of steps taken per day?

## Calculate the total number of steps taken per day?

totalstepaday <- aggregate(steps ~ date, data = data, sum, na.rm= TRUE)
mean(totalstepaday$steps)
summary(totalstepaday$steps)

## Make a histogram of the total number of steps taken each day.
hist (totalstepaday$steps, main = "Total step a day", xlab = "steps")
```

## Question 2:What is the average daily activity pattern?
```{r}
# What is the average daily activity pattern?
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all days (y-axis)?

stepbyinterval <- aggregate(steps ~ interval, data = data, sum, na.rm=TRUE)


stepbyinterval2 <- aggregate(steps ~ interval, data = data, mean, na.rm=TRUE)

plot(stepbyinterval2$interval, stepbyinterval2$steps, type ="l", xlab = "interval",
     ylab = "average number of steps", main = "Average number of steps over all days")
```

## Question 3: Imputing missing values

```{r}
### calculating the max interval.
maxinterval2 <- which.max(stepbyinterval2$steps)
stepbyinterval2[maxinterval2,]

### Imputing missing values.

missingvalue <- table(is.na(data))
missingvalue

# Devise a strategy for filling in all of the missing values in the dataset. The strategy
# does not need to be sophisticated. For example, you could use the mean/median for that day,
# or the mean for that 5-minute interval, etc.

data2 <- data
stepbyinterval <- aggregate(steps ~ interval, data = data, mean)
for (i in 1:nrow(data2)) {
        if (is.na(data2$steps[i])) {
     averagestep <- stepbyinterval$steps[which(stepbyinterval$interval == data2$interval[i])] 
     data2$steps[i] <- averagestep
        }
}

### Calculate mean and median total number of steps taken a day

avgstepaday <- aggregate(steps ~ date, data = data2, sum) 
mean(avgstepaday$steps)
median(avgstepaday$steps)

### Histogram total steps taken a day.

hist(avgstepaday$steps, main = "Total steps taken a day", xlab = "steps")
```

## Question 4:Are there differences in activity patterns between weekdays and weekends?
### creating new factor variable.
```{r}

data$day <- weekdays(as.Date(data$date))
daycat <- function(day) {
      if (!(data$day == "Sunday" || day == "Saturday")) {
      wkday <- 'weekday'
     } 
        else {
         wkday <- 'weekend'
     }
 wkday
}

# apply daycat function.
data$day2 <- as.factor(sapply(data$day, daycat))

# Calculate steps taken a day
step <- aggregate(steps ~ interval+day2, data = data, mean, na.rm=TRUE)

par(mfrow=c(2,1))

ggplot(step, aes (interval, steps)) + 
    geom_line(stat = "identity", aes(colour = day2)) +
    facet_wrap(~ day2, ncol=1) +
    ggtitle ("Average number of steps taken during weekdays days and weekend days")
```