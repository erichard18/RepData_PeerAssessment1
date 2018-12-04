Load the data
-------------

    #Load the data 
    unzip(zipfile="activity.zip")
    activity_data <- read.csv("activity.csv", header = TRUE)

What is mean total number of steps taken per day?
-------------------------------------------------

    # Calculate the total number of steps taken per day
    steps.date <- aggregate(steps~date,activity_data,sum)
    # Make a histogram of the total number of steps taken each day
    hist(steps.date$steps, xlab="Total Steps per Day", ylab="Frequency [Days]")

![](PA1_template_1_files/figure-markdown_strict/steps-1.png)

    #Calculate and report the mean and median of the total number of steps taken per day
    meansteps <- mean(steps.date$steps, na.rm=TRUE)
    meansteps

    ## [1] 10766.19

    mediansteps <- median(steps.date$steps, na.rm=TRUE)
    mediansteps

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

    # Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
    steps.interval <- aggregate(steps ~ interval, data=activity_data, FUN=mean)
    plot(steps.interval, type="l",xlab="Interval (per 5 min)", ylab="Average Daily Activity Pattern")

![](PA1_template_1_files/figure-markdown_strict/activitypattern-1.png)

    # Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
    steps.interval$interval[which.max(steps.interval$steps)]

    ## [1] 835

Impute missing values
---------------------

    # report the total number of missing values in the data
    sum(is.na(activity_data))

    ## [1] 2304

    # fill in missing values - using the mean per 5 minute interval - and create a new dataset that has all missing values filled in
    activity_data.new = merge(activity_data, steps.interval, by="interval")
    activity_data.new$steps.x[is.na(activity_data.new$steps.x)] = activity_data.new$steps.y[is.na(activity_data.new$steps.x)]
    # make a histogram of the total number of steps taken using the new dataset
    activity_data.new <- aggregate(steps.x~interval,activity_data.new,sum)
    hist(activity_data.new$steps.x, xlab="Total Steps per Day", ylab="Frequency [Days]")

![](PA1_template_1_files/figure-markdown_strict/missing%20values-1.png)

    # calculate the mean and median of the new dataset
    meansteps_new <- mean(activity_data.new$steps, na.rm=TRUE)
    meansteps_new

    ## [1] 2280.339

    mediansteps_new <- median(activity_data.new$steps, na.rm=TRUE)
    mediansteps_new

    ## [1] 2080.906

Are there differences in activity level between weekdays and weekends?
----------------------------------------------------------------------

    # Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
    daytype <- function(date) {
        if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
            "Weekend"
        } else {
            "Weekday"
        }
    }
    activity_data$daytype <- as.factor(sapply(activity_data$date, daytype))
    str(activity_data)

    ## 'data.frame':    17568 obs. of  4 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ##  $ daytype : Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...

    # Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
    par(mfrow=c(2,1))
    for (type in c("Weekend", "Weekday")) {
        steps.type <- aggregate(steps ~ interval,
                                data=activity_data,
                                subset=activity_data$daytype==type,
                                FUN=mean)
        plot(steps.type, type="l", main=type)
    }

![](PA1_template_1_files/figure-markdown_strict/weekday/weekend-1.png)
