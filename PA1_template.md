# Reproducible Research: Peer Assessment 1
## Setup options & libraries (always echo)


## Save data to hard drive

```r
rm(list = ls())

parent_folder = "/Users/minna/Documents/datasciencecoursera/reproducibleresearch_week2"
file_url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filename = "activity.zip"

folder = file.path(parent_folder, "data") 

# Create the data folder
if(!file.exists(folder)){dir.create(folder)}
dest_filename = paste(folder,filename, sep = "/")

# Download and unzip the file
if(!file.exists(dest_filename)){
  download.file(file_url,destfile= dest_filename)
  unzip(zipfile = dest_filename, exdir=folder)
}
```
## Load and preprocess data

```r
activity_data <- read.csv(gsub("zip", "csv", dest_filename))

# str(activity_data)
# summary(activity_data)

activity_data$date <- as.Date(activity_data$date , "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
# Total steps/day excluding NAs
daily_steps <- with(activity_data, tapply(steps, list(date), sum, na.rm = TRUE))
# daily_steps <- with(activity_data, aggregate(steps, list(date), sum, na.rm = TRUE))

qplot(daily_steps, main = "Histogram of daily steps", xlab = "Steps daily")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/meanstepsperday-1.png)<!-- -->

```r
avg_steps <- mean(daily_steps)
median_steps <- median(daily_steps)
```
The mean total number of steps per day is 9354.2295082 and the median is 10395.

## What is the average daily activity pattern?

```r
steps_across_days <- with(activity_data, aggregate(steps, list(interval), mean, na.rm = TRUE))

# Plot the activity
ggplot(steps_across_days, aes(x = Group.1, y = x)) + geom_line() + labs(title = "Average Daily Activity Pattern", x = "Minute of the Day", y = "Number of Steps")
```

![](PA1_template_files/figure-html/averagedailyactivity-1.png)<!-- -->

```r
# Find five minute segment with max steps
max_segment <- steps_across_days$Group.1[match(max(steps_across_days$x),steps_across_days$x)]
```

The five minute segment with the maximum average number of steps across all days is between 835 and 840

## Imputing missing values

```r
num_missing <- sum(is.na(activity_data$steps))

# Impute missing values with the average for the five minute interval across all days
# HELP: https://stat.ethz.ch/pipermail/r-help/2010-September/251998.html
# FUTURE: is there a way to do that that's not a for loop?
activity_data_imputed <- cbind(activity_data)
for (i in 1:nrow(activity_data_imputed)){
  if (is.na(activity_data_imputed$steps[i])==TRUE){
    activity_data_imputed$steps[i] <- steps_across_days$x[steps_across_days$Group.1 == activity_data_imputed$interval[i]]} }

# Daily steps
daily_steps_imputed <- with(activity_data_imputed, tapply(steps, list(date), sum, na.rm = TRUE))

qplot(daily_steps_imputed, main = "Histogram of daily steps with imputed data)", xlab = "Steps daily")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/impute-1.png)<!-- -->

```r
avg_steps_imputed <- mean(daily_steps_imputed)
median_steps_imputed <- median(daily_steps_imputed)
```

There are 2304 missing values in the dataset. Those were replaced by the mean for that five minute interval over all days.

The mean total number of steps per day (including imputed data) is 1.0766189\times 10^{4} and the median is 1.0766189\times 10^{4}.

Imputing NA values by replacing them with the interval average across all the days resulted in a significantly higher mean (difference of 1411.959171) and a higher median (difference of 371.1886792).

## Are there differences in activity patterns between weekdays and weekends?

```r
# Create a vector of weekdays
weekdays_vec <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

# Use `%in%` and `weekdays` to create a factor vector
activity_data_imputed$wday <- factor((weekdays(activity_data_imputed$date) %in% weekdays_vec), 
         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

# Average across weekday versus weekend
avg_steps2 <- with(activity_data_imputed, aggregate(steps, list(interval, wday), mean))

ggplot(avg_steps2, aes(x = Group.1, y = x)) + 
  geom_line() + 
  labs(title = "Average Activity Pattern", x = "Minute of the Day", y = "Number of Steps") +
  facet_grid(. ~ Group.2)
```

![](PA1_template_files/figure-html/weekdayvweekend-1.png)<!-- -->



