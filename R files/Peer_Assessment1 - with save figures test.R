###################################################

### Coursera Data Science Specialization course ###
###   Reproducible Research - Peer Assigment 1  ###
        # SONEINT alias @Soc_Net_Intel #

###################################################

################################################################################
## A. Loading and preprocessing the data
# A.1 Load the data (i.e. read.csv())
# A.2 Process/transform the data (if necessary) into a format suitable for your 
# analysis
################################################################################

# Set the working directory on my local machine
setwd("~/Desktop/Repo/RepData_PeerAssessment1")

# Create a function called DownloadFile to download a file from a url with 
# method "curl" and name it activity_file
DownloadFile <- function(FileUrl, FileName) {
  if(!file.exists(FileName)) {
    download.file(FileUrl, destfile=FileName, method="curl")
  }
  FileName
}


# Create the useful directories
dir.create("R files")
dir.create("Figures")

library(ggplot2) # we shall use ggplot2 for plotting figures

# Create a function to unzip & read properly the file
read_data <- function() {
  setwd("~/Desktop/Repo/RepData_PeerAssessment1")  # Set the working directory 
  # on my local machine
  activity_file = "activity.zip" # name the file
  if (file.exists(activity_file)){
    unzip_file <- unz(activity_file, "activity.csv") # unzip the file
    activity_data <- read.csv(
      unzip_file, 
      header = TRUE, 
      colClasses = c("numeric", "character", "numeric")
      ) # Read the file 
    activity_data$interval <- factor(activity_data$interval)
    activity_data$date <- as.Date(activity_data$date, format="%Y-%m-%d")
  }
  else {
    FileName <- DownloadFile(
      "https://github.com/rdpeng/RepData_PeerAssessment1/blob/master/activity.zip", 
      "activity.zip")
    con <- unz(FileName, "activity.csv")
    activity_data <- read.csv(
      con, 
      header=TRUE, 
      colClasses=c("numeric", "character", "numeric")
      )
    close(con)
    activity_data$interval <- factor(activity_data$interval)
    activity_data$date <- as.Date(activity_data$date, format="%Y-%m-%d")
  }
  activity_data
}

# Read the data properly
activity_data <- read_data()

################################################################################
## B. What is mean total number of steps taken per day?
# B.1 Make a histogram of the total number of steps taken each day
# B.2 Calculate & report the mean and median total number of steps taken per day
################################################################################

# Check the data
summary(activity_data) # a good overview
head(activity_data) # begining of the data
tail(activity_data) # end of the data
str(activity_data) # str function is important (ref to R programming course)

# Create a function to compute the total number of steps taken per day
total_steps_per_day <- function(acticity_data) {
sum_steps_day <- aggregate(steps ~ date, activity_data, sum)
colnames(sum_steps_day) <- c("date", "steps")
sum_steps_day
}

# Create a function to plot an histogram of the total number of steps per day
plot_steps <- function(steps, mean, median) {
  col_names=c(paste("Mean:", mean), paste("Median:", median))
  cols = c("orangered", "darkblue")
  
  ggplot(steps, aes(x = steps)) + 
    geom_histogram(
      colour = "darkred", 
      fill = "darkolivegreen1", 
      binwidth = 1000) + 
    geom_point(
      aes(x = mean, y = 10, color = "darkgreen"), 
      size = 4, 
      shape = 4) + 
    geom_point(
      aes(x = median, y = 10, color = "olivedrab4"), 
      size = 4, 
      shape = 4) + 
    geom_segment(
      aes(x = mean, y = 0, xend = mean, yend = 10), 
      color="darkgreen", 
      linetype="dashed") +
    geom_segment(
      aes(x = median, y = 0, xend = median, yend = 10), 
      color="darkblue", 
      linetype="dashed" ) +
    geom_text(data = NULL, x = (median + 1500), y = 10, label = median) +
    scale_color_manual(name=element_blank(), labels=col_names, values=cols) +
    labs(
      title="Histogram of total number of steps taken per day", 
      x="Steps", 
      y="Frequency") + 
    theme_bw() + theme(legend.position = "bottom")
  
  # Save png file in Figures directory
  dev.copy(
    png, 
    filename = "~/Desktop/Repo/RepData_PeerAssessment1/Figures/plot_steps.png", 
    height = 600, 
    width = 800, 
    unit = "px", 
    bg = "transparent"
    )
  
  # Release screen
  dev.off()
}

# Compute the total number of steps per day
steps <- total_steps_per_day(activity_data)
# Compute the mean of the total number of steps per day
mean = round(mean(steps$steps), 2)
# Compute the median of the total number of steps per day
median = round(median(steps$steps), 2)

# Plot the whole values computed with the appropriate function
plot_steps(steps, mean, median)

################################################################################
# C.1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
# C.2 Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
################################################################################

# Create a function to split & compute the steps per interval
steps_interval <- function(activity_data) {
  steps_splitted_by_interval <- aggregate(
    activity_data$steps, 
    by=list(interval=activity_data$interval),
    FUN = mean, 
    na.rm = TRUE
    )
  # Compute interval to integers for plotting in ggplot
  steps_splitted_by_interval$interval <- as.integer(
    levels(steps_splitted_by_interval$interval)[steps_splitted_by_interval$interval]
    )
  colnames(steps_splitted_by_interval) <- c("interval", "steps")
  steps_splitted_by_interval
}

# Create a function to make the plot 
plot_steps_interval_pattern <- function(steps_per_interval, maxStepsInterval) {
  col_names=c(paste("5-min interval with maximum steps: ", maxStepsInterval))
  cols = c("red")
  
  ggplot(steps_per_interval, aes(x = interval, y = steps)) +   
    geom_line(color = "turquoise1", size = 1) +  
    geom_point(
      aes(x = maxStepsInterval, y = 220, color = "red"), 
      size = 4, 
      shape = 4) + 
    geom_segment(
      aes(x = maxStepsInterval, y = 0, xend = maxStepsInterval, yend = 220), 
      color = "orangered", 
      linetype = "dashed") +
    geom_text(
      data = NULL,
      x = (maxStepsInterval + 65),
      y = 220, 
      label = maxStepsInterval) +
    scale_color_manual(name = element_blank(), labels = col_names, values = cols) +     
    labs(
      title = "Average daily activity pattern of steps", 
      x = "Interval", 
      y = "Number of steps") +  
    theme_bw() + theme(legend.position = "bottom")
  
  # Save png file in Figures directory
  dev.copy(
    png, 
    filename = "~/Desktop/Repo/RepData_PeerAssessment1/Figures/plot_steps_interval_pattern.png", 
    height = 600, 
    width = 800, 
    unit = "px", 
    bg = "transparent"
  )
  
  # Release screen
  dev.off()
}

# Compute the number of steps per interval
steps_per_interval <- steps_interval(activity_data)

# Compute the maximum steps per interval
maxStepsInterval <- steps_per_interval[which.max(steps_per_interval$steps),]$interval

# Plot the results
plot_steps_interval_pattern(steps_per_interval, maxStepsInterval)


################################################################################
## D. Imputing missing values
# D.1 Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs).
# D.2 Devise a strategy for filling in all of the missing values in the dataset.
# The strategy does not need to be sophisticated.
# D.3 Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.
# D.4 Make a histogram of the total number of steps taken each day and Calculate
# and report the mean and median total number of steps taken per day. 
################################################################################

numberNAs <- nrow(activity_data)-nrow(activity_data[!complete.cases(activity_data),])

impute_means <- function(data, defaults) {
  na_indices <- which(is.na(activity_data$steps))
  defaults <- steps_per_interval
  na_replacements <- unlist(
    lapply(na_indices, FUN=function(idx){
    interval = activity_data[idx,]$interval
    defaults[defaults$interval == interval,]$steps
  }
  )
  )
  imp_steps <- activity_data$steps
  imp_steps[na_indices] <- na_replacements
  imp_steps
}

finalDataset <- data.frame(  
  steps = impute_means(activity_data, steps_per_interval),  
  date = activity_data$date,  
  interval = activity_data$interval)

summary(finalDataset)

complete_steps <- total_steps_per_day(finalDataset)
complete_mean = round(mean(complete_steps$steps), 2)
complete_median = round(median(complete_steps$steps), 2)

# D.4 Make the requested histogram with the previous function
plot_steps(complete_steps, complete_mean, complete_median)

# Save png file in Figures directory
dev.copy(
  png, 
  filename = "~/Desktop/Repo/RepData_PeerAssessment1/Figures/plot_steps_with_NA.png", 
  height = 600, 
  width = 800, 
  unit = "px", 
  bg = "transparent"
)

# Release screen
dev.off()

################################################################################
## E. Are there differences in activity patterns between weekdays & week-ends ?
# For this part the weekdays() function may be of some help here. 
# Use the dataset with the filled-in missing values for this part.
# E.1 Create a new factor variable in the dataset with two levels – “weekday” 
# and “weekend” indicating whether a given date is a weekday or weekend day.
# E.2 Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis).
################################################################################

# E.1 Create a function to compute the days of the week data
Day_Of_The_Week_data_computing <- function(data) {
  # Create the new factor variable for "weekday" and "weekend"
  data$Days_Of_The_Week <- as.factor(weekdays(data$date))
  # Note that days are in french
  Week_End_Data <- subset(data, Days_Of_The_Week %in% c("Samedi","Dimanche"))
  Week_Day_Data  <- subset(data, Days_Of_The_Week %in% c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi"))
  # Another way: Week_Day_Data <- subset(data, !Days_Of_The_Week %in% c("Samedi","Dimanche"))
  
  weekend_steps_interval <- steps_interval(Week_End_Data)
  weekday_steps_interval <- steps_interval(Week_Day_Data)
  
  weekend_steps_interval$weekday_or_weekend <- rep("weekend", nrow(weekend_steps_interval))
  weekday_steps_interval$weekday_or_weekend <- rep("weekday", nrow(weekday_steps_interval))
  
  Day_Of_The_Week_data <- rbind(weekend_steps_interval, weekday_steps_interval)
  Day_Of_The_Week_data$weekday_or_weekend <- as.factor(Day_Of_The_Week_data$weekday_or_weekend)
  Day_Of_The_Week_data
}

# E.2 Create the plot requested
plot_day_of_the_week <- function(data) {
  ggplot(data, 
         aes(x=interval, y=steps)) + 
    geom_line(color="dodgerblue", size=.5) + 
    facet_wrap(~ weekday_or_weekend, nrow=2, ncol=1) +
    labs(x="Interval", y="Number of steps") +
    theme_bw()
  
  # Save png file in Figures directory
  dev.copy(
    png, 
    filename = "~/Desktop/Repo/RepData_PeerAssessment1/Figures/plot_day_of_the_week.png", 
    height = 600, 
    width = 800, 
    unit = "px", 
    bg = "transparent"
  )
  
  # Release screen
  dev.off()
}


Day_Of_The_Week_data <- Day_Of_The_Week_data_computing(finalDataset)
plot_day_of_the_week(Day_Of_The_Week_data)


