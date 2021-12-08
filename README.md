# Cyclist-Data
Google Cyclist Data Project 


```{r eval=FALSE}
# Setting up the work space

install.packages("tidyverse")
install.packages("dplyr")
install.packages("janitor")
library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)
library(sqldf)
library(dplyr)
```
```{r eval=FALSE}
# Setting up the work space

install.packages("tidyverse")
install.packages("dplyr")
install.packages("janitor")
library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)
library(sqldf)
library(dplyr)

# Checking the summary structure of the data frames using str() function

str(cyclist_03)
str(cyclist_04)
str(cyclist_05)
str(cyclist_06)
str(cyclist_07)
str(cyclist_08)
str(cyclist_09)
str(cyclist_10)
str(cyclist_11)
str(cyclist_12)


# Before merging Data frames, we need to look at column names and make sure they are consistent

colnames(cyclist_03)
colnames(cyclist_04)
colnames(cyclist_05)
colnames(cyclist_06)
colnames(cyclist_07)
colnames(cyclist_08)
colnames(cyclist_09)
colnames(cyclist_10)
colnames(cyclist_11)
colnames(cyclist_12)


# Combine all data sets using rbind()

Mastertable <- rbind(`cyclist_03`,
                     `cyclist_04`,
                     `cyclist_05`,
                      `cyclist_06`,
                     `cyclist_07`,
                     `cyclist_08`,
                     `cyclist_09`,
                     `cyclist_10`,
                      `cyclist_11`,
                     `cyclist_12`)

```
```{r eval=FALSE}
# Add columns that list the date, month, day, year and day_of_week for each ride. 
​
Mastertable$date <- as.Date(Mastertable$started_at)
Mastertable$month <- format(as.Date(Mastertable$date), "%m")
Mastertable$day <- format(as.Date(Mastertable$date), "%d")
Mastertable$year <- format(as.Date(Mastertable$date), "%Y")
Mastertable$day_of_week <- format(as.Date(Mastertable$date), "%A")
​
# Create a season column using lubridate 
​
Mastertable$season <- case_when(
  month(Mastertable$started_at) == 12 | month(Mastertable$started_at) == 1 |
  month(Mastertable$started_at) == 2 ~'Winter' ,
  month(Mastertable$started_at) == 3 | month(Mastertable$started_at) == 4 |
    month(Mastertable$started_at) == 5  ~'Spring' ,
  month(Mastertable$started_at) == 6 | month(Mastertable$started_at) == 7 |
    month(Mastertable$started_at) == 8 ~'Summer' ,
  month(Mastertable$started_at) == 9 | month(Mastertable$started_at) == 10 |
    month(Mastertable$started_at) == 11 ~'Fall')
​
# Add a ride length calculation to all trips in seconds. 
​
Mastertable$ride_length <- difftime(Mastertable$ended_at,Mastertable$started_at)
​
​
# Let's remove any duplicates from the data set 
​
Mastertable %>%distinct() 
Mastertable <- Mastertable[!(Mastertable$ride_length<=0),]
​
# Remove "bad data"
​
skim(Mastertable$started_at)
skim(Mastertable$ended_at)
skim(Mastertable$ride_length)
​
```

```{r eval=FALSE}
# Let's review to verify all columns are correct
​
summary(Mastertable)                        
glimpse(Mastertable)
​
# Mean, Max and Min of ride_length
​
Mastertable %>%
  summarize(mean(ride_length))
​
Mastertable %>%
  summarize(max(ride_length))
​
Mastertable %>%
  summarize(min(ride_length))
​
# Average ride duration for users by day of week 
​
Mastertable %>%
  group_by(day_of_week) %>%
  summarize(mean(ride_length))
​
# Average ride duration for members and casual riders 
​
Mastertable %>%
  group_by(member_casual) %>%
  summarize(mean(ride_length))
​
# Ride duration summary 
​
Mastertable %>%
  group_by(member_casual) %>%
  summarize(number_of_rides = n(),
            average_ride_length = mean(ride_length),
            median_ride_length = median(ride_length))
​
# Ride duration summary by month 
Mastertable %>%
  group_by(member_casual,month) %>%
  summarize(number_of_rides = n(),
            average_ride_length = mean(ride_length),
            median_ride_length = median(ride_length))
​
# Ride duration summary by day of week 
​
Mastertable %>%
  group_by(member_casual,day_of_week) %>%
  summarize(number_of_rides =n(),
    average_ride_length = mean(ride_length),
    median_ride_length = median(ride_length))
​
​
# Ride duration summary by season 
​
Mastertable %>%
  group_by(member_casual,season) %>%
  summarize(number_of_rides =n(),
            average_ride_length = mean(ride_length),
            median_ride_length = median(ride_length)) %>%
  mutate(percent_of_rides = round(number_of_rides*100/sum(number_of_rides), 2)) %>%
mutate(percent_of_rides = paste0(percent_of_rides, '%')) %>%
  select(member_casual,season,number_of_rides,percent_of_rides,
         average_ride_length,median_ride_length)
​
# Bike type usage 
​
Mastertable %>%
  group_by(member_casual,rideable_type) %>%
  summarize(number_of_rides =n(),
            average_ride_length = mean(ride_length),
            median_ride_length = median(ride_length)) %>%
  mutate(percent_of_rides = round(number_of_rides*100/sum(number_of_rides), 2)) %>%
  mutate(percent_of_rides = paste0(percent_of_rides, '%')) %>%
  select(member_casual,rideable_type,number_of_rides,percent_of_rides,
         average_ride_length,median_ride_length)
​
# Bike type usage by season 
​
Mastertable %>%
  group_by(member_casual,rideable_type,season) %>%
  summarize(number_of_rides =n(),
            average_ride_length = mean(ride_length),
            median_ride_length = median(ride_length)) %>%
  arrange(member_casual,rideable_type,season)
​
# Visualization for number of rides by rider type 
​
Mastertable %>%
  mutate(weekday = wday (started_at, label = TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x= weekday, y = number_of_rides, fill = member_casual)) +
           geom_col(position = "dodge") +
           labs(title = "Number of Rides by Day and Rider Type",
                subtitle = "Members versus Casual Riders") +
           ylab("Number of Rides") +
           xlab ("Day of Week")
```
add Codeadd Markdown
```{r eval=FALSE}
# Visualization for average duration 
​
Mastertable %>%
  mutate(weekday = wday (started_at, label = TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarize(average_duration = mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x= weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Duration of Rides by Days and Rider Type",
       subtitle = "Members versus Casual Riders") +
  ylab("Average Duration of Rides") +
  xlab ("Day of Week")
​
# Export a summary file for further analysis. 
​
count <- aggregate(Mastertable$ride_length~Mastertable$member_casual+Mastertable$day_of_week,FUN = mean)
write.csv(count, file ='~/Desktop/Tableau/"avg_ride_length_by_days.csv')
​
```

```{r eval=FALSE}
# Add columns that list the date, month, day, year and day_of_week for each ride. 
​
Mastertable$date <- as.Date(Mastertable$started_at)
Mastertable$month <- format(as.Date(Mastertable$date), "%m")
Mastertable$day <- format(as.Date(Mastertable$date), "%d")
Mastertable$year <- format(as.Date(Mastertable$date), "%Y")
Mastertable$day_of_week <- format(as.Date(Mastertable$date), "%A")
​
# Create a season column using lubridate 
​
Mastertable$season <- case_when(
  month(Mastertable$started_at) == 12 | month(Mastertable$started_at) == 1 |
  month(Mastertable$started_at) == 2 ~'Winter' ,
  month(Mastertable$started_at) == 3 | month(Mastertable$started_at) == 4 |
    month(Mastertable$started_at) == 5  ~'Spring' ,
  month(Mastertable$started_at) == 6 | month(Mastertable$started_at) == 7 |
    month(Mastertable$started_at) == 8 ~'Summer' ,
  month(Mastertable$started_at) == 9 | month(Mastertable$started_at) == 10 |
    month(Mastertable$started_at) == 11 ~'Fall')
​
# Add a ride length calculation to all trips in seconds. 
​
Mastertable$ride_length <- difftime(Mastertable$ended_at,Mastertable$started_at)
​
​
# Let's remove any duplicates from the data set 
​
Mastertable %>%distinct() 
Mastertable <- Mastertable[!(Mastertable$ride_length<=0),]
​
# Remove "bad data"
​
skim(Mastertable$started_at)
skim(Mastertable$ended_at)
skim(Mastertable$ride_length)
​
```
add Codeadd Markdown
```{r eval=FALSE}
# Let's review to verify all columns are correct
​
summary(Mastertable)                        
glimpse(Mastertable)
​
# Mean, Max and Min of ride_length
​
Mastertable %>%
  summarize(mean(ride_length))
​
Mastertable %>%
  summarize(max(ride_length))
​
Mastertable %>%
  summarize(min(ride_length))
​
# Average ride duration for users by day of week 
​
Mastertable %>%
  group_by(day_of_week) %>%
  summarize(mean(ride_length))
​
# Average ride duration for members and casual riders 
​
Mastertable %>%
  group_by(member_casual) %>%
  summarize(mean(ride_length))
​
# Ride duration summary 
​
Mastertable %>%
  group_by(member_casual) %>%
  summarize(number_of_rides = n(),
            average_ride_length = mean(ride_length),
            median_ride_length = median(ride_length))
​
# Ride duration summary by month 
Mastertable %>%
  group_by(member_casual,month) %>%
  summarize(number_of_rides = n(),
            average_ride_length = mean(ride_length),
            median_ride_length = median(ride_length))
​
# Ride duration summary by day of week 
​
Mastertable %>%
  group_by(member_casual,day_of_week) %>%
  summarize(number_of_rides =n(),
    average_ride_length = mean(ride_length),
    median_ride_length = median(ride_length))
​
​
# Ride duration summary by season 
​
Mastertable %>%
  group_by(member_casual,season) %>%
  summarize(number_of_rides =n(),
            average_ride_length = mean(ride_length),
            median_ride_length = median(ride_length)) %>%
  mutate(percent_of_rides = round(number_of_rides*100/sum(number_of_rides), 2)) %>%
mutate(percent_of_rides = paste0(percent_of_rides, '%')) %>%
  select(member_casual,season,number_of_rides,percent_of_rides,
         average_ride_length,median_ride_length)
​
# Bike type usage 
​
Mastertable %>%
  group_by(member_casual,rideable_type) %>%
  summarize(number_of_rides =n(),
            average_ride_length = mean(ride_length),
            median_ride_length = median(ride_length)) %>%
  mutate(percent_of_rides = round(number_of_rides*100/sum(number_of_rides), 2)) %>%
  mutate(percent_of_rides = paste0(percent_of_rides, '%')) %>%
  select(member_casual,rideable_type,number_of_rides,percent_of_rides,
         average_ride_length,median_ride_length)
​
# Bike type usage by season 
​
Mastertable %>%
  group_by(member_casual,rideable_type,season) %>%
  summarize(number_of_rides =n(),
            average_ride_length = mean(ride_length),
            median_ride_length = median(ride_length)) %>%
  arrange(member_casual,rideable_type,season)
​
# Visualization for number of rides by rider type 
​
Mastertable %>%
  mutate(weekday = wday (started_at, label = TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x= weekday, y = number_of_rides, fill = member_casual)) +
           geom_col(position = "dodge") +
           labs(title = "Number of Rides by Day and Rider Type",
                subtitle = "Members versus Casual Riders") +
           ylab("Number of Rides") +
           xlab ("Day of Week")
```

```{r eval=FALSE}
# Visualization for average duration 

Mastertable %>%
  mutate(weekday = wday (started_at, label = TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarize(average_duration = mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x= weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Duration of Rides by Days and Rider Type",
       subtitle = "Members versus Casual Riders") +
  ylab("Average Duration of Rides") +
  xlab ("Day of Week")

# Export a summary file for further analysis. 

count <- aggregate(Mastertable$ride_length~Mastertable$member_casual+Mastertable$day_of_week,FUN = mean)
write.csv(count, file ='~/Desktop/Tableau/"avg_ride_length_by_days.csv')

```
