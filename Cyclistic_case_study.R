##Checking file directory
getwd()

##Installing necessary packages and libraries
install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("dplyr")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(janitor)
library(dplyr)
library(ggplot2)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

##Importing the data
trip24_April <- read_csv("Original data .csv/202404-divvy-tripdata.csv")
trip24_May <- read_csv("Original data .csv/202405-divvy-tripdata.csv")
trip24_June <- read_csv("Original data .csv/202406-divvy-tripdata.csv")
trip24_July <- read_csv("Original data .csv/202407-divvy-tripdata.csv")
trip24_August <- read_csv("Original data .csv/202408-divvy-tripdata.csv")
trip24_September <- read_csv("Original data .csv/202409-divvy-tripdata.csv")
trip24_October <- read_csv("Original data .csv/202410-divvy-tripdata.csv")
trip24_November <- read_csv("Original data .csv/202411-divvy-tripdata.csv")
trip24_December <- read_csv("Original data .csv/202412-divvy-tripdata.csv")
trip25_January <- read_csv("Original data .csv/202501-divvy-tripdata.csv")
trip25_February <- read_csv("Original data .csv/202502-divvy-tripdata.csv")
trip25_March <- read_csv("Original data .csv/202503-divvy-tripdata.csv")

##Checking for consistent column names to proceed in combining of data.frames
colnames(trip24_April)
colnames(trip24_May)
colnames(trip24_June)
colnames(trip24_July)
colnames(trip24_August)
colnames(trip24_September)
colnames(trip24_October)
colnames(trip24_November)
colnames(trip24_December)
colnames(trip25_January)
colnames(trip25_February)
colnames(trip25_March)

##Combining 12 data.frames into 1
all_trips <- rbind(trip24_April, trip24_May, trip24_June, trip24_July, trip24_August, trip24_September,
  trip24_October,trip24_November, trip24_December, trip25_January, trip25_February, trip25_March)

colnames(all_trips)
View(all_trips)
str(all_trips)
glimpse(all_trips)
summary(all_trips)

##Cleaning data

## Removing empty rows and columns
all_trips <- janitor:: remove_empty(all_trips, which = c("rows"))
all_trips <- janitor:: remove_empty(all_trips, which = c("cols"))

#Find the number of null values in each column
colSums(is.na(all_trips))

#seeing how many duplicates there are if any
sum(duplicated(all_trips$ride_id))

#Removing all duplicates and their rows
all_trips <- all_trips[!duplicated(all_trips$ride_id), ]

## Adding in date columns
all_trips$Date <- as.Date(all_trips$started_at)
all_trips$year <- format(as.Date(all_trips$Date), "%y")
all_trips$month <- format(as.Date(all_trips$Date), "%m")
all_trips$day <- format(as.Date(all_trips$Date), "%d")
all_trips$day_of_week <- format(as.Date(all_trips$Date), "%A")

##Calculating ride length time
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at, units =
                               c("secs"))
##Removing columns I don't need and filtering out extreme and invalid ride lengths
all_trips_v2 <- all_trips %>% filter(ride_length >60, ride_length <= 86400) %>% drop_na() %>% select(-start_lat,
-end_lat,-start_lng,-end_lng)

View(all_trips_v2)
summary(all_trips_v2)
colnames(all_trips_v2)

####Analysis
# Analysis on ride_length
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# Comparing members and casual members ride_length
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

# Putting the days in the correct order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Average ride time by each day for members vs casual users with days in correct order
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

View(all_trips_v2)

write.csv(all_trips_v2, "cleaned_data.csv", row.names = FALSE)






