#Capstone Project: Bike_Ride_Share
#Author: Joseph Silva
#Date: 2/2/2023

#Summary: This is an analysis for a theoretical bike sharing company to-
#-understand the difference between casual members and annual members.
# With the objective to identify incentives to convert casual members to-
#- annual members

#Importing packages and libraries
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(skimr)
library(lubridate)


## Preparing Data ##


#setting working directory
setwd("C:/Users/josep/OneDrive/Documents/Learning Programming/Data Analytics/Google Cert Capstones/Bike_Share/Data/Bike_Data_CSV")

# merging csv files
df_merged <- list.files("C:/Users/josep/OneDrive/Documents/Learning Programming/Data Analytics/Google Cert Capstones/Bike_Share/Data/Bike_Data_CSV/") %>%
  lapply(read_csv) %>% 
  bind_rows()

#saving merged data frame to project directory
write.csv(df_merged,"C:/Users/josep/OneDrive/Documents/Learning Programming/Data Analytics/Google Cert Capstones/Bike_Share/Data/Bike_Data_CSV/Bike_Data_Merged.csv")

##Organizing Data##

# adding columns for date, day, month, and year of each ride
# this will allow us to aggregate data based on day, month, and year
#https://www.statmethods.net/input/dates.html

df_merged$date <- as.Date(df_merged$started_at) #default format is yyyy-mm-dd
df_merged$month <- format(as.Date(df_merged$date), "%m")
df_merged$day <- format(as.Date(df_merged$date), "%d")
df_merged$year <- format(as.Date(df_merged$date), "%Y")
df_merged$day_of_the_week <-format(as.Date(df_merged$date), "%A")

#adding a ride length component
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html

df_merged$ride_length <- difftime(df_merged$ended_at, df_merged$started_at)

#ride_length is in factor form and needs to be converted to numeric for analysis
# using the factor, numeric, and character functions to convert as follows

is.factor(df_merged$ride_length) #checks if the data is in factor form
df_merged$ride_length <- as.numeric(as.character(df_merged$ride_length)) #converts data to numeric
is.numeric(df_merged$ride_length) #checks if data is in numeric form

#Removing 'bad' data
# bikes were removed for maitenance resulting in a negative ride_length
# creating a second version of the dataframe with removed data, v2
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/

df_merged_v2 <- df_merged[!(df_merged$start_station_name == "HQ QR" | df_merged$ride_length<0),] 




## Analysis ##

#Descriptive analysis on ride_length, in seconds

summary(df_merged_v2$ride_length) #mean,median,min,and max calculations

#Comparing ride duration of members to casual riders

aggregate(df_merged_v2$ride_length ~ df_merged_v2$member_casual, FUN = summary)

#ordering the days of the week

df_merged_v2$day_of_the_week <- ordered(df_merged_v2$day_of_the_week, levels = c("Monday", "Tuesday","Wednesday","Thursday", "Friday", "Saturday", "Sunday"))

#Compare average ride times by day of members vs casuals

aggregate(df_merged_v2$ride_length ~ df_merged_v2$member_casual + 
            df_merged_v2$day_of_the_week, FUN = mean)


# analyzing the ridership data by member type, weekday, and duration of use

df_merged_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekeday field using wday()
  group_by(member_casual, weekday) %>%  #groups by member type and weekday
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%  #calcs number of rides and average duration
  arrange(member_casual, weekday)

#visualizing number of riders: members vs casuals for the days of the week

df_merged_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE )) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  filter(!is.na(member_casual)) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Number of Member Riders vs Casual", x = "Weekday", y = " Number of Rides")

# visualizing ride length

df_merged_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  filter(!is.na(member_casual)) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Ride Length by Day", x = "Weekday", y = "Ride Length (seconds)")

# visualizing type of bike used by members vs casuals

df_merged_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,rideable_type, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type, weekday) %>% 
  filter(rideable_type %in% c("classic_bike", "electric_bike")) %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual )) +
  geom_col(position = "dodge") + labs(title = "Number of Rides By Bike Type", x = "Bike Type", y = "Number of Rides")

# visualizing the bike type: ride length 

df_merged_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,rideable_type, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type, weekday) %>% 
  filter(rideable_type %in% c("classic_bike", "electric_bike")) %>% 
  ggplot(aes(x = rideable_type, y = average_duration, fill = member_casual )) +
  geom_col(position = "dodge") +
  labs(title = "Bike Type By Ride Duration", x = " Bike Type", y = " Ride Duration (seconds)")

# visualizing bike type, ride length by weekday
df_merged_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,rideable_type, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type, weekday) %>% 
  filter(rideable_type %in% c("classic_bike", "electric_bike")) %>% 
  ggplot(aes(x = weekday,y = average_duration, fill = member_casual )) +
  geom_bar(stat = "identity", position = "dodge") + facet_wrap(~rideable_type) +
  labs(title = "Bike Type Usage By Week", x = "Weekday", y = "Duration Used (seconds)")
