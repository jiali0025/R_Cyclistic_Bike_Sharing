install.packages("tidyverse")
install.packages("dplyr")
library(dylyr)
install.packages("janitor")
library(janitor)
install.packages("skimr")
library(skimr)
install.packages("lubridate")
library(lubridate)
install.packages("stringr")
library(stringr)

########## Data importation and merging ##########
trips1 <- read.csv("~/Desktop/Project/R_Cyclistic_Bike_Sharing/202101-divvy-tripdata.csv")
trips2 <- read.csv("~/Desktop/Project/R_Cyclistic_Bike_Sharing/202102-divvy-tripdata.csv")
trips3 <- read.csv("~/Desktop/Project/R_Cyclistic_Bike_Sharing/202103-divvy-tripdata.csv")
trips4 <- read.csv("~/Desktop/Project/R_Cyclistic_Bike_Sharing/202104-divvy-tripdata.csv")
trips5 <- read.csv("~/Desktop/Project/R_Cyclistic_Bike_Sharing/202105-divvy-tripdata.csv")
trips6 <- read.csv("~/Desktop/Project/R_Cyclistic_Bike_Sharing/202106-divvy-tripdata.csv")
trips7 <- read.csv("~/Desktop/Project/R_Cyclistic_Bike_Sharing/202107-divvy-tripdata.csv")
trips8 <- read.csv("~/Desktop/Project/R_Cyclistic_Bike_Sharing/202108-divvy-tripdata.csv")
trips9 <- read.csv("~/Desktop/Project/R_Cyclistic_Bike_Sharing/202109-divvy-tripdata.csv")
trips10 <- read.csv("~/Desktop/Project/R_Cyclistic_Bike_Sharing/202110-divvy-tripdata.csv")
trips11 <- read.csv("~/Desktop/Project/R_Cyclistic_Bike_Sharing/202111-divvy-tripdata.csv")
trips12 <- read.csv("~/Desktop/Project/R_Cyclistic_Bike_Sharing/202112-divvy-tripdata.csv")
trips_full <- rbind(trips1, trips2, trips3, trips4, trips5, trips6,
                        trips7, trips8, trips9, trips9, trips10, trips11, trips12)
glimpse(trips_full) # has a 6,351,210 rows of data, each representing a trip, with 13 columns, each representing a variable about the trip



########## Data Cleaning ##########
##Removing white space in the start- and end-station names
trips_full <- trips_full %>% mutate(start_station_name = str_trim(start_station_name,side = "both",
                                    end_station_name = str_trim(end_station_name, side = "both")
##Harmonising the rideable_type column
trips_clean <- trips_full %>%
              mutate(rideable_type = str_replace(rideable_type, "docked_bike", "classic_bike"))
trips_clean %>% count(rideable_type)
##Removing redundant data
trips_clean_na <- trips_clean %>% filter(!(rideable_type == "classic_bike" & (is.na(start_station_name)|is.na(end_station_name))))
trips_clean_na 



########## Data transformation & futher cleaning ##########
trips_clean_dt <- trips_clean_na %>% mutate(month = month(started_at, label = TRUE),
                                            day_of_week = wday(started_at, label = TRUE),
                                            hour_of_day = hour(started_at),
                                            ride_duration_hrs = difftime(ended_at, started_at, units = "hours"),
                                            ride_duration_mins = difftime(ended_at, started_at, units = "mins"),
                                            route = str_c(start_station_name, end_station_name, sep = "--"))
trips_clean_dt
#remove all rows containing trips that were longer than 1 day or shorter than 1 min
cleaned_df <- trips_clean_dt %>% filter(ride_duration_hrs < 24) %>% filter(ride_duration_mins > 1)
cleaned_df



