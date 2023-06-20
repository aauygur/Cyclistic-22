library(tidyverse)
library(lubridate)
library(skimr)
library(hms)
library(data.table)
library(janitor)
library(dplyr)

jan_22 <- read.csv('data/202201.csv')
feb_22 <- read.csv('data/202202.csv')
mar_22 <- read.csv('data/202203.csv')
apr_22 <- read.csv('data/202204.csv')
may_22 <- read.csv('data/202205.csv')
jun_22 <- read.csv('data/202206.csv')
jul_22 <- read.csv('data/202207.csv')
aug_22 <- read.csv('data/202208.csv')
sep_22 <- read.csv('data/202209.csv')
oct_22 <- read.csv('data/202210.csv')
nov_22 <- read.csv('data/202211.csv')
dec_22 <- read.csv('data/202212.csv')

compare_df_cols(jan_22,feb_22,mar_22,apr_22,may_22,jun_22,jul_22,aug_22,sep_22,oct_22,nov_22,dec_22)

merged_22 <- bind_rows(jan_22, feb_22, mar_22, apr_22, may_22, jun_22, jul_22, aug_22, sep_22, oct_22, nov_22, dec_22)

remove(jan_22, feb_22, mar_22, apr_22, may_22, jun_22, jul_22, aug_22, sep_22, oct_22, nov_22, dec_22)

#colnames(merged_22)
#dim(merged_22)
#head(merged_22)
#str(merged_22)
#summary(merged_22)
#skim(merged_22)

merged_22$started_at = strptime(merged_22$started_at,"%Y-%m-%d %H:%M:%S")
merged_22$ended_at = strptime(merged_22$ended_at,"%Y-%m-%d %H:%M:%S")

merged_22$ride_length <- difftime(merged_22$ended_at, merged_22$started_at, units = "mins")
merged_22$ride_length <- as.integer(as.character(merged_22$ride_length))
#skim(merged_22$ride_length)
merged_22 <- merged_22[!(merged_22$ride_length<0),]
#skim(merged_22$ride_length)

merged_22$date <- as.Date(merged_22$started_at)
merged_22$month <- format(as.Date(merged_22$date), "%m")
merged_22$day <- format(as.Date(merged_22$date), "%d")
merged_22$year <- format(as.Date(merged_22$date), "%Y")
merged_22$day_of_week <- format(as.Date(merged_22$date), "%A")
merged_22$time <- format(as.Date(merged_22$date), "%H:%M:%S")
merged_22$time <- as_hms((merged_22$started_at))
merged_22$hour <- hour(merged_22$time)

merged_22 <- merged_22 %>% mutate(season = 
                                              case_when(month == "01" ~ "Winter",
                                                        month == "02" ~ "Winter",
                                                        month == "03" ~ "Spring",
                                                        month == "04" ~ "Spring",
                                                        month == "05" ~ "Spring",
                                                        month == "06" ~ "Summer",
                                                        month == "07" ~ "Summer",
                                                        month == "08" ~ "Summer",
                                                        month == "09" ~ "Fall",
                                                        month == "10" ~ "Fall",
                                                        month == "11" ~ "Fall",
                                                        month == "12" ~ "Winter")
)

merged_22 <-merged_22 %>% mutate(time_of_day = 
                                             case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                       hour == "2" ~ "Night",
                                                       hour == "3" ~ "Night",
                                                       hour == "4" ~ "Night",
                                                       hour == "5" ~ "Night",
                                                       hour == "6" ~ "Morning",
                                                       hour == "7" ~ "Morning",
                                                       hour == "8" ~ "Morning",
                                                       hour == "9" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening")
)

#View(merged_22)

merged_22 <- distinct(merged_22)

merged_22 <- merged_22 %>% 
  select(-c(ride_id, start_station_id, end_station_id, start_lat, 
            start_lng, end_lat, end_lng, hour, time, year, day, 
            month, date, started_at, ended_at,rideable_type,
            start_station_name, end_station_name))

merged_22 <- merged_22 %>% 
  rename(user_type=member_casual)

write.csv(merged_22,"cyclistic_22.csv")
