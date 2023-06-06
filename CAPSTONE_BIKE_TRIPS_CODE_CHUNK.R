# Google Analytics Capstone Bike Trips Project

library("tidyverse")
library("dplyr")
library("readr")
library("tidyr")
library("skimr")
library("janitor")
library("lubridate")
library("ggplot2")
library("readxl")
library("here")

getwd()

setwd("/Users/HP/Desktop/Bike_Trips_Files_2022")


q1_2022 <- read_csv("Bike_Trips_2022_01_V01.csv")
q2_2022 <- read_csv("Bike_Trips_2022_02_V02.csv") 
q3_2022 <- read_csv("Bike_Trips_2022_03_V03.csv") 
q4_2022 <- read_csv("Bike_Trips_2022_04_V04.csv") 
q5_2022 <- read_csv("Bike_Trips_2022_05_V05.csv") 
q6_2022 <- read_csv("Bike_Trips_2022_06_V06.csv") 
q7_2022 <- read_csv("Bike_Trips_2022_07_V07.csv") 
q8_2022 <- read_csv("Bike_Trips_2022_08_V08.csv") 
q9_2022 <- read_csv("Bike_Trips_2022_09_V09.csv") 
q10_2022 <- read_csv("Bike_Trips_2022_10_V10.csv") 
q11_2022 <- read_csv("Bike_Trips_2022_11_V11.csv") 
q12_2022 <- read_csv("Bike_Trips_2022_12_V12.csv") 




colnames(q1_2022)
colnames(q2_2022)
colnames(q3_2022)
colnames(q4_2022)
colnames(q5_2022)
colnames(q6_2022)
colnames(q7_2022)
colnames(q8_2022)
colnames(q9_2022)
colnames(q10_2022)
colnames(q11_2022)
colnames(q12_2022)


str(q1_2022)
str(q2_2022)
str(q3_2022)
str(q4_2022)
str(q5_2022)
str(q6_2022)
str(q7_2022)
str(q8_2022)
str(q9_2022)
str(q10_2022)
str(q11_2022)
str(q12_2022)

q1_2022 <- mutate(q1_2022,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
q2_2022 <- mutate(q2_2022,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
q3_2022 <- mutate(q3_2022,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
q4_2022 <- mutate(q4_2022,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
q5_2022 <- mutate(q5_2022,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
q6_2022 <- mutate(q6_2022,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
q7_2022 <- mutate(q7_2022,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
q8_2022 <- mutate(q8_2022,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
q9_2022 <- mutate(q9_2022,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
q10_2022 <- mutate(q10_2022,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
q11_2022 <- mutate(q11_2022,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
q12_2022 <- mutate(q12_2022,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))


all_trips <- bind_rows(q1_2022,q2_2022
                       ,q3_2022,q4_2022
                       ,q5_2022,q6_2022
                       ,q7_2022,q8_2022
                       ,q9_2022,q10_2022
                       ,q11_2022,q12_2022)


all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng,na))

colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
tail(all_trips)
str(all_trips)
summary(all_trips)

skim_without_charts(all_trips)

table(all_trips$member_casual)

all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))


all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

all_trips$ride_length <- difftime(as.POSIXct(strptime(all_trips$ended_at, "%d/%m/%Y %H:%M")),
                                  
                                  as.POSIXct(strptime(all_trips$started_at, "%d/%m/%Y %H:%M")), unit = "mins")

str(all_trips)

is.factor(all_trips$ride_length)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

is.numeric(all_trips$ride_length)


all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),] %>% na.omit(all_trips$ride_length)

skim_without_charts(all_trips_v2)

mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)


summary(all_trips_v2$ride_length)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2 %>% 
  group_by(member_casual) %>% 
  summarise(sample = n(),mean(ride_length),median(ride_length),min(ride_length),max(ride_length)) %>% 
  ungroup()

all_trips_v2 %>% 
  group_by(member_casual,day_of_week) %>% 
  summarise(sample=n(),mean(ride_length),median(ride_length)) %>%
  ungroup()


all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, month)




all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual,weekday)

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Number of Rides: Casual vs Member") +
  ylab("Number of Rides") +
  xlab("Weekday") +
  guides(fill=guide_legend(title="User Type"))
ggsave("number_of_rides_vs_weekday.png")


all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Average Ride Duration: Casual vs Member") +
  ylab("Average Ride Duration") +
  xlab("Weekday") +
  guides(fill=guide_legend(title="User Type"))
ggsave("average_duration_vs_weekday.png")

all_trips_v2 %>%  
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Number of Rides: Casual vs Member") +
  ylab("Number of Rides") +
  xlab("Month") +
  guides(fill=guide_legend(title="User Type"))
ggsave("number_of _rides_vs_month.png")

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

write_csv(counts, "C:\\Users\\HP\\Desktop\\data.csv")

 
