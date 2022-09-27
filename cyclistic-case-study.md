---
title: "cyclistic case study"
author: "Elvis Otwoma"
date: "9/20/2022"
output: github_document
---


# Case Study 1: Unlocking Lost Opportunities to Increase Cyclistic Membership

Cyclistic is a fictional bikeshare company operating in the City of Chicago. The company offers a wide range of bikes to serve different markets. Currently, the company has 5824 bikes and 692 docking stations across the city. A user can take a bike from any station and return it to any station anywhere in the city at anytime.

## Cyclistic Business Model and Business Goal

Cyclistic offers three flexible pricing plans: single-ride passes, full-day passes and annual memberships. Customers with annual memberships are referred to as Cyclistic members while those using single-ride or full-day passes as casual riders.  

The finance team at Cyclistic has determined that Cyclistic members are more profitable than casual riders. The marketing team would like to get insights into how members differ from casual riders. These insights will help the team identified the best incentive for converting more casual riders into members. A marketing campaign designed based on these insights will need to get board approval before going live.  

### Data Sources and Licenses

Lyft Bikes and Scooters, LLC ("Lyft") operates the City of Chicago’s (“City”) Divvy bicycle sharing service. Lyft and the City are committed to supporting bicycling as an alternative transportation option. As part of that commitment, the City permits Lyft to make certain Divvy system data owned by the City (“Data”) available to the public, subject to the terms and conditions of this License Agreement (“Agreement”). By accessing or using any of the Data, you agree to all of the terms and conditions of this Agreement.  

Bikeshare hereby grants to any user a non-exclusive, royalty-free, limited, perpetual license to access, reproduce, analyze, copy, modify, distribute in your product or service and use the Data for any lawful purpose.  

There are certain items listed under prohibited conduct section of the [license agreement](https://ride.divvybikes.com/data-license-agreement). None of them have been violated for the purpose of this case study.  

The data used in this case study is for the past 12 months, from April 2021 to March 2022. For enhanced privacy, the data does not contain any information that would allow any analysis based on individual users identities. 

### Analysis Tools

This case study is done using R in an R Notebook. The entire project was done in R Studio. The presentation of findings is done using Google Slides. 

## Deliverables

1. Summary of Findings Presentation for the Executive Team
2. Data Analysis Report with Findings for the Executive Team
3. R Notebook of the Analysis for the Data Analytics Team to Review

# Process

## Installing R Packages and Loading the Data

Tidyverse is a diverse package of packages suitable for all the analysis and visualization needs for this project. 
 

```{r Installing Tidyverse, message=FALSE, warning=FALSE}
install.packages('tidyverse')
install.packages('lubridate')
install.packages('ggplot2')
install.packages('janitor')
install.packages('scales')
```

```{r}
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(scales)
library(janitor)
```

Next step is to load the data and have a look at the format before cleaning it for analysis.


```{r Loading Data, message=FALSE, warning=FALSE}
# Load the first three months and check the data format
# If the columns are similar we can combine them to a single dataset for Q2 2021
apr_21 <- read_csv("202104-divvy-tripdata.csv")
may_21 <- read_csv("202105-divvy-tripdata.csv")
jun_21 <- read_csv("202106-divvy-tripdata.csv")
jul_21 <- read_csv("202107-divvy-tripdata.csv")
aug_21 <- read_csv("202108-divvy-tripdata.csv")
sep_21 <- read_csv("202109-divvy-tripdata.csv")
oct_21 <- read_csv("202110-divvy-tripdata.csv")
nov_21 <- read_csv("202111-divvy-tripdata.csv")
dec_21 <- read_csv("202112-divvy-tripdata.csv")
jan_22 <- read_csv("202201-divvy-tripdata.csv")
feb_22 <- read_csv("202202-divvy-tripdata.csv")
mar_22 <- read_csv("202203-divvy-tripdata.csv")

```

Working with 12 files can be tedious and repetitive. Creating quarterly data sets makes the process more efficient. The datasets loaded represent Q2, Q3 and Q4 2021 as well as Q1 2022. Before creating the quarterly data sets, let's take a quick look at the data format and column names using view to see whether any formatting is required.

```{r Checking the Format}
# Let's check them out real quick
head(apr_21)
head(may_21)
head(jun_21)
head(jul_21)
head(aug_21)
head(sep_21)
head(oct_21)
head(nov_21)
head(dec_21)
head(jan_22)
head(feb_22)
head(mar_22)
```

```{r}
# Let's also check that the column names match
# colnames(apr_21)
# colnames(may_21)
# colnames(jun_21)
# colnames(jul_21)
# colnames(aug_21)
# colnames(sep_21)
# colnames(oct_21)
# colnames(nov_21)
# colnames(dec_21)
# colnames(jan_22)
# colnames(feb_22)
# colnames(mar_22)

# Found a simpler way to compare and find mismatches in column data types
compare_df_cols(apr_21, may_21, jun_21, jul_21, aug_21, sep_21, oct_21, nov_21,
                dec_21, jan_22, feb_22, mar_22, return = "mismatch") 

```

The datasets have the same format and the columns match by name so we can create quarterly datasets. 

```{r Creating Quarterly Datasets}
# Formats and types match, so we create Q2_2021 for easier wrangling
q2_2021 <- bind_rows(apr_21, may_21, jun_21)
q3_2021 <- bind_rows(jul_21, aug_21, sep_21)
q4_2021 <- bind_rows(oct_21, nov_21, dec_21)
q1_2022 <- bind_rows(jan_22, feb_22, mar_22)

```

The datasets have the same structure, so we can reliably create a unified dataset with all the trips.

```{r}
# Create one dataset with all trips and check the structure
all_trips <- bind_rows(q2_2021, q3_2021, q4_2021, q1_2022)
```

Let's inspect the new dataset we just created.

```{r Inspecting the Data, include=FALSE}
# See the column names
colnames(all_trips)

# How many rows doe we have
nrow(all_trips)

# Dimensions of the dataset
dim(all_trips)

# A quick look at the first 6 rows
head(all_trips)

# Check the structure
str(all_trips)

# Get a summary of the data
summary(all_trips)

```
Our combined dataset has 5.7 million trips data across 13 different attributes. There are a 4716 blank cells in both end_lat and end_long columns, they are very few relative to the size of our dataset and will not affect the analysis if dropped. However, we will not rely on the data in these field for this case study. This is because the destination of each ride can vary significantly.  

Next, we need to get a sense of the different items in one of the key columns in answering the business task, member_casual, which describes the user's membership status. We need to ensure that we only have two types of members, casual and members.

```{r Check Membership Types}
table(all_trips$member_casual)

```
Cyclistic has more members than casual riders. That's a good start. However, the data is significantly insufficient in terms of Station names, and Station ID where the trips start and end for about 10% of the rides. Regardless, this data points will not be used for this case study because we are more interested in the time and length of the ride than the specific stations. If this case study was meant to identify how to improve the customer experience, then the station names would be significant. 

## Feature Engineering

Now we can go ahead and enrich our data by adding more columns to allow for easier grouping by time, day, month, and year. We also need to know the length of each trip.

```{r Adding a Time, warning=TRUE}
# Create a new column for the date
all_trips$date <- as.Date(all_trips$started_at)

# Create a Month column
all_trips$month <- months(as.Date(all_trips$date))

# Create a day column
all_trips$day <- format(as.Date(all_trips$date), '%d')

# Create a year column
all_trips$year <- format(as.Date(all_trips$date), '%Y')

# Create a day_of_week column
all_trips$day_of_week <- format(as.Date(all_trips$date), '%A')

# Create an hour of the day column
all_trips$hour_of_day <- format(as.POSIXct(all_trips$started_at), '%H')


```

Let's look into missing data a little bit.

```{r Missing data check}
# Let's see how many NAs we have in the data 
map(all_trips, ~sum(is.na(.)))
```
Keep these rows in the data as we are more interested in the latitude and longitude of these locations when we eventually visualize the data. Regardless, a view of teh distribution of NAs may give insights into data collection anomalies.

```{r Further missing data check}
# How are the missing details spread out through the year
aggregate(end_station_id ~ month, data=all_trips, function(x) {sum(is.na(x))}, na.action = NULL)
```

The months distribution of missing values in start and end station names and IDs is spread across the year disproportionately. Dropping this rides will affect the outcome of our analysis on monthly and daily trips. It also suggests a problem in data collection throughout the year.

Next we calculate the ride length and clear the data to remove any trips with a negative ride length. 

```{r Ride length, include=FALSE}
# Using difftime() we can get ride length in seconds
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

# Check if the new column is a factor
#is.factor(all_trips$ride_length)

# Convert ride length to numeric to allow for aggregation calculations later on
all_trips$ride_length <- as.numeric(all_trips$ride_length)
is.numeric(all_trips$ride_length)

# Convert it to minutes for easier readability and visualization
all_trips$ride_length <- all_trips$ride_length/60

```
Let's have a look at the dataset now before we proceed.

```{r include=FALSE}
str(all_trips)
```

From the Case Study package for this project, we also know that some rides are taken off the system for repairs and therefore we should not consider them. Additionally, we want to drop any data where the ride length is a negative or NA.

```{r Remove bad data}
# Create a new dataset without the bad data
all_trips_v2 <- all_trips %>%
  # filter(all_trips$start_station_name == "HQ QR") %>%
  # filter(all_trips$ride_length<0)
  filter(all_trips$start_station_name != "HQ QR" & all_trips$ride_length > 0 & all_trips$ride_length != "")
```

## Descriptive Analysis

Now that we have relatively clean data. It is time for some descriptive analysis. Let's check out some descriptive analysis on the ride length data.

```{r Summary on ride length}
summary(all_trips_v2$ride_length)
```
```{r}
aggregate(ride_length ~ month, data=all_trips_v2, function(x) {sum(!is.na(x))}, na.action = NULL) 
```
Removing any NAs in the ride_length column has resulted in a significantly smaller data set for further analysis. However, it appears as though the spread is relatively even across the months. It is also worth stating that the statistics on ride length have not changed alot either.
```{r}
# Ride length summary stats by membership type
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

```

The months and days of the week need to be ordered properly.
```{r Order days of the week and months properly}
# Ordering days
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

# Ordering months
all_trips_v2$month <- ordered(all_trips_v2$month, levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
```


Next we can try and see the average ride time by day of the week for members and casual riders to see any differences.

```{r}
# Average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

```
A look at the ridership data by type and day of the week might give some insight into the differences between members and casual riders.

```{r Create a subset of the data that we can visualize}
viz_data <- all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%#creates weekday field using wday()
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)
```
Now let's have a visual to see the distinction in the number of rides between the two member categories by weekday.
```{r Visualize the Number of Rides by weekday}
viz_data %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge")
```

The chart shows that we have **an inverse behaviour between the number of rides by casual riders and members**. High numbers of casual rides during the weekend suggests that many of these riders likely do it for leisure. Meanwhile, the trend of member rides suggests they use the bikes as a means of transport to commute to work during the week. Looking at the spread in the time of day in which these two groups use the bikes will give further clarity on this. Before we do that, let's have a look at the duration of the rides by weekday. Ideally, average daily commute takes the same duration all week. 

```{r Visualize the average ride length by weekday}
viz_data %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge")
```
As expected, the average ride length for members is relatively stable throughout the week supporting the hypothesis that members use the bikes to commute to work. There is a slight uptick in ride length in the weekends suggesting increased activity, perhaps for leisure or as a workout. The casual ride length throughout the week suggests that most of the riders are commuters as well. However, the average casual ride is twice as long as the member ride. Information on the distance covered in the rides would be useful to determine whether this distinction is because of slow speeds of longer commutes. While this can be achieved using the latitude and longitude information provided, the implementation has proven challenging at this time.  

```{r}
# Check the average ride duration between the two groups
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
```

The **average casual rider's trip duration is 2.5X longer than a member's ride**. This could provide a potential for marketing the use of the bikes as a member as the demand for the bikes exists. 

```{r Visualize the number of trips by hour of the day}
all_trips_v2 %>%
  #filter(ride_length != "") %>%
  group_by(member_casual, hour_of_day) %>%
  summarize(number_of_rides = n()) %>%
  arrange(member_casual, hour_of_day) %>%
  ggplot(aes(x = hour_of_day, y = number_of_rides)) + geom_col(fill = "blue", position = "dodge") + facet_wrap(~ member_casual) + theme(axis.text.x = element_text(angle = 75)) + scale_y_continuous(labels = comma)
```
This shows an almost identical trend between the two categories. There is a noticeable uptick in ridership in the member rides in the morning. Further review is required by day of week to see if there are any distinctions.

```{r}
all_trips_v2 %>%
  #filter(ride_length != "") %>%
  group_by(member_casual, day_of_week, hour_of_day) %>%
  summarize(number_of_rides = n()) %>%
  arrange(member_casual, hour_of_day) %>%
  ggplot(aes(x = hour_of_day, y = number_of_rides)) + geom_col(fill = "blue", position = "dodge") + facet_wrap(~ day_of_week + member_casual) + theme(axis.text.x = element_text(angle = 75)) + scale_y_continuous(labels = comma)
```
From these visualizations, we can tell that **members have an uptick in ridership in the morning and late afternoon on all weekdays**. In the weekends, this trend changes to match the casual riders.  

It is also clear that these two groups have an uptick in ridership at around the same time in the afternoon. A hypothesis for this would be that casual riders opt to use bikes to beat rush hour traffic when leaving work. **There is an opportunity for the company to promote the use of bikes to commute to work in the morning**. It would be useful to understand whether the cause of this is ease of access to bikes in the morning or whether casual riders live in places that typically have little traffic going into work.  

Members and casual riders can differ in terms of the bikes they use and the seasonality of their usage.

```{r number of trips by day of the week}
# visualizing the ridership by ride type and member_casual
all_trips_v2 %>%
  group_by(member_casual, rideable_type, day_of_week) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, rideable_type, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = rideable_type)) + geom_col() + facet_wrap(~member_casual) + theme(axis.text.x = element_text(angle = 75)) + scale_y_continuous(labels = comma)
```

The classic bike is the most common bike. Members use the bikes a lot more than members during the week going by the number of rides. **Casual riders rely on the bikes a lot more on the weekends than on weekdays**. It would be useful to understand user intention a bit more, for example, whether or not they cycle as a means of transport or exercise. Furthermore, **casual riders use the docked bikes more than members**. 
```{r avg trip duration by day of the week}
# visualizing the ridership by ride type and member_casual
all_trips_v2 %>%
  group_by(member_casual, rideable_type, day_of_week) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, rideable_type, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = rideable_type)) + geom_col() + facet_wrap(~member_casual) + theme(axis.text.x = element_text(angle = 75))
```


As established earlier, average casual ride duration is much longer than member rides. The reliance on docked bikes is even more apparent in this graph. You can easily see that the longest rides were on docked bikes.  

Let's have a look at the use of bikes throughout the year to uncover more insights.

```{r rides by month}
all_trips_v2 %>%
  group_by(member_casual, rideable_type, month) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, rideable_type, month) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = rideable_type)) + geom_col() + facet_wrap(~member_casual) + theme(axis.text.x = element_text(angle = 75)) + scale_y_continuous(labels = comma)
```

Monthly ridership maintains a similar pattern for both types of users.

```{r Avg trip duration by month}
all_trips_v2 %>%
  group_by(member_casual, rideable_type, month) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, rideable_type, month) %>%
  ggplot(aes(x = month, y = average_duration, fill = rideable_type)) + geom_col() + facet_wrap(~member_casual) + theme(axis.text.x = element_text(angle = 75))
```
January appears to have extremely long ride durations among casual riders. Casual riders maintain a similar trend across the year with a slight increase in ridership in the middle of the year. 

## Extracting a CSV for further analysis on Tableau.

Tableau visualizations are a powerful means to analyse data and communicate insights. To enable further analysis, I will extract a CSV of the cleaned data.   

```{r Writing to CSV, eval=FALSE, include=FALSE}
# Write the clean df to a csv file for Tableau analysis
write.csv(all_trips_v2, "C:\\Users\\cctvtvtvtvtvtvtvtvt\\Documents\\Case Studies\\1. Cyclistic Case Study\\R Notebook\\cln_trips.csv", row.names = FALSE)
```

The final visualization for the exported data is available on [Tableau](https://public.tableau.com/app/profile/otasium/viz/CyclisticUsagePatternsBetweenMembersandCasualRiders/Dashboard1).

### Author: Elvis Otwoma
