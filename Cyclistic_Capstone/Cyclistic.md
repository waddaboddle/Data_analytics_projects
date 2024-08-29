Analysis of Cyclistic’s Casual Riders vs Annual Members (In Progress)
================
Muhaimin Ahmed
August 28, 2024

The objective of this analysis is to determine how annual members and
casual riders use Cyclistic bikes differently. This analysis will then
be used to determine how to target and convert Casual Riders to Annual
Members. Cyclistic is not a real company one used for the purposes of
this case study.

The data used for this analysis is located
[here](https://divvy-tripdata.s3.amazonaws.com/index.html). It is a
practice dataset used to get familiar with data analysis. From the
available data the 12 most recent months have been chosen for analysis
(August 2023 to August 2024).

The data is reliable as it is provided by at trusted source (Divvy) and
is also current as it includes data from the previous 12 months. The
data is also cited by Google ensuring its credibility.

The license agreement addresses licensing by granting a limited license
with specific restrictions, ensures privacy by prohibiting the
correlation of data with personal information, maintains security by
restricting unauthorized access and data mining, and outlines the
conditions for data accessibility, with no guarantees of its continued
availability.

Overall the data was well organized and did not require any cleaning for
the objective of this case study.

# Libraries

These are the libraries that are used to complete this case study.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
```

# Preparing the Data

12 months of data are merged onto one data frame named cyclistic_df.

``` r
temp <- list.files("./Cyclistic_Data", full.names = TRUE, pattern = "\\.csv$")

cyclistic_df <- readr::read_csv(temp, id = "Bike_Rides_2306_to_2407")
```

    ## Rows: 6616370 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
str(cyclistic_df)
```

    ## spc_tbl_ [6,616,370 × 14] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ Bike_Rides_2306_to_2407: chr [1:6616370] "./Cyclistic_Data/202306-divvy-tripdata.csv" "./Cyclistic_Data/202306-divvy-tripdata.csv" "./Cyclistic_Data/202306-divvy-tripdata.csv" "./Cyclistic_Data/202306-divvy-tripdata.csv" ...
    ##  $ ride_id                : chr [1:6616370] "6F1682AC40EB6F71" "622A1686D64948EB" "3C88859D926253B4" "EAD8A5E0259DEC88" ...
    ##  $ rideable_type          : chr [1:6616370] "electric_bike" "electric_bike" "electric_bike" "electric_bike" ...
    ##  $ started_at             : POSIXct[1:6616370], format: "2023-06-05 13:34:12" "2023-06-05 01:30:22" ...
    ##  $ ended_at               : POSIXct[1:6616370], format: "2023-06-05 14:31:56" "2023-06-05 01:33:06" ...
    ##  $ start_station_name     : chr [1:6616370] NA NA NA NA ...
    ##  $ start_station_id       : chr [1:6616370] NA NA NA NA ...
    ##  $ end_station_name       : chr [1:6616370] NA NA NA NA ...
    ##  $ end_station_id         : chr [1:6616370] NA NA NA NA ...
    ##  $ start_lat              : num [1:6616370] 41.9 41.9 42 42 42 ...
    ##  $ start_lng              : num [1:6616370] -87.7 -87.7 -87.7 -87.7 -87.7 ...
    ##  $ end_lat                : num [1:6616370] 41.9 41.9 41.9 42 42 ...
    ##  $ end_lng                : num [1:6616370] -87.7 -87.7 -87.6 -87.7 -87.7 ...
    ##  $ member_casual          : chr [1:6616370] "member" "member" "member" "member" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   ride_id = col_character(),
    ##   ..   rideable_type = col_character(),
    ##   ..   started_at = col_datetime(format = ""),
    ##   ..   ended_at = col_datetime(format = ""),
    ##   ..   start_station_name = col_character(),
    ##   ..   start_station_id = col_character(),
    ##   ..   end_station_name = col_character(),
    ##   ..   end_station_id = col_character(),
    ##   ..   start_lat = col_double(),
    ##   ..   start_lng = col_double(),
    ##   ..   end_lat = col_double(),
    ##   ..   end_lng = col_double(),
    ##   ..   member_casual = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

Let’s focus on the following columns to start the analysis: - ride_id:
Ride unique ID. - member_casual: Cyclistic member or casual rider. -
started_at and ended_at: Ride start and end times. - rideable_type:
Electric, classic or docked bike.

``` r
cyclistic_df_v2 <- select(cyclistic_df, ride_id, member_casual, started_at, ended_at, rideable_type)
head(cyclistic_df_v2)
```

    ## # A tibble: 6 × 5
    ##   ride_id    member_casual started_at          ended_at            rideable_type
    ##   <chr>      <chr>         <dttm>              <dttm>              <chr>        
    ## 1 6F1682AC4… member        2023-06-05 13:34:12 2023-06-05 14:31:56 electric_bike
    ## 2 622A1686D… member        2023-06-05 01:30:22 2023-06-05 01:33:06 electric_bike
    ## 3 3C88859D9… member        2023-06-20 18:15:49 2023-06-20 18:32:05 electric_bike
    ## 4 EAD8A5E02… member        2023-06-19 14:56:00 2023-06-19 15:00:35 electric_bike
    ## 5 5A36F2193… member        2023-06-19 15:03:34 2023-06-19 15:07:16 electric_bike
    ## 6 CF682EA7D… member        2023-06-09 21:30:25 2023-06-09 21:49:52 electric_bike

Using the selected fields the ride length can be calculated and the day
of the week can also be determined and store in cyclistic_df_v2.

``` r
cyclistic_df_v3 <- mutate(cyclistic_df_v2, 
                          ride_lengths = seconds_to_period(ended_at-started_at), 
                          day_of_week = weekdays(started_at), 
                          month = month.name[month(started_at)])
head(cyclistic_df_v3)
```

    ## # A tibble: 6 × 8
    ##   ride_id    member_casual started_at          ended_at            rideable_type
    ##   <chr>      <chr>         <dttm>              <dttm>              <chr>        
    ## 1 6F1682AC4… member        2023-06-05 13:34:12 2023-06-05 14:31:56 electric_bike
    ## 2 622A1686D… member        2023-06-05 01:30:22 2023-06-05 01:33:06 electric_bike
    ## 3 3C88859D9… member        2023-06-20 18:15:49 2023-06-20 18:32:05 electric_bike
    ## 4 EAD8A5E02… member        2023-06-19 14:56:00 2023-06-19 15:00:35 electric_bike
    ## 5 5A36F2193… member        2023-06-19 15:03:34 2023-06-19 15:07:16 electric_bike
    ## 6 CF682EA7D… member        2023-06-09 21:30:25 2023-06-09 21:49:52 electric_bike
    ## # ℹ 3 more variables: ride_lengths <Period>, day_of_week <chr>, month <chr>

# Analyzing the data

``` r
max(cyclistic_df_v3$ride_lengths)
```

    ## [1] 59.999

``` r
mean(cyclistic_df_v3$ride_lengths)
```

    ## [1] 29.41955

``` r
mode_char <- function(x) unique(x)[which.max(tabulate(match(x, unique(x))))]
mode_char(cyclistic_df_v3$day_of_week)
```

    ## [1] "Saturday"

``` r
mode_char(cyclistic_df_v3$rideable_type)
```

    ## [1] "electric_bike"

``` r
cyclistic_df_v3 %>%
  group_by(member_casual) %>%
  summarize(avg_ride_length = mean(ride_lengths, na.rm = TRUE))
```

    ## # A tibble: 2 × 2
    ##   member_casual avg_ride_length
    ##   <chr>                   <dbl>
    ## 1 casual                   29.4
    ## 2 member                   29.4

``` r
bike_type_by_member <- cyclistic_df_v3 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = rideable_type, values_from = count, values_fill = 0) %>%
  gather(key = "bike_type", value = "count", -member_casual)
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

``` r
head(bike_type_by_member)
```

    ## # A tibble: 6 × 3
    ## # Groups:   member_casual [2]
    ##   member_casual bike_type       count
    ##   <chr>         <chr>           <int>
    ## 1 casual        classic_bike  1178013
    ## 2 member        classic_bike  2079182
    ## 3 casual        docked_bike     49355
    ## 4 member        docked_bike         0
    ## 5 casual        electric_bike 1293730
    ## 6 member        electric_bike 2016090

``` r
ggplot(bike_type_by_member, aes(x = member_casual, y = count, fill = bike_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Bike Types by Member Type",
       x = "Member Type",
       y = "Count",
       fill = "Bike Type") + 
  theme(plot.title = element_text(hjust = 0.5))
```

![](Cyclistic_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
summarized_cyclistic_data <- cyclistic_df_v3 %>%
  group_by(day_of_week, member_casual) %>%
  summarize(avg_ride_length = mean(ride_lengths), 
            num_rides = n())
```

    ## `summarise()` has grouped output by 'day_of_week'. You can override using the
    ## `.groups` argument.

``` r
summarized_cyclistic_data$day_of_week <- factor(summarized_cyclistic_data$day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday", "Sunday"))
```

``` r
head(summarized_cyclistic_data)
```

    ## # A tibble: 6 × 4
    ## # Groups:   day_of_week [3]
    ##   day_of_week member_casual avg_ride_length num_rides
    ##   <fct>       <chr>                   <dbl>     <int>
    ## 1 Friday      casual                   29.4    378729
    ## 2 Friday      member                   29.4    588830
    ## 3 Monday      casual                   29.4    290925
    ## 4 Monday      member                   29.4    572043
    ## 5 Saturday    casual                   29.4    527028
    ## 6 Saturday    member                   29.4    544762

``` r
ggplot(data = summarized_cyclistic_data, aes(x = day_of_week, y = avg_ride_length, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Average Ride Length for Users by Day of the Week",
       x = "Day of the week",
       y = "Average Ride Length",
       fill = "User Type") + 
  theme(plot.title = element_text(hjust = 0.5))
```

![](Cyclistic_files/figure-gfm/average%20ride%20length%20for%20users%20by%20day%20of%20week-1.png)<!-- -->

``` r
ggplot(data = summarized_cyclistic_data, aes(x = day_of_week, y = num_rides, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Number of Rides for Users by Day of the Week",
       x = "Day of the week",
       y = "Number of Rides",
       fill = "User Type") + 
  theme(plot.title = element_text(hjust = 0.5))
```

![](Cyclistic_files/figure-gfm/number%20of%20rides%20for%20users%20by%20day%20of%20week-1.png)<!-- -->

``` r
monthly_cyclistic_data <- cyclistic_df_v3 %>%
  group_by(month, member_casual) %>%
  summarize(avg_ride_length = mean(ride_lengths), 
            num_rides = n())
```

    ## `summarise()` has grouped output by 'month'. You can override using the
    ## `.groups` argument.

``` r
head(monthly_cyclistic_data)
```

    ## # A tibble: 6 × 4
    ## # Groups:   month [3]
    ##   month    member_casual avg_ride_length num_rides
    ##   <chr>    <chr>                   <dbl>     <int>
    ## 1 April    casual                   29.3    131810
    ## 2 April    member                   29.3    283215
    ## 3 August   casual                   29.4    311130
    ## 4 August   member                   29.4    460563
    ## 5 February casual                   29.4     47163
    ## 6 February member                   29.4    176001

``` r
monthly_cyclistic_data <- monthly_cyclistic_data %>%
  mutate(month = factor(month, levels = month.name, ordered = TRUE))


ggplot(data = monthly_cyclistic_data, aes(x = month, y = num_rides, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Number of Rides for Users by Month",
       x = "Month",
       y = "Number of Rides",
       fill = "User Type") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, size = 8))
```

![](Cyclistic_files/figure-gfm/number%20of%20rides%20for%20users%20by%20month-1.png)<!-- -->
