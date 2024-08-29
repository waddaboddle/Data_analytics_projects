Analysis of Cyclistic’s Casual Riders vs Annual Members
================
Muhaimin Ahmed

The objective of this analysis is to determine how annual members and
casual riders use Cyclistic bikes differently. This analysis will then
be used to determine how to target and convert Casual Riders to Annual
Members.

The Data for this analysis is located
[here](https://divvy-tripdata.s3.amazonaws.com/index.html). It is a
practice dataset used to get familiar with data analysis. Of the data
available the 12 most recent months have been chosen for analysis
(August 2023 to August 2024).

The data is reliable as it is provided by at trusted source (Divvy) and
is also current as it includes data from the previous 12 months. The
data is also cited by Google ensuring its credibility.

The Agreement addresses licensing by granting a limited license with
specific restrictions, ensures privacy by prohibiting the correlation of
data with personal information, maintains security by restricting
unauthorized access and data mining, and outlines the conditions for
data accessibility, with no guarantees of its continued availability.

Overall the data was well organized and will not require any cleaning
for the objective of this case study.

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

``` r
temp <- list.files("./Cyclistic_Data", full.names = TRUE, pattern = "\\.csv$")

bike_rides <- readr::read_csv(temp, id = "Bike_Rides_2306_to_2407")
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
head(bike_rides)
```

    ## # A tibble: 6 × 14
    ##   Bike_Rides_2306_to_2407              ride_id rideable_type started_at         
    ##   <chr>                                <chr>   <chr>         <dttm>             
    ## 1 ./Cyclistic_Data/202306-divvy-tripd… 6F1682… electric_bike 2023-06-05 13:34:12
    ## 2 ./Cyclistic_Data/202306-divvy-tripd… 622A16… electric_bike 2023-06-05 01:30:22
    ## 3 ./Cyclistic_Data/202306-divvy-tripd… 3C8885… electric_bike 2023-06-20 18:15:49
    ## 4 ./Cyclistic_Data/202306-divvy-tripd… EAD8A5… electric_bike 2023-06-19 14:56:00
    ## 5 ./Cyclistic_Data/202306-divvy-tripd… 5A36F2… electric_bike 2023-06-19 15:03:34
    ## 6 ./Cyclistic_Data/202306-divvy-tripd… CF682E… electric_bike 2023-06-09 21:30:25
    ## # ℹ 10 more variables: ended_at <dttm>, start_station_name <chr>,
    ## #   start_station_id <chr>, end_station_name <chr>, end_station_id <chr>,
    ## #   start_lat <dbl>, start_lng <dbl>, end_lat <dbl>, end_lng <dbl>,
    ## #   member_casual <chr>

``` r
colnames(bike_rides)
```

    ##  [1] "Bike_Rides_2306_to_2407" "ride_id"                
    ##  [3] "rideable_type"           "started_at"             
    ##  [5] "ended_at"                "start_station_name"     
    ##  [7] "start_station_id"        "end_station_name"       
    ##  [9] "end_station_id"          "start_lat"              
    ## [11] "start_lng"               "end_lat"                
    ## [13] "end_lng"                 "member_casual"

``` r
start_stop_times <- select(bike_rides,ride_id, member_casual, started_at, ended_at)
head(start_stop_times)
```

    ## # A tibble: 6 × 4
    ##   ride_id          member_casual started_at          ended_at           
    ##   <chr>            <chr>         <dttm>              <dttm>             
    ## 1 6F1682AC40EB6F71 member        2023-06-05 13:34:12 2023-06-05 14:31:56
    ## 2 622A1686D64948EB member        2023-06-05 01:30:22 2023-06-05 01:33:06
    ## 3 3C88859D926253B4 member        2023-06-20 18:15:49 2023-06-20 18:32:05
    ## 4 EAD8A5E0259DEC88 member        2023-06-19 14:56:00 2023-06-19 15:00:35
    ## 5 5A36F21930D6A55C member        2023-06-19 15:03:34 2023-06-19 15:07:16
    ## 6 CF682EA7D0F961DB member        2023-06-09 21:30:25 2023-06-09 21:49:52

A parameter we want to look at is the ride length which can be
calculated using the “started_at” and “ended_at” columns.

``` r
bike_time_data <- mutate(start_stop_times, ride_lengths = seconds_to_period(ended_at-started_at), day_of_week = weekdays(started_at))
head(bike_time_data)
```

    ## # A tibble: 6 × 6
    ##   ride_id     member_casual started_at          ended_at            ride_lengths
    ##   <chr>       <chr>         <dttm>              <dttm>              <Period>    
    ## 1 6F1682AC40… member        2023-06-05 13:34:12 2023-06-05 14:31:56 57M 44S     
    ## 2 622A1686D6… member        2023-06-05 01:30:22 2023-06-05 01:33:06 2M 44S      
    ## 3 3C88859D92… member        2023-06-20 18:15:49 2023-06-20 18:32:05 16M 16S     
    ## 4 EAD8A5E025… member        2023-06-19 14:56:00 2023-06-19 15:00:35 4M 35S      
    ## 5 5A36F21930… member        2023-06-19 15:03:34 2023-06-19 15:07:16 3M 42S      
    ## 6 CF682EA7D0… member        2023-06-09 21:30:25 2023-06-09 21:49:52 19M 27S     
    ## # ℹ 1 more variable: day_of_week <chr>

``` r
max(bike_time_data$ride_lengths)
```

    ## [1] 59.999

``` r
mean(bike_time_data$ride_lengths)
```

    ## [1] 29.41955

``` r
mode_char <- function(x) unique(x)[which.max(tabulate(match(x, unique(x))))]
mode_char(bike_time_data$day_of_week)
```

    ## [1] "Saturday"

``` r
bike_time_data %>%
  group_by(member_casual) %>%
  summarize(avg_ride_length = mean(ride_lengths, na.rm = TRUE))
```

    ## # A tibble: 2 × 2
    ##   member_casual avg_ride_length
    ##   <chr>                   <dbl>
    ## 1 casual                   29.4
    ## 2 member                   29.4

``` r
summarized_bike_data <- bike_time_data %>%
  group_by(day_of_week, member_casual) %>%
  summarize(avg_ride_length = mean(ride_lengths), 
            num_rides = n())
```

    ## `summarise()` has grouped output by 'day_of_week'. You can override using the
    ## `.groups` argument.

``` r
summarized_bike_data$day_of_week <- factor(summarized_bike_data$day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday", "Sunday"))
```

``` r
head(summarized_bike_data)
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
ggplot(data = summarized_bike_data, aes(x = day_of_week, y = avg_ride_length, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity")
```

![](Cyclistic_files/figure-gfm/visualizing%20average%20ride%20length%20for%20users%20by%20day%20of%20week-1.png)<!-- -->

``` r
ggplot(data = summarized_bike_data, aes(x = day_of_week, y = num_rides, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity")
```

![](Cyclistic_files/figure-gfm/visualizing%20number%20of%20rides%20for%20users%20by%20day%20of%20week-1.png)<!-- -->
