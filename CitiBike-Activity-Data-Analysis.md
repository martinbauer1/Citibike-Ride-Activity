CitiBike Ride Activity 2019
================

### By Martin Bauer

## Purpose

The goal of this project is to utilize analytics techniques to
understand customer behavior as it relates to renting bikes in NYC. 
What are the most popular days/times of the year that people rent bikes? Which genders
like to go faster on bikes? Is there a difference in volume between weekdays and weekends?
And which stations receive the most arrivals and departures, on average? These are some of
the questions that I attempt to answer in this project.

Citibike provides open-source historical data on their website
<https://www.citibikenyc.com/system-data> for analysis.

![citibike](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/e3f23d5f-38a9-4472-b729-755ca77371e1)

## A. Introduction

Citibike provides open-source historical data on their website:

<https://www.citibikenyc.com/system-data>

## B. Data Preparation

The first step is to install any packages needed, load any libraries
used, and load the dataset from the working directory into the R
environment:

### 1. Loading in any necessary libraries and reading in data

``` r
library(readr)
library(ggplot2)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ stringr   1.5.1
    ## ✔ forcats   1.0.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(geosphere)
library(leaflet)
library(webshot2)

setwd("~/BI/R/Martin's Directory")

jan <- read.csv("201901-citibike-tripdata.csv")
feb <- read.csv("201902-citibike-tripdata.csv")
mar <- read.csv("201903-citibike-tripdata.csv")
apr <- read.csv("201904-citibike-tripdata.csv")
may <- read.csv("201905-citibike-tripdata.csv") 
jun <- read.csv("201906-citibike-tripdata.csv")
jul <- read.csv("201907-citibike-tripdata.csv") 
aug <- read.csv("201908-citibike-tripdata.csv")
sep <- read.csv("201909-citibike-tripdata.csv")
oct <- read.csv("201910-citibike-tripdata.csv")
nov <- read.csv("201911-citibike-tripdata.csv")
dec <- read.csv("201912-citibike-tripdata.csv")
```

### 2. Combining 2019 Citibike historical data

Next, I need to download each month’s data from 2019 and combine them to
create one 2019 dataset. Then, I can explore the dataset, calculate
descriptive statistics, and perform exploratory data analysis through
visualizations.

``` r
bikes <- rbind(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)

str(bikes)
```

    ## 'data.frame':    20551697 obs. of  15 variables:
    ##  $ tripduration           : int  320 316 591 2719 303 535 280 1547 300 123 ...
    ##  $ starttime              : chr  "2019-01-01 00:01:47.4010" "2019-01-01 00:04:43.7360" "2019-01-01 00:06:03.9970" "2019-01-01 00:07:03.5450" ...
    ##  $ stoptime               : chr  "2019-01-01 00:07:07.5810" "2019-01-01 00:10:00.6080" "2019-01-01 00:15:55.4380" "2019-01-01 00:52:22.6500" ...
    ##  $ start.station.id       : chr  "3160" "519" "3171" "504" ...
    ##  $ start.station.name     : chr  "Central Park West & W 76 St" "Pershing Square North" "Amsterdam Ave & W 82 St" "1 Ave & E 16 St" ...
    ##  $ start.station.latitude : num  40.8 40.8 40.8 40.7 40.7 ...
    ##  $ start.station.longitude: num  -74 -74 -74 -74 -74 ...
    ##  $ end.station.id         : chr  "3283" "518" "3154" "3709" ...
    ##  $ end.station.name       : chr  "W 89 St & Columbus Ave" "E 39 St & 2 Ave" "E 77 St & 3 Ave" "W 15 St & 6 Ave" ...
    ##  $ end.station.latitude   : num  40.8 40.7 40.8 40.7 40.7 ...
    ##  $ end.station.longitude  : num  -74 -74 -74 -74 -74 ...
    ##  $ bikeid                 : int  15839 32723 27451 21579 35379 30315 35391 35551 18636 35206 ...
    ##  $ usertype               : chr  "Subscriber" "Subscriber" "Subscriber" "Subscriber" ...
    ##  $ birth.year             : int  1971 1964 1987 1990 1979 1989 1987 1981 1990 1987 ...
    ##  $ gender                 : int  1 1 1 1 1 2 1 1 2 1 ...

### 3. Cleaning the data

Based on the structure above, the dataset needs to be “cleaned” in
various ways. A few of the categories, namely start and end station ids
need to be factors instead of integers. And bike_id and gender should be
factors as well.

``` r
bikes$start.station.id <- as.factor(bikes$start.station.id)
bikes$end.station.id <- as.factor(bikes$end.station.id)
bikes$bikeid <- as.factor(bikes$bikeid)
bikes$gender <- as.factor(bikes$gender)
bikes$usertype <- as.factor(bikes$usertype)

levels(bikes$gender) <- c("Unknown", "Male", "Female")
```

Checking the results of the data cleaning below confirms that all
variables are of the correct type and that the dataset is now ready to
use.

``` r
str(bikes)
```

    ## 'data.frame':    20551697 obs. of  15 variables:
    ##  $ tripduration           : int  320 316 591 2719 303 535 280 1547 300 123 ...
    ##  $ starttime              : chr  "2019-01-01 00:01:47.4010" "2019-01-01 00:04:43.7360" "2019-01-01 00:06:03.9970" "2019-01-01 00:07:03.5450" ...
    ##  $ stoptime               : chr  "2019-01-01 00:07:07.5810" "2019-01-01 00:10:00.6080" "2019-01-01 00:15:55.4380" "2019-01-01 00:52:22.6500" ...
    ##  $ start.station.id       : Factor w/ 937 levels "116","119","120",..: 203 914 215 903 38 583 609 76 369 839 ...
    ##  $ start.station.name     : chr  "Central Park West & W 76 St" "Pershing Square North" "Amsterdam Ave & W 82 St" "1 Ave & E 16 St" ...
    ##  $ start.station.latitude : num  40.8 40.8 40.8 40.7 40.7 ...
    ##  $ start.station.longitude: num  -74 -74 -74 -74 -74 ...
    ##  $ end.station.id         : Factor w/ 974 levels "116","119","120",..: 285 950 197 664 939 511 290 361 402 630 ...
    ##  $ end.station.name       : chr  "W 89 St & Columbus Ave" "E 39 St & 2 Ave" "E 77 St & 3 Ave" "W 15 St & 6 Ave" ...
    ##  $ end.station.latitude   : num  40.8 40.7 40.8 40.7 40.7 ...
    ##  $ end.station.longitude  : num  -74 -74 -74 -74 -74 ...
    ##  $ bikeid                 : Factor w/ 19571 levels "14529","14530",..: 988 12006 7618 5303 14140 9961 14152 14181 3141 13970 ...
    ##  $ usertype               : Factor w/ 2 levels "Customer","Subscriber": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ birth.year             : int  1971 1964 1987 1990 1979 1989 1987 1981 1990 1987 ...
    ##  $ gender                 : Factor w/ 3 levels "Unknown","Male",..: 2 2 2 2 2 3 2 2 3 2 ...

### 4. Summary Statistics

``` r
summary(bikes)
```

    ##   tripduration      starttime           stoptime         start.station.id  
    ##  Min.   :     61   Length:20551697    Length:20551697    519    :  156575  
    ##  1st Qu.:    362   Class :character   Class :character   497    :  121781  
    ##  Median :    615   Mode  :character   Mode  :character   3255   :  119958  
    ##  Mean   :    978                                         402    :  113138  
    ##  3rd Qu.:   1079                                         285    :  113012  
    ##  Max.   :3812666                                         435    :  110305  
    ##                                                          (Other):19816928  
    ##  start.station.name start.station.latitude start.station.longitude
    ##  Length:20551697    Min.   :40.66          Min.   :-74.03         
    ##  Class :character   1st Qu.:40.72          1st Qu.:-74.00         
    ##  Mode  :character   Median :40.74          Median :-73.99         
    ##                     Mean   :40.74          Mean   :-73.98         
    ##                     3rd Qu.:40.76          3rd Qu.:-73.97         
    ##                     Max.   :40.87          Max.   :-73.88         
    ##                                                                   
    ##  end.station.id     end.station.name   end.station.latitude
    ##  519    :  155536   Length:20551697    Min.   :40.66       
    ##  497    :  125466   Class :character   1st Qu.:40.72       
    ##  402    :  123146   Mode  :character   Median :40.74       
    ##  3255   :  121631                      Mean   :40.74       
    ##  426    :  116003                      3rd Qu.:40.76       
    ##  285    :  114178                      Max.   :40.87       
    ##  (Other):19795737                                          
    ##  end.station.longitude     bikeid               usertype          birth.year  
    ##  Min.   :-74.08        35194  :    3150   Customer  : 2872176   Min.   :1857  
    ##  1st Qu.:-74.00        34332  :    3101   Subscriber:17679521   1st Qu.:1970  
    ##  Median :-73.99        34969  :    3097                         Median :1983  
    ##  Mean   :-73.98        34187  :    3093                         Mean   :1980  
    ##  3rd Qu.:-73.97        34295  :    3064                         3rd Qu.:1990  
    ##  Max.   :-73.88        35226  :    3060                         Max.   :2003  
    ##                        (Other):20533132                                       
    ##      gender        
    ##  Unknown: 1560308  
    ##  Male   :14052393  
    ##  Female : 4938996  
    ##                    
    ##                    
    ##                    
    ## 

From the above summary, I can compute certain descriptive summary
statistics for many of the provided variables.

- For gender, the majority of the data entries are from male riders. Of
  the 20.55 million entries, 68% are from males, 24% are from females,
  and the remaining 8% are from individuals with unknown gender.

- For user type, the majority of the data (86%) represents subscribers,
  with the remaining 14% representing regular customers (tourists or
  other non-regular riders).

- For trip duration, the range is anywhere from 61 to 3.8 million
  seconds (1 minute to ~38.5 days). The data is clearly very skewed
  right, as 75% of the duration data falls between 61 and 1,079 seconds
  (1 to 17.98 minutes) and 50% falls between 61 and 615 seconds (1 to
  10.25 minutes). Overall, the average (mean) trip duration is 978
  seconds (16.3 minutes).

- For birth year, there is definitely some left-hand “noise,” or data
  that should be excluded for later analyses, from the fact that the
  minimum birth year is 1857. Even if this observation accurately
  reflects a 162-year-old rider, it’s likely such extreme outliers
  should be omitted for future deeper analysis into relationships that
  may depend on rider age.

- Ignoring this noise, for now, shows that 25% of the birth year data
  falls from 1970-1983 (ages 36-49), another 25% falls from 1983-1990
  (ages 29-36), and another 25% encompasses 1990-2003 (ages 16-29).
  Thus, the birth year variable shows that half of the observations, the
  middle 50%, are concentrated to the age range of 29-49.

### 5. Data Manipulation

In order to reduce computing time for all further analyses, I will
create a smaller sample of the data which contains 20% of the complete
data set. I can then compare the statistical properties to confirm the
necessary data similarity before going ahead.

``` r
bikes <- sample_frac(bikes, 0.20)

str(bikes)
```

    ## 'data.frame':    4110339 obs. of  15 variables:
    ##  $ tripduration           : int  999 1783 1467 587 590 1036 466 584 3055 1457 ...
    ##  $ starttime              : chr  "2019-12-14 09:27:00.8200" "2019-10-25 14:51:25.3510" "2019-11-19 09:02:30.4560" "2019-05-30 01:32:28.3470" ...
    ##  $ stoptime               : chr  "2019-12-14 09:43:40.1590" "2019-10-25 15:21:08.6890" "2019-11-19 09:26:57.9720" "2019-05-30 01:42:16.3190" ...
    ##  $ start.station.id       : Factor w/ 937 levels "116","119","120",..: 82 302 131 867 204 712 863 463 197 223 ...
    ##  $ start.station.name     : chr  "E 15 St & 3 Ave" "W 100 St & Manhattan Ave" "Hope St & Union Ave" "Dean St & 4 Ave" ...
    ##  $ start.station.latitude : num  40.7 40.8 40.7 40.7 40.8 ...
    ##  $ start.station.longitude: num  -74 -74 -74 -74 -74 ...
    ##  $ end.station.id         : Factor w/ 974 levels "116","119","120",..: 814 358 90 857 739 163 268 574 188 454 ...
    ##  $ end.station.name       : chr  "W 26 St & 10 Ave" "E 66 St & Madison Ave" "Broadway & Battery Pl" "Hicks St & Montague St" ...
    ##  $ end.station.latitude   : num  40.7 40.8 40.7 40.7 40.8 ...
    ##  $ end.station.longitude  : num  -74 -74 -74 -74 -74 ...
    ##  $ bikeid                 : Factor w/ 19571 levels "14529","14530",..: 19182 11824 4189 11592 10018 11934 10220 3327 14099 12546 ...
    ##  $ usertype               : Factor w/ 2 levels "Customer","Subscriber": 2 1 2 2 2 1 1 2 2 1 ...
    ##  $ birth.year             : int  1979 1969 1989 1994 1988 1969 1969 1968 1971 1999 ...
    ##  $ gender                 : Factor w/ 3 levels "Unknown","Male",..: 2 1 2 2 2 1 1 3 3 2 ...

``` r
summary(bikes)
```

    ##   tripduration        starttime           stoptime         start.station.id 
    ##  Min.   :     61.0   Length:4110339     Length:4110339     519    :  31438  
    ##  1st Qu.:    362.0   Class :character   Class :character   497    :  24462  
    ##  Median :    615.0   Mode  :character   Mode  :character   3255   :  23891  
    ##  Mean   :    980.2                                         402    :  22750  
    ##  3rd Qu.:   1078.0                                         285    :  22724  
    ##  Max.   :3100420.0                                         435    :  22009  
    ##                                                            (Other):3963065  
    ##  start.station.name start.station.latitude start.station.longitude
    ##  Length:4110339     Min.   :40.66          Min.   :-74.03         
    ##  Class :character   1st Qu.:40.72          1st Qu.:-74.00         
    ##  Mode  :character   Median :40.74          Median :-73.99         
    ##                     Mean   :40.74          Mean   :-73.98         
    ##                     3rd Qu.:40.76          3rd Qu.:-73.97         
    ##                     Max.   :40.87          Max.   :-73.88         
    ##                                                                   
    ##  end.station.id    end.station.name   end.station.latitude
    ##  519    :  30995   Length:4110339     Min.   :40.66       
    ##  497    :  25167   Class :character   1st Qu.:40.72       
    ##  402    :  24605   Mode  :character   Median :40.74       
    ##  3255   :  24114                      Mean   :40.74       
    ##  426    :  23363                      3rd Qu.:40.76       
    ##  285    :  22937                      Max.   :40.87       
    ##  (Other):3959158                                          
    ##  end.station.longitude     bikeid              usertype         birth.year  
    ##  Min.   :-74.08        35330  :    647   Customer  : 574208   Min.   :1857  
    ##  1st Qu.:-74.00        35276  :    633   Subscriber:3536131   1st Qu.:1970  
    ##  Median :-73.99        35152  :    632                        Median :1983  
    ##  Mean   :-73.98        34104  :    631                        Mean   :1980  
    ##  3rd Qu.:-73.97        35316  :    630                        3rd Qu.:1990  
    ##  Max.   :-73.88        35052  :    625                        Max.   :2003  
    ##                        (Other):4106541                                      
    ##      gender       
    ##  Unknown: 311981  
    ##  Male   :2809819  
    ##  Female : 988539  
    ##                   
    ##                   
    ##                   
    ## 

- The smaller sample of the data (4.11 million records) now represents
  68% males, 24% females, and 8% unknown gender, which is very
  consistent with the statistical results found from the original data
  set.

- The proportion of each user type also remained consistent, now with
  85% of the data set representing subscribers with the remaining 15%
  representing regular customers.

- The distribution of trip duration is also incredibly similar; the mean
  has only slightly decreased due to the maximum trip duration outlier
  significantly decreasing.

- Thus, the smaller subset of the entire data collection is sufficiently
  similar to the complete data set. Knowing the conclusions we draw will
  be essentially the same, we can confidently proceed with analyzing and
  visualizing this subset of data.

### 6. Field Creation

Below are newly created variables such as: duration, speed, and age for
more insightful analysis:

``` r
bikes$distance <- distHaversine(bikes[ ,c("start.station.longitude", "start.station.latitude")],bikes[ ,c("end.station.longitude", "end.station.latitude")])
bikes$distance <- ifelse(bikes$distance > 20000, 20000, bikes$distance)

bikes$speed <- bikes$distance/bikes$tripduration

bikes$age <- 2019 - bikes$birth.year
bikes <- select(bikes, -(birth.year))
```

It’s important to note that the distance variable gives the shortest
possible straight-line ride between the two given points, and therefore
the values are underestimates of the real distances actually traveled.
This will cause our speed variables to be underestimates of the actual
speeds.

Then, the time variables are formatted and categorized in order to
streamline exploration involving month, day, and/or time of day. For
example, I can assign times into four equally long “time of day”
categories: morning (6am-12pm), afternoon (12pm-6pm), evening
(6pm-12am), and night (12am-6am). I also can create data that indicates
whether each ride occurred on a weekday or a weekend.

``` r
bikes$starttime <- as.POSIXct(bikes$starttime,format = c("%Y-%m-%d %H: %M"))
bikes$stoptime <- as.POSIXct(bikes$stoptime,format = c("%Y-%m-%d %H: %M"))

bikes$time <- as.numeric(format(bikes$starttime, "%H%M"))
bikes$daytime <- ifelse(bikes$time >=600 & bikes$time < 1200, "Morning", ifelse(bikes$time >=1200 & bikes$time < 1800, "Afternoon", ifelse(bikes$time >=1800 & bikes$time <2400, "Evening", "Night")))
bikes$daytime <- as.factor(bikes$daytime)
bikes <- select(bikes, -(time))

bikes$hour <- as.numeric(format(bikes$starttime, "%H"))
bikes$month <- as.numeric(format(bikes$starttime, "%m"))
bikes$month <- as.factor(bikes$month)
levels(bikes$month) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

bikes$date <- as.Date(bikes$starttime)

bikes$weekday <- wday(bikes$starttime)
bikes$weekday <- as.factor(bikes$weekday)
levels(bikes$weekday) <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
bikes$weekend <- ifelse(bikes$weekday == "Sat" | bikes$weekday == "Sun", "Weekend", "Weekday")
bikes$weekend <- as.factor(bikes$weekend)
```

## C. Exploratory Data Analysis

### 1. Distribution of Trip Duration

Before diving into the data, any extreme outliers for trip duration need
to be filtered out. Trip duration should also be converted to minutes
for more practical analysis.

``` r
bikest <- filter(bikes, tripduration < 3600)

bikest$tripduration <- bikest$tripduration/60

hist(bikest$tripduration, 30, main = "Distribution of Trip Duration", xlab = "Time (minutes)", probability = TRUE)
```

![1  trip_duration_hist](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/d7ce3d8b-d6f4-41f4-9168-ff9bcfc2f5f9)

The above plot is a probability histogram showing that the distribution
of trip durations is heavily skewed to the right, as discussed earlier.
The majority of trips last less than 11 minutes. By adding up the
probabilities, we can estimate that the probability of any given ride
lasting fewer than 11 minutes is approximately 25%.

### 2. Distribution of Age

``` r
hist(bikes$age, main = "Distribution of Age", xlab = "Age", probability = TRUE)
```

![2  age_hist](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/d777ddc1-584b-4895-a772-86fcba9f8393)

The probability histogram of ages shows a few interesting things with
its shape. The first is that this histogram does not represent a normal
distribution and that the majority of the riders are between 23 and 43
years old. By adding up the probabilities, we can estimate that the
probability of any given rider’s age falling from 23-43 is approximately
12-13%. Also worth noting, there seems to be a noticeable subset of the
data of riders aged 48 to 53.

Next, the blank rows for age need to be removed so that the analysis is
more complete.

``` r
bikes_no.age.na <- subset(bikes, (is.na(bikes$age) == FALSE) )

boxplot(bikes_no.age.na$age, main = "Age")
```

![3  age_box](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/241eb01d-e3c2-4103-a904-37c44276cd00)

The boxplot shows that there are a significant number of outliers, which
is not surprising considering the age/birth year problems identified and
briefly discussed earlier. The following changes need to be made to the
age field:

- 871 observations with an age greater than 90 will be deleted.

- 3,245 observations with an age between 75 and 90 will be grouped
  together as age 75. For convenience, these records will not be named
  “75+” so that the program does not return a data type error.

``` r
bikes_no.age.na <- subset(bikes_no.age.na, bikes_no.age.na$age < 90)
bikes_no.age.na$age <- ifelse(bikes_no.age.na$age > 74, 75, bikes_no.age.na$age)
```

This ended up deleting 12% and manipulated a very small percentage of
the original data in bikes.

### 3. Travel Distance

``` r
hist(bikes$distance, main = "Distribution of Trip Distance", xlab = "Distance (m)", probability = TRUE)
```

![4  dist_hist](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/3ca01664-7b28-4022-931f-fec1cb50a754)

Similar to the previous probability histograms, we see that the
distribution of trip distances is skewed right with some
very-small-probability and extremely high-distance outliers.

To understand how the average trip distance may be changing over the
course of the year, average distance as a function of the month provides
a clearer picture.

``` r
barplot(tapply(X = bikes$distance, INDEX = bikes$month, FUN = mean, na.rm =T), names.arg = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ylim = c(0, 2000), main = "Average Travel Distance per Month", ylab = "Distance (m)")
```

![5  dist_trend](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/8afcf460-76d7-463b-9ac7-c58f5e1ca189)

- Though average distance over the course of the year appears to be
  relatively stable, there is still a noticeable increase in the average
  trip distance for spring, summer, and fall months over winter months.
  The average distance appears to be around 1600m in Jan and Feb, while
  the average distance jumps to around 1800m for Jun through Sep.

## D. Identifying Trends in the data

### 1. Traffic Patterns

``` r
plot_uh <- ggplot(data = bikes, aes(x=hour, colour = daytime))
plot_uh + geom_bar() + facet_wrap( ~month) + ggtitle("Rides per Hour Separated by Month")
```

![6  traffic_daytime](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/2fd724c0-1cf4-4b79-a412-fe25eae3bac2)

- This plot shows that overall CitiBike traffic for 2019 has been
  increasing steadily throughout the year and then falling after
  Sep. Dec and Feb had the lowest traffic months while Aug and Sep being
  the highest traffic months.

- For every month, the largest amount of traffic (highest rides per
  hour) occur from 5-6pm, which makes sense as this timeframe is
  consistent with the “rush hour” traffic associated with the end of the
  workday. For every month, the second highest peak throughout the day
  occurs at 8am.

- As expected, the overall traffic pattern (peaks at 8am and 5-6pm,
  steady afternoon traffic, and a steep lull in nighttime traffic) is
  pretty consistent from month to month.

``` r
plot_6 <- ggplot(bikes, aes(x=hour, fill = weekday, label = hour))
plot_6+geom_bar()+ggtitle("Average Rides per Hour")+labs(x= "Hour of the Day", y= "Amount of Rides")
```

![7  dayofweek_time](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/12408059-cca3-4a5b-9c1d-60315d584ea0)

``` r
plot_uh2 <- ggplot(data = bikes, aes(x=hour, colour = daytime))
plot_uh2 + geom_bar()+facet_wrap( ~weekday)+ggtitle("Rides per Hour Separated by Weekday")
```

![8  dayofweek_time2](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/8f27f553-0523-4eed-8246-9754de2cfcfa)

The above plots further reveal that the overall traffic pattern
discussed above arises as the “average” due to the “weekday” data
dominating over the “weekend” data.

As seen from above, the two-peak traffic pattern is very pronounced for
all weekdays. On weekends, a very different traffic pattern emerges.
Instead of the two peaks associated with commuting to/from work or
school, the traffic pattern on Saturdays and Sundays displays a hump,
which resembles a normal distribution. On these days, peak traffic
occurs from 2-3pm and the traffic drops at a smaller slope into the
evening hours, showing there is more evening traffic than morning
traffic. It is also worth noting that the weekend days show more
nighttime traffic (especially 12-2am) than weekdays.

Further comparing weekends to weekdays while also taking date/month into
account, we plot the number of rides per day as a function of both
variables.

``` r
bydate <- group_by(bikes, date)
date_summary1 <- summarise(bydate, count = n(), distance = mean(distance, na.rm =  T), weekend = weekend[1])
plot_3 <- ggplot(date_summary1, aes(x=date, y= count , colour = weekend))
plot_3+geom_point()+geom_smooth()+ggtitle("Rides per Day")+labs(x = "Date", y="Number of Rides")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![9  dayofweek_trend](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/9e75d785-8b9e-4b57-9578-c44a13bba0e3)

Two traffic conclusions can be drawn from the above graph:

- On average, weekdays have more traffic (more rides per day) than
  weekend days consistently throughout the entire year of 2019.

- For every day, both weekdays and weekends, the overall traffic
  significantly increases over the course of the year and then drops
  after Sep. This can be attributed to riders gradually wanting to rent
  bikes during warmer times of the year and then choosing other forms of
  transportation during late fall.

### 2. Frequent Route Patterns

To identify which routes are being most frequently traveled, the
starting and ending destinations first need to be combined into a new
variable. And then it will be more apprarent which of these travel
patterns are recurring most frequently by plotting this new variable.

``` r
bikes$start_end <- paste(bikes$start.station.name, bikes$end.station.name, sep = "-->")

barplot(sort(table(bikes$start_end),decreasing=TRUE)[1:5], main = "Frequencies of Most Recurrent Routes", ylab = "Frequency", las = 1, cex.names = 0.25)
```

![10  freq_routes](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/755009c1-4f62-4bf8-8d0a-c7bb5ed61e3f)

- From this plot, we can see that the most frequent route pattern, is
  the route which starts at the station on E 7 St & Avenue A and ends at
  the station on Cooper Square and Astor.

- The next two start/end stations involve ones around Central Park,
  which makes sense as that area is highly populated day to day.

### 3. Speed Patterns

``` r
plot_as <- ggplot(bikes_no.age.na, aes(x = age, y = speed, colour = gender))
plot_as + geom_smooth()+ggtitle("Average Traveling Speed as a Function of Age")+labs(x = "Age", y="Speed (m/s)")
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![11  speed_age_gender](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/f776b2c1-ed14-4d6e-9124-17eb10f48fe7)

- There seems to be a pattern when looking at speed through the lense of
  age and gender. On average, males bicycle at a faster speed than
  females. Interestingly, this pattern becomes less and less true male
  and female riders approach the age of 70.

- As expected, average speed does gradually decline as age increases,
  but I did not expect that speed also declines for young riders
  beginning at age 20. Both male and females’ average cycling speed
  peaks at age 25. In general, a peak average speed of about 2.6 m/s
  occurs for males aged 25-30.

### 4. Duration Patterns

This section examines whether there are any discernable patterns between
ride duration, time of year, and weekday vs. weekend.

``` r
date_summary4 <- summarise(bydate, tripduration = mean(tripduration, na.rm = T)/60, count = mean(distance, na.rm =T), colour = weekend[1])
plot_4 <- ggplot(date_summary4, aes(x=date, y=tripduration, colour = colour))
plot_4 + geom_point() + geom_smooth() + ggtitle("Average Trip Duration per Ride") + labs(x = "Date", y = "Trip Duration (minutes)")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![12  duration_dow](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/efce4d9d-a3ba-48b7-bcd1-29fef91f6c9c)

- Weekend trip durations are, on average, longer than weekday trip
  durations. Also, the shaded regions around each line show that there’s
  much more variation in the average weekend trip duration; the average
  weekday trip duration is much more stable, which makes sense as users
  are more likely to be on a fixed path instead of exploring around NYC.

- Similar to the increasing patterns of traffic frequency, average trip
  duration appears to noticeably increase over the course of the year
  and then decrease. This pattern is more noticeable for weekend trips
  than weekday trips, however.

- For weekend trips, this peak average is around 22 minutes per trip;
  for weekday trips, this peak average significantly drops to about 17
  minutes.

### 6. Asymmetric Traffic Patterns

Since September had the most activity, I will examine the amount of
incoming and leaving bikes per station only for this month. The same
code would run for the entire period of nine months, but it would use
too much computing power. First, the dataset for September needs to be
prepared by running a couple of the same commands from earlier sections.

``` r
sep$starttime <- as.POSIXct(sep$starttime,format = c("%Y-%m-%d %H: %M"))
sep$stoptime <- as.POSIXct(sep$stoptime,format = c("%Y-%m-%d %H: %M"))

sep$date <- as.Date(sep$starttime)

sep$weekday <- wday(sep$starttime)
sep$weekday <- as.factor(sep$weekday)
levels(sep$weekday) <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
sep$weekend <- ifelse(sep$weekday == "Sat" | sep$weekday == "Sun", "Weekend", "Weekday")
sep$weekend <- as.factor(sep$weekend)
```

I want to analyze the loss and gain of bikes on weekends and weekdays.
The first map shows the changes for weekends while the second map
displays changes for weekdays. Stations with an average gain of bikes
each day are in green and the average loss in red. The size of the
circle indicates the relative amount of bikes being lost or gained.

``` r
stations <- filter(sep, weekend == "Weekday")

bikes.departures <- table(stations$start.station.name)
bikes.arrivals <- table(stations$end.station.name)
departure.arrivals <- merge(bikes.departures, bikes.arrivals, by="Var1")

station.list <- distinct(select (bikes, start.station.name, start.station.latitude, start.station.longitude))
station.list <- rename(station.list, "Var1" = start.station.name)

departure.arrivals <- merge(departure.arrivals, station.list, by = "Var1")

departure.arrivals <- rename(departure.arrivals, "number.dep" = Freq.x, "number.arr" = Freq.y, "start.station.name" = Var1, "latitude" = start.station.latitude, "longitude" = start.station.longitude)

departure.arrivals$sum <- (departure.arrivals$number.arr - departure.arrivals$number.dep)/22

map4 <- departure.arrivals[order(departure.arrivals$sum, decreasing = T),]
map4 <- map4[1:20,]
map5 <- departure.arrivals[order(departure.arrivals$sum, decreasing = F),]
map5 <- map5[1:20,]
map6 <- rbind(map5, map4)
map6 <- map6[order(map6$sum, decreasing = T),]

leaflet(data = map6) %>% addTiles() %>% addCircleMarkers (radius = ~ abs(sum)/4, color = ~ ifelse(sum > 0, "green", "red"), stroke = FALSE, fillOpacity = 0.5, ~ longitude, ~ latitude, label = ~ paste("Station: ", start.station.name, "," , ifelse(sum > 0, "Average gain per day: ", "Average loss per day: "), round(abs(sum), digits = 2)))
```

![13  map1](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/772db183-f638-4323-9965-c5e7b669db14)

For weekdays, there seems to be a pattern of stations losing bikes
around Central Park and Manhattan area while gaining bikes in the South
side of NYC. There are many stations that lose bikes around the
Stuyvesant Town area.

- The station which lost the most bikes per day on weekdays was Columbus
  Ave & W 72 St, with an average loss of 61.82 bikes.

- The station which gained the most bikes per day on weekdays was West
  St & Chambers St, with an average gain of 80.95 bikes.

``` r
stations <- filter(bikes, weekend == "Weekend")

bikes.departures <- table(stations$start.station.name)
bikes.arrivals <- table(stations$end.station.name)
departure.arrivals <- merge(bikes.departures, bikes.arrivals, by="Var1")

station.list <- distinct(select (bikes, start.station.name, start.station.latitude, start.station.longitude))
station.list <- rename(station.list, "Var1" = start.station.name)

departure.arrivals <- merge(departure.arrivals, station.list, by = "Var1")
departure.arrivals <- rename(departure.arrivals, "number.dep" = Freq.x, "number.arr" = Freq.y, "start.station.name" = Var1, "latitude" = start.station.latitude, "longitude" = start.station.longitude)

departure.arrivals$sum <- (departure.arrivals$number.arr - departure.arrivals$number.dep)/8

map <- departure.arrivals[order(departure.arrivals$sum, decreasing = T),]
map <- map[1:20,]
map1 <- departure.arrivals[order(departure.arrivals$sum, decreasing = F),]
map1 <- map1[1:20,]
map3 <- rbind(map, map1)
map3 <- map3[order(map3$sum, decreasing = T),]

leaflet(data = map3) %>% addTiles() %>% addCircleMarkers (radius = ~ abs(sum)/2, color = ~ ifelse(sum > 0, "green", "red"), stroke = FALSE, fillOpacity = 0.5, ~ longitude, ~ latitude, label = ~ paste("Station: ", start.station.name, "," , ifelse(sum > 0, "Avergae gain per day: ", "Average loss per day: "), round(abs(sum), digits = 2)))
```

![14  map2](https://github.com/martinbauer1/Citibike-Ride-Activity/assets/154390228/146d05c2-25ac-4bea-8bd2-873e5df3b145)

There seems to be a pattern of rides beginning in the upper part of
Manhattan and ending in the lower part when people use citiBike on
weekends. Furthermore the stations with the biggest loss and gain have
changed from typical commuting locations (train stations, etc. ) to
touristic hotspots like Central Park or lower Manhattan.

For weekends, there seems to be a similar pattern of stations of gains
and loses seen during weekdays but at a much higher volume. There are a
lot of losses around Central Park and Penn Station while gains bikes in
the South side of NYC and south of FDR Drive. Furthermore, the stations
with the biggest losses and gains are more pronounced around touristic
hotspots like Central Park or lower Manhattan rather than typical
commuting locations.

- The station which lost the most bikes per day on weekends was Grand
  Army Plaza & Central Park S, with an average loss of 122.75 bikes.

- The station which gained the most bikes per day on weekends was Jay St
  & York St, with an average gain of 80.75 bikes.

## Thank You!
