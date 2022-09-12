---
title: "Task 5"
author: "Brink"
date: "9/8/2022"
output: 
  html_document:
    keep_md: TRUE
    code_folding: 'hide'
---




```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
## ✓ tibble  3.1.6     ✓ dplyr   1.0.8
## ✓ tidyr   1.2.0     ✓ stringr 1.4.0
## ✓ readr   2.1.2     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(knitr)
library(downloader)
```

All data showed to be imported correctly in the form of characters

```r
solodata <- read_csv("solo-artist-followers.csv")
```

```
## Rows: 139 Columns: 5
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (5): name, band, followers, band_followers, follower_difference
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
solodata
```

```
## # A tibble: 139 × 5
##    name              band              followers band_followers follower_differ…
##    <chr>             <chr>             <chr>     <chr>          <chr>           
##  1 Daron Jones       112               1.28k     783k           −782k           
##  2 Slim              112               2.14k     783k           −781k           
##  3 Q Parker          112               3.51k     783k           −780k           
##  4 JC Chasez         *NSYNC            30.8k     1.44M          −1.41M          
##  5 Joey Fatone       *NSYNC            1.13k     1.44M          −1.44M          
##  6 Justin Timberlake *NSYNC            10.3M     1.44M          8.90M           
##  7 Ashton Irwin      5 Seconds of Sum… 130k      7.14M          −7.01M          
##  8 Abz Love          5ive              223       19.0k          −18.7k          
##  9 Jeff Timmons      98º               111       302k           −302k           
## 10 Nick Lachey       98º               142k      302k           −160k           
## # … with 129 more rows
```

```r
str(solodata)
```

```
## spec_tbl_df [139 × 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ name               : chr [1:139] "Daron Jones" "Slim" "Q Parker" "JC Chasez" ...
##  $ band               : chr [1:139] "112" "112" "112" "*NSYNC" ...
##  $ followers          : chr [1:139] "1.28k" "2.14k" "3.51k" "30.8k" ...
##  $ band_followers     : chr [1:139] "783k" "783k" "783k" "1.44M" ...
##  $ follower_difference: chr [1:139] "−782k" "−781k" "−780k" "−1.41M" ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   name = col_character(),
##   ..   band = col_character(),
##   ..   followers = col_character(),
##   ..   band_followers = col_character(),
##   ..   follower_difference = col_character()
##   .. )
##  - attr(*, "problems")=<externalptr>
```
 
Correct import for data type for date, title,name,band as character

```r
billboard_data<- read_csv("billboard-hits.csv")
```

```
## Rows: 456 Columns: 5
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (3): name, band, title
## dbl  (1): peak_rank
## date (1): peak_date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
billboard_data
```

```
## # A tibble: 456 × 5
##    name   band  title                     peak_date  peak_rank
##    <chr>  <chr> <chr>                     <date>         <dbl>
##  1 *NSYNC <NA>  It's Gonna Be Me          2000-07-28         1
##  2 *NSYNC <NA>  Music Of My Heart         1999-10-15         2
##  3 *NSYNC <NA>  Bye Bye Bye               2000-04-14         4
##  4 *NSYNC <NA>  This I Promise You        2000-12-01         5
##  5 *NSYNC <NA>  Girlfriend                2002-04-05         5
##  6 *NSYNC <NA>  A Little More Time On You 1999-02-26         8
##  7 *NSYNC <NA>  Gone                      2001-11-23        11
##  8 *NSYNC <NA>  I Want You Back           1998-05-01        13
##  9 *NSYNC <NA>  Pop                       2001-06-15        19
## 10 *NSYNC <NA>  Tearin' Up My Heart       1998-12-04        59
## # … with 446 more rows
```

```r
str(billboard_data)
```

```
## spec_tbl_df [456 × 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ name     : chr [1:456] "*NSYNC" "*NSYNC" "*NSYNC" "*NSYNC" ...
##  $ band     : chr [1:456] NA NA NA NA ...
##  $ title    : chr [1:456] "It's Gonna Be Me" "Music Of My Heart" "Bye Bye Bye" "This I Promise You" ...
##  $ peak_date: Date[1:456], format: "2000-07-28" "1999-10-15" ...
##  $ peak_rank: num [1:456] 1 2 4 5 5 8 11 13 19 59 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   name = col_character(),
##   ..   band = col_character(),
##   ..   title = col_character(),
##   ..   peak_date = col_date(format = ""),
##   ..   peak_rank = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

```r
head(billboard_data)
```

```
## # A tibble: 6 × 5
##   name   band  title                     peak_date  peak_rank
##   <chr>  <chr> <chr>                     <date>         <dbl>
## 1 *NSYNC <NA>  It's Gonna Be Me          2000-07-28         1
## 2 *NSYNC <NA>  Music Of My Heart         1999-10-15         2
## 3 *NSYNC <NA>  Bye Bye Bye               2000-04-14         4
## 4 *NSYNC <NA>  This I Promise You        2000-12-01         5
## 5 *NSYNC <NA>  Girlfriend                2002-04-05         5
## 6 *NSYNC <NA>  A Little More Time On You 1999-02-26         8
```

Singers without 6 top 100 hits

```r
sixtop100 <- billboard_data %>%
  group_by(name) %>%
  filter(n() > 6, band != "")
sixtop100
```

```
## # A tibble: 144 × 5
## # Groups:   name [10]
##    name              band   title                           peak_date  peak_rank
##    <chr>             <chr>  <chr>                           <date>         <dbl>
##  1 Justin Timberlake *NSYNC SexyBack                        2006-09-08         1
##  2 Justin Timberlake *NSYNC My Love                         2006-11-10         1
##  3 Justin Timberlake *NSYNC What Goes Around...Comes Around 2007-03-02         1
##  4 Justin Timberlake *NSYNC Can't Stop The Feeling!         2016-05-27         1
##  5 Justin Timberlake *NSYNC Mirrors                         2013-06-14         2
##  6 Justin Timberlake *NSYNC Cry Me A River                  2003-01-31         3
##  7 Justin Timberlake *NSYNC Suit & Tie                      2013-04-05         3
##  8 Justin Timberlake *NSYNC Rock Your Body                  2003-05-09         5
##  9 Justin Timberlake *NSYNC Summer Love                     2007-06-08         6
## 10 Justin Timberlake *NSYNC Not A Bad Thing                 2014-05-02         8
## # … with 134 more rows
```
144 rows


```r
sixtopbands <- billboard_data %>%
  group_by(band) %>%
  filter(name %in% sixtop100$band)
head(sixtopbands)
```

```
## # A tibble: 6 × 5
## # Groups:   band [1]
##   name   band  title                     peak_date  peak_rank
##   <chr>  <chr> <chr>                     <date>         <dbl>
## 1 *NSYNC <NA>  It's Gonna Be Me          2000-07-28         1
## 2 *NSYNC <NA>  Music Of My Heart         1999-10-15         2
## 3 *NSYNC <NA>  Bye Bye Bye               2000-04-14         4
## 4 *NSYNC <NA>  This I Promise You        2000-12-01         5
## 5 *NSYNC <NA>  Girlfriend                2002-04-05         5
## 6 *NSYNC <NA>  A Little More Time On You 1999-02-26         8
```


```r
ggplot(data = sixtop100, aes(x=peak_date, y=peak_rank, color=name, group=name)) +
  geom_point() +
  geom_line() +
  geom_point(data = sixtopbands, color="black") +
  geom_line(data = sixtopbands, color="black", linetype = "dotted") +
  facet_wrap(~ band, scales = "free") +
  xlab("peak_rank") + ylab("peak_rank") +
  theme_bw()
```

![](Task-5_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
OBSERVATIONS: The thriving appeal of group based music artists continued over time, some failed due to the lack of adapting to modern social media demands. This allowed groups like One Direction and Fifth Harmony to thrive and Destiny's Child and New Edition to fall, until nostalgia brings them back. 

https://www.kaggle.com/datasets/gregorut/videogamesales?resource=download
This data set ranks data collected by vgchartz which categorizes highest sales based on platofrom, genre, publisher, as well as sales related to geographical locations. 

```r
videogamesales <- read_csv("vgsales.csv")
```

```
## Rows: 16598 Columns: 11
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (5): Name, Platform, Year, Genre, Publisher
## dbl (6): Rank, NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
videogamesales
```

```
## # A tibble: 16,598 × 11
##     Rank Name          Platform Year  Genre Publisher NA_Sales EU_Sales JP_Sales
##    <dbl> <chr>         <chr>    <chr> <chr> <chr>        <dbl>    <dbl>    <dbl>
##  1     1 Wii Sports    Wii      2006  Spor… Nintendo      41.5    29.0      3.77
##  2     2 Super Mario … NES      1985  Plat… Nintendo      29.1     3.58     6.81
##  3     3 Mario Kart W… Wii      2008  Raci… Nintendo      15.8    12.9      3.79
##  4     4 Wii Sports R… Wii      2009  Spor… Nintendo      15.8    11.0      3.28
##  5     5 Pokemon Red/… GB       1996  Role… Nintendo      11.3     8.89    10.2 
##  6     6 Tetris        GB       1989  Puzz… Nintendo      23.2     2.26     4.22
##  7     7 New Super Ma… DS       2006  Plat… Nintendo      11.4     9.23     6.5 
##  8     8 Wii Play      Wii      2006  Misc  Nintendo      14.0     9.2      2.93
##  9     9 New Super Ma… Wii      2009  Plat… Nintendo      14.6     7.06     4.7 
## 10    10 Duck Hunt     NES      1984  Shoo… Nintendo      26.9     0.63     0.28
## # … with 16,588 more rows, and 2 more variables: Other_Sales <dbl>,
## #   Global_Sales <dbl>
```

```r
str(videogamesales)
```

```
## spec_tbl_df [16,598 × 11] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ Rank        : num [1:16598] 1 2 3 4 5 6 7 8 9 10 ...
##  $ Name        : chr [1:16598] "Wii Sports" "Super Mario Bros." "Mario Kart Wii" "Wii Sports Resort" ...
##  $ Platform    : chr [1:16598] "Wii" "NES" "Wii" "Wii" ...
##  $ Year        : chr [1:16598] "2006" "1985" "2008" "2009" ...
##  $ Genre       : chr [1:16598] "Sports" "Platform" "Racing" "Sports" ...
##  $ Publisher   : chr [1:16598] "Nintendo" "Nintendo" "Nintendo" "Nintendo" ...
##  $ NA_Sales    : num [1:16598] 41.5 29.1 15.8 15.8 11.3 ...
##  $ EU_Sales    : num [1:16598] 29.02 3.58 12.88 11.01 8.89 ...
##  $ JP_Sales    : num [1:16598] 3.77 6.81 3.79 3.28 10.22 ...
##  $ Other_Sales : num [1:16598] 8.46 0.77 3.31 2.96 1 0.58 2.9 2.85 2.26 0.47 ...
##  $ Global_Sales: num [1:16598] 82.7 40.2 35.8 33 31.4 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   Rank = col_double(),
##   ..   Name = col_character(),
##   ..   Platform = col_character(),
##   ..   Year = col_character(),
##   ..   Genre = col_character(),
##   ..   Publisher = col_character(),
##   ..   NA_Sales = col_double(),
##   ..   EU_Sales = col_double(),
##   ..   JP_Sales = col_double(),
##   ..   Other_Sales = col_double(),
##   ..   Global_Sales = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```
ALL CHARACTER VALUES

http://www.thetransparencyproject.org/download_index.php
This package contains the analytic data set for the first longitudinal analysis of online gambling participation and activity among a population of newly subscribed Internet bettors

```r
gamblingdata <- read_csv("PopTrendsBData1Fixed.csv")
```

```
## Rows: 2108616 Columns: 5
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl  (4): UserID, StakeF, WinF, BetsF
## date (1): DateBet
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
gamblingdata
```

```
## # A tibble: 2,108,616 × 5
##     UserID DateBet    StakeF  WinF BetsF
##      <dbl> <date>      <dbl> <dbl> <dbl>
##  1 1324354 2005-10-01   40      0      6
##  2 1324354 2005-10-12   50      0      2
##  3 1324354 2005-09-24  132.   132.     7
##  4 1324354 2006-06-18   12.6    0      2
##  5 1324354 2005-10-13   15      0      3
##  6 1324354 2005-09-27   60     60      2
##  7 1324354 2005-10-22  158    199.     3
##  8 1324354 2005-09-23   66    196      3
##  9 1324354 2005-09-26   91.2  151.     5
## 10 1324354 2005-09-29  100      0      2
## # … with 2,108,606 more rows
```

```r
str(gamblingdata)
```

```
## spec_tbl_df [2,108,616 × 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ UserID : num [1:2108616] 1324354 1324354 1324354 1324354 1324354 ...
##  $ DateBet: Date[1:2108616], format: "2005-10-01" "2005-10-12" ...
##  $ StakeF : num [1:2108616] 40 50 132.5 12.6 15 ...
##  $ WinF   : num [1:2108616] 0 0 132 0 0 ...
##  $ BetsF  : num [1:2108616] 6 2 7 2 3 2 3 3 5 2 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   UserID = col_double(),
##   ..   DateBet = col_date(format = ""),
##   ..   StakeF = col_double(),
##   ..   WinF = col_double(),
##   ..   BetsF = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```
ALL CHARACTER VALUES

https://www.kaggle.com/datasets/johnharshith/hollywood-theatrical-market-synopsis-1995-to-2021
The Dataset contains various files illustrating statistics such as annual ticket sales, highest grossers each year since 1995, top grossing creative types, top grossing distributors, top grossing genres, top grossing MPAA ratings, top grossing sources, top grossing production methods and the number of wide releases each year by various distributors.

```r
ticketsales <- read_csv("AnnualTicketSales.csv")
```

```
## New names:
## * `` -> ...6
```

```
## Rows: 27 Columns: 6
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (3): TOTAL BOX OFFICE, TOTAL INFLATION ADJUSTED BOX OFFICE, AVERAGE TICK...
## dbl (1): YEAR
## lgl (1): ...6
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
ticketsales
```

```
## # A tibble: 27 × 6
##     YEAR `TICKETS SOLD` `TOTAL BOX OFF…` `TOTAL INFLATI…` `AVERAGE TICKE…` ...6 
##    <dbl>          <dbl> <chr>            <chr>            <chr>            <lgl>
##  1  2021      423774881 $3,881,777,912   $3,881,777,912   $9.16            NA   
##  2  2020      223638958 $2,048,534,616   $2,048,534,616   $9.16            NA   
##  3  2019     1228541629 $11,253,443,955  $11,253,444,050  $9.16            NA   
##  4  2018     1311536128 $11,948,096,650  $12,013,670,952  $9.11            NA   
##  5  2017     1225639761 $10,993,991,460  $11,226,860,216  $8.97            NA   
##  6  2016     1302556378 $11,267,115,924  $11,931,416,424  $8.65            NA   
##  7  2015     1323356776 $11,155,900,636  $12,121,948,075  $8.43            NA   
##  8  2014     1257402920 $10,272,985,008  $11,517,810,744  $8.17            NA   
##  9  2013     1339168926 $10,887,446,341  $12,266,787,382  $8.13            NA   
## 10  2012     1380921942 $10,992,141,616  $12,649,244,986  $7.96            NA   
## # … with 17 more rows
```

```r
str(ticketsales)
```

```
## spec_tbl_df [27 × 6] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ YEAR                               : num [1:27] 2021 2020 2019 2018 2017 ...
##  $ TICKETS SOLD                       : num [1:27] 4.24e+08 2.24e+08 1.23e+09 1.31e+09 1.23e+09 ...
##  $ TOTAL BOX OFFICE                   : chr [1:27] "$3,881,777,912" "$2,048,534,616" "$11,253,443,955" "$11,948,096,650" ...
##  $ TOTAL INFLATION ADJUSTED BOX OFFICE: chr [1:27] "$3,881,777,912" "$2,048,534,616" "$11,253,444,050" "$12,013,670,952" ...
##  $ AVERAGE TICKET PRICE               : chr [1:27] "$9.16" "$9.16" "$9.16" "$9.11" ...
##  $ ...6                               : logi [1:27] NA NA NA NA NA NA ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   YEAR = col_double(),
##   ..   `TICKETS SOLD` = col_number(),
##   ..   `TOTAL BOX OFFICE` = col_character(),
##   ..   `TOTAL INFLATION ADJUSTED BOX OFFICE` = col_character(),
##   ..   `AVERAGE TICKET PRICE` = col_character(),
##   ..   ...6 = col_logical()
##   .. )
##  - attr(*, "problems")=<externalptr>
```
ALL CHARACTER VALUES
