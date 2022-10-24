---
title: "Task 15"
author: "Brink"
date: "10/24/2022"
output: 
  html_document:
    code_folding: 'hide'
    keep_md: TRUE
---

#Packages

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(downloader)
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
## ✔ tibble  3.1.6     ✔ dplyr   1.0.8
## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
## ✔ readr   2.1.2     ✔ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ lubridate::as.difftime() masks base::as.difftime()
## ✖ lubridate::date()        masks base::date()
## ✖ dplyr::filter()          masks stats::filter()
## ✖ lubridate::intersect()   masks base::intersect()
## ✖ dplyr::lag()             masks stats::lag()
## ✖ lubridate::setdiff()     masks base::setdiff()
## ✖ lubridate::union()       masks base::union()
```

```r
library(riem)
library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```
#Download File

```r
temp <- tempfile()
download.file("https://github.com/WJC-Data-Science/DTS350/raw/master/carwash.csv", temp, mode = "wb")
CW <- read_csv(temp)
```

```
## Rows: 533 Columns: 4
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): name, type
## dbl  (1): amount
## dttm (1): time
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```
# Check data

```r
head(CW)
```

```
## # A tibble: 6 × 4
##   name          type     time                amount
##   <chr>         <chr>    <dttm>               <dbl>
## 1 SplashandDash Services 2016-05-13 20:27:00    1  
## 2 SplashandDash Services 2016-05-13 20:27:00    0  
## 3 SplashandDash Services 2016-05-16 19:31:00   23.6
## 4 SplashandDash Services 2016-05-16 17:09:00   18.9
## 5 SplashandDash Services 2016-05-16 17:47:00   23.6
## 6 SplashandDash Services 2016-05-16 17:50:00   23.6
```

```r
tail(CW)
```

```
## # A tibble: 6 × 4
##   name          type     time                amount
##   <chr>         <chr>    <dttm>               <dbl>
## 1 SplashandDash Services 2016-07-08 18:36:00   50  
## 2 SplashandDash Services 2016-07-08 18:50:00   30  
## 3 SplashandDash Services 2016-07-08 21:29:00  -33.6
## 4 SplashandDash Services 2016-07-08 22:42:00   10  
## 5 SplashandDash Services 2016-07-08 22:51:00   15  
## 6 SplashandDash Services 2016-07-08 23:08:00    5
```

```r
str(CW)
```

```
## spec_tbl_df [533 × 4] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ name  : chr [1:533] "SplashandDash" "SplashandDash" "SplashandDash" "SplashandDash" ...
##  $ type  : chr [1:533] "Services" "Services" "Services" "Services" ...
##  $ time  : POSIXct[1:533], format: "2016-05-13 20:27:00" "2016-05-13 20:27:00" ...
##  $ amount: num [1:533] 1 0 23.6 18.9 23.6 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   name = col_character(),
##   ..   type = col_character(),
##   ..   time = col_datetime(format = ""),
##   ..   amount = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

```r
view(CW)
```
#Convert to Mountain Time

```r
CWM <- with_tz(CW, tzone = "US/Mountain")
view(CWM)
```

#Group variable

```r
?ceiling_date

CWH <- CWM %>%
  mutate(hour = ceiling_date(time, "hour"))
```

#Aggregate sales into hours

```r
?aggregate

CWF <- CWH %>%
  group_by(hour) %>%
  summarise(across(amount, sum))

view(CWF)
```

#Find matching temperatures

```r
m <- riem_measures(station = "RXE",  date_start = "2016-05-13",  date_end  = "2016-07-18") %>%
  with_tz(tzone = "US/Mountain") %>%
  filter(tmpf != " ") %>%
  mutate(hour =  ceiling_date(valid, "hour")) %>%
  select(hour, tmpf)
```

#Merge datasets

```r
combo <- merge(CWF, m, by = "hour") %>%
  arrange(hour) %>%
  mutate(nh = hour(hour))

view(combo)
```


#Visualization for sales vs temperature 

```r
combograph <- ggplot(data=combo, mapping=aes(x=nh,y=amount))+
  geom_col()+
  labs(title="Sales by Hour", x="Hour", y="Sales") 
combograph
```

![](Task-15_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

