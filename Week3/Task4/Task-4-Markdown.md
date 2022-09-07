---
title: "Task 4"
author: "Brink"
date: "9/6/2022"
output: 
  html_document:
    keep_md: yes
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
iris.data <- as_tibble(iris)
iris.data
```

```
## # A tibble: 150 × 5
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
##           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
##  1          5.1         3.5          1.4         0.2 setosa 
##  2          4.9         3            1.4         0.2 setosa 
##  3          4.7         3.2          1.3         0.2 setosa 
##  4          4.6         3.1          1.5         0.2 setosa 
##  5          5           3.6          1.4         0.2 setosa 
##  6          5.4         3.9          1.7         0.4 setosa 
##  7          4.6         3.4          1.4         0.3 setosa 
##  8          5           3.4          1.5         0.2 setosa 
##  9          4.4         2.9          1.4         0.2 setosa 
## 10          4.9         3.1          1.5         0.1 setosa 
## # … with 140 more rows
```


```r
a.sepal <- arrange(iris, Sepal.Length)
head(a.sepal,10)
```

```
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1           4.3         3.0          1.1         0.1  setosa
## 2           4.4         2.9          1.4         0.2  setosa
## 3           4.4         3.0          1.3         0.2  setosa
## 4           4.4         3.2          1.3         0.2  setosa
## 5           4.5         2.3          1.3         0.3  setosa
## 6           4.6         3.1          1.5         0.2  setosa
## 7           4.6         3.4          1.4         0.3  setosa
## 8           4.6         3.6          1.0         0.2  setosa
## 9           4.6         3.2          1.4         0.2  setosa
## 10          4.7         3.2          1.3         0.2  setosa
```


```r
testdat <- select(iris.data,Species,Petal.Width)
testdat
```

```
## # A tibble: 150 × 2
##    Species Petal.Width
##    <fct>         <dbl>
##  1 setosa          0.2
##  2 setosa          0.2
##  3 setosa          0.2
##  4 setosa          0.2
##  5 setosa          0.2
##  6 setosa          0.4
##  7 setosa          0.3
##  8 setosa          0.2
##  9 setosa          0.2
## 10 setosa          0.1
## # … with 140 more rows
```


```r
Species_Mean <-iris.data %>%
  group_by(Species) %>%
  summarise(mean(Petal.Width), mean(Petal.Length), mean(Sepal.Length), mean(Sepal.Width))
Species_Mean
```

```
## # A tibble: 3 × 5
##   Species    `mean(Petal.Wi…` `mean(Petal.Le…` `mean(Sepal.Le…` `mean(Sepal.Wi…`
##   <fct>                 <dbl>            <dbl>            <dbl>            <dbl>
## 1 setosa                0.246             1.46             5.01             3.43
## 2 versicolor            1.33              4.26             5.94             2.77
## 3 virginica             2.03              5.55             6.59             2.97
```


```r
iris_min1 <- select(iris.data, Sepal.Width, Petal.Width)
iris_min <- filter(iris_min1, Sepal.Width >= 3, Petal.Width != 2.5)
iris_min
```

```
## # A tibble: 90 × 2
##    Sepal.Width Petal.Width
##          <dbl>       <dbl>
##  1         3.5         0.2
##  2         3           0.2
##  3         3.2         0.2
##  4         3.1         0.2
##  5         3.6         0.2
##  6         3.9         0.4
##  7         3.4         0.3
##  8         3.4         0.2
##  9         3.1         0.1
## 10         3.7         0.2
## # … with 80 more rows
```


```r
iris_size <- iris.data %>%
  mutate(Sepal.Size =
           case_when(
             Sepal.Length < 5 ~ "small",
             Sepal.Length >= 5 & Sepal.Length < 6.5 ~ "medium",
             Sepal.Length >= 6.5 ~ "large"))
head(iris_size)
```

```
## # A tibble: 6 × 6
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species Sepal.Size
##          <dbl>       <dbl>        <dbl>       <dbl> <fct>   <chr>     
## 1          5.1         3.5          1.4         0.2 setosa  medium    
## 2          4.9         3            1.4         0.2 setosa  small     
## 3          4.7         3.2          1.3         0.2 setosa  small     
## 4          4.6         3.1          1.5         0.2 setosa  small     
## 5          5           3.6          1.4         0.2 setosa  medium    
## 6          5.4         3.9          1.7         0.4 setosa  medium
```

```r
iris_rank <- arrange(iris.data, desc(Petal.Length))
mutate(iris_rank, rank=min_rank(Petal.Length))
```

```
## # A tibble: 150 × 6
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species    rank
##           <dbl>       <dbl>        <dbl>       <dbl> <fct>     <int>
##  1          7.7         2.6          6.9         2.3 virginica   150
##  2          7.7         3.8          6.7         2.2 virginica   148
##  3          7.7         2.8          6.7         2   virginica   148
##  4          7.6         3            6.6         2.1 virginica   147
##  5          7.9         3.8          6.4         2   virginica   146
##  6          7.3         2.9          6.3         1.8 virginica   145
##  7          7.2         3.6          6.1         2.5 virginica   142
##  8          7.4         2.8          6.1         1.9 virginica   142
##  9          7.7         3            6.1         2.3 virginica   142
## 10          6.3         3.3          6           2.5 virginica   140
## # … with 140 more rows
```



```r
?summarise_all
means_sd_species <- iris.data %>%
  group_by(Species) %>%
  summarize_all(list(Mean=mean, Std_dev = sd))
means_sd_species
```

```
## # A tibble: 3 × 9
##   Species    Sepal.Length_Me… Sepal.Width_Mean Petal.Length_Me… Petal.Width_Mean
##   <fct>                 <dbl>            <dbl>            <dbl>            <dbl>
## 1 setosa                 5.01             3.43             1.46            0.246
## 2 versicolor             5.94             2.77             4.26            1.33 
## 3 virginica              6.59             2.97             5.55            2.03 
## # … with 4 more variables: Sepal.Length_Std_dev <dbl>,
## #   Sepal.Width_Std_dev <dbl>, Petal.Length_Std_dev <dbl>,
## #   Petal.Width_Std_dev <dbl>
```

```r
#Question 1: How has Grand Theft Auto 5 sustained it's sales through multiple console generations?
#Question 2: Is the average age of gambling addicts decreasing over time? 
#Question 3: How does the amount of studio acquisitions and publisher make impact yearly annual revenue?
#Feedback 5-10 People Summary: A common piece of feedback was that the questions were in fields that I could personality relate to, making it easier to analyze chunks of data if I were to seek them out. Also how the questions took concepts that most people understand and seek to uncover a layer of the data that the general public may not have thought of.  
#Feedback Professional Summary: In regards to two of the questions, they both revolve around the video game industry. The industry itself is constantly evolving and it's people noted how companies are adapting to the market by focusing on quantity rather than quality. Feedback was strong about the gambling question as well, with some noting the unique angle of the question, and how it targeted the youth. A final common comment was the vast potential with various data sets with these questions. The problem comes down to finding it. 
```

