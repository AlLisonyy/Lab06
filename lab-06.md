Lab 06 - Ugly charts and Simpson’s paradox
================
Allison Li
02142025

### Email and name

``` r
library(usethis)
use_git_config(
  user.name = "Allison",
  user.email = "liy423@gmail.com"
)
```

### Load packages and data

``` r
library(tidyverse) 
library(dsbox)
library(mosaicData) 
staff <- read_csv("data/instructional-staff.csv")
```

### Exercise 1

``` r
staff_long <- staff %>%
  pivot_longer(cols = -faculty_type, names_to = "year") %>%
  mutate(value = as.numeric(value))
staff_long
```

    ## # A tibble: 55 × 3
    ##    faculty_type              year  value
    ##    <chr>                     <chr> <dbl>
    ##  1 Full-Time Tenured Faculty 1975   29  
    ##  2 Full-Time Tenured Faculty 1989   27.6
    ##  3 Full-Time Tenured Faculty 1993   25  
    ##  4 Full-Time Tenured Faculty 1995   24.8
    ##  5 Full-Time Tenured Faculty 1999   21.8
    ##  6 Full-Time Tenured Faculty 2001   20.3
    ##  7 Full-Time Tenured Faculty 2003   19.3
    ##  8 Full-Time Tenured Faculty 2005   17.8
    ##  9 Full-Time Tenured Faculty 2007   17.2
    ## 10 Full-Time Tenured Faculty 2009   16.8
    ## # ℹ 45 more rows

``` r
staff_long %>%
  ggplot(aes(x = year, y = value, color = faculty_type)) +
  geom_line()
```

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

![](lab-06_files/figure-gfm/line-plot-1.png)<!-- -->

``` r
staff_long %>%
  ggplot(aes(
    x = year,
    y = value,
    group = faculty_type,
    color = faculty_type
  )) +
  geom_line() +
  labs(title = "Instructional staff employment trends",
       y = "percentage of hires",
       color = "faculty type")
```

![](lab-06_files/figure-gfm/line-plot-2.png)<!-- -->

According to the graph, we can clearly see that the Part-Time faculty
employment trend has increased over the years while the Full-time
Tenure-track Faculty and the Full-time Tenured Faculty showed clear
decreasing trends. For Full-time Non-Tenured-Track Faculty, it seems
that there are higher employment rate in 2011 comparing with 1975. For
Graduate Student Employees, although there is a clear decrease in 1989,
the rest employment rate remained relatively similar over the years,
around 20%.

### Exercise 2

If I want to show the trend of the part-time faculty employment rate
comparing with other staff types, I would either make other types of
faculty type different types of color grey, or I would make this line
thicker so people would know what is the main point of my graph.

### Exercise 3

…

### Exercise 4

### Exercise 5
