
``` r
##knitr::include_graphics("C:\Users\vanya\OneDrive\Desktop\Final_Project\final.jpg")
```

![image of
something](https://s2.research.com/wp-content/uploads/2020/12/24112827/American-Students-Classroom-1-760x380.png)

## Introduction

When it comes to academic performance, one may think that everyone has
the same opportunities to do a satisfactory job. However, this is not
always the case. Most scholars get affected academically by factors such
as their race, gender,income, schedules,culture and expectations from
parents. using information obtained through three datasets we will be
able to visualize the correlation between variables and the success of
students.

## Packages Required

``` r
library(tidyverse) #used to tidy multiple data packages
library(ggplot2) #used for plotting
library(dplyr) #used for data manipulation
library(knitr) #used to dynamic report generation


student <- read.csv('student_data01.csv')
glimpse(student)
```

    ## Rows: 395
    ## Columns: 33
    ## $ school     <chr> "GP", "GP", "GP", "GP", "GP", "GP", "GP", "GP", "GP", "GP",…
    ## $ sex        <chr> "F", "F", "F", "F", "F", "M", "M", "F", "M", "M", "F", "F",…
    ## $ age        <int> 18, 17, 15, 15, 16, 16, 16, 17, 15, 15, 15, 15, 15, 15, 15,…
    ## $ address    <chr> "U", "U", "U", "U", "U", "U", "U", "U", "U", "U", "U", "U",…
    ## $ famsize    <chr> "GT3", "GT3", "LE3", "GT3", "GT3", "LE3", "LE3", "GT3", "LE…
    ## $ Pstatus    <chr> "A", "T", "T", "T", "T", "T", "T", "A", "A", "T", "T", "T",…
    ## $ Medu       <int> 4, 1, 1, 4, 3, 4, 2, 4, 3, 3, 4, 2, 4, 4, 2, 4, 4, 3, 3, 4,…
    ## $ Fedu       <int> 4, 1, 1, 2, 3, 3, 2, 4, 2, 4, 4, 1, 4, 3, 2, 4, 4, 3, 2, 3,…
    ## $ Mjob       <chr> "at_home", "at_home", "at_home", "health", "other", "servic…
    ## $ Fjob       <chr> "teacher", "other", "other", "services", "other", "other", …
    ## $ reason     <chr> "course", "course", "other", "home", "home", "reputation", …
    ## $ guardian   <chr> "mother", "father", "mother", "mother", "father", "mother",…
    ## $ traveltime <int> 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 3, 1, 2, 1, 1, 1, 3, 1, 1,…
    ## $ studytime  <int> 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 1, 2, 3, 1, 3, 2, 1, 1,…
    ## $ failures   <int> 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,…
    ## $ schoolsup  <chr> "yes", "no", "yes", "no", "no", "no", "no", "yes", "no", "n…
    ## $ famsup     <chr> "no", "yes", "no", "yes", "yes", "yes", "no", "yes", "yes",…
    ## $ paid       <chr> "no", "no", "yes", "yes", "yes", "yes", "no", "no", "yes", …
    ## $ activities <chr> "no", "no", "no", "yes", "no", "yes", "no", "no", "no", "ye…
    ## $ nursery    <chr> "yes", "no", "yes", "yes", "yes", "yes", "yes", "yes", "yes…
    ## $ higher     <chr> "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "ye…
    ## $ internet   <chr> "no", "yes", "yes", "yes", "no", "yes", "yes", "no", "yes",…
    ## $ romantic   <chr> "no", "no", "no", "yes", "no", "no", "no", "no", "no", "no"…
    ## $ famrel     <int> 4, 5, 4, 3, 4, 5, 4, 4, 4, 5, 3, 5, 4, 5, 4, 4, 3, 5, 5, 3,…
    ## $ freetime   <int> 3, 3, 3, 2, 3, 4, 4, 1, 2, 5, 3, 2, 3, 4, 5, 4, 2, 3, 5, 1,…
    ## $ goout      <int> 4, 3, 2, 2, 2, 2, 4, 4, 2, 1, 3, 2, 3, 3, 2, 4, 3, 2, 5, 3,…
    ## $ Dalc       <int> 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1,…
    ## $ Walc       <int> 1, 1, 3, 1, 2, 2, 1, 1, 1, 1, 2, 1, 3, 2, 1, 2, 2, 1, 4, 3,…
    ## $ health     <int> 3, 3, 3, 5, 5, 5, 3, 1, 1, 5, 2, 4, 5, 3, 3, 2, 2, 4, 5, 5,…
    ## $ absences   <int> 6, 4, 10, 2, 4, 10, 0, 6, 0, 0, 0, 4, 2, 2, 0, 4, 6, 4, 16,…
    ## $ G1         <int> 5, 5, 7, 15, 6, 15, 12, 6, 16, 14, 10, 10, 14, 10, 14, 14, …
    ## $ G2         <int> 6, 5, 8, 14, 10, 15, 12, 5, 18, 15, 8, 12, 14, 10, 16, 14, …
    ## $ G3         <int> 6, 6, 10, 15, 10, 15, 11, 6, 19, 15, 9, 12, 14, 11, 16, 14,…

``` r
performance <- read.csv('student_performance.csv')
glimpse(performance)
```

    ## Rows: 1,000
    ## Columns: 9
    ## $ gender                      <chr> "female", "female", "female", "male", "mal…
    ## $ race.ethnicity              <chr> "group B", "group C", "group B", "group A"…
    ## $ parental.level.of.education <chr> "bachelor's degree", "some college", "mast…
    ## $ lunch                       <chr> "standard", "standard", "standard", "free/…
    ## $ test.preparation.course     <chr> "none", "completed", "none", "none", "none…
    ## $ math.score                  <int> 72, 69, 90, 47, 76, 71, 88, 40, 64, 38, 58…
    ## $ reading.score               <int> 72, 90, 95, 57, 78, 83, 95, 43, 64, 60, 54…
    ## $ writing.score               <int> 74, 88, 93, 44, 75, 78, 92, 39, 67, 50, 52…
    ## $ total.score                 <int> 218, 247, 278, 148, 229, 232, 275, 122, 19…

``` r
satvsgpa <-read.csv('sat_to_gpa.csv')
glimpse(satvsgpa)
```

    ## Rows: 181
    ## Columns: 2
    ## $ SAT.Score <int> 1600, 1593, 1587, 1580, 1573, 1567, 1560, 1553, 1547, 1540, …
    ## $ GPA       <dbl> 4.00, 4.00, 4.00, 4.00, 4.00, 4.00, 4.00, 4.00, 4.00, 4.00, …

## What gender and race got the higher grades?

The female had high range of scores but they were more dense between 200
to 300. For male, the score range from 110 to 250.

``` r
performance %>% ggplot(aes(x = gender, y = total.score)) + geom_boxplot(alpha = .5, color = "pink")
```

![](FinalProject_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
performance %>% ggplot(aes(x = race.ethnicity, y = total.score)) + geom_boxplot(alpha = .5, color = "pink")
```

![](FinalProject_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

## How did abcenses affect performance?

``` r
student=mutate(student%>%rowwise(),average_grade=rowMeans(cbind(G1,G2,G3)))
abs_eff=student%>%ggplot(aes(x=absences,y=average_grade))+geom_point()
plot(abs_eff)
```

![](FinalProject_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Effects of having a standard lunch vs free lunch

``` r
paid_lunch=student%>%ggplot(aes(x=average_grade,y=paid))+geom_point(color="blue")
plot(paid_lunch)
```

![](FinalProject_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
abs_eff_lunch=student%>%ggplot(aes(x=absences,y=average_grade))+
geom_point(mapping =aes(color=paid))+ geom_smooth()
plot(abs_eff_lunch)
```

![](FinalProject_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

## Did parents’ education influence grades?

## What student got the higher grade?

## Conclusion

To summarize everything mentioned above, we have found that academic
performance is not only based on the student. The environment in which
the scholar is in plays a huge role in how good or bad he or she tries
in school. Their parents might not have pursued a higher education so
there is not much emphasis on the importance of doing well academically.
