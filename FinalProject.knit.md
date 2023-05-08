---
title: "Effects on Academic Performance"
author: "Vanya Trenado and Juana Jimenez"
date: "April 26, 2023"
output: html_document
---


![image of students in class](https://s2.research.com/wp-content/uploads/2020/12/24112827/American-Students-Classroom-1-760x380.png)


## Introduction

When it comes to academic performance, one may think that everyone has the same opportunities to do a satisfactory job. However, this is not always the case. Most scholars get affected academically by factors such as their race, gender,income, schedules,culture and expectations from parents. using information obtained through three datasets we will be able to visualize the correlation between variables and the success of students. 

In order to be able to better analyze this topic, we use information from 3 different schools instead of only focusing on one school.

## Packages Required




```r
library(tidyverse) #used to tidy multiple data packages
library(ggplot2) #used for plotting
library(dplyr) #used for data manipulation
library(knitr) #used to dynamic report generation


student <- read.csv('student_data01.csv')
glimpse(student)
```

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
```

```r
performance <- read.csv('student_performance.csv')
glimpse(performance)
```

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
```

```r
satvsgpa <-read.csv('sat_to_gpa.csv')
glimpse(satvsgpa)
```

```
## Rows: 181
## Columns: 2
## $ SAT.Score <int> 1600, 1593, 1587, 1580, 1573, 1567, 1560, 1553, 1547, 1540, …
## $ GPA       <dbl> 4.00, 4.00, 4.00, 4.00, 4.00, 4.00, 4.00, 4.00, 4.00, 4.00, …
```

## Links to Datasets Used
1) https://www.kaggle.com/code/bhartiprasad17/student-academic-performance-analysis
2) https://www.kaggle.com/datasets/devansodariya/student-performance-data
3) https://www.kaggle.com/datasets/farhansadeek/sat-to-gpa?select=SAT+to+GPA.csv


## Questions Examined
1) What gender got the higher grade?
2) What is the relationship between SAT and GPA scores?
3) How did absences affect performance?
4) Did having a standard lunch vs free lunch affect performace?
5) What was the relationship between types of lunch and abscences?
6) Did parents' education influence grades?
7) How did performance get affected by parents being separated or together?



## What gender got the higher grades?

The female had high range of scores but they were more dense between 200 to 300. For male, the score range from 110 to 250.


```r
performance=mutate(performance%>%rowwise(),p_average_grade=rowMeans(cbind(math.score,reading.score,writing.score)))
performance %>% ggplot(aes(x = gender, y = p_average_grade,color=gender)) + geom_boxplot(alpha = .5,)
```

<img src="FinalProject_files/figure-html/unnamed-chunk-2-1.png" width="672" />

## What is the relationship between SAT and GPA scores?


```r
satvsgpa %>% ggplot(aes(x = SAT.Score, y = GPA,)) + geom_point(color='red')+ ylim(1,4)+xlim(600,1600)
```

<img src="FinalProject_files/figure-html/unnamed-chunk-3-1.png" width="672" />


## How did abcenses affect performance?


```r
student=mutate(student%>%rowwise(),average_grade=rowMeans(cbind(G1,G2,G3)))
abs_eff=student%>%ggplot(aes(x=absences,y=average_grade))+geom_point()

plot(abs_eff)+ylim(15,20)
```

<img src="FinalProject_files/figure-html/unnamed-chunk-4-1.png" width="672" /><img src="FinalProject_files/figure-html/unnamed-chunk-4-2.png" width="672" />


## Did having a standard lunch vs free lunch affect performace?


```r
paid_lunch=student%>%ggplot(aes(x=average_grade,y=paid))+geom_boxplot(color="blue")
plot(paid_lunch)
```

<img src="FinalProject_files/figure-html/unnamed-chunk-5-1.png" width="672" />

## What was the relationship between types of lunch and abscences?


```r
abs_eff_lunch=student%>%ggplot(aes(x=absences,y=average_grade))+
geom_point(mapping =aes(color=paid))+ geom_smooth()
plot(abs_eff_lunch)
```

<img src="FinalProject_files/figure-html/unnamed-chunk-6-1.png" width="672" />

## Did parents' education influence grades?


```r
performance %>% ggplot(aes(x = p_average_grade, colour = parental.level.of.education, fill=parental.level.of.education)) + geom_density(alpha = .05)
```

<img src="FinalProject_files/figure-html/unnamed-chunk-7-1.png" width="672" />

Let's take a closer look

```r
performance %>% ggplot(aes(x = p_average_grade, colour = parental.level.of.education, fill=parental.level.of.education)) + geom_density(alpha = .05)+xlim(80,100)
```

<img src="FinalProject_files/figure-html/unnamed-chunk-8-1.png" width="672" />


## How did performance get affected by parents being separated or together?


```r
student=mutate(student%>%rowwise(),average_grade=rowMeans(cbind(G1,G2,G3)))
student %>% ggplot(aes(x = average_grade, color = Pstatus, fill=Pstatus)) + geom_density(alpha = .05)
```

<img src="FinalProject_files/figure-html/unnamed-chunk-9-1.png" width="672" />

```r
student %>% ggplot(aes(x = average_grade, y = Pstatus, color=Pstatus)) + geom_boxplot()
```

<img src="FinalProject_files/figure-html/unnamed-chunk-9-2.png" width="672" />

## Conclusion

To summarize everything mentioned above, we have found that academic performance is not only based on the student. The environment in which the scholar is in plays a huge role in how good or bad he or she tries in school. This environment involves the relationship that their parents have with each other, education of parents, lunch, and so on.
