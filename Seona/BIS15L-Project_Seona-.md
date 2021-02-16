---
title: "BIS15L Project"
output: 
  html_document: 
    keep_md: yes
---



##MRI Data and Alzhiemer's Disease

In this project, we are using data from the Open Access Series of Imaging Studies (OASIS), made available by the Washingtom UNiversity Alzheimer's Disease Research Center, Dr. Randy Buckner at the Howard Hughes Medical Institute (HHMI), the Neuroinformatics Research Group (NRG) at Washington University School of Medicine, and the Biomedical Informatics Research Network (BIRN). [MRI and Alzheimer's Disease](https://www.kaggle.com/jboysen/mri-and-alzheimers)

Load libraries

```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
## ✓ tibble  3.0.5     ✓ dplyr   1.0.3
## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
## ✓ readr   1.4.0     ✓ forcats 0.5.0
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(RColorBrewer)
library(paletteer)
library(janitor)
```

```
## 
## Attaching package: 'janitor'
```

```
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

```r
library(here)
```

```
## here() starts at /Users/seonapatel/Documents/GitHub/BIS15L-Project
```


```r
options(scipen=999)
```


```r
longitudinal <- readr::read_csv("data/oasis_longitudinal.csv")
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   `Subject ID` = col_character(),
##   `MRI ID` = col_character(),
##   Group = col_character(),
##   Visit = col_double(),
##   `MR Delay` = col_double(),
##   `M/F` = col_character(),
##   Hand = col_character(),
##   Age = col_double(),
##   EDUC = col_double(),
##   SES = col_double(),
##   MMSE = col_double(),
##   CDR = col_double(),
##   eTIV = col_double(),
##   nWBV = col_double(),
##   ASF = col_double()
## )
```

```r
xsectional <- readr::read_csv("data/oasis_cross-sectional.csv")
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   ID = col_character(),
##   `M/F` = col_character(),
##   Hand = col_character(),
##   Age = col_double(),
##   Educ = col_double(),
##   SES = col_double(),
##   MMSE = col_double(),
##   CDR = col_double(),
##   eTIV = col_double(),
##   nWBV = col_double(),
##   ASF = col_double(),
##   Delay = col_character()
## )
```

```r
glimpse(longitudinal)
```

```
## Rows: 373
## Columns: 15
## $ `Subject ID` <chr> "OAS2_0001", "OAS2_0001", "OAS2_0002", "OAS2_0002", "OAS…
## $ `MRI ID`     <chr> "OAS2_0001_MR1", "OAS2_0001_MR2", "OAS2_0002_MR1", "OAS2…
## $ Group        <chr> "Nondemented", "Nondemented", "Demented", "Demented", "D…
## $ Visit        <dbl> 1, 2, 1, 2, 3, 1, 2, 1, 2, 3, 1, 3, 4, 1, 2, 1, 2, 1, 2,…
## $ `MR Delay`   <dbl> 0, 457, 0, 560, 1895, 0, 538, 0, 1010, 1603, 0, 518, 128…
## $ `M/F`        <chr> "M", "M", "M", "M", "M", "F", "F", "M", "M", "M", "M", "…
## $ Hand         <chr> "R", "R", "R", "R", "R", "R", "R", "R", "R", "R", "R", "…
## $ Age          <dbl> 87, 88, 75, 76, 80, 88, 90, 80, 83, 85, 71, 73, 75, 93, …
## $ EDUC         <dbl> 14, 14, 12, 12, 12, 18, 18, 12, 12, 12, 16, 16, 16, 14, …
## $ SES          <dbl> 2, 2, NA, NA, NA, 3, 3, 4, 4, 4, NA, NA, NA, 2, 2, 2, 2,…
## $ MMSE         <dbl> 27, 30, 23, 28, 22, 28, 27, 28, 29, 30, 28, 27, 27, 30, …
## $ CDR          <dbl> 0.0, 0.0, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0, 0.5, 0.0, 0.5, 1…
## $ eTIV         <dbl> 1987, 2004, 1678, 1738, 1698, 1215, 1200, 1689, 1701, 16…
## $ nWBV         <dbl> 0.696, 0.681, 0.736, 0.713, 0.701, 0.710, 0.718, 0.712, …
## $ ASF          <dbl> 0.883, 0.876, 1.046, 1.010, 1.034, 1.444, 1.462, 1.039, …
```


```r
summary(longitudinal)
```

```
##   Subject ID           MRI ID             Group               Visit      
##  Length:373         Length:373         Length:373         Min.   :1.000  
##  Class :character   Class :character   Class :character   1st Qu.:1.000  
##  Mode  :character   Mode  :character   Mode  :character   Median :2.000  
##                                                           Mean   :1.882  
##                                                           3rd Qu.:2.000  
##                                                           Max.   :5.000  
##                                                                          
##     MR Delay          M/F                Hand                Age       
##  Min.   :   0.0   Length:373         Length:373         Min.   :60.00  
##  1st Qu.:   0.0   Class :character   Class :character   1st Qu.:71.00  
##  Median : 552.0   Mode  :character   Mode  :character   Median :77.00  
##  Mean   : 595.1                                         Mean   :77.01  
##  3rd Qu.: 873.0                                         3rd Qu.:82.00  
##  Max.   :2639.0                                         Max.   :98.00  
##                                                                        
##       EDUC           SES            MMSE            CDR              eTIV     
##  Min.   : 6.0   Min.   :1.00   Min.   : 4.00   Min.   :0.0000   Min.   :1106  
##  1st Qu.:12.0   1st Qu.:2.00   1st Qu.:27.00   1st Qu.:0.0000   1st Qu.:1357  
##  Median :15.0   Median :2.00   Median :29.00   Median :0.0000   Median :1470  
##  Mean   :14.6   Mean   :2.46   Mean   :27.34   Mean   :0.2909   Mean   :1488  
##  3rd Qu.:16.0   3rd Qu.:3.00   3rd Qu.:30.00   3rd Qu.:0.5000   3rd Qu.:1597  
##  Max.   :23.0   Max.   :5.00   Max.   :30.00   Max.   :2.0000   Max.   :2004  
##                 NA's   :19     NA's   :2                                      
##       nWBV             ASF       
##  Min.   :0.6440   Min.   :0.876  
##  1st Qu.:0.7000   1st Qu.:1.099  
##  Median :0.7290   Median :1.194  
##  Mean   :0.7296   Mean   :1.195  
##  3rd Qu.:0.7560   3rd Qu.:1.293  
##  Max.   :0.8370   Max.   :1.587  
## 
```

```r
longitudinal <- janitor::clean_names(longitudinal)
xsectional <- janitor::clean_names(xsectional)
```


```r
names(longitudinal)
```

```
##  [1] "subject_id" "mri_id"     "group"      "visit"      "mr_delay"  
##  [6] "m_f"        "hand"       "age"        "educ"       "ses"       
## [11] "mmse"       "cdr"        "e_tiv"      "n_wbv"      "asf"
```

```r
names(xsectional)
```

```
##  [1] "id"    "m_f"   "hand"  "age"   "educ"  "ses"   "mmse"  "cdr"   "e_tiv"
## [10] "n_wbv" "asf"   "delay"
```

```r
longitudinal$age <- as.factor(longitudinal$age)
```



```r
longitudinal %>% 
  ggplot(aes(x=age, y=n_wbv, fill=m_f))+
  geom_col()
```

![](BIS15L-Project_Seona-_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


```r
longitudinal %>% 
  ggplot(aes(x=educ))+geom_bar()
```

![](BIS15L-Project_Seona-_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

