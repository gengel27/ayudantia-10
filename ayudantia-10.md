Ayudantia 10
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.0     ✓ dplyr   1.0.5
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(e1071)
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(caTools)
library(rstan)
```

    ## Loading required package: StanHeaders

    ## rstan (Version 2.21.1, GitRev: 2e1f913d3ca3)

    ## For execution on a local, multicore CPU with excess RAM we recommend calling
    ## options(mc.cores = parallel::detectCores()).
    ## To avoid recompilation of unchanged Stan programs, we recommend calling
    ## rstan_options(auto_write = TRUE)

    ## 
    ## Attaching package: 'rstan'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
library(rstanarm)
```

    ## Loading required package: Rcpp

    ## This is rstanarm version 2.21.1

    ## - See https://mc-stan.org/rstanarm/articles/priors for changes to default priors!

    ## - Default priors may change, so it's safest to specify priors, even if equivalent to the defaults.

    ## - For execution on a local, multicore CPU with excess RAM we recommend calling

    ##   options(mc.cores = parallel::detectCores())

    ## 
    ## Attaching package: 'rstanarm'

    ## The following object is masked from 'package:rstan':
    ## 
    ##     loo

    ## The following objects are masked from 'package:caret':
    ## 
    ##     compare_models, R2

``` r
credit <- read.csv("/Users/gabrielengel/Downloads/Ayudantia_DataMining01_2021-main 2/Ayudantia 10/UCI_Credit_Card.csv")
```

``` r
sample <- sample(1:nrow(credit), size=round(0.7*nrow(credit)), replace=FALSE)

train <- credit[sample,]  
test <- credit[-sample,] 

credit <- train
glimpse(train)
```

    ## Rows: 21,000
    ## Columns: 25
    ## $ ID                         <int> 4209, 29934, 6923, 5369, 13237, 12693, 2521…
    ## $ LIMIT_BAL                  <dbl> 30000, 190000, 260000, 100000, 70000, 12000…
    ## $ SEX                        <int> 1, 1, 2, 2, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1, 1…
    ## $ EDUCATION                  <int> 3, 3, 1, 2, 1, 1, 1, 3, 2, 3, 2, 1, 3, 3, 2…
    ## $ MARRIAGE                   <int> 2, 1, 2, 1, 2, 2, 2, 1, 2, 2, 2, 1, 2, 1, 1…
    ## $ AGE                        <int> 23, 44, 35, 44, 23, 27, 23, 43, 34, 49, 43,…
    ## $ PAY_0                      <int> 2, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 2, 2,…
    ## $ PAY_2                      <int> 2, 2, -1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 2,…
    ## $ PAY_3                      <int> 2, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 2…
    ## $ PAY_4                      <int> 2, 2, -1, 0, 0, 0, 0, 0, 0, -2, 0, -1, 0, 2…
    ## $ PAY_5                      <int> 2, -1, -1, 0, 0, 0, 0, 0, 0, -2, 0, -1, 0, …
    ## $ PAY_6                      <int> 2, 0, -1, 2, 0, 0, 0, 0, 0, -2, 0, -1, 0, 2…
    ## $ BILL_AMT1                  <dbl> 10732, 291, -15, 39000, 8878, 60109, 14544,…
    ## $ BILL_AMT2                  <dbl> 20501, 291, 16596, 40123, 10077, 44206, 155…
    ## $ BILL_AMT3                  <dbl> 20831, 582, 1935, 41217, 11203, 38083, 1664…
    ## $ BILL_AMT4                  <dbl> 21231, 291, 5365, 42193, 11320, 36728, 1765…
    ## $ BILL_AMT5                  <dbl> 20589, 582, 0, 44722, 11487, 36952, 18066, …
    ## $ BILL_AMT6                  <dbl> 31018, 291, 294, 44024, 11895, 35475, 18775…
    ## $ PAY_AMT1                   <dbl> 10000, 0, 16611, 1900, 1500, 2415, 1300, 34…
    ## $ PAY_AMT2                   <dbl> 1000, 582, 1935, 1900, 1600, 1816, 1300, 49…
    ## $ PAY_AMT3                   <dbl> 1000, 0, 5365, 1800, 600, 1381, 1300, 4512,…
    ## $ PAY_AMT4                   <dbl> 0, 582, 0, 3200, 500, 1264, 700, 6708, 3649…
    ## $ PAY_AMT5                   <dbl> 13000, 0, 294, 0, 600, 1228, 1000, 4639, 37…
    ## $ PAY_AMT6                   <dbl> 0, 291, 741, 2000, 2600, 1217, 1000, 4793, …
    ## $ default.payment.next.month <int> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0…

``` r
credit<- credit[c(2,4:25)]
str(credit)
```

    ## 'data.frame':    21000 obs. of  23 variables:
    ##  $ LIMIT_BAL                 : num  30000 190000 260000 100000 70000 120000 30000 190000 100000 20000 ...
    ##  $ EDUCATION                 : int  3 3 1 2 1 1 1 3 2 3 ...
    ##  $ MARRIAGE                  : int  2 1 2 1 2 2 2 1 2 2 ...
    ##  $ AGE                       : int  23 44 35 44 23 27 23 43 34 49 ...
    ##  $ PAY_0                     : int  2 -1 1 0 0 0 0 0 0 0 ...
    ##  $ PAY_2                     : int  2 2 -1 0 0 0 0 0 0 0 ...
    ##  $ PAY_3                     : int  2 -1 -1 0 0 0 0 0 0 0 ...
    ##  $ PAY_4                     : int  2 2 -1 0 0 0 0 0 0 -2 ...
    ##  $ PAY_5                     : int  2 -1 -1 0 0 0 0 0 0 -2 ...
    ##  $ PAY_6                     : int  2 0 -1 2 0 0 0 0 0 -2 ...
    ##  $ BILL_AMT1                 : num  10732 291 -15 39000 8878 ...
    ##  $ BILL_AMT2                 : num  20501 291 16596 40123 10077 ...
    ##  $ BILL_AMT3                 : num  20831 582 1935 41217 11203 ...
    ##  $ BILL_AMT4                 : num  21231 291 5365 42193 11320 ...
    ##  $ BILL_AMT5                 : num  20589 582 0 44722 11487 ...
    ##  $ BILL_AMT6                 : num  31018 291 294 44024 11895 ...
    ##  $ PAY_AMT1                  : num  10000 0 16611 1900 1500 ...
    ##  $ PAY_AMT2                  : num  1000 582 1935 1900 1600 ...
    ##  $ PAY_AMT3                  : num  1000 0 5365 1800 600 ...
    ##  $ PAY_AMT4                  : num  0 582 0 3200 500 ...
    ##  $ PAY_AMT5                  : num  13000 0 294 0 600 ...
    ##  $ PAY_AMT6                  : num  0 291 741 2000 2600 ...
    ##  $ default.payment.next.month: int  0 0 0 0 0 0 1 0 0 0 ...

``` r
credit_linear <- stan_glm(default.payment.next.month ~ AGE + EDUCATION, data = credit, family = gaussian())
```

    ## 
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 8.6e-05 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.86 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.039585 seconds (Warm-up)
    ## Chain 1:                1.81779 seconds (Sampling)
    ## Chain 1:                1.85738 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 1.9e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.19 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.043673 seconds (Warm-up)
    ## Chain 2:                1.80369 seconds (Sampling)
    ## Chain 2:                1.84736 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 3.3e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.33 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.040487 seconds (Warm-up)
    ## Chain 3:                1.79945 seconds (Sampling)
    ## Chain 3:                1.83994 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 2e-05 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.2 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.040319 seconds (Warm-up)
    ## Chain 4:                1.8155 seconds (Sampling)
    ## Chain 4:                1.85582 seconds (Total)
    ## Chain 4:

``` r
model_nb <- naiveBayes(default.payment.next.month ~ AGE + EDUCATION, credit, laplace=1)      
```
