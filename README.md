Predicting Purchases
================
Alice Milivinti
March 8, 2019

## Data Import

Import data and check the
format.

``` r
training <- read.table(file = '/home/alice/Downloads/6sense Task/takehome/training.tsv', sep = '\t', header = FALSE)
test <- read.table(file = '/home/alice/Downloads/6sense Task/takehome/test.tsv', sep = '\t', header = FALSE)

# Check data loading
head(training)
```

    ##                V1         V2         V3
    ## 1 00002acbe042d69 2013-07-11  EmailOpen
    ## 2 00002acbe042d69 2013-07-11 FormSubmit
    ## 3 00002acbe042d69 2013-07-15  EmailOpen
    ## 4 00002acbe042d69 2013-07-17  EmailOpen
    ## 5 00002acbe042d69 2013-07-18  EmailOpen
    ## 6 00002acbe042d69 2013-07-25  EmailOpen

``` r
#head(test)
```

## Data Maganement

Rename columns and define the time column as date format.

``` r
# rename columns
train_data <-  sample_frac(training, 0.1) %>% # Extract random 10% sample of training data
  rename(id = V1, time = V2, state = V3) %>%
  mutate(time = as.Date(time))

#-------------------------------------------------------------------------------


test_data <- test %>%
  rename(id = V1, time = V2, state = V3) %>%
  mutate(time = as.Date(time))
```

Brief summary statistics. Test data does not include CustomerSupport.

``` r
summary(train_data$time)
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu. 
    ## "2013-07-01" "2014-01-28" "2014-06-30" "2014-06-27" "2014-12-14" 
    ##         Max. 
    ## "2015-05-02"

``` r
table(train_data$state)
```

    ## 
    ##   CustomerSupport EmailClickthrough         EmailOpen        FormSubmit 
    ##             10440             28712            319095             17729 
    ##          PageView          Purchase          WebVisit 
    ##             38171             39356             38365

``` r
table(test_data$state)
```

    ## 
    ## EmailClickthrough         EmailOpen        FormSubmit          PageView 
    ##             42364            550886             28765             87149 
    ##          WebVisit 
    ##             88595

Check for non-unique id-time. Data include 27,662 contemporary actions.

``` r
doubles <- train_data %>%
  group_by(id, time) %>%  # Check for non-unique id-time 
  filter( n() > 1 ) 
nrow(doubles)
```

    ## [1] 27436

Reshape the data to a wide (dummy variables) structure to have unique
id-time & harmonize the chronological sequence (new variable fict\_time)
to have all the ids on a common scale.

``` r
train_data1 <- train_data %>%
  group_by(id, time) %>%
  unnest(state) %>% 
  mutate(status = 1, fict_time = 1) %>% 
  spread(state, status, fill = 0)  %>% # from categorical variables to dummies
  arrange(id, time) %>%  # Re-scale the time to have all the ids on a single scale
  group_by(id) %>%
  mutate(fict_time = cumsum(fict_time)) # generate the harmonized time

#-------------------------------------------------------------------------------


test_data1 <- test_data %>%
  group_by(id, time) %>%
  unnest(state) %>% 
  mutate(status = 1, fict_time = 1) %>% 
  spread(state, status, fill = 0)  %>% # from categorical variables to dummies
  arrange(id, time) %>%  # Re-scale the time to have all the ids on a single scale
  group_by(id) %>%
  mutate(fict_time = cumsum(fict_time))

head(train_data1)
```

    ## # A tibble: 6 x 10
    ## # Groups:   id [341,568]
    ##   id    time       fict_time CustomerSupport EmailClickthrou… EmailOpen
    ##   <fct> <date>         <dbl>           <dbl>            <dbl>     <dbl>
    ## 1 0000… 2013-07-11         1               0                0         1
    ## 2 0000… 2014-03-08         1               0                0         1
    ## 3 0000… 2014-08-06         2               0                0         1
    ## 4 0000… 2014-11-27         3               0                0         1
    ## 5 0001… 2014-01-09         1               0                0         0
    ## 6 0002… 2015-04-20         1               0                0         1
    ## # … with 4 more variables: FormSubmit <dbl>, PageView <dbl>,
    ## #   Purchase <dbl>, WebVisit <dbl>

Add new variables for the lagged dummy variables (lag = t-1) as well as
the sequence of actions each ids does before a purchase. The latter is
achieved by nesting by ids and Purchase and doing the cumulative sums of
the dummy variables.

``` r
train_data1 <- train_data1 %>%
  group_by(id) %>%
  mutate(
        # lags
         Purchase_lag1 = lag(Purchase, 1, order_by = fict_time), 
         EmailClickthrough_lag1 = lag(EmailClickthrough, 1, order_by = fict_time),
         EmailOpen_lag1 = lag(EmailOpen, 1, order_by = fict_time),
         FormSubmit_lag1 = lag(FormSubmit, 1, order_by = fict_time),
         PageView_lag1 = lag(PageView, 1, order_by = fict_time),
         WebVisit_lag1 = lag(WebVisit, 1, order_by = fict_time),
         # cumsums Purchase
         Purchase_cumsum = cumsum(Purchase)) %>% 
  # nest by
  group_by(id, Purchase_cumsum) %>%
  
  mutate(
         # cumsums -> # number of occurrances before purcase
         EmailOpen_cumsum = cumsum(EmailOpen), 
         EmailClickthrough_cumsum = cumsum(EmailClickthrough),
         FormSubmit_cumsum = cumsum(FormSubmit),
         PageView_cumsum = cumsum(PageView),
         WebVisit_cumsum = cumsum(WebVisit))


#-------------------------------------------------------------------------------


test_data1 <- test_data1 %>%
  group_by(id) %>%
  mutate(EmailClickthrough_lag1 = lag(EmailClickthrough, 1, order_by = fict_time),
         EmailOpen_lag1 = lag(EmailOpen, 1, order_by = fict_time),
         FormSubmit_lag1 = lag(FormSubmit, 1, order_by = fict_time),
         PageView_lag1 = lag(PageView, 1, order_by = fict_time),
         WebVisit_lag1 = lag(WebVisit, 1, order_by = fict_time)) %>%
  group_by(id) %>%
  mutate(EmailOpen_cumsum = cumsum(EmailOpen), 
         EmailClickthrough_cumsum = cumsum(EmailClickthrough),
         FormSubmit_cumsum = cumsum(FormSubmit),
         PageView_cumsum = cumsum(PageView),
         WebVisit_cumsum = cumsum(WebVisit))

head(train_data1)
```

    ## # A tibble: 6 x 22
    ## # Groups:   id, Purchase_cumsum [341,568]
    ##   id    time       fict_time CustomerSupport EmailClickthrou… EmailOpen
    ##   <fct> <date>         <dbl>           <dbl>            <dbl>     <dbl>
    ## 1 0000… 2013-07-11         1               0                0         1
    ## 2 0000… 2014-03-08         1               0                0         1
    ## 3 0000… 2014-08-06         2               0                0         1
    ## 4 0000… 2014-11-27         3               0                0         1
    ## 5 0001… 2014-01-09         1               0                0         0
    ## 6 0002… 2015-04-20         1               0                0         1
    ## # … with 16 more variables: FormSubmit <dbl>, PageView <dbl>,
    ## #   Purchase <dbl>, WebVisit <dbl>, Purchase_lag1 <dbl>,
    ## #   EmailClickthrough_lag1 <dbl>, EmailOpen_lag1 <dbl>,
    ## #   FormSubmit_lag1 <dbl>, PageView_lag1 <dbl>, WebVisit_lag1 <dbl>,
    ## #   Purchase_cumsum <dbl>, EmailOpen_cumsum <dbl>,
    ## #   EmailClickthrough_cumsum <dbl>, FormSubmit_cumsum <dbl>,
    ## #   PageView_cumsum <dbl>, WebVisit_cumsum <dbl>

## Bayeasian Regression Analysis

# Fitting Stage

I use Bayesian logistic regressions fitted under a spike-and-slab prior
with the prior inclusion probability of each predictor set.

The plot hereafter depicts the most useful activities in predicting
which user will purchase in the future.

``` r
# Bayesian Logistic 
logit_b <- logit.spike(Purchase ~ EmailClickthrough + EmailOpen + FormSubmit + PageView + WebVisit + fict_time +
                 EmailClickthrough_lag1 + EmailOpen_lag1 + FormSubmit_lag1 + PageView_lag1 + WebVisit_lag1 +
                 EmailOpen_cumsum + EmailClickthrough_cumsum + FormSubmit_cumsum + PageView_cumsum + WebVisit_cumsum, 
                 data = train_data1, niter = 500, seed = 1234)
```

    ## =-=-=-=-= Iteration 0 Mon Apr  1 17:22:06 2019 =-=-=-=-=
    ## =-=-=-=-= Iteration 50 Mon Apr  1 17:23:07 2019 =-=-=-=-=
    ## =-=-=-=-= Iteration 100 Mon Apr  1 17:23:51 2019 =-=-=-=-=
    ## =-=-=-=-= Iteration 150 Mon Apr  1 17:24:37 2019 =-=-=-=-=
    ## =-=-=-=-= Iteration 200 Mon Apr  1 17:25:27 2019 =-=-=-=-=
    ## =-=-=-=-= Iteration 250 Mon Apr  1 17:26:07 2019 =-=-=-=-=
    ## =-=-=-=-= Iteration 300 Mon Apr  1 17:26:51 2019 =-=-=-=-=
    ## =-=-=-=-= Iteration 350 Mon Apr  1 17:27:31 2019 =-=-=-=-=
    ## =-=-=-=-= Iteration 400 Mon Apr  1 17:28:12 2019 =-=-=-=-=
    ## =-=-=-=-= Iteration 450 Mon Apr  1 17:28:59 2019 =-=-=-=-=

``` r
# check for the significant coefficients to incude
# shaded by the conditional probability that a coefficient is positive, given that it is nonzero.
plot(logit_b)
```

![](README_files/figure-gfm/bayesian%20reg%201-1.png)<!-- -->

``` r
summary(logit_b)
```

    ## null log likelihood:            -76342 
    ## posterior mean log likelihood:  -6288 
    ## posterior max log likelihood:   -5685 
    ## mean deviance R-sq:             0.9176 
    ## 
    ## predicted vs observed success rates, by decile:
    ##                     predicted   observed
    ## (0.000126,0.000231] 0.0002042 0.00006102
    ## (0.000231,0.000235] 0.0002336 0.00002526
    ## (0.000235,0.000237] 0.0002361 0.00000000
    ## (0.000237,0.000238] 0.0002371 0.00000000
    ## (0.000238,0.000257] 0.0002460 0.00000000
    ## (0.000257,0.000267] 0.0002621 0.00000000
    ## (0.000267,0.000276] 0.0002753 0.00000000
    ## (0.000276,0.00028]  0.0002784 0.00000000
    ## (0.00028,0.00509]   0.0013805 0.00078638
    ## (0.00509,0.997]     0.6263963 0.62973851
    ## 
    ## summary of coefficients:
    ##                             mean      sd mean.inc sd.inc inc.prob
    ## (Intercept)                1.130 0.20500   1.1300  0.205    1.000
    ## WebVisit_cumsum          -13.600 2.90000 -13.6000  2.840    0.998
    ## PageView_cumsum           -7.080 0.91600  -7.0900  0.861    0.998
    ## EmailClickthrough_cumsum  -5.930 1.26000  -5.9400  1.230    0.998
    ## EmailOpen_cumsum         -15.300 3.93000 -15.4000  3.880    0.998
    ## WebVisit_lag1              9.230 1.77000   9.2500  1.730    0.998
    ## PageView_lag1              5.880 0.67400   5.8900  0.621    0.998
    ## FormSubmit_lag1            1.040 0.17500   1.0400  0.168    0.998
    ## EmailOpen_lag1             9.990 2.87000  10.0000  2.840    0.998
    ## fict_time                  0.039 0.00347   0.0391  0.003    0.998
    ## WebVisit                   7.010 2.86000   7.0200  2.850    0.998
    ## FormSubmit                -6.030 0.41600  -6.0400  0.317    0.998
    ## EmailOpen                  8.610 3.91000   8.6300  3.890    0.998
    ## EmailClickthrough         -0.416 0.91900  -1.2500  1.220    0.332
    ## EmailClickthrough_lag1     1.070 2.19000   5.4700  0.687    0.196
    ## PageView                  -0.118 0.72800  -2.4500  2.350    0.048
    ## FormSubmit_cumsum          0.000 0.00000   0.0000  0.000    0.000

## Results Fitting Stage

I visually assess the results of the logit model. The model fails when
it predicts either a false negative or false positive outcome. I set a
sensitivity threshold of 65% after a first graphical inspection.

``` r
train_data_m <- as.data.table(train_data1)

fit_prob_b <- as.data.table(logit_b$fitted.probabilities) %>%
  rename(pred = V1) %>%
  cbind(., train_data_m[!is.na(train_data_m$PageView_lag1)])


# ggplot function
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$Purchase == 1, "True-Pos", v)
  v <- ifelse(df$pred >= threshold & df$Purchase == 0, "False-Pos", v)
  v <- ifelse(df$pred < threshold & df$Purchase == 1, "False-Neg", v)
  v <- ifelse(df$pred < threshold & df$Purchase == 0, "True-Neg", v)
  
  df$pred_type <- v
  
  ggplot(data=df, aes(x=Purchase, y=pred)) + 
    geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}

plot_pred_type_distribution(fit_prob_b, 0.65)
```

![](README_files/figure-gfm/results%20bayesian-1.png)<!-- -->

``` r
# Probability False Positive
(nrow(fit_prob_b[fit_prob_b$pred >= 0.65 & fit_prob_b$Purchase == 0,]) / nrow(fit_prob_b[fit_prob_b$Purchase == 0,])) * 100
```

    ## [1] 0.6678

``` r
# Ptobability False Negative
(nrow(fit_prob_b[fit_prob_b$pred < 0.65 & fit_prob_b$Purchase == 1,]) / nrow(fit_prob_b[fit_prob_b$Purchase == 1,])) * 100
```

    ## [1] 0.9078

## Predictions

I use the model fitted on the training set to predict the test set.

``` r
pred_b <- predict(logit_b, newdata = test_data1)

test_data1 <- as.data.table(test_data1)

pred_mean <- as.data.table(rowMeans(pred_b)) %>%
  cbind(., test_data1[, c('id')]) %>%
  group_by(id) %>%
  summarise(tot_prob = sum(V1, na.rm = TRUE)) %>%
  arrange(desc(tot_prob)) 

# extract the 1000 user_id's most likely to purchase.

write.csv(pred_mean[1:1000, 1], "/home/alice/Downloads/6sense Task/1000_users1.csv", sep = ',')
```
