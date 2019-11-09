## This script is the solution to question 1 in problem set 3 for stats 506 
## Author: Ningyuan Wang 
## Date: November 8, 2019

## Packages
library(datasets)
library(dplyr)
library(tidyr)
library(data.table)

# a. 
jk_ci_vec= function(x,y, alpha = 0.05){
  ## the function serves to estimate confidence interval of  E(X) / E(Y) with 
  ## jackknife standard error method.
  ## input: two numeric vectors: x and y
  ## output: a confidence interval with jackknife method 
  
  x_bar = (sum(x) - x)/ (length(x) - 1) # mean of x dropping ith term
  y_bar = (sum(y)-y) / (length(y) - 1) # mean of y dropping ith term
  theta = c(x_bar / mean(y) , mean(x) / y_bar) 
  #combine x_bar / y_bar with dropping one term either in x or y

  theta_bar = mean(theta) #compute the estimate of theta
  
  se = sqrt((length(theta)-1)/length(theta) * sum((theta - theta_bar)^2)) #compute se
  
  lwr = theta_bar - qnorm(1-alpha/2) * se 
  upr = theta_bar + qnorm(1-alpha/2) * se #compute confidence bound
  c('2.5%' = lwr, '97.5%' =upr) # the function returns to the confident interval
}  
  

# b. 
boot_ci_vec = function(x, y, n_boot = 1e3, alpha = 0.05) {
  ## The function serves to estmiate the confdience inteval of  E(X) / E(Y) with
  ## with three bootstrap methods.
  ## Input: two numeric vectors x and y
  ## Output: three confidence intervals of  E(X) / E(Y)  with the methods of 
  ## percentile, basic bootstrap and normal approximation, seperately.
  
  # bootsstrap resamples for x
  mat_x = sample(x, n_boot * length(x), replace = TRUE)
  dim(mat_x) = c(n_boot, length(x)) # each row is a dataset
  
  # bootsstrap resample for y
  mat_y = sample(y, n_boot * length(y), replace = TRUE)
  dim(mat_y) = c(n_boot, length(y)) # each row is a dataset
  
  # bootsrap estimates
  est = rowMeans(mat_x) / rowMeans(mat_y) 
  dim(est) = c(n_boot, 1) # each row is an estmiate
  est_hat = mean(est) # center of bootstrap distribution
  
  # sd of bootstrap
  sd_boot = sqrt(sum((est - est_hat)^2) / (length(est) - 1)) 
  
  # construct ci
  percent_ci = quantile(est, c(alpha/2, 1-alpha/2)) 
  boot_ci = c(lwr = 2 * est_hat - unname(percent_ci[2]), 
              upr = 2 * est_hat - unname(percent_ci[1] ))
  norm_ci = c(lwr = est_hat - qnorm(1-alpha/2)  * sd_boot, 
              upr = est_hat + qnorm(1-alpha/2)  * sd_boot)
  
  return(rbind(percent_ci, boot_ci, norm_ci))
}



# c. 
## apply above two functions to the dataset ToothGrwoth, and 
## compute the point estimate and four confidence intervals 
## for the ratio of length by OJ and VC on each level of dose.

# compute the point estimate
point_est = ToothGrowth %>%
  pivot_wider(names_from = supp, values_from = len, 
              values_fn = list(len = mean)) %>%
  transmute(dose = dose, ratio = OJ / VC) # point estimate of ratio for each dose, 3 in total


oj0.5 = ToothGrowth %>%
  filter(dose == 0.5 & supp == "OJ") %>%
  select(dose, len) %>%
  pull(len) #n=10

oj1 = ToothGrowth %>%
  filter(dose == 1.0 & supp == "OJ") %>%
  select(dose, len) %>%
  pull(len) #n=10

oj2 = ToothGrowth %>%
  filter(dose == 2.0 & supp == "OJ") %>%
  select(dose, len) %>%
  pull(len) #n=10

vc0.5 = ToothGrowth %>%
  filter(dose == 0.5 & supp == "VC") %>%
  select(dose, len) %>%
  pull(len) #n=10

vc1 = ToothGrowth %>%
  filter(dose == 1.0 & supp == "VC") %>%
  select(dose, len) %>%
  pull(len) #n=10

vc2 = ToothGrowth %>%
  filter(dose == 2.0 & supp == "VC") %>%
  select(dose, len) %>%
  pull(len) #n=10


# compute CI 
# dose = 0.5
jk_ci_0.5 = jk_ci_vec(x = oj0.5, y = vc0.5)
boot_ci_0.5 = boot_ci_vec(x = oj0.5, y = vc0.5) 
tb0.5 = rbind(jk_ci_0.5,boot_ci_0.5 )

# dose = 1
jk_ci_1 = jk_ci_vec(x = oj1, y = vc1)
boot_ci_1 = boot_ci_vec(x = oj1, y = vc1) 
tb1 = rbind(jk_ci_1,boot_ci_1 )

# dose = 2
jk_ci_2 = jk_ci_vec(x = oj2, y = vc2)
boot_ci_2 = boot_ci_vec(x = oj2, y = vc2) 
tb2 = rbind(jk_ci_2,boot_ci_2 )


## table output 
cap = 
  '*Confidence Intervals for Odontoblasts Length Estimate on Levels of Dose *'

data.table( Dose = rep(c(0.5, 1.0, 2.0), each = 4), 
            Estimate =rep(point_est$ratio, each = 4),
            'CI Method' = rep(c("jackknife", "percentile", "bootstrap", 
                           "normaprox"), 
                         times = 3),
            rbind(tb0.5, tb1, tb2)) %>% 
  knitr::kable(align = "r", digits = 3, caption = cap)






