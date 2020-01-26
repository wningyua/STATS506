## 506 Group Project -NW
#
# How are drinking habits related to general health condition for adults in the USA ?
#
# Fit the linear regression of response frequency of drinking alcohol in the past
# 12 months with the predictors health condition, age, gender and, PIR
#
# Samples were limited to helathy people aged over 21 years (3 healthy levels)
# continuous variables frequency of drinking, age and PIR was standardized
# 
# Author: Ningyuan Wang
# Date: December 1, 2019
#
# Data manipulation was done with package dplyr() in R
#------------------------------------------------------------------------------
## libraries
library(Hmisc) # for read xpt datasets 
library(dplyr)
library(tidyverse)
library(boot)
library(ggplot2)

# read in the datasets and select the variables 
alcohol = sasxport.get("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/ALQ_D.XPT") %>% 
  select(seqn, alq120q) %>%
  filter(alq120q < 366) %>%
  drop_na()

health = sasxport.get("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/HSQ_D.XPT") %>%
  select(seqn, hsd010) %>%
  filter(hsd010 <=3 ) %>%
  drop_na() # limit to the healthy samples


demography = sasxport.get("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT") %>%
  filter(ridageyr >=21,dmdeduc2 <= 5 ) %>%
  transmute(seqn, age = ridageyr, gender = riagendr, education = dmdeduc2, pir = indfmpir ) %>%
  drop_na() # limit to people > 20 years

# join above datasets
df = left_join(alcohol, health, by = "seqn") %>% 
  left_join(., demography, by = "seqn") %>%
  drop_na() %>%
  rename(drinks = alq120q, health = hsd010) # 2791*7

# convert variables to factors and rescale continous variables
df = df %>% 
  mutate_at(vars(seqn, health, gender, education), as.factor) %>%
  mutate(drinks_std = scale(drinks),
         age_std = scale(age),
         pir_std = scale(pir))



#  plot the relation and fit the model
ggplot(df, aes(x=health, y= drinks_std)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, color='#2C3E50')


lm1 = lm(drinks_std ~ relevel(health, ref = 3)  + age_std + gender + 
           education + pir_std, data = df) 

summary(lm1) # only gender and edcuation are statistically significant, RSE = 0.999 and R^2 = 0.0053


# model diagnostics 
res = lm1$residuals 
fit = lm1$fitted
plot(fit, res, xlab = "fitted", ylab = "Residuals") # unequal varlaince in residuals
qqnorm(res, ylab = "Residuals"); qqline(res) # non-normal residual


# coverage probability on how much observation can be catched by confidence intervals
t = qt(1 - 0.05/2, df = lm1$df.residual)
rse = sqrt(sum(res^2) / lm1$df.residual) # 0.9997812
lwr = fit - t * rse
upr = fit + t * rse


ci = cbind(df$drinks_std, lwr, upr) %>%
  as_tibble() %>%
  transmute(obs = V1, lwr = lwr, upr = upr, 
            cover = as.numeric(obs > lwr & obs < upr) )

coverage = sum(ci$cover) / nrow(ci) # 0.9914009
length = mean(ci$upr - ci$lwr) # 3.920777


# so far, we learned that residuals of the above model are not normal which will 
# break the model assumption. To enhance the model, we decided to use bootstrapping
# method to resample residuals, and then we hope to increaase the coverage and 
# shorten the confidence interval in the meantime. 

# Also note, the model failed F test but it is common that the data is not perfect
# in reality and therefore we still use the model to explore the relationship between
# health condition and drinking alcohol habits

#------------------------------------------------------------------------------

# Part I： bootstrapping residuals and compute the coverage (our original approach, 
# to improve standard error  in response

# 
# n = nrow(df) # sample size
# B = 1e3  # number of bootstrap samples 
# boot_samples = sample(res, n * B, replace = TRUE)
# dim(boot_samples) = c(n, B) # each column is a dataset
# 
# boot_rse = sqrt(colSums(boot_res^2) / lm1$df.residual) # 1000 boot_rse samples
# hist(boot_rse)
# 
# boot_rse  = matrix(boot_rse, nrow = n, ncol = B, byrow = TRUE) # expand boot_rse to a matrix
# 
# boot_lwr = fit - t * boot_rse 
# boot_upr = fit + t * boot_rse
# 
# 
# obs = matrix(df$drinks_std, nrow = n, ncol = B) # expand observations to a matrix
# boot_cover = matrix(as.numeric(obs > boot_lwr & obs < boot_upr), nrow = n, ncol = B)
# boot_coverprob = colSums(boot_cover) / n # cover probability as our estimate
# 
# # distribution of cover probability 
# mean(boot_coverprob) # 0.9912827
# quantile(boot_coverprob)
# hist(boot_coverprob)
# 
# # we find that the coverage decreases a little but the length reduced more


#------------------------------------------------------------------------------

# Part II： bootstrapping  regression residuals to correct point estimates

# a. construct bootstrap samples : res and new response y* (skip the original 
# linear model which is same as in part I)
n = nrow(df) # sample size
B = 1e3  # number of bootstrap samples 

boot_samples = sample(res, n * B, replace = TRUE)
dim(boot_samples) = c(n, B) # each column is a dataset
boot_y = matrix(fit, n, ncol(boot_samples)) + boot_samples # bootstrap y as faked response

# b. refit the model with bootstrap samples on y* and get the estimate of coefficents
lm_coef = function(y) lm(y ~ df$health + df$age_std + df$gender + df$education + df$pir_std)$coef
boot_lms = apply(boot_y, 2, lm_coef)

# c. get the bootstrap estimates of the model
boot_mean_est = rowMeans(boot_lms) # mean point estimates 
boot_median_est = apply(boot_lms, 1, quantile) # quantile point estimate 

apply(boot_lms, 1, hist) # check the distributions of beta hat

# d. compute standard error and do t_test for each term 
se = sqrt(rowSums((boot_lms - rowMeans(boot_lms))^2) / B)
t =  lm1$coefficients / se
