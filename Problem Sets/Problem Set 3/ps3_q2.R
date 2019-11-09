#Question 2
#set.seed(666)
#------------------------------------------------------------------------------
library(dplyr)
library(data.table)
# a. 
jk_ci = function(x, y, alpha = 0.05){
  ## the function serves to estimate the CI with jackknife standard error
  ## input: two numerix matrices: x, y, each rowrepresenting a Monte Carlo 
  ## replicate.
  ## output: confidence intervals of theta = E(x) / E(y) with jackknife se
  ## for each replicate.
  
  # compute mean of x and y seperately with ith dropping, row by row
  x_bar = matrix( data = (rowSums(xmat) - xmat) / (ncol(xmat) - 1 ), 
                 nrow = nrow(xmat), ncol = ncol(xmat)) # each row is a dataset
  
  y_bar = matrix( data = (rowSums(ymat) - ymat) / (ncol(ymat) - 1 ), 
                  nrow = nrow(ymat), ncol = ncol(ymat)) # each row is a dataset
                   
  
  # compute theta = E(x) / E(y) with ith dropping
  theta = cbind(x_bar / rowMeans(ymat), rowMeans(xmat) / y_bar)
  
  theta_bar = rowMeans(theta) #compute theta_bar for each row

  #compute se and ci
  se = sqrt( (ncol(theta) - 1) / ncol(theta) * rowSums( (theta - theta_bar)^2 ) )
  l = theta_bar - qnorm(1-alpha/2) * se
  u = theta_bar + qnorm(1-alpha/2) * se
  cbind('2.5%' = l, '97.5%' =u) # the function returns to the confident intervals 
}


## b. 
boot_ci = function(xmat, ymat, n_boot = 10, alpha = 0.05){
  ## the function serves to estimate the CI with three bootstrap methods
  ## input: two numerix matrices: xmat, ymat representing Monte Carlo replicates in every row
  ## output: three methods of CI for each Monte Carlo replicate (each row is a replicate)
  
  # bootstrap resampling for x and y separately
  x_boot = apply(xmat, MARGIN = 1, FUN = sample, size = ncol(xmat) * n_boot, replace =TRUE) 
  dim(x_boot) = c(ncol(xmat), n_boot, rep) # sample * bootsrap * rep
  x_est = colMeans(x_boot, dims = 1) # estimate on each combination of bootstrap and replicate
  
  
  y_boot = apply(ymat, MARGIN = 1, FUN = sample, size = ncol(ymat) * n_boot, replace =TRUE) 
  dim(y_boot) = c( ncol(ymat), n_boot, rep)
  y_est = colMeans(y_boot, dims = 1) 
  
  est = x_est / y_est # bootstrap *  replicate estimations, each column is a replicate
  est_hat = colMeans(est) # est_hat for each Monte Carlo replicate, each column is a replicate
  
  # quantiles 
  q = apply(est, MARGIN = 2, FUN = quantile, probs =  c(alpha/2, 1 - alpha/2) ) # quantile for bootstrap distribution
  # each column is  a replicate
  
  # sd for each replicate
  sd_boot = sqrt(colSums((est - est_hat) ^2) / (nrow(est) - 1)) 
  
  # construct ci for each replicate
  percent_ci = t(q) 
  boot_ci = cbind(lwr = 2 * est_hat - q[2, ], upr = 2 * est_hat - q[1, ]) 
  norm_ci = cbind(lwr = est_hat - qnorm(1-alpha/2) * sd_boot, upr = est_hat + qnorm(1-alpha/2) * sd_boot)
  out = cbind( percent_ci, boot_ci,  norm_ci) # combine three types of confidence intervals
  colnames(out) = c("percentile_lwr", "percentile_upr", "basicboot_lwr", "basicboot_upr", "normaprox_lwr", "normaprox_upr" )
  out
}



# c. 
# I choose two possion distributions for X and Y. Lambda for X is 10 and Lambda for Y is 20
# The size for both X and Y is 50

# generate Monte Carlo replicates for X and Y, each row is a replicate dataset
size = 50 # the size of X and Y 
rep = 1e3 # Monte Carlo replicate size
xmat = rpois( n = size * rep, lambda = 10 )
dim(xmat) = c(rep, size) # rep * size

ymat = rpois(n = size * rep, lambda = 20)
dim(ymat) = c(rep, size) # rep * size

# compute the CI by calling above functions
jk = jk_ci(xmat, ymat) 
bt = boot_ci(xmat, ymat) 
cis = matrix(data = cbind(jk, bt), ncol = 8)
colnames(cis) = c("jk_lwr", "jk_upr", "pt_lwr", "pt_upr", "boot_lwr", "boot_upr",
               "norm_lwr", "norm_upr")

# i. coverage probability
point_est = mean(xmat) / mean(ymat) 
true = 10 / 20 # lambda(X) / lambda(Y)

jk_prob = cis %>% 
  as_tibble() %>%
  select(jk_lwr, jk_upr) %>%
  filter(true >= jk_lwr & true<= jk_upr) %>%
  summarize(jk_prob = n() / rep)


pt_prob = cis %>% 
  as_tibble() %>%
  select(pt_lwr, pt_upr) %>%
  filter(true >= pt_lwr & true<= pt_upr) %>%
  summarize(pt_prob = n() / rep) 

boot_prob = cis %>% 
  as_tibble() %>%
  select(boot_lwr, boot_upr) %>%
  filter(true >= boot_lwr & true<= boot_upr) %>%
  summarize(boot_prob = n() / rep) 

norm_prob = cis %>% 
  as_tibble() %>%
  select(norm_lwr, norm_upr) %>%
  filter(true >= norm_lwr & true<= norm_upr) %>%
  summarize(norm_prob = n() / rep) 

data.table::data.table(jk_prob, pt_prob, boot_prob, norm_prob)

# ii. average length of CI 
jk_len = mean(cis[,2] - cis[,1])
pt_len = mean(cis[,4] - cis[,3]) 
boot_len = mean(cis[,6] - cis[,5]) 
norm_len = mean(cis[,8] - cis[,7]) 

# iii. average shape of CIs
jk_shape = mean((cis[,2] - point_est) / (point_est - cis[,1])) 
pt_shape = mean((cis[,4] - point_est) / (point_est - cis[,3])) 
boot_shape = mean((cis[,6] - point_est) / (point_est - cis[,5])) 
norm_shape = mean((cis[,8] - point_est) / (point_est - cis[,7])) 









