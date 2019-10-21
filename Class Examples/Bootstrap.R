# suppose we have iid data X1 to Xn from an unknown distribution and want to 
# construct a confidence interval for the inter-quartile range
n = 50
x = rgamma(n, shape = 3, rate = 1) 
boxplot(x, las = 1, main = 'Boxplot of Data')

iqr_est = unname(diff(quantile(x, c(.25,.75)))) 
iqr_est

# to form a (non-parametric) bootstrap estimate, we draw B new data sets from the
# original data.
B = 1e3 # Number of bootstrap samples
boot_samples = sample(x, size = n*B, replace = TRUE) ## sample with replacement
dim(boot_samples) = c(n, B) ## each column is a dataset

# for each bootstrap sample, we compute the statistic of interest, in this case the IQR. 
# This operation is not easy to vectorize so we use an implicit loop instead.
boot_iqr = apply(boot_samples, 2, function(b) unname(diff(quantile(b, c(.25, .75))))) 

# This gives us B samples of the IQR. We use these to estimate the sampling distribution
# and construct a 95% confidence interval
hist(boot_iqr, las = 1, col = 'green4', xlab = 'Estimated IQR',
     cex.axis = 1.5, cex.lab = 1.5, main = '')
boot_q = quantile(boot_iqr, c(.025, .975))
abline(v = boot_q, lty = 'dashed', lwd = 2)
boot_ci = sprintf('%3.1f (%3.1f, %3.1f)', iqr_est, boot_q[1], boot_q[2])
cat(boot_ci, '\n')

# here is a function that collects the above computations into one place and is repeatable
iqr_ci = function(y, n_boot = 1e3){
  m = length(y)
  mat = sample(y, n_boot * m, replace = TRUE)
  dim(mat) = c(n_boot, m)
  f = function(x) diff(quantile(x, c(.25, .75)))
  v = apply(mat, 1, f)
  lcb = quantile(v, 0.025)
  ucb = quantile(v, 0.975)
  return(c(lcb, ucb))
}
iqr_ci(x)




