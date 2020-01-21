  ## 506 Problem Set 5
  #
  #  The script is the solution  of 506 problme set 5 question 2
  # 
  # Author: Ningyuan Wang
  # Date: December 4, 2019
  #
  # Use data.table to clean and analyze the DNA methylation data, as well as answer the 
  # following questions 
  #
  # data source: 
  # ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE138nnn/GSE138311/matrix/GSE138311_series_matrix.txt.gz
  #----------------------------------------------------------------------------------------
  # libraries
  library(tidyverse)
  library(data.table)
  library(doParallel)
  library(future)
  set.seed(666)
  alpha = .05
  
  #a.----------------------------------------------------------------------------
  # download the data and inspect the first 100 rows at the command line
  
  wget "ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE138nnn/GSE138311/matrix/GSE138311_series_matrix.txt.gz"
  gunzip GSE138311_series_matrix.txt.gz # uncompress the data
  head -n 100 GSE138311_series_matrix.txt
  
  wc -l GSE138311_series_matrix.txt #786090
  # in the header, we notice that the sample count is 786020, but the 
  # total line is 786090, so there are 69 lines of header information.
  # the first sample is on the line 70
  
  #b.----------------------------------------------------------------------------
  # read the data into data.table and reduce the data to chromosomal locations
  # beginning with "ch" and only keep no missing values. 
  # The final data table with each row corresponding to a single sample-probe pair
  
tb = fread("./GSE138311_series_matrix.txt", skip = 68) #skip the information
names(tb)
dim(tb) # 786020 * 14
tb =  tb %>% .[ID_REF %like% "^ch", ] 
which(tb[ , colSums(is.na(tb)) == nrow(tb)]) # find the column with all NAs 
  
tb = tb %>%
    .[, -"GSM4105199", with = FALSE]  %>% 
    melt(., id_vars = "ID_REF", measure.vars = c(paste0('GSM', 4105187:4105198)),
         variable.name = "sample", value.name = "value") %>% rename(probe = ID_REF)
  
#c.----------------------------------------------------------------------------
# add a column called sample_group to the data table which labeled with Crohn's
# disease or not. 
  
tb = tb[, `:=`(sample_group = 1L * {sample %in% c(paste0('GSM', 4105187:4105193))})][]  

#d.----------------------------------------------------------------------------
# compute t_statistics comparing the difference in means between groups for each 
# unique probe

t_stats = tb[, .(est = mean(value), s = sd(value), n = length(value)), 
               by = .(probe, sample_group)] %>% 
    .[, .( n = sum(n), 
           t = (est[1] - est[2]) / ((sqrt(sum((n-1) * s^2) / (sum(n) - 2))) * sqrt(sum(1/n)))),
      by = .(probe)]
  

#e.----------------------------------------------------------------------------
# add a column probe_group assigning probes to groups using the first 5 digits
# of the probe ID

t_stats = t_stats[, probe_group := str_extract(probe, "^.{5}")][]
  
#f.----------------------------------------------------------------------------
# compute the proportion of probes within each probe group that are nominally 
# significant at the 5% level assuming a two-tailed test.

proportion =   t_stats[, q := qt( 1 - alpha/2 , df = n-2)] %>%
  .[, `:=`(sig = 1L * {abs(t) > q})] %>%
  .[, .( proportion =  sum(sig) / length(sig) * 100), by = .(probe_group)] %>%
  .[order(-proportion)]


# the figure below comparing the percentages between groups and we found ch.14
# over-represented. 

proportion %>% ggplot(., aes(x=probe_group, y = proportion)) +
  geom_bar(fill= rgb(0,0,1,.5), stat="identity")+
  ylab("Proportion of Probes") +
  xlab("Probe Group")
#f igcap = cap
cap = paste0(
  "**Figure 1.** *Proportion of probes within each probe group that are moninally significant* ",
  "The figure shows the proportion of probes within each probe group that are",
  "nominally significant at the 5% level assuming a two-tailed t test. ",
  "group 'ch.14' stands out as potentially over-represented."
)

  
#g.----------------------------------------------------------------------------
# use permutation tests to assess the statistical significance of each probe group

compute_t = function(table, type = c("two-tailed", "greater", "lesser"),  permute = c(TRUE, FALSE)){
  # the function serves use permutation test to assess the statistical significance
  # of each probe group with three moethods.
  # input: a data table on DNA methylation. We also need type of the statistics 
  # and logical flag for permutation test
  # output: a list of t-scores for each probe group with certain type 

  
  # a. compute t-statistic for each probe
  ti =  table[, .(est = mean(value), s = sd(value), n = length(value)), 
                    by = .(probe, sample_group)] %>% 
    .[, .( n = sum(n), df = sum(n) -2,
           t = (est[1] - est[2]) / ((sqrt(sum((n-1) * s^2) / (sum(n) - 2))) * sqrt(sum(1/n)))),
      by = .(probe)] %>%
    .[, probe_group := str_extract(probe, "^.{5}")] 


  
  #b. permutation: resampling without replacement
  if (permute == TRUE){  # flag is on

    sample_list = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
    table = table[, .(probe, sample, value,
              group_p = rep(sample(sample_list, replace = FALSE), times = length(unique(probe))))]%>%
      .[, probe_group := str_extract(probe, "^.{5}")] 

    # update ti after permutation test
    ti =  table[, .(est = mean(value), s = sd(value), n = length(value)),
                by = .(probe, group_p)] %>%
      .[, .( n = sum(n), df = sum(n) -2,
             t = (est[1] - est[2]) / ((sqrt(sum((n-1) * s^2) / (sum(n) - 2))) * sqrt(sum(1/n)))),
        by = .(probe)] %>%
      .[, probe_group := str_extract(probe, "^.{5}")] 
  }

  
  # c. compute statistics for each probe group
  
  # compute all quantities in the table t_all:
    # g: numer of probes in a given probe group 
    # tq_abs: alpha-quantile from t-distribution with two-tailed method
    # tq_up: alpha-quantile from t-distribution with greater method
    # tq_down: alpha-quantile from t-distribution with lesser method
  

  t_all = ti[, .(g = .N, t, probe, probe_group, df = df,
                 tq_abs = qt(1 - alpha/2, df), 
                  tq_up = qt(1 - alpha, df), 
                 tq_down = qt(alpha, df)), 
             by = .(probe_group)]
  
  # compute t scores with three methods seperately
  
    # two-tailed
  if (type == "two-tailed"){
    t_out = t_all[, `:=`(ind_abs = 1L * {abs(t) > tq_abs })] %>%
      .[, .( t_abs = 1/g * sum(abs(t) * ind_abs)), by = "probe_group"] %>% 
      setkey(., "probe_group") %>% 
      unique(.)
  }
  
  
    # greater
  if(type == "greater"){
    t_out = t_all[, `:=`(ind_up = 1L * {t > tq_up })]  %>%
      .[, .( t_up = 1/g * sum(t * ind_up)), by = "probe_group"] %>% 
      setkey(., "probe_group") %>%
      unique(.)

  }
  
   # lesser
  if(type == "lesser"){
    t_out = t_all[, `:=`(ind_down = 1L * {t < tq_down })]  %>%
      .[, .(t_down = 1/g * sum(t * ind_down)), by = "probe_group"] %>% 
      setkey(., "probe_group") %>%
      unique(.)
  }
  
  return(t_out)
}

  
  
#h.----------------------------------------------------------------------------  
# t scores without permutation test
t1 = compute_t(table = tb, type = "two-tailed", FALSE)
t2 = compute_t(table = tb, type = "greater", FALSE)
t3 = compute_t(table = tb, type = "lesser", FALSE)

# with permutation test for 1000 times 
nperm = 1e3
system.time({
perm_t1 = replicate(nperm, compute_t(table = tb, type = "two-tailed", TRUE))
})

# create a matrix for permutation results
perm_t1_mat = matrix(unlist(perm_t1), ncol = nperm * 2) 
delete_cols = seq(1, ncol(perm_t1_mat), 2)
perm_t1_mat = perm_t1_mat[, -delete_cols] 
perm_t1_mat = apply(perm_t1_mat, c(1, 2), as.numeric)


# compute p-value
t1_obs = as.numeric(t1$t_abs)
p_abs = {1 + rowSums(abs(t1_obs) <= abs(perm_t1_mat))} / {1 + nperm } 
data.table(`probe group` = t1$probe_group, `p value` = round(p_abs, 3))

#i.----------------------------------------------------------------------------  

# compute p_values to assess significance of each probe group with greater method 
# under 1000 permutations, paralleling with mclapply

system.time({
  perm_t2 = parallel::mclapply(1:nperm, function(i) compute_t(table = tb, type = "greater", TRUE))
}
)

# create a matrix for permutation results
perm_t2_mat = matrix(unlist(perm_t2), ncol = nperm * 2) 
delete_cols = seq(1, ncol(perm_t2_mat), 2)
perm_t2_mat = perm_t2_mat[, -delete_cols] 
perm_t2_mat = apply(perm_t2_mat, c(1, 2), as.numeric)


# compute p-value
t2_obs = as.numeric(t2$t_up)
p_up = {1 + rowSums(abs(t2_obs) <= abs(perm_t2_mat))} / {1 + nperm } 
data.table(`probe group` = t2$probe_group, `p value` = round(p_up, 3))


#j.----------------------------------------------------------------------------  
# compute p_values to assess significance of each probe group with lesser method 
# under 1000 permutations, paralleling with future

plan(multisession)
perm_t3 = list()
system.time({
for( i in 1: nperm ){
  perm_t3[[i]] = future({compute_t(table = tb, type = "lesser", TRUE)})
  }
}
) 

# extract values
perm_t3 = rbindlist( lapply(perm_t3, value) )

# create a matrix for permutation results
perm_t3_mat = matrix(unlist(perm_t3), ncol = nperm * 2) 
delete_cols = c(1:1000)
perm_t3_mat = perm_t3_mat[, -delete_cols] 
perm_t3_mat = apply(perm_t3_mat, c(1, 2), as.numeric)


# compute p-value
t3_obs = as.numeric(t3$t_down)
p_down = {1 + rowSums(abs(t3_obs) <= abs(perm_t3_mat))} / {1 + nperm } 
data.table(`probe group` = t3$probe_group, `p value` = round(p_down, 3))
