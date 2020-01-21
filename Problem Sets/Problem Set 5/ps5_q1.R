## 506 Problem Set 5
#
#  The script is the solution  of 506 problme set 5 question 1
# 
# Author: Ningyuan Wang
# Date: December 4, 2019
#
# Use the RECS 2015 Data to find which census division has the largest 
# disparity between urban and rural areas in terms of the proportion of 
# homes with internet access. 
#
# data source: https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv
# Notes: some code referred to Dr.Henderson's code from ps3_q1 fall 2018 to enhance the efficency
#----------------------------------------------------------------------------------------
# libraries
library(tidyverse)
library(data.table)

# read in the data
recs = fread("https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv")

# replicate weights
weights = recs[, c('DOEID', paste0('BRRWT', 1:96)), with = FALSE] # extract columns
weights_long = melt(weights, id_vars = "DOEID", measure.vars = c(paste0('BRRWT', 1:96)), 
                    variable.name = "repl", value.name = "w") %>% rename(id = DOEID)


# recode division
divisions = c("New England",
             "Middle Atlantic",
             "East North Central",
             "West North Central",
             "South Atlantic",
             "East South Central",
             "West South Central",
             "Mountain North",
             "Mountain South",
             "Pacific")
recs[, Division := factor(DIVISION, 1:10, divisions)]

# reduce the dataset 
internet = recs[, .(id = DOEID, w = NWEIGHT, x = 100*INTERNET, Division,
                    urban = UATYP10 %in% c("U", "C"))]

# compute point estimate
pe = internet[, .(est = sum(w*x)/sum(w)), keyby = .(Division, urban)] %>%
  .[, .(Est = abs(est[1] - est[2])), keyby = .(Division)] %>%
  .[order(-Est)]
  

# replicate estimate
pe_r = merge(weights_long, internet[, -"w", with = FALSE], by = "id")%>%
  .[, .(est = sum(w*x) / sum(w)), keyby = .(repl, Division, urban)] %>%
  .[, .(r = abs(est[2] - est[1])), keyby= .(repl, Division)]


# standard error and confidence interval
cap1 = "**Disparity between urban and rural areas of the proportion of homes with internet**"
internet_diff = merge(pe_r, pe, by = c("Division"))%>%
  .[, .(Est = round(Est[1],3), 
        SE = round((sqrt(mean({r - Est}^2)) *2), 3)), Division]%>%
  .[, CI := sprintf( '(%4.3f, %4.3f)', 
                     Est - qnorm(.975) * SE, Est + qnorm(.975) * SE)] %>%
  .[order(-Est)]  %>% 
  knitr::kable(align = "r", cap = cap1)





    






