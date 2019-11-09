# Stats 506, Fall 2019
# Problem Set 2, Question 1
# 
# This script contains answers for several questions using the 2015 RECS survey data,
# all data manipulation done with the packages 'dplyr', 'tidyr' and 'stringr', and 
# 'ggplot2' for plotting. 
# 
# Author: Ningyuan Wang
# Date: Oct 12, 2019


# Libraries
library(tidyverse)
library(ggplot2)
library(dplyr)


# read in the data
url_base = paste0('https://www.eia.gov/consumption/residential/data/2015/csv')
csv_file = './recs2015_public_v4.csv'
csv_url = sprintf('%s/recs2015_public_v4.csv', url_base)
recs_tb = readr::read_delim(csv_url, delim = ',') #recs_tb
#write_delim(recs_tb, path = csv_file, delim = ',')

# weights data
weights = recs_tb %>%
  select(DOEID = DOEID, nweight = NWEIGHT, starts_with("BRRWT")) # weights

# Convert weights to long
weights_long = weights %>%
  gather(key = 'repl', value = 'w', BRRWT1:BRRWT96)


# a. --------------------------------------------------------------------------
# compute national average home temperature at night in winter, among homes
# homes that use space heating.

# sample weighted mean
temp_mean = recs_tb %>%
  transmute(tempnite = na_if(TEMPNITE, -2), 
              heathome = HEATHOME, 
              weight = NWEIGHT) %>%
  filter(heathome == 1) %>%
  mutate( homes = sum(weight), temps = sum(weight*tempnite), 
             temp_mean = temps / homes) %>%
  select(temp_mean) %>%
  distinct() # temp_mean = 68.1

# replicate weighted means and compute ci
rep_mean = weights_long %>%
  left_join(recs_tb %>%
              mutate(DOEID = as.integer(DOEID), by ='DOEID')) %>%
  transmute(tempnite = TEMPNITE, 
            heathome = HEATHOME, 
            repl = repl,
            w =w) %>%
  filter(tempnite > 0, heathome == 1) %>%
  group_by(repl) %>%
  summarize( homes = sum(w), temps = sum(w*tempnite), 
             rep_mean = temps / homes) %>% # rep_mean 
  group_by(repl) %>%
  summarize(temp_mean = 68.1, diff_square = (rep_mean - temp_mean)^2) %>%
  ungroup() %>%
  mutate(temp_mean = temp_mean,
            sum_sqr = sum(diff_square), se = sqrt(1/ (96 * (1-0.5)^2) * sum_sqr)) %>% #0.199 
    transmute(est = temp_mean, se_est = se, 
            lwr = est - qnorm(.975) * se, 
            upr = est + qnorm(.975) * se)%>%
  distinct() 

# to produce a table 
cap_mean ="National Average Home Temperatrure At Night"
knitr::kable(rep_mean, 
             col.names= c("Estimate", "Standard Error", "Lower Bound", "Upper Bound"),
             caption = cap_mean, align = 'c', digits =2)



# b. --------------------------------------------------------------------------
# Compute the proportion of homes using each level of "main space heating
# fuel" within each unique combination of census division and census (2010)
# urban type among homes that use space heating.
# store the rsult in a table called "home_prop"

# sample weighted proportion 
home_prop = recs_tb %>%
  filter(HEATHOME == 1) %>% #keep the homes that using space heating only
  transmute(division = DIVISION, type = UATYP10, 
            fuel = FUELHEAT, weight = NWEIGHT) %>%
  complete(division, type) %>%
  replace_na(list(fuel = -2))  %>% #replace missing values with NA
  drop_na() %>%
  group_by(division, type, fuel) %>% 
  summarize( homes = sum(weight)) %>%
  group_by(fuel) %>%
  mutate( home_prop = 100 * homes / sum(homes))  #124 levels 

# replicated weighted proportion
rep_prop = weights_long %>%
  left_join(recs_tb %>%
              mutate(DOEID = as.integer(DOEID), by = 'DOEID')) %>%
  filter(HEATHOME == 1) %>%
  transmute(division = DIVISION, type = UATYP10, 
            fuel = FUELHEAT, 
            repl = repl, w = w) %>%
  complete(division, type) %>%
  replace_na(list(fuel = -2))  %>% #replace missing values with NA
  drop_na() %>%
  group_by(division, type, fuel, repl) %>%
  summarize( homes_r = sum(w)) %>%
  group_by(fuel, repl) %>%
  mutate(rep_prop_r = 100 * homes_r / sum(homes_r)) %>% #rep_prop 
  ungroup() %>% 
  left_join(home_prop, by = c("division", "type", "fuel"))
  

# compute standard errors
prop_ci = rep_prop %>%
    group_by(division, type, fuel, repl) %>%
    mutate(diff_squares = (rep_prop_r - home_prop)^2)%>%
    ungroup() %>%
    group_by(division, type, fuel) %>%
    mutate(se =sqrt( 4 * (sum(diff_squares) / 96))) %>%
    select(division, type, fuel, home_prop, se) %>%
    distinct() %>%
    mutate(lwr =max(0,home_prop - qnorm(.975) * se) ,
           upr = home_prop + qnorm(.975) * se )


# to produce a table
caps_prop = "Proportion of Homes Using Fuel"
cn = c('Divison', 'Type',"Fuel","Prop Estimate", 'Standard Error', 'lower Bound', "Upper Bound")
knitr::kable(prop_ci, col.names = cn,caption = caps_prop, digits = 2)


# c. --------------------------------------------------------------------------
# Create a plot comparing by census division and urban type, the average 
# winter home temperatures at night, during the day with someone home, and 
# during the day with no one home. 

# sample weighted means 
temps_mean = recs_tb %>%
  transmute(division = DIVISION, type = UATYP10, temp_home =na_if(TEMPHOME, -2),
            temp_gone =na_if(TEMPGONE,-2), temp_nite = na_if(TEMPNITE, -2), 
            weight = NWEIGHT) %>% 
  drop_na() %>%
  group_by(division, type) %>%
  summarize(homes = sum(weight),
            temp_nite_mean = sum(weight * temp_nite) / homes,
            temp_gone_mean = sum(weight * temp_gone) / homes,
            temp_home_mean = sum(weight * temp_home) / homes
            ) 
  
# replicated weighted means  
temps_rep = weights_long %>%
  left_join(recs_tb %>%
              mutate(DOEID = as.integer(DOEID), by ='DOEID')) %>%
  transmute(division = DIVISION, type = UATYP10, temp_home =na_if(TEMPHOME, -2),
            temp_gone =na_if(TEMPGONE,-2), temp_nite = na_if(TEMPNITE, -2), 
            repl = repl, w = w) %>%
  drop_na() %>%
  group_by(division, type, repl) %>%
  summarize(homes = sum(w), 
            nite_r = sum(w * temp_nite) / homes,
            gone_r = sum(w * temp_gone) / homes,
            home_r = sum(w * temp_home) / homes
            ) %>% #temps_rep 
  ungroup() %>% 
  left_join(temps_mean, by = c("division", "type"))

# compute standard error and confidence interval 
temps_ci = temps_rep %>%
  group_by(division, type, repl) %>%
  mutate(diffs_nite = (nite_r - temp_nite_mean)^2,
         diffs_gone = (gone_r - temp_gone_mean)^2,
         diffs_home = (home_r - temp_home_mean)^2
         ) %>%
  ungroup() %>%
  group_by(division, type) %>%
  mutate(se_nite =sqrt( 4 * (sum(diffs_nite) / 96)),
         se_gone = sqrt(4 * (sum(diffs_gone) / 96)),
         se_home = sqrt(4 * (sum(diffs_home) / 96))) %>%
  select(division, type, temp_nite_mean, se_nite,
         temp_home_mean, se_home, 
         temp_gone_mean, se_gone) %>%
  distinct() %>%
  mutate( nite_lwr = temp_nite_mean - qnorm(.975) * se_nite,
          nite_upr = temp_nite_mean + qnorm(.975) * se_nite,
          home_lwr = temp_home_mean - qnorm(.975) * se_home,
          home_upr = temp_home_mean + qnorm(.975) * se_home,
          gone_lwr = temp_gone_mean - qnorm(.975) * se_gone,
          gone_upr = temp_gone_mean + qnorm(.975) * se_gone
          ) %>%
  select(division, type, temp_nite_mean, nite_lwr, nite_upr,
         temp_home_mean, home_lwr, home_upr, temp_gone_mean, 
         gone_lwr, gone_upr)

# to produce a table
cap_temps = "Temperatures by Division and Urban Type"
temps_labs = c('Divison', "Type", "Nite_est","Nite_lwr","Nite_upr", 
               'Home_est', 'Home_lwr', "Home_lwr",
               "Gone_est", "Gone_lwr", "Gone_upr")
knitr::kable(temps_ci, caption = cap_temps,
             col.names = temps_labs, digits = 2)


# plot the average temperatures
plot_temps = gather(data = temps_mean,temp_type, temp, temp_nite_mean:temp_home_mean, 
                    factor_key = TRUE) # convert the wide data to a long one

ggplot(plot_temps) +
  aes(x = `type`, y = temp, colour = temp_type) +
  geom_boxplot(fill = "#0c4c8a") +
  scale_color_hue() +
  theme_gray() +
  facet_wrap(vars(`division`))


#d. ---------------------------------------------------------------------------
# compute the (national) median
# difference between the day time (with someone home) and night time 
# temperatures for each level of "main heating equipment household behavior"

# compute median difference
median_recs_tb = recs_tb %>%
  filter(HEATHOME == 1) %>% # homes that use speace heating only 
  transmute(temp_home = na_if(TEMPHOME, -2), 
            temp_nite = na_if(TEMPNITE, -2), 
            behav = na_if(EQUIPMUSE, -9),
            weight = NWEIGHT)%>% #replace missing values with NA
  drop_na() %>%
  group_by(behav) %>%
  mutate(diffs = temp_home - temp_nite) %>%
  arrange(behav,diffs ) %>%
  group_by(behav, diffs) %>%
  summarize_at(vars(weight), funs(sum)) %>% 
  mutate( cum = cumsum(weight), prop = cum /max(cum)) %>%
  filter(prop > 1/2 ) %>%
  slice(which.min(prop))  %>%
  select(behav, diffs)
  
# to produce a table
cap_median = "National Median Difference between Day Time and Night Time Temperature"
name = c('Behavior Level', 'Median Difference')
knitr::kable(median_recs_tb, caption = cap_median,
             col.names = name, digits = 2)









