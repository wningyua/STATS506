# Stats 506, Fall 2019
# Problem Set 2, Question 2
# 
# This script contains solutions for question 2. The question includes organizing
# trajectory data into a tidy dataframe and use data to compute curvatures. Then, build linear
# mixed-effect models and compare the fixed effect between models. 
# 
# Author: Ningyuan Wang
# Date: Oct 12, 2019

# Libraries
library(tidyverse)
library(dplyr)
library(mousetrap)
library(stringr)
library(lme4)

# Read in the data
dt = mousetrap::KH2017_raw

# a. utility functions---------------------------------------------------------
source('./ps2_q2_funcs.R')

# b. --------------------------------------------------------------------------
# get the position columns
x_pos = dt$xpos_get_response 
y_pos = dt$ypos_get_response
t_pos = dt$timestamps_get_response
sub_pos = dt$subject_nr
tra_pos = dt$count_trial

# extract xyt
extract_xyt = function(pos){
  # the function extract the coordinates (xyt) from lists of columns
  # input: the column of x, y or t
  # output: the list of characters of coordinate x, y or t
  pos_sub = str_replace_all(pos, "\\[|\\]", "" )
  str_split(pos_sub, ', ')
}

# call above function and combine xyt with trial and subject to create 
# a new dataframe
traj_df = dt  %>% 
  transmute(subject =subject_nr, trail = count_trial, 
            x = extract_xyt(x_pos), y = extract_xyt(y_pos), 
            t = extract_xyt(t_pos)) %>%
  unnest(cols = c('x', 'y', 't')) %>%
  mutate_if(is.character, as.numeric) # traj_df

# c ---------------------------------------------------------------------------
# apply the source functions to the data and store the curvatures into a data frame
convert_vec2tibble = function(x) as_tibble( as.list(x) ) #Formats output 
# of comp_measures() as a tibble

# compute curvatures
comp_results = 
  traj_df %>% 
  group_by(subject, trail) %>%
  do( convert_vec2tibble( 
    comp_measures( normalize_traj( cbind(.$x, .$y, .$t ) ) )
  ) 
  ) #comp_results

# store the curvature results
dt_out = dt %>% 
  bind_cols(comp_results) %>%
  filter(correct == 1) %>%
  transmute('Subject' = subject,
         'Trail' = trail, 
         'Condition' = Condition,
         'Exemplar' = Exemplar,
         'Total Distance' = tot_dist,
         'Max Abs Dev' = max_abs_dev,
         'Avg Abs Dev' = avg_abs_dev,
         'AUC' = AUC
         ) #dt_out

# d.---------------------------------------------------------------------------
# build linear mixed-effect models for different curvatures 
# and compare the relative fixed effect 
mod_dist = lmer(log(`Total Distance`) ~ Condition + (1 | Subject) + (1 | Exemplar), 
                  data = dt_out, REML = FALSE) # lmer model for tot_dist
Dist = fixef(mod_dist)[2]

mod_max = lmer(log(`Max Abs Dev`) ~ Condition + (1 | Subject) + (1 | Exemplar), 
                data = dt_out, REML = FALSE) # lmer model for max
Max = fixef(mod_max)[2]

mod_avg = lmer(log(`Avg Abs Dev`) ~ Condition + (1 | Subject) + (1 | Exemplar), 
               data = dt_out, REML = FALSE) # lmer model for avg
Avg = fixef(mod_avg)[2]

mod_auc = lmer(log(AUC) ~ Condition + (1 | Subject) + (1 | Exemplar), 
               data = dt_out, REML = FALSE) # lmer model for auc
Auc = fixef(mod_auc)[2]

# to produce a table 
cap_efffect = "Relative Effect of Condition"
knitr::kable(rbind(Dist, Max, Avg, Auc), caption = cap_efffect,  digits =2)




