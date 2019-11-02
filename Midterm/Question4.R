## Question 4
## dplyr ad tidyr
## write or interpret short dplyr pipes that explore or analyze  Orange data,
## use help page to learn more about Orange dataset. 
library(dplyr)
library(tidyverse)

# a.
# write a dplyr pipe to determine the number of observations per tree
Orange
Orange %>% 
  group_by(Tree) %>%
  summarize(n=n())

# b. 
# write a dplyr pipe to change the units of age to "years" and circumference to cm
Orange %>%
  mutate(age = age/365, circumference = circumference/10) 

# c. 
# write a dplyr pipe to add a column assigning a z-score to each tree, centered 
# around the mean circumference for all trees at a given age.
Orange %>%
  group_by(age) %>%
  mutate(z = circumference-mean(circumference)/sd(circumference))


# d. 
# write a dplyr pipe to calculate the average rate of grwoth (mm/day) between age 0 and 
# the final measurement across all trees. 
# The earliest age in the data is after planting (after 0). Where necessary, assume the 
# circumference was zero at age 0.
Orange %>%
  group_by(Tree) %>%
  summarize(growth = max(circumference)/max(age))%>%
  summarize(avg_growth = mean(growth))

# e. 
# describe the result of the following pipe
# The final result has one row for each of the observation ages and columns for age, the 
# average rate of growth since the last observation, and the standard error of that average. 
# This allows us to compare growth rates at different points in the life-cycle of the tree. 
Orange %>%
  group_by(Tree) %>%
  mutate(new_growth = c(circumference[1], diff(circumference)),
         elapsed = c(age[1], diff(age))
         ) %>%
  group_by(age) %>%
  summarize(
    avg_rate = mean(new_growth / elapsed),
    se = sd(new_growth / elapsed)/ sqrt(n())
  )
