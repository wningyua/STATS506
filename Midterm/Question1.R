# a. 
x = 1:10
result = max(x %% 3L)

# b. 
x = 2*sum(1:100)
y = x > 5e3
result = typeof(c(x, y))

# c. 
result = 10
f = function(input) {
  result = sqrt (input)
  return(result)
}
input = f(result * result)
output = f(input)

# d.
start = 1
goal = 5^2
total = 0
while(total <= goal){
  for (i in 1: start){
    total = total + i
  }
  start = start + 1
}
 result = total
 
 # e.
 x = 1:30
 dim(x) = c(10,3)
 x = x %/% c(1,30)
 result = any(colSums(x) %% 2 == 0)
 
 # f.
library(tidyverse)
n = 100
df = tibble(a = rnorm(n))
df = df %>%
  mutate( b = rbinom(n(), size = 1, prob = pnorm(a))) %>%
  group_by(b) %>%
  summarize( n = n(), a = mean(a))
result = unname(as.numeric(lapply(df, length)))
