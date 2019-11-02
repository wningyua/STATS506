## Question 2 
## Regular Expression

# a. consider the function below and write the result 
is_syn = function(x){
  grepl('^([A-Za-z]|[.][._A-Za-z])[._A-Za-z0-9]*$', x) # . is the actual period here, not any character
}
  
# i
is_syn('.2way')
# ii
is_syn('.twoway')
# iii
is_syn('2way')
# iv-
is_syn('._2way')

# c.
# write a call to grep that will find all lines in .R or .Rmd files in the local
# directory that use the functions gather or spread
grep -n "spread(\|gather(" *.{R,Rmd}
