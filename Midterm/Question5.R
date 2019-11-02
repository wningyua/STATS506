## Question 5
## S3 and S4 Methods in R
## define a new S3 generic about for summarizing basic information about an R
## object in a compact fashion.

# a. define a new S3 generic about.. Use ... in the function template
about = function(x, ...){
  UseMethod('about')
}
  
# b. define a default method for the about generic Your method should call 
# the str generic with the option give. attr = FALSE
about.default = function(x){
  str(x, given.attr = FALSE)
}
  
# c. 
# define an about method for objects of class data.frame that prints
# the following information to the console
# i. A string, "'data.frame': NN obs of PP variables:" with NN and PP replaced
#    with appropriate numbers. This should be its own line. If the object being
#    summarized is a member of one or more data.frame sub-classes, replace "data.frame"
#    with the name of the primary class
# ii. A string, " PP numeric variables: [V1, V2, V3, ...]" giving the numbers
#     of numeric columns (PP), and listing the names (V1, V2, ...) as a comma 
#     seperate list.
# iii. A string, "PP factor variables [V1, V2, V3, ...]" giving the numbers of 
#     factor and/or character columns(PP), and listing the names (V1, V2, ...)
#     as a comma separated list
about.data.frame = function(x){
  stopifnot('data.frame' %in% class(x)) # the input should be in the class of
  # dataframe
  P_num = dplyr::select_if(x, is.numeric) # to get numeric variables 
  P_fac = dplyr::select_if(x, is.factor) # to get factors
  l1 = paste0("'", class(x)[1], "': ", nrow(x), " obs of ", ncol(x), " variables") 
  # l1: a string line, “‘data.frame’: NN obs of PP variables:”
  l2 = paste0(ncol(P_num), " numeric variable(s): ", paste(names(P_num), collapse = ", ")) 
  # l2: A string line " PP numeric variables: [V1, V2, V3, ...]"
  l3 = paste0(ncol(P_fac), " categorical variable(s) ", paste(names(P_fac), collapse = ", ") ) 
  # l3: a string line " PP factor variables: [V1, V2, V3, ...]"
  cat(c(l1, l2, l3), sep  = "\n") #return several 3 lines with above information
}


# d.
# define an about method for objects of class tbl (a tibble, inheriting from 
# the data.frame class) that calls the data.frame method above and then also prints,
# when applicable, the number and names of any "list" columns
about.tbl = function(x)  {
  stopifnot('tbl' %in% class(x)) # the input should be in the class of tbl
  NextMethod()
  P_list = dplyr::select_if(x, is.list) # to get list columns
  l4 = paste0(ncol(P_list), " list column(s): ", paste(names(P_list), collapse = ", "))
  # l4 is a string line with number and names of list columns
  cat(l4, sep = "\n") 
  # the function return four lines with above information
}
