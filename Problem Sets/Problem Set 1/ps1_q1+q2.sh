# Question 1
#!/bin/env bash
# Stats 506, Fall 2019
# This script contains solutions for
# problem set 1, question 1.
#
# Author: Ningyuan Wang
# Updated: Sep 23, 2019
#
# a. download data if not exist
file ="recs2015_public_v4.csv"
url="https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv"

if [ ! "$file" ]; then
   wget $url
fi

# b. extract header row and output to a file with one name per line
new_file="recs_names.txt"

# delete the file if it is present
if [ -f "$file" ]; then
   rm "$new_file"
fi

< $file head -n1 | tr , \\n > "$new_file"


#b. alternative (by NW)
< recs2015_public_v4 head -1 | tr "," "\n" > recs_names.txt

# c. get column numbers for DOEID and the brr weights
# as a comma seperated string
cols=$(
	< $new_file
	grep -n -E "DOEID|BRR" |
	cut -f1 -d: |
	paste -s -d, -
	)

#c. alternative (by NW)
cols=$(grep -En 'DOEID|^BRRWT' recs_names.txt | awk '{print $1}' FS=":" |paste -s -d, -)

# d. cut out the appropriate columns
<"$file" cut -f"$cols" -d, > recs_weights.csv

#d. alternative (by NW)
<recs2015_public_v4 cut -d, -f$echo ${cols} > recs_weights
#--------------------------------------------------------------------------------------------------------
# Question 2
#!/usr/bin/env bash
# Stats 506, Fall 2019
# This script contains solutions for problem set 1, question 2.

# Author: Ningyuan Wang
# Date: Sep 23, 2019

# Our solution is a command line utility for
# extracting columns from a csv file by name.

# Usage: ./cutnames.sh file expression

# Note: to make the script executable, use
# chmod +x ./cutnames.sh
# Otherwise, call as bash ./cutnames.sh

# First argument is the file we are extracting columns from
file=$1

# Second argument is an (extended) regular expression
# for the column names we want.
expr=$2

# Here is where we do the work
if [! -f "$file" ]; then
 #This line echos to stderr rather than stdout
  echo "Could not find file $file."
else
    #get column numbers whose names match expr
    cols=$(
    <"$file" head -n1 |
    tr , \\n |
    grep -n -E "$expr" |
    cut -f1 -d: |
    paste -s -d, -
    )
    
    #cut those columns out
    <"$file" cut -f"$cols" -d,
fi
