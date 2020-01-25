#!/bin/bash

# arg check
if [ $# -lt 1 ]; then
   echo "Missing input filename"
   exit 1
fi

# file read
if [ ! -f $1 ]
then
  echo "File not found."
  exit 1
fi
exp=`cat $1`

# setup dependancies
ghc parse.hs > /dev/null 2>&1

echo "$exp = "

./parse $exp 



# clean up
rm *.hi *.o *.tmp > /dev/null 2>&1