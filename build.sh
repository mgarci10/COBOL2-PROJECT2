#!/bin/bash
set -e -x
cobc -x GARCIA-P02-SORT.cob
./GARCIA-P02-SORT
cat LNAME-p02-sort.rpt
