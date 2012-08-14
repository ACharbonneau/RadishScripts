#! /bin/bash

#Takes a .txt file of restriction enzyme recognition sequences format 
# AT/GMTA
# TGH/ATC
# ...
#and makes it a file grep can use

cat $* | sed 's/\///g' | sed 's/N/./g' | sed 's/M/[AC]/g' | sed 's/K/[GT]/g' | sed 's/Y/[CT]/g' | sed 's/R/[AG]/g' | sed 's/S/[GC]/g' | sed 's/W/[AT]/g' | sed 's/B/[GCT]/g' | sed 's/D/[ATG]/g' | sed 's/H/[ATC]/g' | sed 's/V/[AGC]/g' > REseq.txt
