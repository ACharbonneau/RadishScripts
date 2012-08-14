#! /bin/bash

#Needs .txt file called REseq with cleaned RE recognition sites, one per 
#line. Made with REclean.sh
#This program may introduce a few spurious RE sites

#Make sequence search file: Take all of the sequences from a FASTA file, 
#concatanate them together into a single line
grep -v ">" $* > temp.txt
cat temp.txt | perl -pe 's/\n//' > $*.txt
cat $*.txt > /dev/null

#Search the concatanated sequence for all instances of each restriction 
#enzyme recongnition site, return the number of times each is found to a 
#.txt document in same order as input

for i in `cat REseq.txt`
    do grep -o $i $*.txt | wc -l
    done > $*REcount.txt

rm temp.txt
