#! /bin/bash

for i in `ls *_f`
	do grep -A 269 "Inferred ancestry of individuals" $i > $i.forparse
done

for i in `ls *forparse`
	do python /Volumes/Storage/RadishData/RadishScripts/MarkerAnalysis/STRUCTURE/structureparse.py $i
done

mkdir parsed_data

mv *_f.parsed parsed_data/

