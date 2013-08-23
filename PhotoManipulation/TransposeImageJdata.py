#!/usr/bin/env python

import sys

Usage="TransposeImageJdata.py input_file.csv output_file.csv\n\
Takes csv data with many rows of date per individual with one measurment per line,\
and converts it to one row per individual with many measurments.\n"

if len(sys.argv) != 3:
	print Usage

else:
	filenames = sys.argv
	infile = open(filenames[1],'r')
	outfile = open(filenames[2],'w')

unique_names = {}

for line in infile:
	line = line.strip()
	line = line.split(",")
	testname = unique_names.has_key(line[1])
	if testname == False:
		unique_names[line[1]] = [line[2]]
	else:
		unique_names[line[1]] = unique_names[line[1]] + [line[2]]
		
sorted_keys = sorted(unique_names.keys())
		
outfile.write("PlantName,1cm_in_pixels,Length1,Width1,Length2,Width2,Length3,Width3\n")		
for name in sorted_keys:
	N = len(unique_names[name])
	outfile.write("%s," % (name))
	for measurement in range(0,N):
		outfile.write("%s," % (unique_names[name][measurement]))
	outfile.write("\n")


infile.close()
outfile.close()
		
