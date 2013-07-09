#! /usr/bin/env python

import sys

#input_files = sys.argv

input_files = "imageJ2xcel.py CombinedMeasures.csv 7_26_12photos.csv"
input_files = input_files.split()

try:
	imagej = open(input_files[1], "r")
	fixedimage = open(input_files[2], "w")

except:
	print "Usage: input_file output_file"
	sys.exit(1)

fixed_dic = {}
	
for photo in imagej:
	photo = photo.strip()
	photo = photo.split(",")
	#Get looks for the first thing and if it doesn't find it, returns the second thing as a default
	x = fixed_dic.get(photo[1], [])
	x.append(photo[2])
	fixed_dic[photo[1]] = x
	
fixed_keys = fixed_dic.keys()

for individual in fixed_keys:
	fixedimage.write("%s,%f,%f,%f,%f,%f,%f\n" % \
	(individual, float(fixed_dic[individual][0]), 
	float(fixed_dic[individual][1]), float(fixed_dic[individual][2]),\
	float(fixed_dic[individual][3]), float(fixed_dic[individual][4]),\
	float(fixed_dic[individual][5]), float(fixed_dic[individual][6])))