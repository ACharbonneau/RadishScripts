import sys
import re

sys_input = sys.argv

#sys_input = "best_recip_match.py -b1 PF3D7_yoelii_blast -b2 yoelii_against_PF_blast \
#-sb1 PF_PF_blast -sb2 yoelii_against_yoelii_blast"
#sys_input = sys_input.split()

sysdata={}

try:
    sys_data={sys_input[1]:sys_input[2],
        sys_input[3]:sys_input[4],
        sys_input[5]:sys_input[6],
        sys_input[7]:sys_input[8]}
    blast_S1 = open(sys_data["-sb1"], "r");
    blast_S2 = open(sys_data["-sb2"], "r"); 
    blast_D1 = open(sys_data["-b1"], "r");
    blast_D2 = open(sys_data["-b2"], "r")
     
except IOError:
    print "\nError: File not found, check filenames and directory\n"
    sys.exit(1)

except:
    print "\nUsage: \best_recip_match.py [-b1 crossblastfile1] [-b2 crossblastfile2]\
 [-sb1 selfblastfile1] [-sb2 selfblastfile2]\n"
    print "Files can be input in any order. All files must be in blast tabular output \
format.\nCrossblastfiles are files from two different species blasted against each other.\n\
Selfblastfiles are each species blasted against themselves.\nSelf and Cross blast files \
should be paired by query and number, \ni.e. -b1 and -sb1 should have the same queries.\n\n\
Example:\n\nbest_recip_match.py -b1 PF3D7_yoelii_blast -b2 yoelii_against_PF_blast \
-sb1 PF_PF_blast -sb2 yoelii_against_yoelii_blast\n"
    sys.exit(2) 



def GetRecips(file):
    recip_hit = {}
    for file_line in file:
        file_line = file_line.split()
        try:
            recip_hit[file_line[0]] 
        except:
            recip_hit[file_line[0]]=(file_line[1], file_line[2], file_line[11])  
    return recip_hit
    #file.close()


def GetSelfs(file):
    self_hit = {}
    for file_line in file:
        file_line = file_line.split()
        if file_line[0] != file_line[1]:
            try:
                self_hit[file_line[0]] 
            except:
                self_hit[file_line[0]]=(file_line[1], file_line[2], file_line[11])
    return self_hit
    #file.close()

#def GetOnlySelfs(file):
#    self_hit = {}
#    for file_line in file:
#        file_line = file_line.split()
#        if file_line[0] == file_line[1]:
#            try:
#                self_hit[file_line[0]] 
#            except:
#                self_hit[file_line[0]]=(file_line[1], file_line[2])
#    return self_hit
#    #file.close()
    
def BestRecipHits(key1, dict1, dict2):
    best = []
    for line in key1:
        query1 = line
        sub1 = dict1[query1]
        sub2 = dict2[sub1[0]]
        if query1 == sub2[0]:
            best.append((query1, sub1[0], \
            sub1[1], sub2[1], \
            sub1[2], sub2[2]))
    return best    

def FilterBestRecip(list1, dict1, dict2):
    filtered = []
    singles = []
    self_bit = []
    for line in list1:
        try:
            self_bit.append(dict1[line[0]][2])
            self_bit.append(dict2[line[1]][2])
            sorted(self_bit)
            #print(self_bit)
            cross_bit = line[4], line[5]
            sorted(cross_bit)
            #print(cross_bit)
            if float(cross_bit[0]) > float(self_bit[1]):
                filtered.append(line)
            else:
                None
        except:
            singles.append(line)
    return singles, filtered       
        
        

S1 = GetSelfs(blast_S1)
S2 = GetSelfs(blast_S2)
D1 = GetRecips(blast_D1)
D2 = GetRecips(blast_D2)

D1k = D1.keys()
#D2k = D2.keys()


D1k_best = BestRecipHits(D1k, D1, D2)
#D2k_best = BestRecipHits(D2k, D2, D1)

singles,Filtered_best = FilterBestRecip(D1k_best, S1, S2)

print "\nThere are %i genes with a 1:1 match between your species\n" % (len(Filtered_best))












    
