#!/bin/sh -login
#PBS -o
#PBS -j oe
#PBS -l nodes=1:ppn=1,walltime=4:00:00,mem=10gb
#PBS -M
#PBS -m abe
#PBS -r n
#PBS -N

# -o : tells it where to put output from your job
# -j oe : specifies that output and error messages from your job can be placed in the same location
# -l : resource requests (maximum amounts needed for each)
# -M : email address to send status updates to
# -m abe : what to send email updates about (abort, begin, end)
# -N : names your job
# -r n : tells it not to re-run the script in the case of an error (so it doesn't overwrite any results generated$
# -t 0-? : job numbers for array submission


module load YourProgram

bash yourscript.sh
