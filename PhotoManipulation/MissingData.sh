#looks for files that exist in some directory, but are not listed in a file.
#Good for checking if you missed recording data from a large set of files
if [ ! $2 ]
then
    echo "Usage: directory, filelist"
    exit 1
fi

dir=$1
list=$2

for file in `ls $dir`
do 
    cat $list | grep $file > /dev/null 2>&1 || echo "$file"
done > dead$list
