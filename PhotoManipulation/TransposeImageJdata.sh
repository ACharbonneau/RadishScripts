name='none'

while read line
do
    newname=$(/bin/echo $line | awk '{print $1}')
    x=$(/bin/echo $line | awk '{print $2}')
        if [ $newname == $name ]
            then
            /bin/echo -n " $x" >> $*out.txt
        else 
            name=$newname
            /bin/echo "" >> $*out.txt
            /bin/echo -n "$name $x" >> $*out.txt
        fi
done <$*
