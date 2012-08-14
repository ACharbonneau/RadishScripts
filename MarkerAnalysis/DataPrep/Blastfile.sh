for i in `cut -d ' ' -f 1 SNPmarkers/SNPmarker.snp`
	#do grep $i `bash cut.sh` > BLASTfile.txt
    do grep -i -m 1 $i output.txt >> BLASTfile.txt
done
