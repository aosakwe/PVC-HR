#!/bin/bash

if [[ $# != 2 ]]
then
        echo "Error Need to provide path to raw data directory AND length of interval to use in seconds"
        echo "Usage: ./process.sh new_files insert_interval_length_here OR ./process.sh old_files insert_interval_length_here"
        exit 1
fi


files=$(ls ./$1 | grep beats.csv)

echo "Beginning Calculations "
for arg in ${files[@]}
do
        ./alt_HR_Calc.py $1 $arg $2 
	#echo $(ls | grep output'*')
done

mkdir "../Inputs/$2sec_$1_Outputs"

mv output* "../Inputs/$2sec_$1Outputs/"
