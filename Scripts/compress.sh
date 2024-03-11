#!/bin/bash

if [[ $# != 1 ]]
then
        echo "Error: Need to provide command with length of intervals used in seconds"
        echo "Usage: ./compress.sh insert_interval_length_here"
        exit 1
fi

mkdir Output

mv output* Output/

tar -czvf  $1sec_outputs.tar.gz Output/
