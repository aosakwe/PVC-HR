#!/bin/python3
import csv
import sys

time_frame = int(sys.argv[3])
##initial conditions for iterations
valid = False
intervals = []
start = None
pvc = 0 

with open(sys.argv[1] + '/' + sys.argv[2], 'r') as read_obj, open("output" + '' + sys.argv[2], 'w', newline="") as output:
    csv_reader = csv.reader(read_obj)
    csv_writer = csv.writer(output)
    csv_writer.writerow(["Time (s)","Mean Heartbeat Interval", "PVC Frequency", "All Intervals","Interval Duration (s)"])
    for row in csv_reader:   
        if valid:
            if row[1] == 'N' or row[1] == 'V':
                if row[1] == 'V':
                    pvc += 1
                
                intervals.append(float(row[0]) - float(prev_time))
                
                #Current setup is to end on an N/V beat
                if float(row[0]) - float(start) >= time_frame:
                    csv_writer.writerow([start,sum(intervals)/len(intervals),pvc,intervals,float(row[0]) - float(start)])
                    valid = False
                    intervals = []
                    start = None
                    pvc = 0 
            else:
                valid = False #continue search in interval
      
        elif row[1] == 'N' or row[1] == 'V':
            valid = True
            if len(intervals) == 0: #if there is no first beat, set the start to current beat
                start = row[0]
            if row[1] == 'V':
                pvc += 1
            
        else:
            valid = False #continue search in interval
            
        prev_time = row[0]
    print("output" + '' + sys.argv[2] + " complete")    
        
            
