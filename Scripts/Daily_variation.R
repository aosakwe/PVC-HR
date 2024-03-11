##
library(tidyverse)
library(grid)
library(gridExtra)
library(plotly)

#setwd("C:/Users/noahw/OneDrive - McGill University/ecg_project")

path <-"./Inputs/"
source("./Scripts/PVCvsHR_stats.R")
#source()


################################################
#### 60 sec intervals ##########################

{

  pat.stats.per.day60<-PVC_HR_patient_stats(version="Alt",interval=60,is.hourly=F,
                                          use.pc.pvc = F, write.plot = F, day.number = 1)
  pat.stats.per.day60$day<-1
  
  for(d in 2:9){
    day<-PVC_HR_patient_stats(version="Alt",interval=60,is.hourly=F,
                              use.pc.pvc = F, write.plot = F, day.number = d)
    day$day<-d
    pat.stats.per.day60<-rbind(pat.stats.per.day60,day)
  }
  
  pat.stats.per.day60<-pat.stats.per.day60[order(pat.stats.per.day60$patient_id,pat.stats.per.day60$day),]
  
  write.csv(pat.stats.per.day60,"Inputs/Daily_variation/Daily_patient_stats_60s.csv")

  
}

##############################################
##### 600 sec intervals ######################
{
  
  
  pat.stats.per.day600<-PVC_HR_patient_stats(version="Alt",interval=600,is.hourly=F,
                                             use.pc.pvc = F, write.plot = F, day.number = 1)
  pat.stats.per.day600$day<-1
  
  for(d in 2:9){
    day<-PVC_HR_patient_stats(version="Alt",interval=600,is.hourly=F,
                              use.pc.pvc = F, write.plot = F, day.number = d)
    day$day<-d
    pat.stats.per.day600<-rbind(pat.stats.per.day600,day)
  }
  
  pat.stats.per.day600<-pat.stats.per.day600[order(pat.stats.per.day600$patient_id,pat.stats.per.day600$day),]
  
  write.csv(pat.stats.per.day600,"Inputs/Daily_variation/Daily_patient_stats_600s.csv")
 
}


####################
## hourly

{
  pat.stats.per.day1h<-PVC_HR_patient_stats(version="Alt",interval=3600,is.hourly=F,
                                            use.pc.pvc = F, write.plot = F, day.number = 1)
  pat.stats.per.day1h$day<-1
  
  for(d in 2:9){
    day<-PVC_HR_patient_stats(version="Alt",interval=3600,is.hourly=F,
                              use.pc.pvc = F, write.plot = F, day.number = d)
    day$day<-d
    pat.stats.per.day1h<-rbind(pat.stats.per.day1h,day)
  }
  
  pat.stats.per.day1h<-pat.stats.per.day1h[order(pat.stats.per.day1h$patient_id,pat.stats.per.day1h$day),]
  
  write.csv(pat.stats.per.day1h,"Inputs/Daily_variation/Daily_patient_stats_1hs.csv")
  
}