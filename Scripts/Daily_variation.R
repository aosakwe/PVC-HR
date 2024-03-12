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
  pat.stats.total<-PVC_HR_patient_stats(version="Alt",interval=60,is.hourly=F,
                                        use.pc.pvc = F, write.plot = F, day.number = NA)
  
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
  #percent miscategorized
  ind<-match(pat.stats.per.day60$patient_id,pat.stats.total$patient_id)
  pat.stats.per.day60$true_class<-pat.stats.total$class[ind]  
  pat.stats.per.day60$miscatagorized<-pat.stats.per.day60$class != pat.stats.per.day60$true_class
  pc_miss<-mean(pat.stats.per.day60$miscatagorized,na.rm=T)
  paste(60," Percent Misclassified:",round(pc_miss,3))
  
}

##############################################
##### 600 sec intervals ######################
{
  pat.stats.total<-PVC_HR_patient_stats(version="Alt",interval=600,is.hourly=F,
                                        use.pc.pvc = F, write.plot = F, day.number = NA)
  
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
  #percent miscategorized
  ind<-match(pat.stats.per.day600$patient_id,pat.stats.total$patient_id)
  pat.stats.per.day600$true_class<-pat.stats.total$class[ind]  
  pat.stats.per.day600$miscatagorized<-pat.stats.per.day600$class != pat.stats.per.day600$true_class
  pc_miss<-mean(pat.stats.per.day600$miscatagorized,na.rm=T)
  paste(600," Percent Misclassified:",round(pc_miss,3))
}


####################
## hourly

{
  pat.stats.total<-PVC_HR_patient_stats(version="Alt",interval=3600,is.hourly=F,
                                        use.pc.pvc = F, write.plot = F, day.number = NA)
  
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
  #percent miscategorized
  ind<-match(pat.stats.per.day1h$patient_id,pat.stats.total$patient_id)
  pat.stats.per.day1h$true_class<-pat.stats.total$class[ind]  
  pat.stats.per.day1h$miscatagorized<-pat.stats.per.day1h$class != pat.stats.per.day1h$true_class
  pc_miss<-mean(pat.stats.per.day1h$miscatagorized,na.rm=T)
  paste(3600," Percent Misclassified:",round(pc_miss,3))
}