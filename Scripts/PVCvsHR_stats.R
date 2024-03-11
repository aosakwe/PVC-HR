
###This script takes Processed patient interval data and hourly average data and produces a dataframe with 
## PVC-HR correlation (r), p-value and other parameters for each patient and each combination of different intervals, new vs old, 
# regular vs hourly avg and pvc vs pc.pvc (proportion of beats that are PVCs)
#it also write figures to file showing scatter plots for all patients for all combinations of old/new, intervals and hourly vs regular

##NOT DONE don't alter or use

library(tidyverse)
library(grid)
library(gridExtra)

##Imput: your path to the ecg project folder:

#path<-"C:/Users/noahw/OneDrive - McGill University/ecg_project/Inputs/"
path <- "./Inputs/"
##function that takes a combination of imput parameters (interval, old/new, hourly/reg) and outputs dataframe of correlation stats of 
## and writes a figure to file

#imputs: version (old/new), interval (15,60,600,900),is.hourly (logical), 
#use.pc.pvc (logical): look at pvc freq or pvc proportion?
# day.number: limits data to one 24hr period of the recording

PVC_HR_patient_stats<-function(version,interval,is.hourly,use.pc.pvc,write.plot,day.number=NA){
  
  
  
  ##get filepath to folder with patient data matching the given parameters: different folder for hourly data
  if (is.hourly){
    filepath<-paste(path,"hourly_avgs/",version,"_files_Processed/",interval,"sec_",version,"_files",sep="")
  }else{
  filepath<-paste(path,version,"_files_Processed/",interval,"sec_",version,"_files_Outputs",sep="")
  } 
 
  #get list of patient files within the folder 
  filenames<-list.files(filepath,full.names = T)
  
 
  #initialise empty dataframe and plot list
  pat.stats<-tibble(patient_id=NA,r=NA,p=NA,B0=NA,B1=NA,sigma=NA,class=NA,interval=NA,version=NA,hourly=NA,pc.pvc_as_resp=NA)#dataframe the function will output
  plotlist<-list()
  
  #fill dataframe (one row per patient)
  for(i in 1:length(filenames)){
    
    ##################
    ## statistics ###
    ##################
    
    pat<-read.csv(filenames[i])#extracts single patient's data
    
    #Makes the hourly and regular dataframes have the same format
    if(is.hourly){
      pat.id<-str_replace(filenames[i],paste(filepath,"/",sep = ''),"") %>%
        str_replace(pattern =  paste("_hourly_avg_Alt",interval,".csv",sep = ''),
                    replacement = '')
      
      pat<-pat[,-1]
    }else{
      pat.id<- str_replace(filenames[i],paste(filepath,"/",sep = ''),"") %>%
        str_replace(pattern = "_beats.csv",
                    replacement = '')  %>%
        str_replace(pattern = "output",
                    replacement = '')
        #str_sub(filenames[i], -16, -11)
      pat<-pat[,-4]
      if (version == "Alt"){
        colnames(pat)<-c("time","hbi","pvc","IntervalDuration")
        pat$hr<-60/pat$hbi#calculates heart rate in bpm
        pat$pc.pvc<-((pat$pvc)*(60/pat$IntervalDuration))/pat$hr #calculate proportion of pvcs
      }else {
      colnames(pat)<-c("time","hbi","pvc")#renames columns
      pat$hr<-60/pat$hbi#calculates heart rate in bpm
      pat$pc.pvc<-((pat$pvc)*(60/interval))/pat$hr #calculate proportion of pvcs
      }
      
    }  
    
    #Make either pc.pvc or pvc the response variable:
    if(use.pc.pvc){
      #colnames(pat)[5]<-"resp"
      colnames(pat)[which(colnames(pat) == 'pc.pvc')] <- 'resp'
      #resp <- 'pc.pvc'
    }else{
      colnames(pat)[which(colnames(pat) == 'pvc')] <- 'resp'
    }
    
    
    #limit data to 24 hour window if day.number is specified
    if(!is.na(day.number)){
      tot_range<-c(min(pat$time),max(pat$time))
      if(!is.hourly){
            day_range<-c(0,86400)+((day.number-1)*86400)
      }else{
            day_range<-c(1,24)+((day.number-1)*24) 
      }                  
      pat<-pat[which(pat$time > day_range[1] & pat$time < day_range[2]),]
    }
    
    
    
    
    
    
    
    #logical variable to only allow stats calculation if we have min 6 datapoints 
    # and at least 3 datapoints with PVCs 
    is.valid<- ifelse('pvc' %in% colnames(pat),
                      (nrow(pat)>5) & (sum(pat$pvc>0,na.rm=T) > 3),
                      (nrow(pat)>5) & (sum(pat$resp>0,na.rm=T) > 3))
    
    ###############################################
    ## Modification to have PVCs/hour as requested by Leon
    if (interval == '60'){
      pat$resp <- pat$resp*60
    } else if (interval == '600'){
      pat$resp <- pat$resp*6
    } else {
      pat$resp <- pat$resp
    }
    
    #correlation test/regression (only if we have >6 datapoints)
    if(is.valid){
      ct<-cor.test(pat$hr,pat$resp)#correlation test (hr vs response var)
      rg<-lm(resp~hr,data=pat)#linear regression
      
      r<-ct$estimate #correlation coeffient
      p<-ct$p.value #p-value
      B0<-rg$coefficients[1] #regression intercept
      B1<-rg$coefficients[2] #regression slope
      sigma<-sigma(rg) #regression sd?
      ##evaluates class of each patient: pos, neg or neither
      ##depending on if r is significantly pos or neg
      if(p>0.05){cl<-"neutral"
      }else if(r<0){cl<-"negative"
      }else{cl<-"positive"}
      
    }else{ ##if <6 points fill in NA for all parameters
      r<-NA
      p<-NA
      B0<-NA
      B1<-NA
      sigma<-NA
      cl<-NA
      
    }
    #Fill in row of pat.stats
    pat.stats[i,]<-list(pat.id,r,p,B0,B1,sigma,cl,interval,version,is.hourly,use.pc.pvc)
    
    

  }
  return(pat.stats)  
    
}
### FUNCTION ENDS ####
######################
