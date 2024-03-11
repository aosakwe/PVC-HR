##Code for updated scatter plots as of 23/11/30
library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)
library(patchwork)
library(reshape2)
int_names <- c('15sec','1min','10min','15min','1hr')
names(int_names) <- c('15','60','600','900','3600')

Spec_PVCHR_Stats<-function(use.pc.pvc,day.number=NA,patient_id,
                           x_min = 45,x_max = 125, y_max = 3000){
  dirs <- list.dirs("./Inputs/Alt_files_Processed")
  dirs <- dirs[str_detect(dirs,'Outputs')]
  dirs <- dirs[c(1,4,3,5,2)]
  pat.stats<-tibble(patient_id=NA,r=NA,p=NA,B0=NA,B1=NA,sigma=NA,class=NA,interval=NA,pc.pvc_as_resp=NA)#dataframe the function will output
  plotlist<-list()
  i <- 0
  for (dir in dirs){
    i <- i + 1
    interval <- str_replace(dir,'sec_Alt_files_Outputs','') %>%
      str_replace('./Inputs/Alt_files_Processed/','')
    patient <- list.files(dir)
    patient <- patient[str_detect(patient,patient_id)]
    pat<-read.csv(paste(dir,patient, sep = '/'))
    pat<-pat[,-4]
    colnames(pat)<-c("time","hbi","pvc","IntervalDuration")
    pat$hr<-60/pat$hbi#calculates heart rate in bpm
    pat$pc.pvc<-((pat$pvc)*(60/pat$IntervalDuration))/pat$hr #calculate proportion of pvcs
    
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
      day_range<-c(0,86400)+((day.number-1)*86400)
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
    
    ###############################################
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
    pat.stats[i,]<-list(patient_id,r,p,B0,B1,sigma,cl,interval,use.pc.pvc)
    
    ################
    ### Plotting ## 
    ############### 
    
    #text object (patient ID) to put on plot later
    # text <- grobTree(textGrob(patient_id, x=0,  y=0.95, hjust=0,
    #                           gp=gpar(col="black", fontsize=7.5)))
    
    if(is.valid){
      
      #assign color based on class
      if(ct$p.value>0.05){col_line<-"darkgrey"
      }else if(ct$estimate<0){col_line<-"darkred"
      }else{col_line<-"blue"}
      
      
      
      #we want y axis to be constant among patients, but must be set differtly 
      #for pc.pvc (between 0 and 0.5) compared to pvc (0 to 50 when we have 60 sec 
      #intervals, but must scale with interval size)
     
      ###################################################################
      # Edit for Leon's suggestions
      # ymax<-ifelse(use.pc.pvc,0.6,ifelse(interval == '3600',y_max,
      #                                    50*(as.numeric(interval)/60))) 
      ymax <- y_max
      ###################################################################
      #make points solid in hourly plots, but transparent in regular plots
      alpha<- ifelse(interval %in% c('600','900','3600'),
                     ifelse(interval == '3600',1,0.5),
                     ifelse(is.numeric(day.number),0.5,0.25))
      int_names[interval]
      # Make scatter plot
      sp<-ggplot(data=pat,aes(x=hr,y=resp))+
        geom_point(size=0.5,alpha=alpha)+
        geom_smooth(method=lm,col=col_line)+
        #annotation_custom(text)+
        labs(title = paste(patient_id,' ',int_names[interval],
                           ifelse(is.na(day.number),'Week',day.number),sep = ''),
             subtitle = paste('r = ',round(r,3),'\n','B = ',round(B1,3),sep = '')) +
        xlab('Heart Rate') + ylab('PVC Count') +
        ylim(0,ymax) +
        scale_x_continuous(limits = c(x_min - 2, x_max)) + 
        theme_classic()   + theme(text = element_text(size = 14),
                                  plot.margin = margin(0,0,0,0),
                                  plot.subtitle = element_text(size = 12,
                                                               margin=margin(t=40,b=-30),
                                                               hjust = 1,
                                                               vjust = 1),
                                  axis.text.x = element_text(size = 10),  # Change x-axis tick label size
                                  axis.text.y = element_text(size = 9))
      
    }else{ ##if we had <6 points, make a blank plot
      ymax<-ifelse(use.pc.pvc,0.6,ifelse(interval == '3600',y_max,
                                         50*(as.numeric(interval)/60))) 
      
      sp<-ggplot()+
        geom_blank()+
        annotation_custom(text)+
        ylim(0,ymax)+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()
        )
      
    }
    #add plot to plotlist
    plotlist[[i]]<-sp
    
  }
  return(plotlist) 
}
