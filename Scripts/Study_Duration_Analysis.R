source("./Scripts/PVCvsHR_stats.R")
ver <- "Alt"
day1_data <- tibble(patient_id=NA,r=NA,p=NA,B0=NA,B1=NA,sigma=NA,class=NA,interval=NA,version=NA,hourly=NA,pc.pvc_as_resp=NA)
for(int in c(60,600,3600)){
  for (is.hourly in c(F)){ #c(T,F)
    for (use.pc.pvc in c(F)){
      dat<-PVC_HR_patient_stats(ver,int,is.hourly,use.pc.pvc,write.plot = T,day.number = 1)
      day1_data<-rbind(day1_data,dat)#attach produced df to full_data
    }
  }
}
write.csv(day1_data,file="./stats_results/final_day1_Alt_PVCvsHR_stats.csv")
## Final hour interval
# initialise empty dataframe
full_data<-tibble(patient_id=NA,r=NA,p=NA,B0=NA,B1=NA,sigma=NA,class=NA,interval=NA,version=NA,hourly=NA,pc.pvc_as_resp=NA)

int <- 3600

use.pc.pvc <- F
for(int in c(60,600,3600)){
  dat<-PVC_HR_patient_stats(ver,int,is.hourly,use.pc.pvc,write.plot = T)
  full_data<-rbind(full_data,dat)#attach produced df to full_data
}

full_data
write.csv(full_data,file="./stats_results/new_final_Alt_PVCvsHR_stats.csv")


#Generate Plots
library(tidyverse)
library(grid)
library(gtable)
library(patchwork)
library(reshape2)

stats <- read.table("./stats_results/new_final_Alt_PVCvsHR_stats.csv",sep = ',', header = TRUE)
# Alt_PVCvsHR_stats.csv
stats <- stats[2:nrow(stats),2:ncol(stats)]

stats <- stats[stats$interval %in% c('60','600','3600'),]
stats$interval <- as.character(stats$interval)
stats$interval[stats$interval == '3600'] <- '1hr'
stats$interval[stats$interval == '600'] <- '10min'
stats$interval[stats$interval == '60'] <- '1min'
stats$interval <- factor(stats$interval,levels =c('1min','10min','1hr'))
donors <- read.csv("./Inputs/patientIDS_ECG.csv")
#Adding donor with missing holter info 
donors[nrow(donors)+1,] <- c(999,"MC175",NA,NA)
#Convertin patient_id to recordid
stats$record_id <- stats$patient_id
stats <- select(stats,-c("patient_id")) %>%
  merge(donors,by.x = "record_id",by.y = "record_id")




##Subset Alt data
alt_stat <- stats[stats$version == "Alt",]
alt_stat$interval <- as.factor(alt_stat$interval)
alt_stat$class <- stringr::str_to_title(alt_stat$class)

#Get Day 1 data
day1_data <- read.csv("./stats_results/final_day1_Alt_PVCvsHR_stats.csv",row.names = 'X')

day1_data <- day1_data[day1_data$interval %in% c('60','600','3600'),]
day1_data$interval <- as.character(day1_data$interval)
day1_data$interval[day1_data$interval == '3600'] <- '1hr'
day1_data$interval[day1_data$interval == '600'] <- '10min'
day1_data$interval[day1_data$interval == '60'] <- '1min'
day1_data$interval <- factor(day1_data$interval,levels =c('1min','10min','1hr'))
day1_data <- day1_data[-1,] #removing NA row
day1_data$class <- stringr::str_to_title(day1_data$class)




# alt Data
alt_prep <- alt_stat[!alt_stat$pc.pvc_as_resp & !alt_stat$hourly,]
alt_prep$interval <- as.character(alt_prep$interval)
alt_prep$interval[alt_prep$hourly] <- paste("hourly_",alt_prep$interval[alt_prep$hourly], sep = '')
alt_prep$interval <- as.factor(alt_prep$interval)
alt_prep <- alt_prep[,c(1,8,7,10)]


alt_bar_stats <- as.data.frame(table(alt_prep$class,alt_prep$interval))
colnames(alt_bar_stats) <- c("class","interval","Freq")


# Alt Data Proportion plot
alt_bar_stats$prop <- sapply(1:nrow(alt_bar_stats),function(a){
  alt_bar_stats$Freq[a]/sum(alt_bar_stats$Freq[alt_bar_stats$interval == alt_bar_stats$interval[a]])
})
alt_bar_stats$interval <- as.character(alt_bar_stats$interval)
alt_bar_stats$interval <- factor(alt_bar_stats$interval,levels =c('1min','10min','1hr'))#,'hourly_15','hourly_60','hourly_600','hourly_900','hourly_3600','3600'))
#alt_bar_stats <- alt_bar_stats[order(alt_bar_stats$interval),]
alt_bar_plot <- ggplot(alt_bar_stats,aes(x = interval, y = prop, fill = class)) +
  geom_col(position = 'fill') + labs(title = "Week Classification",
                                     x = 'Interval',
                                     y = 'Proportion',
                                     fill = 'Label')  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = c("Negative" = "red", "Neutral" = "gray","Positive" = "blue"))

## 24 hour analysis

day1_prep <- day1_data[!day1_data$pc.pvc_as_resp  & !day1_data$hourly,]
day1_prep$interval <- as.character(day1_prep$interval)
day1_prep$interval[day1_prep$hourly] <- paste("hourly_",day1_prep$interval[day1_prep$hourly], sep = '')
day1_prep$interval <- as.factor(day1_prep$interval)
day1_prep <- day1_prep[,c(1,8,7,10)]


day1_bar_stats <- as.data.frame(table(day1_prep$class,day1_prep$interval))
colnames(day1_bar_stats) <- c("class","interval","Freq")

day1_bar_stats$prop <- sapply(1:nrow(day1_bar_stats),function(a){
  day1_bar_stats$Freq[a]/sum(day1_bar_stats$Freq[day1_bar_stats$interval == day1_bar_stats$interval[a]])
})

day1_bar_stats <- day1_bar_stats[order(day1_bar_stats$interval),]
day1_bar_stats$interval <- as.character(day1_bar_stats$interval)
day1_bar_stats$interval <- factor(day1_bar_stats$interval,levels =c('1min','10min','1hr'))
day1_bar_plot <- ggplot(day1_bar_stats,aes(x = interval, y = prop, fill = class)) +
  geom_col(position = 'fill') + labs(title = '24-Hour Classification',
                                     x = 'Interval',
                                     y = 'Proportion',
                                     fill = 'Label') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = c("Negative" = "red", "Neutral" = "gray","Positive" = "blue"))

day1_bar_plot <- day1_bar_plot +
  theme_classic()   + theme(text = element_text(size = 17),
                            plot.margin = margin(0,0,0,0),
                            plot.title = element_text(hjust = 0.5),
                            plot.subtitle = element_text(size = 12,
                                                         margin=margin(t=40,b=-30),
                                                         hjust = 1,
                                                         vjust = 1),
                            axis.text.x = element_text(size = 15),  # Change x-axis tick label size
                            axis.text.y = element_text(size = 15)) +
  labs(fill = 'Classification')


alt_bar_plot <- alt_bar_plot +
  theme_classic()   + theme(text = element_text(size = 17),
                            plot.margin = margin(0,0,0,0),
                            plot.title = element_text(hjust = 0.5),
                            plot.subtitle = element_text(size = 12,
                                                         margin=margin(t=40,b=-30),
                                                         hjust = 1,
                                                         vjust = 1),
                            axis.text.x = element_text(size = 15),  # Change x-axis tick label size
                            axis.text.y = element_text(size = 15)) +
  labs(y = "") +
  labs(fill = 'Classification')