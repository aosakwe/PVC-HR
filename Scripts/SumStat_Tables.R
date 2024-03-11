## Table generation

#Daily Variation Tables
int_files <- list.files("./Inputs/Daily_variation/")[c(5,4,2)]
pat.days <- list()
for (file in int_files){
  pat.days[[file]] <- read.csv(paste("./Inputs/Daily_variation/",file,sep = ''),header = T,row.names = 'X')
}


pat.stats <- list()
for (file in int_files){
  pat.day <- read.csv(paste("./Inputs/Daily_variation/",file,sep = ''),header = T,row.names = 'X')
  pat.stat<- matrix(0,nrow = 3,ncol = 7)
  
  pat.stat[1,] <- sapply(1:7,function(a){length(which(pat.day$class[pat.day$day == a] == 'positive'))})
  pat.stat[2,] <- sapply(1:7,function(a){length(which(pat.day$class[pat.day$day == a] == 'negative'))})
  pat.stat[3,] <- sapply(1:7,function(a){length(which(pat.day$class[pat.day$day == a] == 'neutral'))})
  sums <- apply(pat.stat,2,function(a){sum(a)})
  # Replace with proportions
  pat.stat <- apply(pat.stat,2,function(a){round(a/sum(a),2)})
  pat.stat <- rbind(pat.stat,sums)
  colnames(pat.stat) <- paste('Day_',1:7,sep ='')
  rownames(pat.stat) <- c('positive','negative','neutral','Total Recordings')
  pat.stats[[file]] <- pat.stat
  write.csv(pat.stat,paste("./stats_results/Sumstat_",file,sep = ''))
}
#pat.stats <- pat.stats
pat.stats

#Shannon Entropy Table
pat_ent <- sapply(pat.days,function(a){
  pat_ents <- c()
  for (pat in unique(pat.days[[1]]$patient_id)){
    temp <- a[!a$hourly & a$patient_id == pat & a$day %in% 1:7,]

    
    temp <- as.data.frame(table(temp$class))
    temp$p <- temp$Freq/sum(temp$Freq)
    pat_ents <- c(pat_ents,sum(-temp$p*log2(temp$p)))
  }
  pat_ents
})
colnames(pat_ent) <- c('1min','10min','1hr')
rownames(pat_ent) <- unique(pat.days[[1]]$patient_id)
library(pheatmap)
library(grid)

write.csv(pat_ent,"./stats_results/daily_entropy_pvc.csv")
