library(ggplot2)
library(dplyr)

dat<-(ndc_to_atc)
diseases<-as.data.frame(read.csv("disease_atc5.csv"))

#i="J04A"
dat$condition<-"Undetermined"
for(x in 1:nrow(diseases[1])){
  i=diseases$ATC_5_str[x]
  for(n in 1:nrow(dat[1])){
    if(grepl(i,dat[n,4])==TRUE){
      dat$condition[n]<-as.character(diseases$Disease[x])
    }
  }
}



write.csv(dat, '~/NDC_to_ATC/ndc_map-master/Output/atc5/NDC_to_ATC_with_inferred_conditions.csv', row.names = F)
