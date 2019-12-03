#Annotate original NDC codes with atc_5 code
library(ggplot2)
library(dplyr)
library(stringr)
#Create dictionary with ndc_atc map

dat <- dat<-cbind(code_map[1],code_map[9],code_map[6])
#Count number of NDC codes that did not map to ATC5 codes
uni<-distinct(dat,ndc,.keep_all = TRUE)
empt<-array()
final_map<-uni
for(r in 1:nrow(uni[1]))
  if(is.na(uni[r,2])==TRUE){
    empt<-rbind(empt,uni[r,])
    #uni<-uni[-c(r),]
  }
#Generate final code map

final_map<-na.omit(uni)
ndc_to_atc<-array()
ndc_to_atc=master_source
atcs=list()
in_names=list()
for(n in 1:nrow(master_source[1])){
  i=match(master_source[n,3],final_map[,1])
  atcs=as.character(append(atcs,final_map[i,2]))
  in_names=as.character(append(in_names,final_map[i,3]))
}
ndc_to_atc<-cbind(ndc_to_atc,atcs,in_names)
names(atcs)<-in_names



in_names=list()
count_atc<-as.data.frame(table(ndc_to_atc[,4]))

#count ATC5 codes using final map and annotate with name
distinct_map<-distinct(final_map,atc5,.keep_all = TRUE)
count_atc$in_names="TBD"
for(n in 1:nrow(count_atc[1])){
  i=match(distinct_map[n,2],count_atc[,1])
  count_atc$in_names[i]<-as.character(distinct_map[n,3])
}

#Annotate ATC5 Level 1 group
count_atc$ATC_Level_1_group="TBD"
for(n in 1:nrow(count_atc[1])){
  if(substr(count_atc[n,1],1,1)=="A"){
    count_atc$ATC_Level_1_group[n]<-"Alimentary Tract and Metabolism"
  } else if(substr(count_atc[n,1],1,1)=="B"){
    count_atc$ATC_Level_1_group[n]<-"Blood and Blood forming organs"
  } else if(substr(count_atc[n,1],1,1)=="C"){
    count_atc$ATC_Level_1_group[n]<-"Cardiovascular system"
  } else if(substr(count_atc[n,1],1,1)=="D"){
    count_atc$ATC_Level_1_group[n]<-"Dermatologicals"
  } else if(substr(count_atc[n,1],1,1)=="G"){
    count_atc$ATC_Level_1_group[n]<-"Genito urinary system and sex hormones"
  } else if(substr(count_atc[n,1],1,1)=="H"){
    count_atc$ATC_Level_1_group[n]<-"Systemic hormonal preparations, excluding sex hormones and insulin"
  } else if(substr(count_atc[n,1],1,1)=="J"){
    count_atc$ATC_Level_1_group[n]<-"Antiinfective for systemic use"
  } else if(substr(count_atc[n,1],1,1)=="L"){
    count_atc$ATC_Level_1_group[n]<-" Antineoplastic and immunomodulating agents"
  } else if(substr(count_atc[n,1],1,1)=="M"){
    count_atc$ATC_Level_1_group[n]<-"Musculo-skeletal system"
  } else if(substr(count_atc[n,1],1,1)=="N"){
    count_atc$ATC_Level_1_group[n]<-"Nervous system"
  } else if(substr(count_atc[n,1],1,1)=="P"){
    count_atc$ATC_Level_1_group[n]<-"Antiparasitic products, insecticides and repellents"
  } else if(substr(count_atc[n,1],1,1)=="R"){
    count_atc$ATC_Level_1_group[n]<-"Respiratory system"
  } else if(substr(count_atc[n,1],1,1)=="S"){
    count_atc$ATC_Level_1_group[n]<-"Sensory organs"
  } else if(substr(count_atc[n,1],1,1)=="V"){
    count_atc$ATC_Level_1_group[n]<-"Various"
  }
  
}
#Annotate ATC5 level 2 subgroups

atc_5_subgroups<-as.data.frame(read.csv("atc.csv"))
count_atc$ATC_Level_2_subgroup="TBD"
for(n in 1:nrow(atc_5_subgroups[1])){
#  i=str_length(atc_5_subgroups$Subgroup[n])
  for(v in 1:nrow(count_atc[1])){
    i=str_length(atc_5_subgroups$Subgroup[n])
    x=match(substr(count_atc$Var1[v],1,i),atc_5_subgroups$Subgroup)
    if(is.na(x)==FALSE){count_atc$ATC_Level_2_subgroup[v]<-as.character(atc_5_subgroups$Description[x])}
  }
}
#Annotate ATC5 level 3 class
atc_5_class<-as.data.frame(read.csv("atc_codes.csv"))
count_atc$ATC_Level_3_class="TBD"
for(n in 1:nrow(atc_5_class[1])){
#  i=str_length(atc_5_class$Class[n])
  for(v in 1:nrow(count_atc[1])){
    i=str_length(atc_5_class$Class[n])
    x=match(substr(count_atc$Var1[v],1,i),atc_5_class$Class)
    if(is.na(x)==FALSE){count_atc$ATC_Level_3_class[v]<-as.character(atc_5_class$Description[x])}
  }
}


ndc_to_atc$ATC_level_1<-"TBD"
ndc_to_atc$ATC_level_2<-"TBD"
ndc_to_atc$ATC_level_3<-"TBD"

for(n in 1:nrow(ndc_to_atc[1])){
  x=match(ndc_to_atc$atcs[n],count_atc$Var1)
  ndc_to_atc$ATC_level_1[n]<-as.character(count_atc$ATC_Level_1_group[x])
  ndc_to_atc$ATC_level_2[n]<-as.character(count_atc$ATC_Level_2_subgroup[x])
  ndc_to_atc$ATC_level_3[n]<-as.character(count_atc$ATC_Level_3_class[x])
}
  
  
  
  
#write out count_file
count_outfile <- paste0(out_dir, 'atc_count_file ', curtime(), ' (', exec_label, ').csv')
console('Writing ATC map to file ', count_outfile, '.')
write.csv(count_atc, '~/NDC_to_ATC/ndc_map-master/Output/atc5/count_outfile.csv', row.names = F)
remove(count_outfile)
console('Completed.')
console(nrow(empt),' NDC codes did not map')
write.csv(ndc_to_atc, '~/NDC_to_ATC/ndc_map-master/Output/atc5/ndc_to_atc.csv', row.names = F)
write.csv(final_map, '~/NDC_to_ATC/ndc_map-master/Output/atc5/Supplementary_table_1.csv', row.names = F)
