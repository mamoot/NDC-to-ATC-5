library(gganatogram)
library(dplyr)
library(viridis)
library(gridExtra)

mydata <- as_tibble(count_atc)
Ali<-mydata %>% filter(ATC_Level_1_group == "Alimentary Tract and Metabolism")
Blood<-mydata %>% filter(ATC_Level_1_group =="Blood and Blood forming organs")
CV<-mydata %>% filter(ATC_Level_1_group == "Cardiovascular system")
Derm<-mydata %>% filter(ATC_Level_1_group =="Dermatologicals")
GU<-mydata %>% filter(ATC_Level_1_group =="Genito urinary system and sex hormones")
Horm<-mydata %>% filter(ATC_Level_1_group =="Systemic hormonal preparations, excluding sex hormones and insulin")
Abx<-mydata %>% filter(ATC_Level_1_group =="Antiinfective for systemic use")
Neo<-mydata %>% filter(ATC_Level_1_group =="Antineoplastic and immunomodulating agents")
MC<-mydata %>% filter(ATC_Level_1_group =="Musculo-skeletal system")
NS<-mydata %>% filter(ATC_Level_1_group =="Nervous system")
AP<-mydata %>% filter(ATC_Level_1_group =="Antiparasitic products, insecticides and repellents")
RS<-mydata %>% filter(ATC_Level_1_group =="Respiratory system")
SO<-mydata %>% filter(ATC_Level_1_group =="Sensory organs")
VA<-mydata %>% filter(ATC_Level_1_group =="Various")

#totals$Level1<-ATC5_level1$ATC_Level_1_group

totals=cbind.data.frame(c(sum(Ali$Freq),sum(Blood$Freq),sum(CV$Freq),sum(Derm$Freq),sum(GU$Freq),sum(Horm$Freq),
                          sum(Abx$Freq),sum(Neo$Freq),sum(MC$Freq),sum(NS$Freq),sum(AP$Freq),sum(RS$Freq),sum(SO$Freq),
                          sum(VA$Freq)),ATC5_level1$ATC_Level_1_group)
names(totals)<-c("Freq","Level_1")
totals_data<-as_tibble(totals)

top_10<-totals_data %>% arrange(desc(Freq)) %>% slice(1:10) 


organPlot <- data.frame(organ = c("lung", "nerve", "brain", "liver", "stomach", "colon", "pancreas","lymph_node","skin","kidney","urinary_bladder","bone_marrow","skeletal_muscle","heart", "leukocyte"), 
                        type = c("respiratory","circulation", "circulation",  "nervous system", "nervous system", "digestion", "digestion", "digestion","digestion","other","other","other","other","other","other"), 
                        colour = c("blue","red", "red", "purple", "purple", "orange", "orange", "orange","orange","green","yellow","yellow"," red","blue","orange"), 
                        value = c(3,17,17,15,15,15,15,7,6,4,4,4,2,40,40), 
                        stringsAsFactors=F)


gganatogram(data=organPlot, fillOutline='#a6bddb', organism='human', sex='male', fill="value")+
theme_void()+  scale_fill_gradient(low = "white", high = "red")+theme(legend.position="top")


#ATC_classes <- data.frame(read.csv(file="count_outfile_summary.csv", header=TRUE, sep=","))
df=data.frame(group=c("Alimentary Tract and Metabolism","Antiinfectives","Antineoplastic and immunomodulating","	Blood and Blood forming organs","Cardiovascular","Dermatologicals","Genitourinary and sex hormones","Musculo-skeletal systems","	Nervous system","Respiratory System")
              ,value=c(15.5,6.4,2.0,3.6,39.4,6.2,3.8,2.1,16.5,2.7))
top_ten<-data.frame(group=c("Lipid Modifying Agents","Diuretics","Calcium Channel Blockers","Renin-Angiotensin system agents",
                    "Beta blocking agents","Acid-relateted disorders","Diabetic drugs","Analgesics","Corticosteroids","Anxiolytics"),
                    value=c(6.9,4.1,3.6,5.7,3.1,2.1,1.6,1.6,1.6,1.5))

bp<- ggplot(df, aes(x="", y=value, fill=group))+ggtitle("Top 10 ATC Levels (%)")+
  geom_bar(width = 1, stat = "identity")+theme(axis.title=element_blank())


bp
#pie <- bp + coord_polar("y", start=0)+theme_void()
#pie

p <- ggplot(df, aes(x=1, y=value, fill=group)) +
  ggtitle("Top 10 ATC Levels (%)") +
  coord_polar(theta='y')+theme(axis.ticks=element_blank(),  # the axis ticks
                               axis.title=element_blank(),  # the axis labels
                               axis.text.y=element_blank(), # the 0.75, 1.00, 1.25 labels
                               axis.text.x=element_text(color='black'))
p <- p +
  # black border around pie slices
  geom_bar(stat="identity", color='black') +theme(axis.ticks=element_blank(),  # the axis ticks
                                                  axis.title=element_blank(),  # the axis labels
                                                  axis.text.y=element_blank(), # the 0.75, 1.00, 1.25 labels
                                                  axis.text.x=element_text(color='black'),
                                                  ) +
  # remove black diagonal line from legend
  guides(fill=guide_legend(override.aes=list(colour=NA)))+theme_void()+scale_fill_brewer(palette="Set3")

print(p)
bp<- ggplot(top_ten, aes(x="", y=value, fill=group))+ggtitle("Top ten drug classes(%)")+
  geom_bar(width = 1, stat = "identity")+theme(axis.title=element_blank())


bp
#pie <- bp + coord_polar("y", start=0)+theme_void()
#pie

p <- ggplot(top_ten, aes(x=1, y=value, fill=group)) +
  ggtitle("Top ten medications classified by ATC level 3 (%)") +
  coord_polar(theta='y')+theme(axis.ticks=element_blank(),  # the axis ticks
                               axis.title=element_blank(),  # the axis labels
                               axis.text.y=element_blank(), # the 0.75, 1.00, 1.25 labels
                               axis.text.x=element_text(color='black'))
p <- p +
  # black border around pie slices
  geom_bar(stat="identity", color='black') +theme(axis.ticks=element_blank(),  # the axis ticks
                                                  axis.title=element_blank(),  # the axis labels
                                                  axis.text.y=element_blank(), # the 0.75, 1.00, 1.25 labels
                                                  axis.text.x=element_text(color='black'),
  ) +
  # remove black diagonal line from legend
  guides(fill=guide_legend(override.aes=list(colour=NA)))+theme_void()+scale_fill_brewer(palette="Set3")

print(p)

