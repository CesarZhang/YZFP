setwd("C:/Users/Cesar/Documents/Code/YZFP/PERMANOVA/")
# setwd("M:/Code/IPRS/PERMANOVA/")
rm(list = ls())
library(vegan)
library(dplyr)
meta<-read.csv("../Result/metadata.tsv",header = T,
               sep = "\t") 
# %>% subset(!Body_Site %in% c("Blowhole"))
meta$Body_Site<-factor(meta$Body_Site,levels = c("Fecal","Rectal","Oral",
                                                 "Blowhole","Skin","Freshwater"))
meta<-meta[order(meta$Body_Site),]


data<-read.csv("../Result/Beta/distance-matrix.tsv",
               sep = "\t",row.names = 1,header = T,check.names = F)
data<-data[meta$sample.id,meta$sample.id]

#########Body Site
for(i in c(5)){
  tmeta<-subset(meta,!Body_Site %in% c("Freshwater"))
  tdata<-data[tmeta$sample.id,tmeta$sample.id]
  sink(paste(colnames(tmeta)[i],".txt",sep = ""))  
  print(adonis2(tdata ~ tmeta[,i],data = meta,permutations = 999))
  sink()
}


#########Indivial
for(i in c(2)){
  tmeta<-subset(meta,!Animal_O %in% c("Freshwater"))
  tdata<-data[tmeta$sample.id,tmeta$sample.id]
  sink(paste(colnames(tmeta)[i],".txt",sep = ""))  
  print(adonis2(tdata ~ tmeta[,i],data = meta,permutations = 999))
  sink()
}

###### between two type
type<-unique(meta$Body_Site)
for(i in 1:(length(type)-1)){
  for (k in (i+1):length(type)) {
  tmeta<-subset(meta,Body_Site %in% type[c(i,k)])
  tdata<-data[tmeta$sample.id,tmeta$sample.id]
  sink(paste(type[i],"-",type[k],".txt",sep = ""))  
  print(adonis2(tdata ~ tmeta[,"Body_Site"],data = tmeta,permutations = 999))
  sink()
  } 
}

########
type<-unique(meta$Body_Site)[c(1:3,5)]
for(i in 1:length(type)){
  for (k in c(3,4,7)) {
    tmeta<-subset(meta,Body_Site %in% type[i])
    tdata<-data[tmeta$sample.id,tmeta$sample.id]
    sink(paste(type[i],"-",colnames(tmeta)[k],".txt",sep = ""))  
    print(adonis2(tdata ~ tmeta[,k],data = tmeta,permutations = 999))
    sink()
  } 
}

