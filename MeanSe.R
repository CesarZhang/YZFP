rm(list = ls())
library(reshape2)
library(stringr)
library(dplyr)

setwd("C:/Users/Cesar/Documents/Code/YZFP/MeanSe/")
# setwd("M://Code/IPRS/")

meta<-read.csv("../Result/metadata.tsv",header = T,
               sep = "\t") 
meta$Body_Site<-factor(meta$Body_Site,levels = c("Fecal","Rectal","Oral",
                                                 "Blowhole","Skin","Freshwater"))
meta<-meta[order(meta$Body_Site),]


##############Phylumn
data<-read.table("../Result/Barplot/L2.tsv",row.names = 1,
                 header = T,check.names = F)
data<-data[,meta$sample.id]
data<-data[rowSums(data) >0,]

stde <- function(x) sd(x)/sqrt(length(x))
i<-5
# for(i in 2:ncol(metadata)){
new<-cbind(t(data),meta[,i,drop=F])
colnames(new)[ncol(new)]<-c("Type")

#mean
meann<-new %>% group_by(Type) %>% summarise_all(list(mean = mean)) %>%
  as.data.frame()
rownames(meann)<-meann$Type
meann<-meann[,2:ncol(meann)] %>%
  t() %>% as.data.frame()
rownames(meann)<-str_replace_all(rownames(meann),"_mean","")

#se

se<-new %>% group_by(Type) %>% summarise_all(list(se = stde)) %>%
  as.data.frame()
rownames(se)<-se$Type
se<-se[,2:ncol(se)] %>% 
  t() %>% as.data.frame()
rownames(se)<-str_replace_all(rownames(se),"_se","")


x<-as.data.frame(t(data)) %>%
  cbind(meta[,i,drop=F])
nr<-nrow(x)
pv<-data.frame()
for (i in 1:(ncol(x)-1)) {
  tmp<-x[1:nr,c(i,ncol(x))]
  colnames(tmp)<-c("v","t")
  kt<-kruskal.test(v ~ t, data=tmp)
  pv[i,1]<-kt$p.value
}
rownames(pv)<-colnames(x)[1:(ncol(x)-1)]

###### merge
all<-merge(meann,se,by=0,all=T) %>%
  merge(pv,by.x="Row.names",by.y = 0,all = T)

n<-ncol(all)
for (k in 1:ncol(meann)) {
  all[,(n+k)]<-paste(100*signif(all[,(k+1)],2),"¡À",
                     100*signif(all[,(ncol(meann)+k+1)],2),
                     "%",sep="")
}
n<-unique(meta$Body_Site)
colnames(all)<-c("Taxonomy",
                 paste(n,"_mean",sep = ""),
                 paste(n,"_se",sep = ""),"p",
                 paste(n,"_mean¡Àse",sep = ""))
write.csv(all,"Phy_meanse.csv")

###########Genus
data<-read.table("../Result/Barplot/L6.tsv",row.names = 1,
                 header = T,check.names = F)
data<-data[,meta$sample.id]
data<-data[rowSums(data) >0,]

stde <- function(x) sd(x)/sqrt(length(x))
i<-5
# for(i in 2:ncol(metadata)){
new<-cbind(t(data),meta[,i,drop=F])
colnames(new)[ncol(new)]<-c("Type")

#mean
meann<-new %>% group_by(Type) %>% summarise_all(list(mean = mean)) %>%
  as.data.frame()
rownames(meann)<-meann$Type
meann<-meann[,2:ncol(meann)] %>%
  t() %>% as.data.frame()
rownames(meann)<-str_replace_all(rownames(meann),"_mean","")

#se

se<-new %>% group_by(Type) %>% summarise_all(list(se = stde)) %>%
  as.data.frame()
rownames(se)<-se$Type
se<-se[,2:ncol(se)] %>% 
  t() %>% as.data.frame()
rownames(se)<-str_replace_all(rownames(se),"_se","")


x<-as.data.frame(t(data)) %>%
  cbind(meta[,i,drop=F])
nr<-nrow(x)
pv<-data.frame()
for (i in 1:(ncol(x)-1)) {
  tmp<-x[1:nr,c(i,ncol(x))]
  colnames(tmp)<-c("v","t")
  kt<-kruskal.test(v ~ t, data=tmp)
  pv[i,1]<-kt$p.value
}
rownames(pv)<-colnames(x)[1:(ncol(x)-1)]

###### merge
all<-merge(meann,se,by=0,all=T) %>%
  merge(pv,by.x="Row.names",by.y = 0,all = T)

n<-ncol(all)
for (k in 1:ncol(meann)) {
  all[,(n+k)]<-paste(100*signif(all[,(k+1)],2),"¡À",
                     100*signif(all[,(ncol(meann)+k+1)],2),
                     "%",sep="")
}
n<-unique(meta$Body_Site)
colnames(all)<-c("Taxonomy",
                 paste(n,"_mean",sep = ""),
                 paste(n,"_se",sep = ""),"p",
                 paste(n,"_mean¡Àse",sep = ""))
write.csv(all,"Genus_meanse.csv")
