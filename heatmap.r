rm(list = ls())
# library(reshape2)
library(stringr)
# library(ggplot2)
library(pheatmap)
library(dplyr)

setwd("~/Code/YZFP/Picrust/")


##############
meta<-read.csv("../Result/metadata.tsv",header = T,
               sep = "\t") 
meta$Body_Site<-factor(meta$Body_Site,levels = c("Fecal","Rectal","Oral",
                                                 "Blowhole","Skin","Freshwater"))
meta<-meta[order(meta$Body_Site),] %>% subset(Body_Site %in% c("Fecal","Rectal","Oral",
                                                               "Blowhole","Skin"))

data<-read.table("../Result/Picrust/test.txt",
                 row.names = 2,header = T,
                 check.names = F,sep = "\t")[,meta$sample.id]

####relative
x<-t(data)
x<-x/rowSums(x)
data<-as.data.frame(t(x))

x<-as.data.frame(t(data)) %>%
  cbind(meta[,5,drop=F])
nr<-nrow(x)
pv<-data.frame()
for (i in 1:(ncol(x)-1)) {
  tmp<-x[1:nr,c(i,ncol(x))]
  colnames(tmp)<-c("v","t")
  kt<-kruskal.test(v ~ t, data=tmp)
  pv[i,1]<-kt$p.value
}
rownames(pv)<-colnames(x)[1:(ncol(x)-1)]

tmp<-merge(data,pv,by=0,all=T)
tmp$pad<-p.adjust(tmp$V1,"bonferroni")
# test<-subset(tmp,V1<0.05)
test<-subset(tmp,pad<0.05)


rownames(test)<-test$Row.names
test<-test[,meta$sample.id]
# 
x<-test[order(rowSums(test),decreasing = T),][1:50,]
# 
# 
# dd<-rowSums(test)
# dd<-rowSums(test)/56
# ddf<-cbind(test,dd)
# ddff<-subset(ddf,dd>0.56)[,meta$sample.id]

data1<-data[rownames(x),meta$sample.id]
nname<-rownames(data1)
rownames(data1)<-c(1:nrow(data1))
i<-5
# for(i in 2:ncol(metadata)){
new<-cbind(t(data1),meta[,i,drop=F])
colnames(new)[ncol(new)]<-c("Type")

#mean
meann<-new %>% group_by(Type) %>% summarise_all(list(mean = mean)) %>%
  as.data.frame()
rownames(meann)<-meann$Type
meann<-meann[,2:ncol(meann)] %>%
  t() %>% as.data.frame()
rownames(meann)<-str_replace_all(rownames(meann),"_mean","")
rownames(meann)<-nname



tiff("heatmap2.tiff",width = 1980,height = 3600)
pheatmap(meann,
         # colorRampPalette(c("white","grey50","grey30","grey10","grey1","black"))(250) ,
         show_rownames = T,show_colnames = T,
         cluster_rows = T,cluster_cols = F,
         scale = "row",
         # clustering_distance_rows = "binary",
         # clustering_method = "average",
         treeheight_row = 80,
         treeheight_col = 40,
         legend = T,
         border_color = "white",
         #kmeans_k = 30,
         legend_breaks = c(1.5,0.5,-0.5,-1.5),
         # treeheight_col = 0,
         # annotation_col = map,
         # annotation_names_col = T,
         # labels_row = c(1:50),
         cellwidth=50, cellheight=50,
         fontsize_row = 30,
         fontsize_col = 50,
         fontsize = 30)
dev.off()







