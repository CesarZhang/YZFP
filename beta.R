rm(list = ls())

setwd("M://Code/YZFP/Beta/")
# setwd("C://Users/Cesar/Documents/Code/YZFP/Barplot/")
library(vegan)
library(ggplot2)

meta<-read.csv("../Result/metadata.tsv",header = T,
               sep = "\t") 
meta$Body_Site<-factor(meta$Body_Site,levels = c("Fecal","Rectal","Oral",
                                                 "Blowhole","Skin","Freshwater"))
meta<-meta[order(meta$Body_Site),]

data<-read.csv("../Result/Beta/distance-matrix.tsv",
                sep = "\t",row.names = 1,header = T,check.names = F)
data<-data[meta$sample.id,meta$sample.id]

ord<-metaMDS(data)

NMDS<-list()
NMDS[[1]]<-data.frame(x=ord$points[,1],y=ord$points[,2], 
                      meta1= as.factor(meta[,"Body_Site"]))

#NMDS[[i]][,3]<-factor(NMDS[[i]][,3],levels = c("Chuff","Oral","Fecal","Rectal","Skin","Freshwater"))
colnames(NMDS[[1]])<-c("x","y",colnames(meta)[3])
color_munual<-c("#FFB415","#3983AD","#64638E","#A43E55","#419583","#78767D")

tiff(filename = paste(colnames(meta)[5], ".tiff",sep = ""),
    width = 2048,height = 2048)
print(ggplot(data = NMDS[[1]],aes(x,y,col=NMDS[[1]][,3]))+
        geom_point(size=30)+
        scale_color_manual(values=color_munual)+
        theme_light()+
        guides(color = guide_legend(""))+
        theme(title= element_text(size=50),
              axis.text= element_text(size=50),
              legend.text = element_text(size = 50))+
        
        labs(x="NMDS1",y="NMDS2"))
dev.off()

############
#Different habitat
meta<-read.csv("../Result/metadata1.tsv",header = T,
               sep = "\t") 
meta$Body_Site<-factor(meta$Body_Site,levels = c("Fecal","Rectal","Oral",
                                                 "Blowhole","Skin","Freshwater"))
meta<-meta[order(meta$Body_Site),]

data<-read.csv("../Result/Beta/distance-matrix.tsv",
               sep = "\t",row.names = 1,header = T,check.names = F)
data<-data[meta$sample.id,meta$sample.id]

meta$State<-factor(meta$State,levels = c("Semi-natural","Natural"))
meta<-subset(meta,Body_Site %in% c("Fecal","Rectal","Oral","Skin"))

data<-data[meta$sample.id,meta$sample.id]

ord<-metaMDS(data)
NMDS<-list()
NMDS[[1]]<-data.frame(x=ord$points[,1],y=ord$points[,2], 
                      meta1= as.factor(meta[,"Body_Site"]),
                      meta2=as.factor(meta[,"State"]))

colnames(NMDS[[1]])<-c("x","y",colnames(meta)[5],colnames(meta)[4])
color_munual<-c("#FFB415","#3983AD","#64638E","#419583")
shape_value<-c(15,17)
tiff(filename = paste("State1", ".tiff",sep = ""),
     width = 2048,height = 2048)
print(ggplot(data = NMDS[[1]],aes(x,y,col=NMDS[[1]][,3]))+
        geom_point(aes(shape=NMDS[[1]][,4]),size=30)+
        scale_color_manual(values=color_munual)+
        scale_shape_manual(values =shape_value)+
        theme_light()+
        guides(color = guide_legend(""),shape=guide_legend(""))+
        theme(title= element_text(size=50),
              axis.text= element_text(size=50),
              legend.text = element_text(size = 50))+
        
        labs(x="NMDS1",y="NMDS2"))
dev.off()
