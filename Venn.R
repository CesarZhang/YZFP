rm(list = ls())

library(stringr)
library(dplyr)
# library(ggplot2)
library(VennDiagram)


# setwd("M://Code/IPRS/Venn/")
setwd("C:/Users/Cesar/Documents/Code/YZFP/Venn/")
meta<-read.csv("../Result/metadata.tsv",header = T,
               sep = "\t") 
meta$Body_Site<-factor(meta$Body_Site,levels = c("Fecal","Rectal","Oral",
                                                 "Blowhole","Skin","Freshwater"))
meta<-meta[order(meta$Body_Site),]

data<-read.table("../Result/OTU/table.tsv",row.names = 1,
                 header = T,check.names = F)
data<-data[,meta$sample.id]


stde <- function(x) sd(x)/sqrt(length(x))
nonzero <- function(x) sum(x != 0)
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


rt<-meann[,colnames(meann) %in% c("Fecal","Rectal","Freshwater")]
list1<-list()
for(i in colnames(rt)){
  rt1<-rt[rt[,i]!=0,]
  # print(head(rownames(rt1)))
  list1[[i]]=rownames(rt1)
}


# color_m<-c("#FFB415","#3983AD","#64638E","#A43E55","#419583","#78767D")
color_m<-c("#FFB415","#3983AD","#78767D")
venn.diagram(list1,
             fill = color_m,
             imagetype = "png",
             height = 2400,width = 2400, resolution = 600,
             compression = "lzw",
             filename = "Bodysite1.png",
             lty="blank",
             cex=1,
             fontfamily="sans",
             # cat.pos=c(0),
             cat.default.pos="outer",
             cat.dist=c(0.05),
             cat.cex=1
)


rt<-meann[,colnames(meann) %in% c("Oral","Blowhole","Skin","Freshwater")]
list1<-list()
for(i in colnames(rt)){
  rt1<-rt[rt[,i]!=0,]
  # print(head(rownames(rt1)))
  list1[[i]]=rownames(rt1)
}


# color_m<-c("#FFB415","#3983AD","#64638E","#A43E55","#419583","#78767D")
color_m<-c("#64638E","#A43E55","#419583","#78767D")
venn.diagram(list1,
             fill = color_m,
             imagetype = "png",
             height = 2400,width = 2400, resolution = 600,
             compression = "lzw",
             filename = "Bodysite2.png",
             lty="blank",
             cex=1,
             fontfamily="sans",
             # cat.pos=c(0),
             cat.default.pos="outer",
             cat.dist=c(0.05),
             cat.cex=1
)
