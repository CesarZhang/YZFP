rm(list = ls())
# library(reshape2)
library(stringr)
library(dplyr)
library(pheatmap)

setwd("C://Users/Cesar/Documents/Code/YZFP/Heatmap/")


##############
meta<-read.csv("../Result/metadata.tsv",header = T,
               sep = "\t") 
meta$Body_Site<-factor(meta$Body_Site,levels = c("Fecal","Rectal","Oral",
                                                 "Blowhole","Skin","Freshwater"))
meta<-meta[order(meta$Body_Site),]

data<-read.table("../Result/Barplot/L6.tsv",row.names = 1,
                 header = T,check.names = F)
data<-data[,meta$sample.id]


i<-5
new<-cbind(t(data),meta[,i,drop=F])
colnames(new)[ncol(new)]<-c("Type")

#mean
meann<-new %>% group_by(Type) %>% summarise_all(list(mean = mean)) %>%
  as.data.frame()
rownames(meann)<-meann$Type
meann<-meann[,2:ncol(meann)] %>%
  t() %>% as.data.frame()
rownames(meann)<-str_replace_all(rownames(meann),"_mean","")


for (i in unique(meta$Body_Site)[1:5]) {
  tmp<-meann[,c(i,"Freshwater")] 
  tmp<-tmp[tmp[,1]>0 & tmp[,2]>0,]
  
  tmp1<-data[rownames(tmp),
             subset(meta,Body_Site %in% c(i,"Freshwater"))[,1]]
  
  x<-as.data.frame(t(tmp1)) %>%
    cbind(subset(meta,Body_Site %in% c(i,"Freshwater"))[,5])
  nr<-nrow(x)
  pv<-data.frame()
  for (k in 1:(ncol(x)-1)) {
    tmp2<-x[1:nr,c(k,ncol(x))]
    colnames(tmp2)<-c("v","t")
    kt<-wilcox.test(v ~ t, data=tmp2)
    pv[k,1]<-kt$p.value
  }
  rownames(pv)<-colnames(x)[1:(ncol(x)-1)]
  
  tmpf<-merge(tmp,pv,by=0,all=T)
  tmpf$pad<-p.adjust(tmpf$V1)
  test<-subset(tmpf,pad<0.05,Freshwater>Oral)
}


  
  # tiff(filename = "barplot_Genus.tiff",
  #      width = 2800,height = 2200)
  p[[k]]<-ggplot(df,aes(Sample,Value,fill=Taxa))+
    geom_bar(stat="identity",position = "stack")+
    # facet_grid(. ~ Type, drop=F,scale="free",space="free_x")+
    scale_fill_manual(values = cols)+
    theme_classic()+ylab("")+xlab(k)+
    guides(fill=guide_legend(title = "Taxonomy",ncol = 1))+
    theme(axis.text.x=element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text= element_text(size=50),
          axis.title.y = element_text(size=60),
          axis.title.x = element_text(size=60),
          axis.line.y = element_blank(),
          legend.key.height = unit(4,"line"),
          legend.text = element_text(size = 60),
          legend.title = element_text(size = 60),
          strip.text = element_text(size=50))
  # dev.off()
  
}

library(gridExtra)
tiff(filename = "barplot_Genus.tiff",
     width = 4800,height = 3600)
grid.arrange(grobs=p,ncol=2)
dev.off()
