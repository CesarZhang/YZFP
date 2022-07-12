rm(list = ls())

# setwd("M://Code/YZFP/Beta/")
setwd("C://Users/Cesar/Documents/Code/YZFP/Beta/")
library(vegan)
library(ggplot2)
library(dplyr)

meta<-read.csv("../Result/metadata.tsv",header = T,
               sep = "\t") 
meta$Body_Site<-factor(meta$Body_Site,levels = c("Fecal","Rectal","Oral",
                                                 "Blowhole","Skin","Freshwater"))
meta<-meta[order(meta$Body_Site),]

data<-read.csv("../Result/Beta/distance-matrix.tsv",
               sep = "\t",row.names = 1,header = T,check.names = F)

fr<-subset(meta,Body_Site=="Freshwater")[,1]
df<-data.frame()
for (i in unique(meta$Body_Site)[1:5]) {
  bd<-subset(meta,Body_Site==i)[,1]
  tdata<-data[fr,bd]
  tmp<-data.frame(unlist(tdata,use.names = F))
  tmp[,"Body_Site"]<-rep(i,nrow(tmp))
  df<-rbind(df,tmp)
}
colnames(df)<-c("Value","Type")
#####
stde <- function(x) sd(x)/sqrt(length(x))
meann<-df %>% group_by(Type) %>% summarise_all(list(mean = mean)) %>%
  as.data.frame()
se<-df %>% group_by(Type) %>% summarise_all(list(se = stde)) %>%
  as.data.frame()
all<-merge(meann,se,by="Type",all=T) 
all[,"Ms"]<-paste(signif(all[,"mean"],4),"??",
                  signif(all[,"se"],4))
write.csv(all,"distance-mean.csv")
#####
color_munual<-c("#FFB415","#3983AD","#64638E","#A43E55","#419583")
df$Type<-factor(df$Type,levels = c("Fecal","Rectal","Oral",
                                    "Blowhole","Skin"))


tiff(filename = "Beta-Distance.tiff",
     width = 620,height = 1480)
ggplot(df,aes(x=Type,y=Value,color=Type))+
  geom_boxplot(lwd=2.5)+
  # coord_flip()+
  theme_bw()+
  ylab("")+
  xlab("")+
  theme(legend.position = "none",plot.title = element_text(size = 1))+
  guides(color = guide_legend(nrow = 5))+
  scale_color_manual(values=color_munual)+
  geom_jitter(shape=16,size=3.5 ,position=position_jitter(0.2))+
  theme(title= element_text(size=40),
        axis.text.y = element_text(size = 60),
        axis.text.x = element_text(size = 60,
                                   angle = -90, vjust=0.3,hjust = 0))

dev.off()

