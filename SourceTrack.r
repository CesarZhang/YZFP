rm(list = ls())

library(dplyr)
library(ggplot2)
library(stringr)

setwd("M://Code/YZFP/SourceTrack/")
# setwd("C:/Users/Cesar/Documents/Code/YZFP/Venn/")
meta<-read.csv("../Result/metadata.tsv",header = T,
               sep = "\t") %>% subset(!Body_Site %in% c("Freshwater"))
meta$Body_Site<-factor(meta$Body_Site,levels = c("Fecal","Rectal","Oral",
                                                 "Blowhole","Skin"))
meta<-meta[order(meta$Body_Site),]

data<-read.table("../Result/mixing_proportions.txt",
                 row.names = 1,header = T,check.names = F)
data<-data[meta$sample.id,]


stde <- function(x) sd(x)/sqrt(length(x))
i<-5
# for(i in 2:ncol(metadata)){
new<-cbind(data,meta[,i,drop=F])
colnames(new)[ncol(new)]<-c("Type")

#mean
meann<-new %>% group_by(Type) %>% summarise_all(list(mean = mean)) %>%
  as.data.frame()
rownames(meann)<-meann$Type
meann<-meann[,2:ncol(meann)]
colnames(meann)<-str_replace_all(colnames(meann),"_mean","")

se<-new %>% group_by(Type) %>% summarise_all(list(se = stde)) %>%
  as.data.frame()
rownames(se)<-se$Type
se<-se[,2:ncol(se)] 
colnames(se)<-str_replace_all(colnames(se),"_se","")

all<-merge(meann,se,by=0,all=T) 
write.csv(all,"st.csv")
n<-ncol(all)
for (k in 1:ncol(meann)) {
  all[,(n+k)]<-paste(100*signif(all[,(k+1)],2),"??",
                     100*signif(all[,(ncol(meann)+k+1)],2),
                     "%",sep="")
}

df<-data.frame()
for (i in 1:ncol(meann)) {
  tmp<-data.frame(Mean = meann[,i],
                  Se= se[,i],
                  Type= rownames(meann), 
                  Source=rep(colnames(meann)[i],nrow(meann)))
  if(i==1){df<-tmp} else {df<-rbind(df,tmp)}
}

df<-subset(df,Source==c("Freshwater"))
df$Type<-factor(df$Type,levels = c("Fecal","Rectal","Oral",
                                                 "Blowhole","Skin"))
cols<-c("#FFB415","#3983AD","#64638E","#A43E55","#419583")

# df$Mean<-100*df$Mean
# df$Se<-100*df$Se
tiff("SourceTrack1.tiff",width = 620,height = 1480)
ggplot(df,aes(x=Type, y=Mean, fill= Type))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Mean-Se, ymax=Mean+Se), width=0.5,
                size=2,
                position=position_dodge(.9))+
  scale_fill_manual(values = cols)+
  theme_bw()+ylab("")+xlab("")+
  # coord_flip()+
  ylim(c(0,0.041))+
  guides(fill=guide_legend(title = ""))+
  theme(legend.position = "none",
      axis.text.x=element_text(size=60,
                               angle = -90, vjust=0.3,hjust = 0),
      axis.text.y=element_text(size=60),
      # axis.ticks.x = element_text(size=60),
      axis.text= element_text(size=50),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.title.y = element_text(size=60),
      axis.title.x = element_text(size=60),
      legend.key.height = unit(4,"line"),
      legend.text = element_text(size = 60),
      legend.title = element_text(size = 60))
dev.off()
