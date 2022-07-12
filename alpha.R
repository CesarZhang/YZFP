############
# install.packages("ggpubr")
library(ggpubr)
library(dplyr)
rm(list = ls())
setwd("C:/Users/Cesar/Documents/Code/YZFP/Alpha/")
# setwd("M:/Code/IPRS/Alpha/")
############
meta<-read.csv("../Result/metadata.tsv",header = T,
               sep = "\t") 
# %>% subset(!Body_Site=="Freshwater")
meta$Body_Site<-factor(meta$Body_Site,levels = c("Fecal","Rectal","Oral",
                                                 "Blowhole","Skin","Freshwater"))
meta<-meta[order(meta$Body_Site),]


aldn<-c("observed_features","shannon","faith_pd")
ald<-data.frame()
for (i in 1:length(aldn)) {
  path<-"C:/Users/Cesar/Documents/Code/YZFP/Result/Alpha/"
  if (i == 1) {
   ald<-read.csv(paste(path,aldn[i],"/alpha-diversity.tsv",sep = ""),
                   sep = "\t")
   } else {
   tmp<-read.csv(paste(path,aldn[i],"/alpha-diversity.tsv",sep = ""),
                sep = "\t")
   ald<-merge(ald,tmp,by.x = "X")
   }
  print(aldn[i])
}

df<-merge(subset(ald,X %in% meta$sample.id),meta[,c("sample.id","Body_Site")],
          by.x="X",by.y = "sample.id")

colnames(df)<-c("ID","observed_features","shannon","faith_pd","Type")
  
  
df1<-NULL
for (i in (3:dim(df)[2])-1){
  tmp<-data.frame(row.names=NULL,Sample=df[,"ID"],
                  Index=rep(colnames(df)[i],dim(df)[1]),
                  Value=df[,i],Type=df[,"Type"])
  print(colnames(df)[i])
  #it can't be used? [order(tmp$Type,tmp$Value),]
  if(i==1){df1<-tmp} else {df1<-rbind(df1,tmp)}
}

my_comparisons_pd<-list( c("Fecal","Rectal"),c("Rectal", "Oral"),
                         c("Oral", "Blowhole"),c("Blowhole","Skin"),
                         c("Fecal","Oral"),c("Rectal", "Blowhole"),c("Oral", "Skin"),
                         c("Fecal","Blowhole"),c("Rectal", "Skin"),
                         c("Fecal","Skin"),
                         c("Freshwater","Fecal"),c("Freshwater","Rectal"),
                         c("Freshwater","Oral"),c("Freshwater","Blowhole"),
                         c("Freshwater","Skin")
                         )
color_munual<-c("#FFB415","#3983AD","#64638E","#A43E55","#419583","#78767D")
df1$Index<-factor(df1$Index,levels = c("observed_features",
                                       "shannon",
                                       "faith_pd"))

labeldat = df1 %>%
  group_by(Index,Type) %>%
  summarize(ypos = max(Value) + 0.20*max(Value)-1)

labeldat<-labeldat[order(labeldat$Index),]
labeldat$lab<-c("a","a","b","b","c","c",
                "a","a","b","c","cd","d",
                "a","a","b","b","c","c")

tiff(filename = "alpha2.tiff",
     width = 2800,height = 1280)
ggplot(df1,aes(x=Type,y=Value,color=Type))+
  geom_boxplot(lwd=1.5)+
  theme_bw()+
  ylab("")+
  xlab("")+
  # stat_compare_means(comparisons = my_comparisons_pd,size=8)+
  theme(legend.position = "bottom",plot.title = element_text(size = 8))+
  guides(color = guide_legend(nrow = 1))+
  scale_color_manual(values=color_munual)+
  geom_text(data = labeldat, aes(label = lab, y = ypos), 
            colour="black",size=13,
            position = position_dodge(width = .5),
            show.legend = FALSE )+
  facet_wrap(. ~ Index, drop=F,scales = "free_y",nrow = 1)+
  geom_jitter(shape=16,size=7 ,position=position_jitter(0.2))+
  theme(title= element_text(size=40),
        strip.text = element_text(size = 60),
        
        axis.text.y = element_text(size = 40),
        legend.key.height = unit(2.5, 'cm'), 
        legend.key.width  = unit(3.2, 'cm'), 
        legend.text = element_text(size = 40),
        legend.title = element_blank(),
        axis.text.x = element_blank())
  
dev.off()

