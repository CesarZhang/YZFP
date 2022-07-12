rm(list = ls())

library(dplyr)
library(stringr)
library(ggplot2)

##############
meta<-read.csv("../Result/metadata.tsv",header = T,
               sep = "\t") 
meta$Body_Site<-factor(meta$Body_Site,levels = c("Fecal","Rectal","Oral",
                                                 "Blowhole","Skin","Freshwater"))
meta<-meta[order(meta$Body_Site),]


data<-read.table("../MeanSe/Genus_meanse.csv",row.names =2,
                 header = T,check.names = F,
                 sep = ",")[,c("Skin_mean","Freshwater_mean",
                               "Skin_se","Freshwater_se")]
rownames(data)<-str_replace_all(rownames(data),"-","_")
rownames(data)<-str_replace_all(rownames(data),"\\(","_")
rownames(data)<-str_replace_all(rownames(data),"\\)","_")
rownames(data)<-str_replace_all(rownames(data),"\\.","_")

data1<-read.table("Lefse-result.txt",row.names = 1,header = T,
                  check.names = F)
res<-data[rownames(data1),] %>% subset(Skin_mean>0.01)

df<-merge(res,data1,by=0)

tax<-str_split_fixed(df$Row.names,";",6)
tax[,6]<-str_replace_all(tax[,6],"g__","")

df$LDA<-signif(df$LDA,2)
df$lab<-paste(tax[,6],"\n","(LDA =",df$LDA,")",sep="")
df<-df[order(df$Skin_mean,decreasing = T),]

df1<-data.frame()
tp<-c("Skin","Freshwater")

for (i in 1:2) {
  tmp<-data.frame(Mean=df[,i+1],Se=df[,i+3],
                  Type=rep(tp[i],nrow(df)),
                  Label=df[,9])
  df1<-rbind(df1,tmp)
}

df1$Type<-factor(df1$Type,levels = c("Skin","Freshwater"))
df1$Label<-factor(df1$Label,levels = df$lab)

col<-c("#419583","#78767D")

tiff(filename = "Rich_Skin.tiff",
     width = 1980,height = 2640)
ggplot(df1,aes(Type,Mean,fill=Type))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Mean-Se, ymax=Mean+Se), width=0.5,
                size=2,
                position=position_dodge(.9))+
  facet_wrap(. ~ Label, drop=F,nrow=3,scales = "free_y")+
  scale_fill_manual(values = col)+
  theme_bw()+ylab("")+xlab("")+
  guides(fill=guide_legend(title = ""))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text= element_text(size=50),
        axis.title.y = element_text(size=60),
        axis.title.x = element_text(size=60),
        axis.line.y = element_blank(),
        legend.key.size = unit(2.5,"cm"),
        legend.text = element_text(size = 60),
        legend.title = element_text(size = 60),
        strip.text = element_text(size=50))
dev.off()

