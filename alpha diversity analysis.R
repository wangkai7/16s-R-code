rm(list=ls())
setwd('D:/amplicon/2018.8\\analysis\\R\\aplha\\p1vsp2')
# library(amplicon)
#load metadata and alpha diversity index data
metadata=read.table("sample-metadata11.28.txt",header=T,row.names=1,sep="\t",comment.char="")
head(metadata,n=3)
alpha_div=read.table("alpha diversity table.txt",header=T,row.names=1,sep="\t",comment.char = "")
head(alpha_div,n=3)
alpha_div<- alpha_div[2:5]
library(Hmisc)
colnames(alpha_div)=c('Shannon','Pielou_Evenness','Observed_Otus','Faith_Pd')
colnames(alpha_div)
#set experimental group
group="Parity"
#picture export size
width=89
height=85
#alpha diversity function
alpha_boxplot <- function(alpha_div, metadata, index = "richness", groupID = "genotype") {
  
  # 依赖关系检测与安装
  p_list = c("ggplot2", "agricolae", "dplyr")
  for(p in p_list){
    if (!requireNamespace(p)){
      install.packages(p)}
    library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)}
  
  # 测试默认参数
  # index = "richness"
  # groupID = "genotype"
  
  # 交叉筛选
  idx = rownames(metadata) %in% rownames(alpha_div)
  metadata = metadata[idx,]
  alpha_div = alpha_div[rownames(metadata),]
  
  # 提取样品组信息,默认为group可指定
  sampFile = as.data.frame(metadata[, groupID],row.names = row.names(metadata))
  # colnames(sampFile)[1] = "group"
  
  # 合并alpha_div和metadata
  df = cbind(alpha_div[rownames(sampFile),index], sampFile)
  colnames(df) = c(index,"group")
  
  # 统计各种显著性
  model = aov(df[[index]] ~ group, data=df)
  # 计算Tukey显著性差异检验
  Tukey_HSD = TukeyHSD(model, ordered = TRUE, conf.level = 0.95)
  # 提取比较结果
  Tukey_HSD_table = as.data.frame(Tukey_HSD$group)
  # LSD检验，添加差异组字母
  out = LSD.test(model, "group", p.adj="none")
  stat = out$groups
  # 分组结果添入Index
  df$stat=stat[as.character(df$group),]$groups
  # 设置分组位置为各组y最大值+高的5%
  max=max(df[,c(index)])
  min=min(df[,index])
  x = df[,c("group",index)]
  y = x %>% group_by(group) %>% summarise_(Max=paste('max(',index,')',sep=""))
  y=as.data.frame(y)
  rownames(y)=y$group
  df$y=y[as.character(df$group),]$Max + (max-min)*0.05
  
  # 绘图 plotting
  p = ggplot(df, aes(x=group, y=df[[index]], color=group)) +
    geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") +
    labs(x="Groups", y=index, color=groupID) + theme_classic() +
    geom_text(data=df, aes(x=group, y=y, color=group, label=stat)) +
    geom_jitter(position=position_jitter(0.17), size=1, alpha=0.7)+
    theme(text=element_text(family="sans", size=7))
  p
}
#
alpha_index="Observed_Otus"
(p=alpha_boxplot(alpha_div,index = alpha_index,metadata,groupID = group))
p1=p
alpha_index="Pielou_Evenness"
(p=alpha_boxplot(alpha_div,index = alpha_index,metadata,groupID = group))
p2=p
alpha_index="Shannon"
(p=alpha_boxplot(alpha_div,index = alpha_index,metadata,groupID = group))
p3=p

alpha_index="Faith_Pd"
(p=alpha_boxplot(alpha_div,index = alpha_index,metadata,groupID = group))
p4=p
#bind pictures into one board
library(grid)
library(ggpubr)
board=ggarrange(p1,p2,p3,p4, labels = c("A", "B","C","D"), ncol=2,nrow=2,common.legend = TRUE,legend = "bottom")
board
#save
ggsave(paste0("alpha_div.pdf"),board,width=width,height=height,units="mm")
ggsave(paste0("alpha_div.png"),board,width=width,height=height,units="mm")
