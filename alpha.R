rm(list=ls())
setwd("~/Desktop/GITproj/2AlphaDiv/")

# data --------------------------------------------------------------------

# raw
asv<-read.delim("../1Rawdata/feature-table.tsv",row.names = 1,skip = 1)
sam<-read.delim("../1Rawdata/metadata.txt",row.names = 1)


if (!identical(colnames(asv),rownames(sam))) {
  sam<-sam[colnames(asv),]
} else ("nothing changed")

# delete useless samples
man<-read.delim("../1Rawdata/manifest.txt")
samples<-man[,3] %>% .[-which(.=="I2")]

sam<-sam[samples,]
sam$site<-factor(sam$site,
                 levels = c("rumen","reticulum","omasum","abomasum","duodenum","jejunum","ileum","cecum","colon","rectum")
)
sam <- sam[order(sam$site),]
# sam$group<-factor(sam$group,levels = c("stomach","small intestine","large intestine"))

asv<-asv[rownames(sam)]
asv<-asv[rowSums(asv)>0,]

# quality control
(rowSums(asv>0)>1) %>% sum()

asv<-asv[rowSums(asv>0)>1,]



# venn --------------------------------------------------------------------

library(ggVennDiagram)

table(sam$group)
{
  s1<-rownames(subset(sam,group=="stomach")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  s2<-rownames(subset(sam,group=="small intestine")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  s3<-rownames(subset(sam,group=="large intestine")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  
  ggVennDiagram(x=list(s1,s2,s3),
                category.names = c("Stomach","Small intestine","Large Intestine"),
                label_percent_digit = 1,
                set_size = 6, # size of set lables
                label_size = 6, # size of region labels
                edge_size = 0,
                label_alpha = 0, # background of region labels
  ) +
    scale_x_continuous(expand = expansion(mult = .3)) +
    scale_fill_gradient(low = "#f7fcf5",high = "#008B45FF") +
    theme(legend.text = element_text(size=16),
          legend.title = element_text(size=16),
    ) -> 
    v1
  
  ggsave("v1.pdf",v1,width = 8,height = 6)
}

{
  s1<-rownames(subset(sam,site=="rumen")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  s2<-rownames(subset(sam,site=="reticulum")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  s3<-rownames(subset(sam,site=="omasum")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  s4<-rownames(subset(sam,site=="abomasum")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  
  
  ggVennDiagram(x=list(s1,s2,s3,s4),
                category.names = c("rumen","reticulum","omasum","abomasum"),
                label_percent_digit = 1,
                set_size = 6, # size of set lables
                label_size = 4, # size of region labels
                edge_size = 0,
                label_alpha = 0, # background of region labels
  ) +
    scale_x_continuous(expand = expansion(mult = .3)) +
    scale_fill_gradient(low = "#f7fcf5",high = "#008B45FF") +
    theme(legend.text = element_text(size=16),
          legend.title = element_text(size=16),
    ) -> 
    v2
  
  ggsave("v2.pdf",v2,width = 8,height = 6)
}

{
  s1<-rownames(subset(sam,site=="duodenum")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  s2<-rownames(subset(sam,site=="jejunum")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  s3<-rownames(subset(sam,site=="ileum")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  
  ggVennDiagram(x=list(s1,s2,s3),
                category.names = c("duodenum","jejunum","ileum"),
                label_percent_digit = 1,
                set_size = 6, # size of set lables
                label_size = 4, # size of region labels
                edge_size = 0,
                label_alpha = 0, # background of region labels
  ) +
    scale_x_continuous(expand = expansion(mult = .3)) +
    scale_fill_gradient(low = "#f7fcf5",high = "#008B45FF") +
    theme(legend.text = element_text(size=16),
          legend.title = element_text(size=16),
    ) -> 
    v3
  
  ggsave("v3.pdf",v3,width = 8,height = 6)
}

{
  s1<-rownames(subset(sam,site=="cecum")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  s2<-rownames(subset(sam,site=="colon")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  s3<-rownames(subset(sam,site=="rectum")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  
  ggVennDiagram(x=list(s1,s2,s3),
                category.names = c("cecum","colon","rectum"),
                label_percent_digit = 1,
                set_size = 6, # size of set lables
                label_size = 4, # size of region labels
                edge_size = 0,
                label_alpha = 0, # background of region labels
  ) +
    scale_x_continuous(expand = expansion(mult = .3)) +
    scale_fill_gradient(low = "#f7fcf5",high = "#008B45FF") +
    theme(legend.text = element_text(size=16),
          legend.title = element_text(size=16),
    ) -> 
    v4
  
  ggsave("v4.pdf",v4,width = 8,height = 6)
}

{
  s1<-rownames(subset(sam,site=="abomasum")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  s2<-rownames(subset(sam,site=="duodenum")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  s3<-rownames(subset(sam,site=="ileum")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  s4<-rownames(subset(sam,site=="cecum")) %>% 
    asv[,.] %>% 
    .[rowSums(.>0)>0,] %>% rownames()
  
  
  
  ggVennDiagram(x=list(s1,s2,s3,s4),
                category.names = c("abomasum","duodenum","ileum","cecum"),
                label_percent_digit = 1,
                set_size = 6, # size of set lables
                label_size = 4, # size of region labels
                edge_size = 0,
                label_alpha = 0, # background of region labels
  ) +
    scale_x_continuous(expand = expansion(mult = .3)) +
    scale_fill_gradient(low = "#f7fcf5",high = "#008B45FF") +
    theme(legend.text = element_text(size=16),
          legend.title = element_text(size=16),
    ) -> 
    v5
  
  ggsave("v5.pdf",v5,width = 8,height = 6)
}


# alpha -------------------------------------------------------------------

library(phyloseq)
library(ggsci)

mytheme<-theme(panel.background = element_blank(),
               legend.key = element_blank(),
               legend.background = element_blank(),
               axis.text.x = element_text(size = 14),
               axis.text.y = element_text(size = 14),
               axis.title = element_text(size=16),
               legend.text = element_text(size=16),
               legend.title = element_text(size=16),
               axis.line.x = element_line(),
               axis.line.y = element_line())

tree<-read_tree("../1Rawdata/rooted-tree.nwk")
ape::is.binary(tree)
tree <- ape::multi2di(tree)

mdata<-phyloseq(otu_table(asv,taxa_are_rows = T),sample_data(sam),tree)
summary(colSums(asv))
even<-rarefy_even_depth(mdata,rngseed = 123,sample.size = 18000)

a<-plot_richness(physeq = even,
                 x = "site",
                 color = "group",
                 measures = c("Observed","Shannon"),
                 )+
  geom_boxplot(alpha=0.4,outlier.colour = "black")+
  mytheme+
  scale_color_aaas()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45,hjust = 0.5,vjust = 0.5),
        strip.text = element_text(size=16)
  )
a

ggsave("a.pdf",a,width =8 ,height = 6)

alpha<-estimate_richness(even,
                         measures = c("Observed","Chao1","Shannon","Simpson"))

write.csv(alpha,"alpha.csv",row.names = T)
