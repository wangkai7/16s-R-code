group_comp<- function(asv,tax,sam,level="Genus",topN=10,group="site",clos=NULL){
  
  
  norm<-(t(t(asv)/colSums(asv))*100) %>% as.data.frame()
  
  index<-sam[colnames(norm),group]
  norm<-apply(norm, 1, function(x)tapply(x,index,mean)) %>% t %>% as.data.frame()
  
  
  tax_table<-cbind(norm,tax[rownames(norm),level])
  names(tax_table)[ncol(tax_table)]<-'tax'
  
  tax_table = tax_table %>% group_by(tax) %>% summarise_all(sum)
  tax_table=tax_table[order(-rowSums(tax_table[,-1])),]
  dir.create("group",showWarnings = F,recursive = T)
  write.csv(tax_table,paste0("group/",level,"_group.csv"),row.names = F,quote = F)
  
  top=tax_table[1:topN,]
  others=colSums(tax_table[(topN+1):nrow(tax_table),-1])
  
  top[(topN+1),2:ncol(tax_table)]<-t(others)
  top[(topN+1),1]<-'others'
  # top=top[order(-rowSums(top[,-1])),]
  top$tax=factor(top$tax,levels = top$tax)
  
  p_data<-reshape2::melt(top,id='tax',variable.name = 'sample_id')
  
  p <- ggplot(p_data, aes(sample_id, value, fill = tax)) +
    geom_col(position = "stack") + 
    labs(x = '', y = 'Relative Abundance(%)') +
    scale_y_continuous(expand = c(0,0))+
    guides(fill=guide_legend(title = level))+
    theme(panel.grid = element_blank(), 
          panel.background = element_blank(),
          axis.line.x = element_line(),
          axis.line.y = element_line()
    ) +
    theme(axis.text = element_text(size = 14), 
          axis.title = element_text(size = 16), 
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16)
    )+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  
  if (!is.null(clos)) {
    p<-p+scale_fill_manual(values=clos)
  }
  
  return(p)
}
