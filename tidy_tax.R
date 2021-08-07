## tax: Taxonomic annotation based on silva data base, loaded in R with rownames.
tidy_tax<-function(tax){
  tax<-data.frame(str_split_fixed(tax$Taxon,";",7),row.names = rownames(tax))
  tax<-apply(tax, 2, function(x){
    x[grep("^[^D]",x)]<-""
    return(x)
  })
  tax<-apply(tax,2,function(x)substring(x,6))
  tax<-apply(tax,2,function(x){
    x[x==""]<-"Unassigned"
    return(x)
  })
  for (i in 1:7) {
    pf<-c("k","p","c","o","f","g","s")
    tax[,i]<-paste0(pf[i],"_",tax[,i])
  }
  tax<-as.data.frame(tax)
  colnames(tax)<-c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
  return(tax)
}
