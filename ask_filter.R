#计算asv在样本中的检出率、均值、中位数、标准差、变异系数、相对丰度
#输入featuretable：feature by sample
asv_stats=function(otutab){
  norm=t(t(otutab)/colSums(otutab))*100
  asv_stats=cbind(t(apply(otutab,1,function(row){
    rate=1-length(row[which(row==0)])/length(row)
    mean=mean(row)
    median=median(row)
    sd=sd(row)
    cv=sd(row)/mean(row)
    return(c(rate=rate,mean=mean,median=median,sd=sd,cv=cv))
  })),RA=rowMeans(norm))
  return(asv_stats)
}
mystats=asv_stats(otutab)
