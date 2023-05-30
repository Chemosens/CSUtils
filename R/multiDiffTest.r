#' @export
#' @importFrom stats wilcox.test
#' @importFrom dunn.test dunn.test
multDiffTest=function(block,group,test="wilcoxon",correction="bonferoni" , gp=levels(factor(group)))
{
  pval=rep(NA,dim(block)[2]);names(pval)=colnames(block)
  group1=group2=foldchange=stat=pval
  #gp=levels(factor(group))
  gp1=gp[1];gp2=gp[2]
  for(variable in colnames(block))
  {
    group1[variable]=mean(block[group==gp1,variable])
    group2[variable]=mean(block[group==gp2,variable])
    foldchange[variable]=group1[variable]/group2[variable]
    if(test=="wilcoxon")
    {
      reswilcox=wilcox.test(block[group==gp1,variable],block[group==gp2,variable])
      stat[variable]=reswilcox$statistic
      pval[variable]=reswilcox$p.value
    }
    if(test=="ttest")
    {
      resttest=t.test(block[group==gp1,variable],block[group==gp2,variable])
      stat[variable]=resttest$statistic
      pval[variable]=resttest$p.value
    }
    if(test=="dunn")
    {
      resttest=dunn.test(block[,variable],g=group)
      stat[variable]=resttest$statistic
      pval[variable]=resttest$p.value
    }
  }
  res=list(stat=stat,pval=pval,foldchange=foldchange,group1=group1,group2=group2,gp=gp)
  return(res)
}

