#' @param foldchange ratio between control and condition group: this vector is then transformed by taking log2.
#' @param pvalue pvalue of the wilcox or t.test comparing the two populations. This vector is transformed by taking -log10()
#' @param foldchange_threshold threshold (in log2): a foldchange_threshold of 2 indicates that the log was
#'  data.frame whose colnames are 'log2foldChange','-log10(padj)
#' @export
volcanoPlot=function(foldchange,pvalue,foldchange_threshold=2,pval_threshold=0.05,log2FC=T)
{
  lab=NULL
  interestingPval=pvalue<pval_threshold
  log2FoldChange=log(foldchange,base=2)
  interestingStat=abs(log2FoldChange)> foldchange_threshold
  if(log2FC)
  {
    log2statThreshold=log(foldchange_threshold,base=2)
  }
  log10p=-log(pvalue,base=10)
  log10pthreshold=-log(pval_threshold,base=10)
   color=rep("NS",length(foldchange))
   color[interestingPval]="sig. pval"
 #  color[interestingStat]=paste0("high F")
   color[interestingPval&interestingStat]="high F and sig. pval"

   colorValues=c("grey","black","red")
   names(colorValues)=c("NS","sig. pval","high F and sig. pval")
  df=data.frame(lab=names(foldchange),log2FoldChange=log2FoldChange,log10p=log10p,color=color)
  gg= ggplot(df) +
    geom_point(aes(x=log2FoldChange, y=log10p,color=color)) +
    ggtitle("Volcano plot") +
    xlab("log2 fold change") +
    ylab("-log10 p-value") +
   # scale_y_continuous(limits = c(0,50)) +
    #theme(legend.position = "none",
    #      plot.title = element_text(size = rel(1.5), hjust = 0.5),
    #      axis.title = element_text(size = rel(1.25))) #
      theme_bw()+scale_color_manual(values=colorValues)
     gg=gg+geom_vline(xintercept=-foldchange_threshold,color="yellow")+geom_vline(xintercept=foldchange_threshold,color="yellow")
     gg=gg+geom_hline(yintercept=log10pthreshold,color="darkgrey")

  gg=gg+geom_vline(xintercept=0)
  gg=gg+geom_text_repel(data=df[interestingPval|interestingStat,],aes(x=log2FoldChange,y=log10p,label=lab,color=color))+scale_color_manual(values=colorValues)

  return(list(df=df,gg=gg))
}
