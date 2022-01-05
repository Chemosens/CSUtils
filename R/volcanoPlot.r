volcanoPlot=function(anovaTable)
{
  log2FoldChange=padj=threshold=NULL
  ggplot(anovaTable) +
    geom_point(aes(x=log2FoldChange, y=-log10(padj), colour=threshold)) +
    ggtitle("Mov10 overexpression") +
    xlab("log2 fold change") + 
    ylab("-log10 adjusted p-value") +
    #scale_y_continuous(limits = c(0,50)) +
    theme(legend.position = "none",
          plot.title = element_text(size = rel(1.5), hjust = 0.5),
          axis.title = element_text(size = rel(1.25)))    
}
