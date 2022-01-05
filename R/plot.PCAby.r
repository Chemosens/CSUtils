plot.resPCAby=function(resPCAby,rows=".",cols=".",...)
{
  parameters = list(...)
  
  if (is.null(parameters$title)) { title="Duration PCA" } else { title=parameters$title }
  if (is.null(parameters$confInt)) { confInt=0.95 } else { confInt=parameters$confInt }
  if (is.null(parameters$dim)) { dim=list(c(1,2)) } else { dim=parameters$dim }
  listCoord=resPCAby
  gg = gMapPlot(listCoord,title=title, rows=rows,cols=cols, facet="grid",type="points") 
}
