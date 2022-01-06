#' @title Plot a barplot.
#' @description Extends gBasePlot.
#' @param df A dataframe
#' @param y Name of numeric variable of df corresponding to the Y axis of the barplot.
#' @param x Name of character variable of df corresponding to the X axis of the curve.
#' @param group The group variable
#' @param groupvars A vector of coulmns to be aggregated as the experimental unit.
#' @param fill  TODO
#' @param confInt The confidence interval for the bars.
#' @param errorBars The type of error bar: "", SD, SEM, CI
#' @inheritParams gBasePlot
#' @return A plot of class ggplot.
#' @export
#' @importFrom Rmisc summarySE
#' @import ggplot2
#gBarPlot=function(df, y="score", x="descriptor", group="product", fill="product", groupvars=c("product","rep","descriptor","period"), confInt=0.95, errorBars="CI", facet="grid", rows=".",cols=".", wrap=NULL, title="", labX = NULL, labY=NULL, grid=FALSE, labels=NULL, colors=NULL,scales="fixed",vnames=NULL, ...){
gBarPlot=function(df, y="score", x="descriptor", group="product", fill="product", groupvars=c("product","rep","descriptor","period"), confInt=0.95, errorBars="CI", ...){
  
  #parameters=list(...)
  
  # Calcul des moyennes SD, SEM et intervalle de confiance
  if (errorBars!="") {
    df=summarySE(data = df, measurevar=y, groupvars=groupvars, na.rm = TRUE, conf.interval = 0.95)
  }

  #barplot=gBasePlot(title=title, rows=rows, cols=cols, facet=facet,labX=labX, labY=labY, grid=grid, labels=labels, colors=colors,scales=scales,vnames=vnames) +
  #barplot=gBasePlot(parameters) +
  barplot=gBasePlot(...) +
    geom_bar(data=df,stat="identity",position="dodge",color="black",ggplot2::aes_string(y=y, x=x, group=group, fill=fill)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,NA)) 
  
  if (errorBars != "") 
  {
    subtitle=""
    if (errorBars == "SD") 
    {
      df$upper=df[,y]+df$sd
      df$lower=df[,y]-df$sd
      subtitle="standard deviation"
    }
    if (errorBars == "SEM") 
    {
      df$upper=df[,y]+df$se
      df$lower=df[,y]-df$se
      subtitle="standard error of the mean"
    }
    if (errorBars == "CI") 
    {
      df$upper=df[,y]+df$ci
      df$lower=df[,y]-df$ci
      subtitle=paste("confidence interval, level=",confInt,sep="")
    }
    
    df$lower[df$lower<0]=0
    
    barplot=barplot+
      ggplot2::geom_errorbar(data=df,ggplot2::aes_string(ymin="lower",ymax="upper",x=x, group=group), width=.2, position=ggplot2::position_dodge(.9)) +
      ggplot2::labs(subtitle = paste("Error bars:",subtitle))
   
    #TODO : ANOVA + barplot
     
  }
  
  return(barplot)
  
}
