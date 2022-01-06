#' @title Plot distributions (boxplot, violin).
#' @description Extends gBasePlot.
#' @param df Dataframe
#' @param plot Character: "violin" and/or "boxplot" and/or "jitter"
#' @param x Character. Name of character variable of df corresponding to the Y axis of the curve.
#' @param y Character. Name of numeric variable of df corresponding to the Y axis of the plot.
#' @param fill Character. Name of character variable of df corresponding to the label of the boxplots/violins/jitters.
#' @import ggplot2
#' @inheritParams gBasePlot
#' @return A plot of class ggplot.
#' @export
gDistributionPlot=function(df, plot=c("violin","boxplot"), y="score", x="product", fill="product", ...) {
#gDistributionPlot=function(df, plot=c("violin","boxplot"), y="score", x="product", fill="product", rows=".",cols=".", wrap=NULL, facet="wrap", title="", labX = NULL, labY=NULL, grid=FALSE, labels=NULL, colors=NULL,scales="free_y",vnames=NULL){

  #parameters=list(...)

  # if (is.null(parameters$facet)) { parameters$facet = "wrap" }
  # if (is.null(parameters$scales)) { parameters$scales = "free_y" }

  #violin=gBasePlot(title=title, rows=rows, cols=cols, wrap=wrap,facet=facet, labX=labX, labY=labY, grid=grid, labels=labels, colors=colors,scales=scales,vnames=vnames)
  #violin=gBasePlot(parameters)
  violin=gBasePlot(...)
  if ("violin" %in% plot) {
    violin=violin+geom_violin(data=df, aes_string(x=x,y=y, fill=x))
  }
  if ("boxplot" %in% plot) {
    violin=violin+geom_boxplot(data=df, aes_string(x=x,y=y, fill=x), notch=FALSE, outlier.size=1, width=0.2, color="black", alpha=0.8, outlier.colour="red")
  }
  if ("jitter" %in% plot) {
    violin=violin+geom_jitter(data=df, aes_string(x=x,y=y, colour=x), size=0.4, alpha=0.9)
  }
  violin=violin+stat_summary(data=df, aes_string(x=x,y=y),fun=mean, geom="point", shape=22, size=1, color="black",fill="black")

  return (violin)
}