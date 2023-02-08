#' @title Plot curves.
#' @description Extends gBasePlot.
#' @param df Dataframe.
#' @param x Character. Name of numeric variable of df corresponding to the X axis of the curve (time).
#' @param y Character. Name of numeric variable of df corresponding to the Y axis of the curve.
#' @param color Character. Name of character variable of df corresponding to the label of the curve.
#' @param smooth Logical. if TRUE, curves are smoothed.
#' @param highlight highlighting the curves
#' @import ggplot2
#' @inheritParams gBasePlot
#' @return A plot of class ggplot.
#' @export
gCurvePlot=function(df, x="time", y="score", color="descriptor", highlight="", smooth=TRUE, ...)
  {
  curves=gBasePlot(...) +
    scale_x_continuous(expand = c(0, 0))
  if (smooth==TRUE)
  {
    if(is.null(colors))
    {
      curves = curves + geom_smooth(data = df, aes_string(x = x, y = y,color=color), span = 0.1, se = FALSE, size=0.5, method = "loess", span = 0.1)
    }
    if(!is.null(colors))
    {
      curves = curves + geom_smooth(data = df, aes_string(x = x, y = y,color=color), span = 0.1, se = FALSE, size=0.5, method = "loess", span = 0.1)+scale_color_manual(values=colors)
    }
  }
  else
  {
    if(is.null(colors))
    {
      curves = curves + geom_line(data = df, aes_string(x = x, y = y,color=color))
    }
    else
    {

      curves = curves + geom_line(data = df, aes_string(x = x, y = y,color=color)) +scale_color_manual(values=colors)
    }
  }

  if (highlight!="") {
    curves = curves + geom_point(data = df[df[,"highlight"]==TRUE,], aes_string(x = x, y = y,color=color), size=1)
  }

 curves = curves +  scale_y_continuous(expand = c(0, 0), limits = c(NA,NA))

  return (curves)
}
