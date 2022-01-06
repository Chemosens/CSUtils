#' @title gGetLegend
#' @param myggplot a ggplot object with a legend to catch
#' @export
gGetLegend=function(myggplot)
{
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    print(tmp)
    legend <- tmp$grobs[[leg]]
    return(legend)
}