#' @title Plot a map.
#' @description Extends gBasePlot.
#' @param listCoord List of dataframes \cr
#' $indivCoord : dataframe of individual coordinates with columns name/x/y/axes
#' $varCoord : dataframe of variable coordinates with columns name/x/y/axes
#' $indivEllipsesCoord : dataframe of ellipse coordinates with columns name/x/y/axes
#' $inertia : dataframe of explained variance x/y/axes
#' @param type Character: "points" or "arrows"
#' @inheritParams gBasePlot
#' @param output =c("ind","var")
#' @param indsup Boolean indicating whether individuals should be projected on the map or not (FALSE by default)
#' @param ellipses  Boolean indicating whether confidence ellipses should be on the map or not (TRUE by default)
#' @return A plot of class ggplot.
#' @export
#' @importFrom stringr str_split
#' @importFrom ggrepel geom_text_repel
gMapPlot=function(listCoord, type="points",output=c("ind","var"), indsup=FALSE, ellipses=TRUE,...) {

  parameters=list(...)
  x=y=NULL

  if (is.null(listCoord$option)) { listCoord$option="" }
  if (is.null(parameters$fontSizeCex)) { parameters$fontSizeCex=1 }





  minY=min(listCoord$indivCoord$y,listCoord$varCoord$y)
  maxY=max(listCoord$indivCoord$y,listCoord$varCoord$y)
  nudge=(maxY-minY)/20

  listCoord$inertia$txt=""
  for (i in 1:nrow(listCoord$inertia)) {
    dims=str_split(listCoord$inertia$axes[i],",")
    listCoord$inertia[i,"txt"]=paste("Dim. ", dims[[1]][1],": ",listCoord$inertia[i,"x"],"%, Dim.",dims[[1]][2],": ",listCoord$inertia[i,"y"],"%",sep="")
  }

  #parameters$vnames=listCoord$inertia[,"txt"]

  #BUG ICI
  # listCoord$indivCoord=merge(listCoord$indivCoord,listCoord$inertia[c("axes","txt")],by="axes")
  # listCoord$indivCoord$axes=listCoord$indivCoord$txt
  # listCoord$varCoord=merge(listCoord$varCoord,listCoord$inertia[c("axes","txt")],by="axes")
  # listCoord$varCoord$axes=listCoord$varCoord$txt
  # listCoord$indivEllipsesCoord=merge(listCoord$indivEllipsesCoord,listCoord$inertia[c("axes","txt")],by="axes")
  # listCoord$indivEllipsesCoord$axes=listCoord$indivEllipsesCoord$txt
  # listCoord$indSup=merge(listCoord$indSup,listCoord$inertia[c("axes","txt")],by="axes")
  # listCoord$indSup$axes=listCoord$indSup$txt
  #
  # print(listCoord)


  res=gBasePlot(..., vnames=listCoord$inertia[,"txt"]) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    theme(aspect.ratio=1,axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.ticks.x=element_blank())
    #scale_colour_manual(values=color_group)


  # BUG ICI, affiche les dims 1, 2 quelle que soit la carte

  #res=res+labs(subtitle= paste("Dim. 1: ", round(listCoord$inertia[,1],2),"%, Dim. 2: ",round(listCoord$inertia[,2],2)))

  #dims=stringr::str_split(listCoord$inertia$axes,",")
  #res = res + geom_text(data=listCoord$inertia, aes(label = paste("Dim. ",dims[[1]][1],": ", round(x,2),"%, Dim. ",dims[[1]][2],": ",round(y,2),"%",sep=""), x = xlim[2], y = ylim[1]),  hjust = 1, vjust = 1)


  if("ind"%in%output)
  {
    res = res + geom_point(data = listCoord$indivCoord, aes_string(x = "x", y = "y"), col="blue") +

     # geom_text(data = listCoord$indivCoord, aes_string(x = "x", y = "y", label="name"),colour="blue", nudge_y = nudge, size=3*parameters$fontSizeCex) +

    geom_text_repel(data = listCoord$varCoord, aes_string(x = "x", y = "y", label="name"),colour="blue", nudge_y = nudge,segment.color = NA, size=3*parameters$fontSizeCex)

    if(indsup)
    {

      res=res+geom_point(data=listCoord$indSup, aes_string(x = "x", y = "y",col="product"))
    }

  }
  if("var"%in%output)
  {

    res=res +
    geom_hline(yintercept = 0, lty = 2) +
      geom_vline(xintercept = 0, lty = 2)
    #res = res + geom_text(data=listCoord$inertia, aes(label = paste("Dim. ",dims[[1]][1],": ", round(x,2),"%, Dim. ",dims[[1]][2],": ",round(y,2),"%",sep=""), x = xlim[2], y = ylim[1]),  hjust = 1, vjust = 1)


    if (type=="arrows") {
      res=res+geom_segment(data = listCoord$varCoord, aes_string(x=0, y=0,xend = "x", yend = "y"), arrow=arrow(length = unit(0.2, "cm")), col="red")
    }
    if (type=="points") {
      res=res+geom_point(data = listCoord$varCoord, aes_string(x = "x", y = "y"), shape=24, col="red", fill="red")
    }

    if(listCoord$option=="correlation")
    {
      circleFun <- function(center = c(0,0),diameter = 2, npoints = 100){
        r = diameter / 2
        tt <- seq(0,2*pi,length.out = npoints)
        xx <- center[1] + r * cos(tt)
        yy <- center[2] + r * sin(tt)
        return(data.frame(x = xx, y = yy))
      }

      dat <- circleFun()
      res=res+ geom_path(data=dat,aes(x,y),inherit.aes = FALSE)

    }
#  }

    res = res + geom_text_repel(data = listCoord$varCoord, aes_string(x = "x", y = "y", label="name"),colour="red", nudge_y = nudge,segment.color = NA, size=3*parameters$fontSizeCex)

  }

  if (ellipses==TRUE)
  {
    res = res + geom_path(data = listCoord$indivEllipsesCoord, aes_string(x = "x", y = "y", group="name"),color="blue")
  }

  if (indsup==TRUE)
  {
    res=res+geom_point(data=listCoord$indSup, aes_string(x = "x", y = "y",col="product"))
  }

  #ylim=ggplot_build(res)$layout$panel_scales_y[[1]]$range$range
  #xlim=ggplot_build(res)$layout$panel_scales_x[[1]]$range$range
  #dims=stringr::str_split(listCoord$inertia$axes,",")
  #res = res + geom_text(data=listCoord$inertia, aes(label = paste("Dim. ",dims[[1]][1],": ", round(x,2),"%, Dim. ",dims[[1]][2],": ",round(y,2),"%",sep=""), x = xlim[2], y = ylim[1]),  hjust = 1, vjust = 1)

  return (res)
  #TODO : relier les produits par des segments
  #TODO : 2 cartes séparées
  #TODO : tableau à c^té des graphiques (hotelling , test benjamin)
}