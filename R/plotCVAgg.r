#' @param respcagg res of PCA gg
#' @param type "ind" for individual graph, "var" for variable graph,"cor" for univariate correlations in decreasing order with the PCA axes
#' @param text Boolean indicating whether the labels should be displayed (TRUE) or not (FALSE)
#' @param n number of variables to be selected in correlation graph (when type="cor")
#' @param colorInd if type(respcagg)="raw", this parameter allows to color the individuals according to a letter in the name of individual. It can be the first one (with "first") or another subset of character ("substr") defined in substrVec
#' @param substrVec vector of two numbers representing the two limit positions of characters to be selected when colorInd="substr"
#' @param axes c(1,2). Axes to be plotted
#' @param indsup "ell","points" or "none"
#' @export
#' @importFrom ggforce geom_circle
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @importFrom utils tail
plotCVAgg=function(respcagg,type="ind",text=TRUE,n=10,colorInd="all",substrVec=c(1,2),axes=c(1,2),indSup=c("ell"),repel=FALSE)
{
  x=y=product=name=x0=y0=r=NULL
   if(type=="ind")
  {
    indiv=respcagg$indivCoord[respcagg$indivCoord[,"axes"]==paste(axes[1],axes[2],sep=","),]

      if(colorInd=="first")
      {
        indiv$product=substr(rownames(indiv),1,1)
      }
      if(colorInd=="substr")
      {
        indiv$product=substr(rownames(indiv),substrVec[1],substrVec[2])
      }
    if(colorInd=="all")
    {
      indiv$product=rownames(indiv)
    }
    if(colorInd=="vec")
    {
      indiv$product=substrVec
    }
    indiv$name=rownames(indiv)

    if(text)
    {
      gg=ggplot(indiv,aes(x=x,y=y,color=product,label=name))+   geom_text()+     theme_bw()+     geom_hline(yintercept=0)+     geom_vline(xintercept=0)
    }
    if(!text)
    {
      gg=ggplot(indiv,aes(x=x,y=y,color=product,label=name))+   geom_point()+     theme_bw()+     geom_hline(yintercept=0)+     geom_vline(xintercept=0)
    }

      if("ell"%in%indSup)
      {
        indivEll=respcagg$indivEllipsesCoord[respcagg$indivEllipsesCoord[,"axes"]==paste(axes[1],axes[2],sep=","),]
        gg=gg+geom_path(data = indivEll,aes(x=x,y=y,name=name,color=name))

      }
       if("points"%in%indSup)
      {
         indsupdat=respcagg$indSup
         indsupdat[,"name"]=paste0(indsupdat[,"subject"],"_",indsupdat[,"product"])
         gg=gg+geom_point(data=indsupdat,aes(x=x,y=y,color=product,name=name),size=1)
      }

     pct_x=100*respcagg$eigenValues[axes[1]]/sum(respcagg$eigenValues)
     pct_y=100*respcagg$eigenValues[axes[2]]/sum(respcagg$eigenValues)
     xlab=paste0("Axis",axes[1]," (",round(pct_x,digits=2),"%)")
     ylab=paste0("Axis",axes[2]," (",round(pct_y,digits=2),"%)")
     gg=gg+xlab(xlab)+ylab(ylab) +ggtitle(paste0("Individual map (", round(pct_x+pct_y,digits=2),"%)"))
    return(gg)
  }

  if(type=="var")
  {
    circle <- data.frame( x0 =0,  y0 = 0,  r =1)

    if(text)
    {
      indiv=respcagg$varCoord[respcagg$varCoord[,"axes"]==paste(axes[1],axes[2],sep=","),]
      gg=ggplot(indiv,aes(x=x,y=y,name=name,label=name))+
        geom_text_repel()+
        geom_point()+
        theme_bw()+
        geom_hline(yintercept=0)+
        geom_vline(xintercept=0)+ggforce::geom_circle(data=circle,aes(x0=x0,y0=y0,r=r),color="grey",inherit.aes=FALSE)+ggtitle("Variable map")
    }
    if(!text)
    {
      indiv=respcagg$varCoord[respcagg$varCoord[,"axes"]==paste(axes[1],axes[2],sep=","),]
      gg=ggplot(indiv,aes(x=x,y=y,name=name,label=name))+
        geom_point()+
        theme_bw()+
        geom_hline(yintercept=0)+
        geom_vline(xintercept=0)+ggforce::geom_circle(data=circle,aes(x0=x0,y0=y0,r=r),color="grey",inherit.aes=FALSE)+ggtitle("Variable map")
    }
    return(gg)
  }

  if(type=="cor1")
  {
    varcor=respcagg$varCoord[respcagg$varCoord[,"axes"]==paste(axes[1],axes[2],sep=","),]
    varcor_order1=varcor[order(abs(varcor[,"x"]),decreasing=F),]
    varcor_order1[,"name"]=factor(varcor_order1[,"name"],levels=varcor_order1[,"name"])
    gg3=ggplot2::ggplot(data=tail(varcor_order1,n=n),ggplot2::aes(x = x, y = name))+ggplot2::geom_col()+ggplot2::ggtitle(paste0("Best correlations with axis ",axes[1]))+ggplot2::theme_bw()
    return(gg3)
  }
  if(type=="cor2")
  {
    varcor=respcagg$varCoord[respcagg$varCoord[,"axes"]==paste(axes[1],axes[2],sep=","),]
    varcor_order2=varcor[order(abs(varcor[,"y"]),decreasing=F),]
    varcor_order2[,"name"]=factor(varcor_order2[,"name"],levels=varcor_order2[,"name"])
    gg4=ggplot2::ggplot(data=tail(varcor_order2,n=n),ggplot2::aes(x = y, y = name))+ggplot2::geom_col()+ggplot2::ggtitle(paste0("Best correlations with axis ",axes[2]))+ggplot2::theme_bw()
    return(gg4)
  }
  if(type=="eigenValues")
  {
    dfev=data.frame(x=1:length(respcagg$eigenValues),eigen=respcagg$eigenValues)
    p=ggplot(dfev,aes(x=x,y=eigen))+geom_line()+geom_point()+theme_bw()
    return(p)
  }
}
