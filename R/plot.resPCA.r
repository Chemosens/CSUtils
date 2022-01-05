#'@title plot.resPCA
#' @param axes vector containing the two axes to be plotted. Default to c(1,2)
#' @param respca result of a PCA function. See \link{PCA}
#' @param type ind or var 
plot.resPCA=function(respca,type="ind",axes=c(1,2))
{
  x=y=name=product=NULL
  if(type=="ind")
  {
    df=data.frame(name=rownames(respca$IndivCoord),x=respca$IndivCoord[,axes[1]],y=respca$IndivCoord[,axes[2]])
    coordIndSup=as.matrix(respca$IndSup[,-which(colnames(respca$IndSup)%in%c("product","subject"))])%*%respca$EigenVectors[,1:2]
    dfindsup=as.data.frame(coordIndSup);colnames(dfindsup)=c("x","y")
    dfindsup[,"subject"]=respca$IndSup[,"subject"]
    dfindsup[,"product"]=respca$IndSup[,"product"]
    calc_ell=calculateEllipses(respca$IndSup,respca$EigenVectors,axes=axes,confInt=0.9,ellipseType="barycentric",productName="product",subjectName="subject",ellipseCalculation="Chi",bootstrap=FALSE,nSamples=100)
    dfell=calc_ell$ellPoints
    colnames(dfell)=c("x","y","name"); dfell[,"axes"]=paste(axes[1],axes[2],sep=",");
    p=ggplot(df,aes(x=x,y=y,label=name,color=name))+geom_point(size=3)+theme_bw()
    p=p+geom_point(data=dfindsup,aes(x=x,y=y,color=product),inherit.aes = FALSE )
    p=p+geom_path(data=dfell,aes(x=x,y=y,color=name),inherit.aes=FALSE)
    p=p+geom_vline(xintercept=0)+geom_hline(yintercept=0)
  }
  if(type=="var")
  {
    circleFun <- function(center = c(0,0),diameter = 2, npoints = 100){
      r = diameter / 2
      tt <- seq(0,2*pi,length.out = npoints)
      xx <- center[1] + r * cos(tt)
      yy <- center[2] + r * sin(tt)
      return(data.frame(x = xx, y = yy))
    }
    
    dat <- circleFun()
    #geom_path will do open circles, geom_polygon will do filled circles
    df=data.frame(name=rownames(respca$VarCoord),x=respca$VarCoord[,axes[1]],y=respca$VarCoord[,axes[2]])
    p=ggplot(data=df,aes(x=x,y=y,label=name))+geom_point()+theme_bw()+geom_text()+geom_vline(xintercept=0)+geom_hline(yintercept=0)
    p=p+ geom_path(data=dat,aes(x,y),inherit.aes = FALSE)
   }
  type="both"
  return(p)
}

