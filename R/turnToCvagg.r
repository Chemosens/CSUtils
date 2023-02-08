#' @title turnToPCAgg
#' Return a pcagg object from a PCA object
#' @param resCva result of PCA function
#' @return listCoord
#' @inheritParams PCA
#' @inheritParams calculateEllipses
#' @param expandBiplot numeric that allows to custumize the size of variables arrows. If NULL, this parameter is optimized according to the graph.
#' @export
turnToCVAgg=function(resCva,axes=list(c(1,2)),representation="DistanceBiplot",expandBiplot=NULL,confInt=0.9,ellipseType="barycentric",ellipseCalculation="Chi",bootstrap=FALSE,nSamples=100)
{
  listCoord=list()
  indivCoord=varCor=NULL
  varCoord=NULL
  inertia=NULL
  indivEllipsesCoord=NULL
  indSupCoord=NULL
  for(i in 1:length(axes))
  {
    axes_tmp=axes[[i]]
    dfind=resCva$IndivCoord[,axes_tmp];
    dfind=as.data.frame(dfind);colnames(dfind)=c("x","y");dfind[,"name"]=rownames(resCva$IndivCoord); dfind[,"axes"]=paste(axes_tmp[1],axes_tmp[2],sep=",");
    dfvar=resCva$VarCoord[,axes_tmp];dfvar=as.data.frame(dfvar);colnames(dfvar)=c("x","y");dfvar[,"name"]=rownames(resCva$VarCoord); dfvar[,"axes"]=paste(axes_tmp[1],axes_tmp[2],sep=",");
    dfvarcor=resCva$VarCor[,axes_tmp];dfvarcor=as.data.frame(dfvarcor);colnames(dfvarcor)=c("x","y");dfvarcor[,"name"]=rownames(resCva$VarCor); dfvarcor[,"axes"]=paste(axes_tmp[1],axes_tmp[2],sep=",");
  #  if(dataType=="productMeans")
  #  {
      if(!is.null(resCva$IndSup))
      {

        resell=calculateEllipses(resCva$IndSup,resCva$EigenVectors,axes=axes_tmp,confInt=confInt,ellipseType=ellipseType,productName="product",subjectName="subject",ellipseCalculation=ellipseCalculation,bootstrap=bootstrap,nSamples=nSamples)
        dfell=resell$ellPoints
        dfindivSup=resell$indivCoord
        colnames(dfell)=c("x","y","name"); dfell[,"axes"]=paste(axes_tmp[1],axes_tmp[2],sep=",");
        colnames(dfindivSup)=c("x","y","subject","product","axes")
      }
  #  }
    if(representation=="DistanceBiplot")
    {

      if(is.null(expandBiplot))
      {
        max.norm.prod=max(dfind[,"x"])
        max.norm.suj=max(dfvar[,"x"])
        expand.conseil=max.norm.prod/max.norm.suj
      }
      else{expand.conseil=expandBiplot}

      dfvar[,"x"]=expand.conseil*dfvar[,"x"];
      dfvar[,"y"]=expand.conseil*dfvar[,"y"]
    }

    indivCoord=rbind(indivCoord,dfind);
    varCoord=rbind(varCoord,dfvar)
    varCor=rbind(varCor,dfvarcor)
    if(!is.null(resCva$IndSup))
    {
      indivEllipsesCoord=rbind(indivEllipsesCoord,dfell);
      indSupCoord=rbind(indSupCoord,dfindivSup)
    }
    dfinertia=data.frame(x=100*round(resCva$EigenValues[axes_tmp[1]]/sum(resCva$EigenValues),digits=4),y=100*round(resCva$EigenValues[axes_tmp[2]]/sum(resCva$EigenValues),digits=4),axes=paste(axes_tmp[1],axes_tmp[2],sep=","))
    inertia=rbind(inertia,dfinertia)
  }

  listCoord$indivCoord=indivCoord;
  listCoord$varCoord=varCoord;
  listCoord$varCor=varCor
  if(!is.null(resCva$IndSup))
  {
    listCoord$indivEllipsesCoord=indivEllipsesCoord;
    listCoord$indSup=indSupCoord;
  }else{listCoord$indivEllipsesCoord=NULL;listCoord$indSup=NULL}
  listCoord$inertia=inertia;
  listCoord$option=resCva$option
  listCoord$eigenValues=resCva$EigenValues
  listCoord$representation=resCva$representation
  listCoord$stats=resCva$Stats
  listCoord$eigenVectors=resCva$EigenVectors
  listCoord$hotelling=resCva$HotellingTable$hotellingTable
  listCoord$nbDimSig=resCva$NbDimSig
  return(listCoord)
}