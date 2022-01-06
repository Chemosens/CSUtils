#' @title CVAgg
#' Calculates the CVAs (potentially by period of time) and returns coordinates as long data.frames to make easier ggplot
#' @param df a long dataframe with subject, product, descriptor, score, rep and period
#' @return listCoord
#' @inheritParams CVA
#' @inheritParams calculateEllipses
#' @export
#' @importFrom stats as.formula
CVAgg=function(df,confInt=0.9,axes=list(c(1,2)),option="TwoWayANOVA",  representation="DistanceBiplot",ellipseType="barycentric",productName="product",subjectName="subject",ellipseCalculation="Chi",bootstrap=FALSE,nSamples=100,nbDimHotelling=NULL)
{

  formula="subject+product+rep~descriptor"
  df$period="1"
  formula=as.formula(formula)
  listCoord=list()
  indivCoord=NULL
  varCoord=NULL
  inertia=NULL
  indivEllipsesCoord=NULL

  extendedData=reshape2::dcast(df, formula=formula,fun.aggregate=mean,value.var="score")
  resCva=CVA(extendedData, option=option,  representation=representation,nbDimHotelling=nbDimHotelling)

  for (i in 1:length(axes) )
  {
      axes_tmp=axes[[i]]
        # ellipse calculation
       dfind=resCva$IndivCoord[,axes_tmp];dfind=as.data.frame(dfind);colnames(dfind)=c("x","y");dfind[,"name"]=rownames(resCva$IndivCoord); dfind[,"axes"]=paste(axes_tmp[1],axes_tmp[2],sep=",");
      dfvar=resCva$VarCoord[,axes_tmp];dfvar=as.data.frame(dfvar);colnames(dfvar)=c("x","y");dfvar[,"name"]=rownames(resCva$VarCoord); dfvar[,"axes"]=paste(axes_tmp[1],axes_tmp[2],sep=",");
      dfell=calculateEllipses(resCva$IndSup,resCva$EigenVectors,axes=axes_tmp,confInt=confInt,ellipseType=ellipseType,productName="product",subjectName="subject",ellipseCalculation=ellipseCalculation,bootstrap=bootstrap,nSamples=nSamples)$ellPoints
      colnames(dfell)=c("x","y","name"); dfell[,"axes"]=paste(axes_tmp[1],axes_tmp[2],sep=",");
      indivCoord=rbind(indivCoord,dfind);
      max.norm.prod=max(dfind[,"x"])
      max.norm.suj=max(dfvar[,"y"])
      expand.conseil=max.norm.prod/max.norm.suj
      dfvar[,"x"]=expand.conseil*dfvar[,"x"];dfvar[,"y"]=expand.conseil*dfvar[,"y"]
      varCoord=rbind(varCoord,dfvar)
      indivEllipsesCoord=rbind(indivEllipsesCoord,dfell);
      dfinertia=data.frame(x=round(100*resCva$EigenValues[axes_tmp[1]]/sum(resCva$EigenValues),digits=0),y=round(100*resCva$EigenValues[axes_tmp[2]]/sum(resCva$EigenValues),digits=0),axes=paste(axes_tmp[1],axes_tmp[2],sep=","))
      inertia=rbind(inertia,dfinertia)
    }
#  }
  listCoord$indivCoord=indivCoord;
  listCoord$varCoord=varCoord
  listCoord$indivEllipsesCoord=indivEllipsesCoord;
  listCoord$inertia=inertia
  listCoord$eigenValues=resCva$EigenValues
  listCoord$option="Covariance"

  listCoord$hotelling=resCva$HotellingTable$hotellingTable
  listCoord$nbDimSig=resCva$NbDimSig
  listCoord$nbDimHotelling=nbDimHotelling
  return(listCoord)
}