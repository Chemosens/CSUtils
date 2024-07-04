#' PCAgg
#' Performs a PCA from a long dataset (potentially by rep/period or rep and period)
#' @param df with subject, product, descriptor, score, rep and period
#' @param value.var Name of the score column ("score" by default)
#' @param variable Name of the variable column ("descriptor" by default)
#' @param expandBiplot numeric that allows to custumize the size of variables arrows. If NULL, this parameter is optimized according to the graph.
#' @return listCoord
#' @inheritParams PCA
#' @inheritParams calculateEllipses
#' @export
PCAgg=function(df,dataType="productMeans",option="Covariance",representation="DistanceBiplot",value.var="score",variable="descriptor",axes=list(c(1,2)),expandBiplot=NULL,confInt=0.9,ellipseType="barycentric",ellipseCalculation="Chi",bootstrap=FALSE,nSamples=100)
{
  match.arg(option,c("Covariance","Correlation"))
  match.arg(dataType,c("productMeans","raw"))
  if(dataType=="productMeans")
  {
    df$rep="1"
    df$period="1"
    formula=paste0("subject+product~",variable)
    formula=as.formula(formula)
  }
  if(dataType=="raw")
  {
    df$period="1"
    formula=paste0("subject+product+rep~",variable)
    formula=as.formula(formula)
  }
 # parameters=list(...)
 # axes=parameters$axes

 # if(is.null(parameters$option)){option="Covariance"}
#  for (r in unique(df$rep)) {
#    for (p in unique(df$period))
#    {


      #    if("rep"%in%colnames(extendedData)){extendedData=extendedData[,-which(colnames(extendedData)=="rep")]}
   #   if("period"%in%colnames(extendedData)){extendedData=extendedData[,-which(colnames(extendedData)=="period")]}
  extendedData=reshape2::dcast(df, formula=formula,function(x){return(mean(x,na.rm=T))},value.var=value.var)
  if(dataType=="productMeans")
  {
       resPca=PCA(extendedData,dataType="productMeans", option=option, representation=representation)
  }
  if(dataType=="raw")
  {
    resPca=PCA(extendedData,option=option,representation=representation,dataType="raw")
  }

  listCoord=turnToPCAgg(resPca,axes=axes,representation=representation,expandBiplot=expandBiplot,confInt=confInt,ellipseType=ellipseType,ellipseCalculation=ellipseCalculation,bootstrap=bootstrap,nSamples=nSamples)

  return(listCoord)
}