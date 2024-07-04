#' PCA
#' Performs a Principal Component Analysis
#' @param extendedData an extended dataframe with "subject", "product" as two first columns then a column by variable (descriptor)
#' @param option "Covariance" or "Correlations".
#' @param representation "DistanceBiplot","CorrelationBiplot" or "TwoMaps".
#' @param dataType "productMeans" or "raw"
#' @return A list contaning the PCA results: IndivCoord, VarCoord,IndSup, EigenVectors,EigenValues
#' @export
#' @examples
#' {
#' df=data.frame(product=rep(c("a","b","c"),each=10),
#' subject=rep(paste0("S",1:10),3),acide=rnorm(30),amer=rnorm(30))
#'  respca=PCA(df)
#' }
#' @importFrom stats cor aggregate sd
PCA=function(extendedData,dataType="productMeans", option="covariance", representation="DistanceBiplot")
{
  match.arg(option, c("covariance","correlation","Covariance","Correlation"))
  match.arg(representation,c("DistanceBiplot","distanceBiplot","twoMaps","TwoMaps"))
  match.arg(dataType,c("productMeans","raw"))
  adaptedRownames=function(extendedData)
  {
    if("subject"%in% colnames(extendedData))
    {
      if(length(unique(extendedData[,"subject"]))>1)
      { differentsSujets=TRUE }else{differentsSujets=FALSE}
    }else{differentsSujets=FALSE}


    if("rep"%in% colnames(extendedData))
    {
      if(length(unique(extendedData[,"rep"]))>1)
      {
        differentesRep=TRUE
      }else{differentesRep=FALSE}
    }else{differentesRep=FALSE}

    if(differentsSujets&differentesRep)
    {
      newRownames=paste(extendedData[,"product"],extendedData[,"subject"],extendedData[,"rep"],sep="_")
    }
    if(differentsSujets&!differentesRep)
    {
      newRownames=paste(extendedData[,"product"],extendedData[,"subject"],sep="_")
    }
    if(differentesRep&!differentsSujets)
    {
      newRownames=paste(extendedData[,"product"],extendedData[,"rep"],sep="_")
    }
    if(!differentesRep&!differentsSujets)
    {
      newRownames=extendedData[,"product"]
    }


    return(list(differentsSujets=differentsSujets,differentesRep=differentesRep,newRownames=newRownames))
  }
	scaleUnit=FALSE
  if(option=="correlation"||option=="Correlation") {
		scaleUnit=TRUE
	}
  if(option=="covariance"||option=="Covariance") {
		scaleUnit=FALSE
  }

  attributs=colnames(extendedData)[-which(colnames(extendedData)%in%c("product","subject","rep"))]
  nbAttributes=length(attributs)
	if(nbAttributes < 2) 	{
	  stop("Insufficient number of attributes.")
	}

	# Obtention de matBmod, la matrice à travailler et des valeurs manquantes

	if(dataType=="productMeans")
	{
	  CenteredProductMeansTable=aggregate(.~product,extendedData[,c("product",attributs)],function(x){mean(x,na.rm=TRUE)})
	  meanByAttribute=apply(as.matrix(CenteredProductMeansTable[,attributs]),2,function(x){mean(x,na.rm=T)})
	  sdByAttributes=apply(as.matrix(CenteredProductMeansTable[,attributs]),2,function(x){sd(x,na.rm=T)})
	  CenteredProductMeansTable[,attributs]=apply(CenteredProductMeansTable[,attributs],2,"scale",center=TRUE,scale=scaleUnit)
	  matBmod=as.matrix(CenteredProductMeansTable[,attributs])
	  rownames(matBmod)=CenteredProductMeansTable[,"product"]
	}
	if(dataType=="raw")
	{
	 	 	if (length(which(is.na(as.matrix(extendedData[,attributs])))>0))
	  {

	    extdat=apply(as.matrix(extendedData[,attributs]),2,as.numeric)
	    warning("NA in the PCA matrix. Missing values are imputed by the mean of the variable: you should use the imputePCA function of the missMDA package")
	    avgTable=matrix(apply(extdat,2,function(x){return(mean(as.numeric(x),na.rm=T))}),nrow(extdat),ncol(extdat),byrow=TRUE)
	    extendedData[,attributs][is.na(extdat)]=avgTable[is.na(extdat)]
	  }

	  CenteredProductMeansTable=apply(as.matrix(extendedData[,attributs]),2,"scale",center=TRUE,scale=scaleUnit)
	  matBmod=CenteredProductMeansTable
    newRownames=adaptedRownames(extendedData)$newRownames
	  rownames(matBmod)=newRownames
	}




	# Calcul ACP
	matSvd=svd(matBmod)

	# Valeurs et vecteurs propres


	# Matrice des individus et scoreName de l'ACP
	if(representation=="twoMaps"||representation=="TwoMaps")
	{
	  eigVal=(matSvd$d)^2
	  eigVec=matSvd$v
	  individuals=matBmod%*%eigVec
	  variables=cor(matBmod,individuals)
	  varCor=cor(matBmod,individuals)
	}
	if(representation=="DistanceBiplot"||representation=="distanceBiplot")
	{
	  eigVal=(matSvd$d)^2
	  U=(matSvd$u)
	  D=diag(matSvd$d)
	  eigVec=matSvd$v
	  ud=U%*%D
	  individuals=ud
	  variables=eigVec # loadings
	  #	  title=paste(title," - ","Distance Biplot",sep="")
	  biplot=TRUE
	  varCor=cor(matBmod,individuals)
	}
	# if(representation=="CorrelationBiplot"||representation=="correlationBiplot")
	# {
	#   matsvd=svd(as.matrix(res.PCA$B))
	#   U=(matsvd$u)
	#   D=diag(matsvd$d)
	#   V=matsvd$v
	#   dv=(D)%*%t(V)
	#   u=U
	#   individuals=U
	#   variables=dv
	#   #	  title=paste(title," - ","Correlation Biplot",sep="")
	#   biplot=TRUE
	# }

  rownames(individuals)=rownames(matBmod)
	rownames(variables)=colnames(matBmod)


	# Dimensions significatives : contributions > 1/nb variables
	if(option=="Correlation"||option=="correlation")
	{
	  p=1/nbAttributes
	  contrib=eigVal/sum(eigVal)
	  NbDimSig=length(contrib[contrib>p])
	}
	if(option=="Covariance"||option=="covariance")
	{
	  NbDimSig=NULL
	}

	suppIndividuals=NULL

	# Individus supplementaires
	if(dataType=="productMeans")
	{

	  # On recupere la table moyennee produit*sujet : possible seulement on a des sujets
	 	 if("subject" %in% colnames(extendedData))
	   {
	    if("rep" %in% colnames(extendedData) )
	    {
	      if(length(unique(extendedData[,"rep"]))>1)
	      {
	        CenteredProductSubjectMeanTable=aggregate(.~product+subject,extendedData,FUN=function(x){return(mean(x,na.rm=T))})
	        CenteredProductSubjectMeanTable=CenteredProductSubjectMeanTable[,-which(colnames(CenteredProductSubjectMeanTable)=="rep")]
	      }
	      else
	      {
	        CenteredProductSubjectMeanTable=extendedData
	      }
	    }
	    else
	    {
	      CenteredProductSubjectMeanTable=extendedData
	    }

	 	 # On soustrait de cette table la moyenne des produits

	    #	CenteredProductSubjectMeanTable[,attributs]=apply(CenteredProductSubjectMeanTable[,attributs],2,"scale",center=TRUE,scale=FALSE)

	    CenteredProductSubjectMeanTable[,attributs]=sweep(as.matrix(CenteredProductSubjectMeanTable[,attributs]),2,meanByAttribute,"-")
	    suppIndividuals=CenteredProductSubjectMeanTable

	    if(option=="correlation"||option=="Correlation")
	   {
	      suppIndividuals[,attributs]=sweep(CenteredProductSubjectMeanTable[,attributs],2,sdByAttributes,"/")

	     }
	  }

	}

	resPca=list(B=matBmod,IndivCoord=individuals,VarCoord=variables,VarCor=varCor,IndSup=suppIndividuals,EigenVectors=eigVec,EigenValues=eigVal,option=option,representation=representation,dataType=dataType)

	return(resPca)
}