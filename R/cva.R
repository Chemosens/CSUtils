#' CVA
#'
#' Canonical Variate Analysis for sensory data
#'
#' @param extendedData an extended dataframe whose first three columns are "subject", "product" and "rep". All the other columns are variables (as many columns as descriptors for sensory analysis)
#' @param test "Pillai", "Wilks", "Hotelling-Lawley" or "Roy". Default to "Hotelling-Lawley". The name of the test statistic to be used. Partial matching is used so the name can be abbreviated.
#' @param option "OneWayANOVA","TwoWayANOVA","MAM","MultiMAM" or "TwoWayWithoutTakingSubjectEffectIntoEllipses". The type of model chosen for the CVA
#' @param representation "Biplot" or "TwoMaps"
#' @param nbDimHotelling If NULL, nbDimSig,
#' @return a list containing IndivCoord,VarCoord,IndSup,EigenVectors, EigenValues
#' @importFrom stats pchisq lm manova cor reshape aggregate  sd
#' @importFrom utils combn
#' @export
CVA=function(extendedData, test="Hotelling-Lawley",nbDimHotelling=NULL,option="TwoWayANOVA", representation="distanceBiplot") {

#  extendedData=reshape2::dcast(df, Subject+Product+Rep~Attribute,mean)
  match.arg(representation,c("DistanceBiplot","distanceBiplot","twoMaps","TwoMaps"))
  getNumberOfSignificantDimensionsOfCVA=function(eigVal,I,ddlW,p,alpha=0.05)
  {
    # I: product number
    # p: attribute number
    significance=TRUE
    n=length(eigVal)
    k=0 # le rang de la matrice
    K=min(p,I)
    while(significance==TRUE)
    {
      chi2=ddlW*sum(eigVal[(k+1):K],na.rm=TRUE)
      df=(p-k)*(I-k-1)
      if (df<1)
      {
        return(k)
      }
      proba=pchisq(chi2,df=df,lower.tail=FALSE)
      if(proba>alpha){significance=FALSE}
      k=k+1
    }
    return(k-1)
  }
  if(!(option%in%c("OneWayANOVA","TwoWayANOVA","MAM","MultiMAM","TwoWayWithoutTakingSubjectEffectIntoEllipses","OneWayANOVAMod")))
  {
    stop("Option does not exist. Please choose between OneWayANOVA,Two-Way ANOVA, MAM or MultiMAM.")
  }
  if(!"rep"%in%colnames(extendedData)){print("No rep in dataset, a rep column was added");extendedData[,"rep"]=1}

  attributes=unique(as.character(colnames(extendedData[,-which(colnames(extendedData)%in%c("product","subject","rep"))])))
  replicates=unique(extendedData[,"rep"])
  nbReplicates=length(replicates)
  nbAttributes=length(attributes)
  products=unique(as.character(extendedData[,"product"]))
  subjects=unique(as.character(extendedData[,"subject"]))
  nbSubjects=length(subjects)
  nbProducts=length(products)

  wDemi=NULL
  if(nbAttributes < 2)
  {
    stop("Insufficient number of attributes.")
  }
  if((nbSubjects-1)*(nbProducts-1) < nbAttributes)
  {
    stop("Insufficient number of individuals.")
  }
  # Matrice produit en ligne et attributs en colonnes
  CenteredProductTable=aggregate(.~product,extendedData[,c("product",attributes)],mean,na.rm=TRUE)
  CenteredProductTable[,attributes]=apply(CenteredProductTable[,attributes],2,"scale",center=TRUE,scale=FALSE)
  matrixOfCenteredProduct=as.matrix(CenteredProductTable[,attributes])
  rownames(matrixOfCenteredProduct)=CenteredProductTable[,"product"]

  # matrice attributs en colonne, produit*sujet en ligne
  CenteredProductSubjectTable=aggregate(.~product+subject,extendedData[,c("product","subject",attributes)],mean,na.rm=TRUE)
  CenteredProductSubjectTable[,attributes]=apply(CenteredProductSubjectTable[,attributes],2,"scale",center=TRUE,scale=FALSE)
  matrixOfCenteredProductSubject=as.matrix(CenteredProductSubjectTable[,attributes])
  rownames(matrixOfCenteredProductSubject)=paste(CenteredProductSubjectTable[,"product"],CenteredProductSubjectTable[,"subject"],sep="")

  # matrice attributs en colonne, sujet en ligne
  CenteredSubjectTable=aggregate(.~subject,extendedData[,c("subject",attributes)],mean,na.rm=TRUE)
  CenteredSubjectTable[,attributes]=apply(CenteredSubjectTable[,attributes],2,"scale",center=TRUE,scale=FALSE)
  matrixOfCenteredSubject=as.matrix(CenteredSubjectTable[,attributes])
  rownames(matrixOfCenteredSubject)=CenteredSubjectTable[,"subject"]

  decomposition=NULL
  statTest=NULL
  statF=NULL
  statPval=NULL

  # Choose the MANOVA model
  if(option=="OneWayANOVAMod")
  {
    SSProd=t(matrixOfCenteredProduct)%*%matrixOfCenteredProduct
    SSTotal=t(matrixOfCenteredProductSubject)%*%matrixOfCenteredProductSubject
    SSres=SSTotal-SSProd

    if(sum(!is.na(SSres))==0){
      stop("Your dataset is probably not complete. Please complete it, or run a PCA instead of CVA to obtain a map.")
    }

    Y=as.matrix(CenteredProductSubjectTable[,attributes])
    Product=as.factor(CenteredProductSubjectTable[,"product"])
    Subject=as.factor(CenteredProductSubjectTable[,"subject"])
    if(qr(lm(Y~Product)$residuals)$rank<dim(Y)[2])  {
      statTest=NA
      statF=NA
      statPval=NA
      stop("Not enough observations compared to the number of attributes or your dataset is not balanced enough.")
    } else   {
      performances=summary(manova(lm(Y~Product)),test=test)$stats
      statTest=performances[1,2]
      statF=performances[1,3]
      statPval=performances[1,6]
    }
  }
  if(option=="OneWayANOVA")
  {
    centeredData=scale(extendedData[, attributes],scale=F)
    SSProd = nbSubjects *nbReplicates* t(matrixOfCenteredProduct) %*% matrixOfCenteredProduct
    SSTotal = t(centeredData) %*% centeredData
    SSres = SSTotal - SSProd
    if (det(SSres) <= 2.2e-16) {
      L = list()
      L[[1]] = det(SSres)
      names(L) = c("Det")
      return(L)
      stop("Non invertible matrix")
    }
    Y = as.matrix(centeredData)
    Product = as.factor(extendedData[, "product"])
    performances = summary(manova(lm(Y ~ Product)), test = test)$stats
    statTest = performances[1, 2]
    statF = performances[1, 3]
    statPval = performances[1, 6]
  }
  if(option=="TwoWayANOVA")
  {
      SSProd=nbSubjects*t(matrixOfCenteredProduct)%*%matrixOfCenteredProduct
      SSTotal=t(matrixOfCenteredProductSubject)%*%matrixOfCenteredProductSubject
      SSsujet=nbProducts*t(matrixOfCenteredSubject)%*%matrixOfCenteredSubject
      SSres=SSTotal-SSsujet-SSProd
      dataExtended=extendedData[,c("subject","product","rep",attributes)]
      tryCatch({
        resPerf=PanelPerformances(frame=dataExtended,modelType="classic",negativeCorrection=FALSE,correctOnlyIfSignificant=FALSE,limitOfSignificance=0.05,onlySignificantDim=FALSE,manovaTest="Hotelling")
        decomposition=resPerf$decomposition
        performances=resPerf$multiPanelPerformances
        statTest=performances["discrimination","stat"]
        statF=performances["discrimination","f"]
        statPval=performances["discrimination","pvalue"]
      })
  }
  if(option=="MAM")
  {
      SSProd=nbReplicates*nbSubjects*t(matrixOfCenteredProduct)%*%matrixOfCenteredProduct
      dataExtended=extendedData[,c("subject","product","rep",attributes)]
      resMAM=PanelPerformances(frame=dataExtended,modelType="mam",negativeCorrection=FALSE,correctOnlyIfSignificant=FALSE,limitOfSignificance=0.05,onlySignificantDim=FALSE,manovaTest="Hotelling")
      SSres=resMAM$matW
      decomposition=resMAM$decomposition
      performances=resMAM$multiPanelPerformances
      statTest=performances["discrimination","stat"]
      statF=performances["discrimination","f"]
      statPval=performances["discrimination","pvalue"]
  }
  if(option=="MultiMAM")
  {
      SSProd=nbReplicates*nbSubjects*t(matrixOfCenteredProduct)%*%matrixOfCenteredProduct
      dataExtended=extendedData[,c("subject","product","rep",attributes)]
      resMAM=PanelPerformances(frame=dataExtended,modelType="overall",negativeCorrection=FALSE,correctOnlyIfSignificant=FALSE,limitOfSignificance=0.05,onlySignificantDim=FALSE,manovaTest="Hotelling")
      SSres=resMAM$matW
      decomposition=resMAM$decomposition
      performances=resMAM$multiPanelPerformances
      statTest=performances["discrimination","stat"]
      statF=performances["discrimination","f"]
      statPval=performances["discrimination","pvalue"]
  }
  if(option=="TwoWayWithoutTakingSubjectEffectIntoEllipses")
  {

      SSProd=nbSubjects*t(matrixOfCenteredProduct)%*%matrixOfCenteredProduct
      SSTotal=t(matrixOfCenteredProductSubject)%*%matrixOfCenteredProductSubject
      SSsujet=nbProducts*t(matrixOfCenteredSubject)%*%matrixOfCenteredSubject
      SSres=SSTotal-SSsujet-SSProd
      dataExtended=dataExtended[,c("subject","product","rep",attributes)]
      resPerf=PanelPerformances(frame=dataExtended,modelType="classic",manovaTest="Hotelling",onlySignificantDim=FALSE)
      decomposition=resPerf$decomposition
      performances=resPerf$multiPanelPerformances
      statTest=performances["discrimination","stat"]
      statF=performances["discrimination","f"]
      statPval=performances["discrimination","pvalue"]
  }



  if(is.list(option))
  {
    SSProd=option[[1]]
    SSres=option[[2]]
  }

  tryCatch(solve(SSres),error=function(e){
    stop("Non inversible matrix.")
  })

  # Matrice non inversible
  if(det(SSres)<=2.2e-16)
  {
    L=list()
    L[[1]]=det(SSres)
    names(L)=c("Det")
    stop("[TS]Non inversible matrix")
    return(L)

  }

  ConditioningOfW=sqrt(max(Re(eigen(SSres)$values))/min(Re(eigen(SSres)$values)))

  # Eigenvalues and eigenvectors
  discriminationMatrix=solve(SSres)%*%SSProd
  eigVal=Re(eigen(discriminationMatrix)$values)
  vecOrtho=Re(eigen(discriminationMatrix)$vectors)
  rownames(vecOrtho)=rownames(SSProd)

  # Deduction du nombre de dimensions significatives
  if(option=="TwoWayANOVA"||option=="TwoWayWithoutTakingSubjectEffectIntoEllipses")
  {
    nbDimSig=getNumberOfSignificantDimensionsOfCVA(eigVal=eigVal,I=nbProducts,ddlW=(nbProducts-1)*(nbSubjects-1),p=nbAttributes,alpha=0.05)

  }
  if(option=="OneWayANOVAMod")
  {
    nbDimSig=getNumberOfSignificantDimensionsOfCVA(eigVal=eigVal, I=nbProducts,ddlW=nbSubjects*(nbProducts-1),p=nbAttributes,alpha=0.05)
  }
  if(option=="OneWayANOVA")
  {
    nbDimSig=getNumberOfSignificantDimensionsOfCVA(eigVal=eigVal, I=nbProducts,ddlW=nbSubjects*nbProducts*nbReplicates-nbProducts-1,p=nbAttributes,alpha=0.05)
  }
  if(option=="MultiMAM")
  {
    nbDimSig=getNumberOfSignificantDimensionsOfCVA(eigVal=eigVal, I=nbProducts,ddlW=(nbSubjects-1)*(nbProducts-2),p=nbAttributes,alpha=0.05)
  }
  if(option=="MAM")
  {
    nbDimSig=getNumberOfSignificantDimensionsOfCVA(eigVal=eigVal, I=nbProducts,ddlW=(nbSubjects-1)*(nbProducts-2),p=nbAttributes,alpha=0.05)
  }

  if(is.null(nbDimHotelling)){nbDimHotelling=nbDimSig}

  nbAxes=min(nbProducts-1,nbAttributes-1)

  # Normalization of eigenvectors
  normW=function(v,W) {
    return(sqrt(t(v)%*%W%*%v))
  }

  norms=apply(vecOrtho,2,normW,SSres)
  eigVec=vecOrtho
  for(i in 1:ncol(eigVec))  {
    eigVec[,i]=vecOrtho[,i]/norms[i]
  }
  # les vecteurs sont a present W-orthonorm?s

  # Getting individuals and scoreName
  individuals=matrixOfCenteredProduct%*%eigVec
  rownames(individuals)=rownames(matrixOfCenteredProduct)
  varCor=cor(matrixOfCenteredProduct,individuals)

  # Transformation des individus et scoreName pour le biplot
  biplot=FALSE

  rootAndVap=function(matSymDefPos) {
    eig=eigen(matSymDefPos)
    rac=eig$vectors%*%sqrt(diag(eig$values))%*%t((eig$vectors))
    return(rac)
  }

  wDemi=rootAndVap(SSres)

  if(representation %in% c("distanceBiplot","DistanceBiplot","biplot","Biplot"))
  {
    # Biplot CVA
    # Matrice Y pour le biplot

    yMat=matrixOfCenteredProduct%*%solve(wDemi) #XL in gower
    individuals=matrix(NA,nbProducts,nbAxes)
    rownames(individuals)=rownames(matrixOfCenteredProduct)
    variables=matrix(NA,nbAttributes,nbAxes)
    rownames(variables)=colnames(matrixOfCenteredProduct)
    matsvd=svd(yMat)
    U=(matsvd$u)
    D=diag(matsvd$d)
    V=matsvd$v
    # les vecteurs obtenus sont de norme 1, il faut les W-orthonormer.
    eigVec=V
    ud=U%*%D
    individuals[,1:nbAxes]=ud[,1:nbAxes]
    variables[,1:nbAxes]=V[,1:nbAxes] # ce sont les loadings
    CenteredProductSubjectTable[,3:dim(CenteredProductSubjectTable)[2]]=matrixOfCenteredProductSubject%*%solve(wDemi)
  }
  else{variables=varCor}
  gettingAppropriateData=function(selectedData ,asMatrix=FALSE, scoreName="Variable", productName="Product",subjectName="Subject",attributeName="Attribute",replicateName="Rep",replaceMode="crossmean",scaleUnit=FALSE)
  {
    # Transforms a canonical data into a extended data and averages on product*subject

    selectedData=selectedData[,c(productName,subjectName,replicateName,attributeName,scoreName)]

    extendedData=reshape(selectedData,idvar=c(productName,subjectName,replicateName),timevar=attributeName,direction="wide")
    attributeNames=substr(colnames(extendedData)[4:dim(extendedData)[2]],nchar(scoreName)+2,1000)
    newColnames=c(productName,subjectName,replicateName,attributeNames)
    colnames(extendedData)=newColnames
    appropriateData=data.frame(NULL)
    appropriateData=aggregate(extendedData[,attributeNames[1]],by=list(extendedData[,productName],extendedData[,subjectName]),FUN="mean",na.rm=TRUE)
    for(i in 2:length(attributeNames))
    {
      appropriateData[,attributeNames[i]]=aggregate(extendedData[,attributeNames[i]],by=list(extendedData[,productName],extendedData[,subjectName]),FUN="mean",na.rm=TRUE)[,3]
    }
    colnames(appropriateData)=c(productName,subjectName,attributeNames)
    appropriateData[,attributeNames]=apply(appropriateData[,attributeNames],2,"scale",center=TRUE,scale=scaleUnit)
    if(asMatrix)
    {
      resultingTable=as.matrix(appropriateData[,attributeNames])
      rownames(resultingTable)=paste(appropriateData[,productName],appropriateData[,subjectName],sep="")
    }
    else
    {
      resultingTable=appropriateData
    }

    return(resultingTable)
  }




  # Matrice des individus et variables de la CVA
  statResults=list();
  statResults[["Test"]]=test;
  statResults[["Stat"]]=statTest;
  statResults[["F"]]=statF;
  statResults[["pval"]]=statPval
  tabtot=NULL
  if(option=="MAM"||option=="MultiMAM")
  {
    decomposition[,"scoreWithoutScaling"]=decomposition[,"prodEffect"]+decomposition[,"Disag"]
    pureData=gettingAppropriateData(decomposition,scoreName="scoreWithoutScaling",subjectName="ass",replicateName="rep",productName="prod",attributeName="Attribute")
    newColnamesPureData=colnames(pureData)
    newColnamesPureData[which(colnames(pureData)=="ass")]="Subject"
    newColnamesPureData[which(colnames(pureData)=="prod")]="Product"
    colnames(pureData)=newColnamesPureData

    if(nbDimHotelling>nbAxes){nbDimHotelling=min(nbDimHotelling,nbAxes);warning("hotelling dimension were higher than the number of axes and was consequently reduced to the number of axes")}
    tabtot=hotellingTable(matCva=pureData,vep=eigVec[,1:nbAxes],axes=c(1:nbDimHotelling),colAttributes=3:dim(pureData)[2],productName="Product")
  }

  if(option=="TwoWayWithoutTakingSubjectEffectIntoEllipses")
  {
    decomposition[,"scoreWithoutSubject"]=decomposition[,"prodEffect"]+decomposition[,"int"]
    pureData=gettingAppropriateData(decomposition,scoreName="scoreWithoutSubject",subjectName="ass",replicateName="rep",productName="prod",attributeName="Attribute")
    newColnamesPureData=colnames(pureData)
    newColnamesPureData[which(colnames(pureData)=="ass")]="Subject"
    newColnamesPureData[which(colnames(pureData)=="prod")]="Product"
    colnames(pureData)=newColnamesPureData
    if(nbDimHotelling>nbAxes){nbDimHotelling=min(nbDimHotelling,nbAxes);warning("hotelling dimension were higher than the number of axes and was consequently reduced to the number of axes")}
    tabtot=hotellingTable(matCva=pureData,vep=eigVec[,1:nbAxes],axes=c(1:nbDimHotelling),colAttributes=3:dim(CenteredProductSubjectTable)[2],productName="Product")
  }

  if(option!="MAM"&& option!="MultiMAM"&&option!="TwoWayWithoutTakingSubjectEffectIntoEllipses")
  {
    if(nbDimHotelling>nbAxes){nbDimHotelling=min(nbDimHotelling,nbAxes);warning("hotelling dimension were higher than the number of axes and was consequently reduced to the number of axes")}

    tabtot=hotellingTable(matCva=CenteredProductSubjectTable,vep=eigVec[,1:nbAxes],axes=c(1:nbDimHotelling),colAttributes=3:dim(CenteredProductSubjectTable)[2],productName="product")
  }
  res.CVA=list(IndivCoord=individuals,VarCoord=variables,VarCor=varCor,NbDimSig=nbDimSig,HotellingTable=tabtot,ConditioningOfW=ConditioningOfW,B=SSProd,W=SSres,EigenVectors=eigVec,EigenValues=eigVal,Stats=statResults,decomposition=decomposition,IndSup=CenteredProductSubjectTable ,nbAxes=nbAxes,wDemi=wDemi,option=option,representation=representation,ExtendedData=extendedData)

   return (res.CVA)

}

