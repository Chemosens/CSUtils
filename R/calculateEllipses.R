#'calculateEllipses
#'calculates the coordinates of confidence ellipses

#'@param suppIndividualTable an extended data frame with "product","subject" and as many further column as variables (descriptors) in the dataframe
#'@param vep matrix of eigenvectors obtained by PCA or CVA
#'@param axes the two axes to be chosen for the plot
#'@param confInt threshold for confidence intervall. A value of 0.9 (the default) for barycentric ellipses indicates that the mean is inside the ellipse at 90\%. For individual ellipses, one individual has 90\% of chances to be in the ellipse
#'@param ellipseType "barycentric" or "individual". Barycentric is the confidence ellipse for the mean while individual is the confidence ellipse for the individuals.
#'@param productName name of the column containing the main effect. "product" by default.
#'@param subjectName name of the column containing the variability around the main effect. "subject" by default.
#'@param ellipseCalculation "Chi","F" or "SAS". Method to estimate the ellipse calculation.
#'@param bootstrap =FALSE If TRUE the ellipses represents the variability
#'@param nSamples =100 Numer of samples used with bootstrap option
#'@details If missing data
#'@examples{
#' # df=data.frame(product=rep(c("a","b","c"),each=10),
#' # subject=rep(paste0("S",1:10),3),acide=rnorm(30),amer=rnorm(30))
#' # respca=PCA(df)
#' # calculateEllipses(df,respca$EigenVectors)
#' }
#'@importFrom ellipse ellipse
#'@importFrom stats cov qchisq qf
#'@export
calculateEllipses=function(suppIndividualTable,vep,axes=c(1,2),confInt=0.9,ellipseType="barycentric",productName="product",subjectName="subject",ellipseCalculation="Chi",bootstrap=FALSE,nSamples=100)
{ #print
	products=levels(as.factor(as.character(suppIndividualTable[,productName])))
	nb.prod=length(products)
	suj=levels(as.factor(suppIndividualTable[,subjectName]))
	nb.suj=length(suj)
	ell=array(dim=c(nb.prod,100,2))
	coord.pts=list()
	attributs=colnames(suppIndividualTable)[-c(1,2)]
	nbAttributes=length(attributs)
	ax1=axes[1]
	ax2=axes[2]
	coorCentre=data.frame()
	coord.mean.simul=list()
  ell=data.frame()
  df_coord=NULL
	for(i in 1:nb.prod)
	{
	  indiv.sup=as.matrix(suppIndividualTable[suppIndividualTable[,productName]==products[i],3:ncol(suppIndividualTable)])
		sujets=suppIndividualTable[suppIndividualTable[,productName]==products[i],subjectName]
		indiv.sup[is.na(indiv.sup)]=0
		coord.indiv.sup=indiv.sup%*%vep[,c(ax1,ax2)]
		coorCentre[i,1]=mean(coord.indiv.sup[,1],na.rm=TRUE)
		coorCentre[i,2]=mean(coord.indiv.sup[,2],na.rm=TRUE)
		coorCentre[i,productName]=products[i]
		rownames(coord.indiv.sup)=sujets
		coord.pts[[i]]=coord.indiv.sup[,c(1,2)]
		coord.pts2=as.data.frame(coord.pts[[i]])
		coord.pts2[,subjectName]=rownames(coord.pts[[i]])
		coord.pts2[,productName]=products[i]
		coord.pts2[,"axes"]=paste(axes[1],axes[2],sep=",")
		df_coord=rbind(df_coord,coord.pts2)
		ddlN=length(coord.pts[[i]][,1])
		matCov=cov(coord.pts[[i]],use="pairwise.complete.obs") # on calcule la matrice de covariance des points
		matCov2=matCov/dim(coord.indiv.sup)[1]  # on calcule la matrice de covariance des points moyens
		coord.mean.simul[[i]]=data.frame()
		if(bootstrap)
		{
			for(k in 1:nSamples)
			{
				simul.indiv=coord.indiv.sup[sample(1:dim(coord.indiv.sup)[1],size=dim(coord.indiv.sup)[1],replace=TRUE),]
				simulatedMeanPoint=apply(as.matrix(simul.indiv[,c(1,2)]),2,mean,na.rm=TRUE)

				coord.mean.simul[[i]]=rbind(coord.mean.simul[[i]],simulatedMeanPoint)

			}
			matCov=cov(coord.mean.simul[[i]],use="pairwise.complete.obs")
			matCov2=cov(coord.mean.simul[[i]],use="pairwise.complete.obs")
			#matCov2=matCov/dim(coord.indiv.sup)[1]
		}

		if(ellipseType!="individual"){covarianceMatrix=matCov2;}
		if(ellipseType=="individual"){covarianceMatrix=matCov}
		if(ellipseCalculation=="Chi"){quant=sqrt(qchisq(confInt,2))}
		if(ellipseCalculation=="F"){quant=sqrt(2*nb.suj*qf(confInt,2,nb.suj-2)/(nb.suj-2))}
		if(ellipseCalculation=="Sas"){quant=sqrt((2*nb.suj*qf(confInt,2,nb.suj-2)/(nb.suj-2))*(nb.suj-1)/nb.suj)}
		#)
		#quant_f=sqrt(2*nb.suj*qf(confInt,2,nb.suj-2)/(nb.suj-2))
		#quant_sas=sqrt((2*nb.suj*qf(confInt,2,nb.suj-2)/(nb.suj-2))*(nb.suj-1)/nb.suj)
		#print(covarianceMatrix)
		elli=ellipse::ellipse(x=covarianceMatrix,centre=c(coorCentre[i,1],coorCentre[i,2]),level=confInt,t=quant)[,1:2]
		elli=as.data.frame(elli);elli[,"product"]=products[i]
	  ell=rbind(ell, elli)
	}
	names(coord.pts)=products
	rownames(coorCentre)=products
	L=list(indivCoord=df_coord,ellPoints=ell,centers=coorCentre)
	return(L)
}

# # test verification:
		 # centre=c(coorCentre[i,1],coorCentre[i,2])
		 # # n*t(g-mu)(VarTheorique)^(-1)(g-mu) suit une loi du chi 2 (Saporta)
		# # et donc on a t(g-mu)(VarTheorique)^(-1)(g-mu) < chi2(p , 1-alpha)/n est l'equation de l'ellipsoide de confiance pour mu.
		# # on suppose ici que VarTheorique=matCov
		# # on a alors l'ellipse suivante qui correspond ? ce qu'on obtient en mettant
		 # covarianceMatrix2=matCov
		# # c=-2*qf(confInt,2,nb.suj-2)/(nb.suj-2)


		 # # en utilisant la matrice de covariance divisee par n
		 # covarianceMatrix2=matCov2
		  # c=-(qchisq(confInt,2))

		 # # c=-2*qf(confInt,2,nb.suj-2)/(nb.suj-2)

		 # saportaEllipses=function(covarianceMatrix2,nb.suj,test="f")
		 # {
			 # if(test=="chi"){c=-(qchisq(confInt,2))/nb.suj;print(-c)}
			 # if(test=="f"){ c=-2*qf(confInt,2,nb.suj-2)/(nb.suj-2) ;print(-c)}
			  # if(test=="sas"){ c=-(2/(nb.suj-2))*qf(confInt,2,nb.suj-2)*(nb.suj-1)/nb.suj ;print(-c)}
			 # vap=eigen(solve(covarianceMatrix2))
			  # vep2=eigen(solve(covarianceMatrix2))$vectors
			  # a=sqrt(-c/(vap$values[1]))
			  # b=sqrt(-c/(vap$values[2]))
			  # x=rep(NA,length(seq(0,2*pi,0.1)));y=rep(NA,length(seq(0,2*pi,0.1)))
			  # for (j in 1:length(seq(0,2*pi,0.1)))
			 # {
			 # x[j]= a*cos(seq(0,2*pi,0.1)[j])
				 # y[j]= b*sin(seq(0,2*pi,0.1)[j])
			  # }
			   # covariancemat=vep2%*%diag(vap$values)%*%t(vep2)
			  # x2=cbind(x,y)%*%t(as.matrix(vep2))[,1]
			  # y2=cbind(x,y)%*%t(as.matrix(vep2))[,2]
			  # x3=centre[1]+x2
			  # y3=centre[2]+y2
			  # l=cbind(x3,y3)
			  # return(l)
		  # }

		 # l_chi=saportaEllipses(matCov,nb.suj=nb.suj,test="chi")
		 # l_f=saportaEllipses(matCov,nb.suj=nb.suj,test="f")
		 # l_sas=saportaEllipses(matCov,nb.suj=nb.suj,test="sas")

		 #plot(ellipse::ellipse(x=covarianceMatrix,centre=c(coorCentre[i,1],coorCentre[i,2]),level=confInt,t=quant2),type="l",col="pink")

		# lines(x=l_f[,1],y=l_f[,2],type="l",col="red")
		# lines(x=l_chi[,1],y=l_chi[,2],col="blue",type='l')
		# lines(x=l_sas[,1],y=l_sas[,2],col="orange",type='l')
		#lines(ellipse::ellipse(x=covarianceMatrix,centre=c(coorCentre[i,1],coorCentre[i,2]),level=confInt,t=quant),type="l",col="green")

		#lines(ellipse::ellipse(x=covarianceMatrix,centre=c(coorCentre[i,1],coorCentre[i,2]),level=confInt,t=quant_sas),type="l",col="yellow")
		# # fin test