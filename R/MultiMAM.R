#' @importFrom doBy orderBy summaryBy
#' @importFrom stats anova
MultiMAM=function(frame, modelType="overall",negativeCorrection=TRUE,correctOnlyIfSignificant=FALSE,limitOfSignificance=0.05,plotReg=FALSE)
{#dataMF2=dataMFInd[,c(2,1,3,5:dim(dataMFInd)[2])]
#res=MultiMAM(frame=dataMF2,modelType="multivariate")

#res=MultiMAM(frame=dataMF2,modelType="total")
#res=MultiMAM(frame=dataMF2,modelType="classic")
	#MultiMAM(cheeses,"total",plotReg=TRUE)

#	LoadPackage("doBy")
	attnames       =  labels(frame)[[2]][-1:-3]                    ; natt  = length(attnames)                                                          # Save the attribute names and number.
	ass            =  factor(as.character(frame[,1])) ; assnames  = levels(ass)  ; nass  = length(assnames)                                                          # Define as factor, save level names (alphanum.) and #.
	prod           =  factor(frame[,2]) ; prodnames = levels(prod) ; nprod = length(prodnames)                                                         #                          -"-
	rep            =  factor(frame[,3]) ; repnames  = levels(rep)  ; nrep  = length(repnames)                                                          #                          -"-
	nrow           =  dim(frame)[1]                                ; ncol  = dim(frame)[2]                                                             # Number of rows and selected columns in the data frame.
	attnames1      =  attnames2 = attnames ; asslevels=levels(unique(ass))
	frame[,1]=ass;colnames(frame)[1]="ass"
	frame[,2]=prod;colnames(frame)[2]="prod"
	frame[,3]=rep;colnames(frame)[3]="rep"
	if(ncol==4){nameColonnes=colnames(frame)[4]}else{nameColonnes=colnames(frame[,4:ncol])}
	averages=rep(NA,natt);names(averages)=nameColonnes
	nnegatives=rep(NA,natt);names(nnegatives)=nameColonnes
	matriceDisag=matrix(NA,nprod*nass,natt)	; colnames(matriceDisag)=nameColonnes
	matriceInter=matrix(NA,nprod*nass,natt)	; colnames(matriceInter)=nameColonnes
	matriceProd=matrix(NA,nprod,natt)		; colnames(matriceProd)=nameColonnes
	matriceScaling=matrix(NA,nprod*nass,natt); colnames(matriceScaling)=nameColonnes
	matriceError=matrix(NA,nprod*nass*nrep,natt); colnames(matriceError)=nameColonnes
	matriceXS=matrix(NA,nprod*nass,natt)	; colnames(matriceXS)=nameColonnes
	matriceXSu=matrix(NA,nass,natt)	; colnames(matriceXSu)=nameColonnes
	matrice2=matrix(NA,nass*nprod,natt)	; colnames(matrice2)=nameColonnes

	matriceXSInt=matrix(NA,nprod*nass,natt)	;  colnames(matriceXSInt)=nameColonnes
	matriceBeta=matrix(NA,nass,natt)		; colnames(matriceBeta)=nameColonnes;rownames(matriceBeta)=asslevels
	BetaMulti=rep(NA,nass);names(BetaMulti)=asslevels
	pScaling=rep(NA,natt)
	significativity=rep(NA,nass)
	significativityMatrix=matrix(NA,nass,natt); colnames(significativityMatrix)=nameColonnes;rownames(significativityMatrix)=asslevels
	
significativityMultiBeta=rep(NA,nass);names(significativityMultiBeta)=asslevels

	
	decompositionDataFrame=NULL

	if(!(modelType%in%c("overall","classic","mam"))){		modelType="overall"}
		if(nprod<3){print("Less than 3 products: the scaling option is impossible, classic option is run");modelType="classic"}
		if(nrep<2){print("Less than 2 replicates");modelType="classic"}

	#	if(nprod*nass*nrep!=dim(frame)[1]){stop("Not balanced");modelType="classic"}
		if(ncol==4){modelType="mam";warnings("overall option makes no sense as there is only one attribute in the dataset: the classical mam is run")}
		if(modelType=="overall")
		{
				for (p in 4:ncol)
				{	 # running through attributes one by one
				  #if(ncol==4){X=as.matrix(frame[,4]);colnames(X)="X"}else{ X=frame[,p]}
				  X=frame[,p]
				  mu=mean(X) 
				  frame$X=X
				  xam=orderBy(~ass,data=data.frame(summaryBy(X~ass,data=frame)))
				  xpm=orderBy(~prod,data=data.frame(summaryBy(X~prod,data=frame)))
				  xapm=orderBy(~ass+prod,data=data.frame(summaryBy(X~ass+prod,data=frame)))
				  frame=frame[,which(colnames(frame)!="X")]
				  xapm$int=xapm$X.mean-rep(xam$X.mean,rep(nprod,nass))-rep(xpm$X.mean,nass)+mu
				  xapm$xsuj=xapm$X.mean-rep(xam$X.mean,rep(nprod,nass))
				  xapm$xs=rep(xpm$X.mean,nass)-mu
				  #xapm$xs=xapm$int
				  matriceXS[,p-3]=xapm[,"xs"]
				  matrice2[,p-3]=xapm[,"xsuj"]
				  # on veut un scaling centr? sur zero
				  matriceXSInt[,p-3]=xapm[,"int"]
				}
			
		
			
				for(i in 1:nass)
				{ 
					#linearRegression=lm(as.vector(matriceXSu[xapm[,"ass"]==asslevels[i],])~as.vector(matriceXS[xapm[,"ass"]==asslevels[i],]))

					linearRegression=lm(as.vector(matriceXSInt[xapm[,"ass"]==asslevels[i],])~as.vector(matriceXS[xapm[,"ass"]==asslevels[i],]))
					coeff=linearRegression$coefficients
					summaryResults=summary(linearRegression)
				  #coeff=lm(as.vector(matriceXSu[xapm[,"ass"]==asslevels[i],])~as.vector(matriceXS[xapm[,"ass"]==asslevels[i],]))$coefficients
					
					# if(plotReg)
					# {
					#   #x11()
					#   dev.new()
					# # win.metafile(paste("GraphScal_",i,".wmf",sep=""))
					#  # print(asslevels[i])
					#   xpts=as.vector(matriceXS[xapm[,"ass"]==asslevels[i],])
					#   ypts=as.vector(matrice2[xapm[,"ass"]==asslevels[i],])
					#   colorpts=as.vector(rep(rainbow(natt),each=nprod))
					#   plot(0,0,xlab="Centered panel scores",ylab="Centered panelist scores", main=paste("Regression for individual beta \n (",asslevels[i],": beta =",round(coeff[2]+1,digits=3),")"),xlim=c(min(xpts,ypts)-0.05,max(xpts,ypts)+0.05),ylim=c(-0.05+min(ypts,xpts),.05+max(xpts,ypts)))
					#   points(xpts,ypts,col=colorpts,pch=16) 
					#   abline(a=coeff[1],b=coeff[2]+1,col="red")
					#   abline(a=0,b=1,col="black")
					#   abline(v=0)
					#   abline(h=0)
					# }
					#  dev.off()
					BetaMulti[i]=coeff[2]
					significativityMultiBeta[i]=summaryResults[[4]][2,4]
				}
				names(BetaMulti)=asslevels
		}
		for (p in 4:ncol)
		{ # running through attributes one by one

		# initialisations
			# ssp=rep(0,nass)
			# sse=rep(0,nass)
			# sssca=rep(0,nass)
			# ssdis=rep(0,nass)
			Beta=rep(NA,nass);BetaToUse=rep(NA,nass)
			if(modelType!="overall"){	significativity=rep(NA,nass) }

			# Data decomposition
			X=frame[,p]
			mu=mean(X)
			averages[p-3]=mu
			frame$X=X
			xam=orderBy(~ass,data=data.frame(summaryBy(X~ass,data=frame)))
			xpm=orderBy(~prod,data=data.frame(summaryBy(X~prod,data=frame)))
			xapm=orderBy(~ass+prod,data=data.frame(summaryBy(X~ass+prod,data=frame)))
			xaprm=orderBy(~ass+prod+rep,data=data.frame(summaryBy(X~rep+ass+prod,data=frame)))
			frame=frame[,which(colnames(frame)!="X")]
			
			xapm$int=xapm$X.mean-rep(xam$X.mean,rep(nprod,nass))-rep(xpm$X.mean,nass)+mu
			xapm$prodEffect=rep(xpm$X.mean,nass)-mu
			xam$sujEffect=xam$X.mean-mu
			xapm$sujEffect=rep(xam$X.mean,rep(nprod,nass))-mu
			xapm$xs=rep(xpm$X.mean,nass)
			
			xaprm$mu=mu
			xaprm$centeredMean=xaprm$X.mean-mu
			xaprm$sujEffect=rep(xapm$sujEffect,each=nrep)
			xaprm$prodEffect=rep(xapm$prodEffect,each=nrep)
			xaprm$int=rep(xapm$int,each=nrep)
			xaprm$err=xaprm$centeredMean-xaprm$sujEffect-xaprm$prodEffect-xaprm$int
			#sses=X-ave(X,factor(ass:prod))

			# scaling coefficient calculations
		
			nnegatives[p-3]=0
			for (i in 1:nass)
			{
				if(modelType=="mam")
				{
				#print(xapm[xapm==asslevels[i],"int"])
				# print(xapm[xapm==asslevels[i],"xs"])
				 subsetData=subset(xapm,ass==asslevels[i])
				  res=lm(int~xs,data=subset(xapm,ass==asslevels[i]))
				
				#  print(anova(res))
				  aovres=anova(res)[,2]
				  # ssp[i]=anova(lm(X~prod,data=subset(frame,ass==asslevels[i])))[1,2]
				  # sssca[i]=nrep*aovres[1] # sumsquare produit
				  # ssdis[i]=nrep*aovres[2]# erreur sumsquare
				  # ssp[i]=ssp[i]-sssca[i]-ssdis[i] # on leur retire les termes de desaccord et de 
				  Beta[i]= res$coef[2]
				  BetaToUse[i]=Beta[i]
				  if(is.na(BetaToUse[i])){BetaToUse[i]=0}
					
					if(!is.na(aovres[2])& aovres[2]>1e-16){significativity[i]=summary(res)[[4]][2,4]}
				}
				if(modelType=="overall")
				{	
					Beta[i]=BetaMulti[i]	
					BetaToUse[i]=BetaMulti[i]	
				}
				if(modelType=="classic")
				{ 
					Beta[i]=0
					BetaToUse[i]=0
				}
				
				if(BetaToUse[i]<(-1))
				{ 
					nnegatives[p-3]=nnegatives[p-3]+1
					if(negativeCorrection){BetaToUse[i]=0}
								
				}
				  #sse[i]=sum(sses[ass==asslevels[i]]^2)
				
				  xapm[xapm[,"ass"]==asslevels[i],"Beta"]=BetaToUse[i]
				 
			}
			# further decomposition taking scaling into account
			xapm[,"disag"]=xapm[,"int"]-xapm[,"Beta"]*(xapm[,"xs"]-mu)
			xapm$scaling=xapm[,"Beta"]*(xapm[,"xs"]-mu)
			xapm[,"sujmean"]=rep(xam$X.mean,each=nprod)
			# xapm[,"correctedDisag"]=xapm[,"disag"]
			# xapm[,"correctedScaling"]=xapm[,"scaling"]
			if(correctOnlyIfSignificant&modelType!="classic")
			{
				SSScal=nrep*t(xapm[,"scaling"])%*%xapm[,"scaling"]
				SSDisag=nrep*t(xapm[,"disag"])%*%xapm[,"disag"]
				SSAss=nrep*nprod*t(xam[,"sujEffect"])%*%xam[,"sujEffect"]
				dfScal=(nass-1)
				dfDisag=(nass-1)*(nprod-2)
				
				# print(SSScal/dfScal)
				# print(SSDisag/dfDisag)
				FScal=(SSScal/dfScal)/(SSDisag/dfDisag)
				 pScaling[p-3]=pf(FScal,dfScal,dfDisag,lower.tail=FALSE);	
				 xapm[,"disag"]=xapm[,"int"]
				 xapm[,"scaling"]=0
				if(pScaling[p-3]<limitOfSignificance)
				{
					 xapm[,"disag"]=xapm[,"int"]-xapm[,"Beta"]*(xapm[,"xs"]-mu)
					 xapm$scaling=xapm[,"Beta"]*(xapm[,"xs"]-mu)
				}
			}
			xapmass=xapm$ass
			xapmprod=xapm$prod
			xaprm$Beta=rep(xapm$Beta,each=nrep)
			xaprm$Scaling=rep(xapm[,"scaling"],each=nrep)
			xaprm$Disag=rep(xapm[,"disag"],each=nrep)

			matriceScaling[,p-3]=xapm[,"Beta"]*(xapm[,"xs"]-mu)
			matriceDisag[,p-3]=xapm[,"disag"] 
			matriceBeta[,p-3]=Beta
			significativityMatrix[,p-3]=significativity
			matriceInter[,p-3]=xapm[,"int"]
			matriceProd[,p-3]=xpm[,"X.mean"]
			matriceXS[,p-3]=xapm[,"xs"]-mu
			matriceXSu[,p-3]=xam[,"sujEffect"]
			matriceError[,p-3]=xaprm$err
				
			xaprm2=xaprm
			xaprm2[,"Beta"]=xaprm2[,"Beta"]+1
			xaprm2[,"Attribute"]=attnames[p-3]
			decompositionDataFrame=rbind(decompositionDataFrame,xaprm2)
		}
		rownames(matriceProd)=xpm[,1];
		rownames(matriceXSu)=xam[,1];
		rownames(matriceScaling)=paste(xapm[,1],xapm[,2],sep="_");
		rownames(matriceDisag)=paste(xapm[,1],xapm[,2],sep="_"); 
		rownames(matriceInter)=paste(xapm[,1],xapm[,2],sep="_");
		rownames(matriceError)=paste(xaprm[,1],xaprm[,2],xaprm[,3],sep="_")
	if(modelType=="mam"){resultedBeta=matriceBeta+1 }
	if(modelType=="overall"){resultedBeta=BetaMulti+1}
	if(modelType!="overall"&modelType!="mam"){resultedBeta=NULL}
	L=list(decompositionDataFrame,resultedBeta,averages,matriceProd,matriceXSu,matriceScaling,matriceDisag,matriceError,matriceInter,nnegatives,pScaling,modelType,significativityMultiBeta)
	names(L)=c("decomposition","Beta","avgMat","prodMat","subjMat","scalMat","disagMat","errMat","intMat","nNeg","pvalScal","modelType","sigMultiBeta")
	return(L)
}