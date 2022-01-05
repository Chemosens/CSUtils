#'@importFrom stats t.test cor.test
PanelPerformances=function(frame, modelType="overall",negativeCorrection=TRUE,correctOnlyIfSignificant=FALSE,limitOfSignificance=0.05,onlySignificantDim=FALSE,manovaTest="Hotelling", panelistPerf=FALSE,correlationTest="none",multidimLine=FALSE,fisherRatio=FALSE,levelOption=FALSE,whenOverallUseMAMForTest=FALSE)
{# frame est de la forme "subjectCode","prod","rep", att1,... attP
	#LoadPackage("doBy")
	
	attnames       =  labels(frame)[[2]][-1:-3]                    ; natt  = length(attnames)                                                          # Save the attribute names and number.
	ass            =  factor(as.character(frame[,1])) ; assnames  = levels(ass)  ; nass  = length(assnames)                                                          # Define as factor, save level names (alphanum.) and #.
	prod           =  factor(frame[,2]) ; prodnames = levels(prod) ; nprod = length(prodnames)                                                         #                          -"-
	rep            =  factor(frame[,3]) ; repnames  = levels(rep)  ; nrep  = length(repnames)                                                          #                          -"-
	if(!(modelType%in%c("overall","classic","mam"))){stop("[TS]please choose modelType in 'overall','classic' or 'mam'")}
	if(nprod<3){modelType="classic";print("Less than 3 products, the scaling option is impossible, classic option is run")}
	if(nrep<2){modelType="classic";print("Less than 2 replicates, the scaling option is impossible, classic option is run")}
	if((correlationTest=="pearson"|correlationTest=="kendall"|correlationTest=="spearman") & nprod<3){print("Not enough products for correlations");correlationTest="none"}

	ListResults=MultiMAM(frame=as.data.frame(frame), modelType=modelType,negativeCorrection=negativeCorrection,correctOnlyIfSignificant=correctOnlyIfSignificant,limitOfSignificance=limitOfSignificance,plotReg=FALSE)
	
	if(modelType=="overall")
	{
		ListResultsMAMmultivariate=MultiMAM(frame=frame, modelType="mam",negativeCorrection=negativeCorrection,correctOnlyIfSignificant=correctOnlyIfSignificant,limitOfSignificance=limitOfSignificance,plotReg=FALSE)
		UsualBeta=ListResultsMAMmultivariate$Beta
		#added for level and multivariate options
		sigMultiBeta=ListResults$sigMultiBeta
		#/added for level
		CorrectedBeta=apply(UsualBeta,2,"/",ListResults$Beta)
		if(whenOverallUseMAMForTest){ListResultsToUse=ListResultsMAMmultivariate}else{ListResultsToUse=ListResults}
	}
	#added 
	else
	{
		ListResultsToUse=ListResults
		UsualBeta=ListResults$Beta
		sigMultiBeta=NULL
	}
	#/added 
	
	scalingCoefficient=ListResultsToUse$Beta
	matriceScaling=ListResultsToUse$scalMat
	matriceProd=ListResultsToUse$prodMat
	matriceDisag=ListResultsToUse$disagMat
	matriceError=ListResultsToUse$errMat
	matriceInter=ListResultsToUse$intMat
	scalPval=ListResultsToUse$pvalScal
	nNeg=ListResultsToUse$nNeg
	PanelProd=nrep*nass*t(scale(matriceProd,scale=FALSE))%*%scale(matriceProd,scale=FALSE)
	#PanelProd=nrep*nass*t(matriceProd)%*%(matriceProd)

	PanelScal=nrep*t(matriceScaling)%*%matriceScaling
	PanelDisag=nrep*t(matriceDisag)%*%matriceDisag	
	PanelInter=nrep*t(matriceInter)%*%matriceInter
	PanelError=t(matriceError)%*%matriceError
	###############################
	# Getting degrees of freedom
	###############################
	
	if(modelType=="overall"){disagDF=(nprod-2)*(nass-1)}
	if(modelType=="classic"){disagDF=(nprod-1)*(nass-1)}
	if(modelType=="mam")
	{
		if(negativeCorrection)
		{ # disagDF is the degrees of freedom for the modified (interOrDisag) interaction whereas disagDF2 is the degree of freedom for the classical MAM disagreement
			if(correctOnlyIfSignificant)
			{
				scalingDisc=sum(scalPval<limitOfSignificance,na.rm=TRUE)
				disagDF=(scalingDisc/natt)*((nprod-2)*(nass-1)+mean(nNeg[scalingDisc],na.rm=TRUE))+(1-scalingDisc/natt)*(nprod-1)*(nass-1)
				scalingDF=(scalingDisc/natt)*((nass-1)-mean(nNeg[scalingDisc],na.rm=TRUE))
			}
			else
			{
				disagDF=(nprod-2)*(nass-1)+mean(nNeg)
				scalingDF=(nass-1)-mean(nNeg)
			}
		  
		}
		else
		{
			if(correctOnlyIfSignificant)
			{	
				scalingDisc=sum(scalPval<limitOfSignificance,na.rm=TRUE)
				disagDF=(scalingDisc/natt)*((nprod-2)*(nass-1))+(1-scalingDisc/natt)*(nprod-1)*(nass-1)
				scalingDF=(scalingDisc/natt)*(nass-1)
			}
			else
			{
				disagDF=(nprod-2)*(nass-1)
				scalingDF=(nass-1)
			}
		}
	}
	#For CVA
		tryCatch({
		multiPanelDiscrimination=Manova(P=PanelProd,R=PanelDisag,disagDF,test=manovaTest) 
		multiPanelDisagreement=Manova(P=PanelDisag,R=PanelError,nass*nprod*(nrep-1),test=manovaTest) 
		multiPanelScaling=Manova(PanelScal,PanelDisag,disagDF,test=manovaTest) 
		},error=function(e){print(e);stop("[TS] Missing values in the dataset: the dataset is unbalanced")})
	
	
	if(modelType=="mam"||modelType=="overall")
	{
		multiPanelPerformances=matrix(NA,4,3);rownames(multiPanelPerformances)=c("discrimination","scaling","agreement","repeatability")

	}
	if(modelType=="classic")
	{
		multiPanelPerformances=matrix(NA,3,3)
		rownames(multiPanelPerformances)=c("discrimination","agreement","repeatability")
	}
	colnames(multiPanelPerformances)=c("stat","f","pvalue")
	multiPanelPerformances["discrimination","f"]=round(Re(multiPanelDiscrimination$f),digits=3)
	multiPanelPerformances["discrimination","stat"]=round(Re(multiPanelDiscrimination$stat),digits=3)
	multiPanelPerformances["discrimination","pvalue"]=round(Re(multiPanelDiscrimination$pvalue),digits=4)
	
	multiPanelPerformances["agreement","stat"]=round(Re(multiPanelDisagreement$stat),digits=3)
	multiPanelPerformances["agreement","f"]=round(Re(multiPanelDisagreement$f),digits=3)
	multiPanelPerformances["agreement","pvalue"]=round(Re(multiPanelDisagreement$pvalue),digits=4)
	if(modelType=="mam"||modelType=="overall")
	{
		multiPanelPerformances["scaling","stat"]=round(Re(multiPanelScaling$stat),digits=3)
		multiPanelPerformances["scaling","f"]=round(Re(multiPanelScaling$f),digits=3)
		multiPanelPerformances["scaling","pvalue"]=round(Re(multiPanelScaling$pvalue),digits=4)
	}	
	
	# Performances unidimensionelles du Panel et des pan?listes
	avgPanel=rep(NA,natt);names(avgPanel)=attnames
	FsujPanel=rep(NA,natt); names(FsujPanel)=attnames
	PsujPanel=rep(NA,natt); names(FsujPanel)=attnames
	FdiscrPanel=rep(NA,natt); names(FdiscrPanel)=attnames
	PdiscrPanel=rep(NA,natt); names(PdiscrPanel)=attnames
	FscalPanel=rep(NA,natt); names(FscalPanel)=attnames
	PscalPanel=rep(NA,natt); names(PscalPanel)=attnames
	FdisagPanel=rep(NA,natt); names(FdisagPanel)=attnames
	PdisagPanel=rep(NA,natt); names(PdisagPanel)=attnames
	PerrPanel=rep(NA,natt); names(PerrPanel)=attnames
	levelPanel=rep(NA,natt); names(levelPanel)=attnames
	tTestPanelist=matrix(NA,natt,nass);colnames(tTestPanelist)=assnames;rownames(tTestPanelist)=attnames
	FdiscrPanelist=matrix(NA,natt,nass);colnames(FdiscrPanelist)=assnames;rownames(FdiscrPanelist)=attnames
	PdiscrPanelist=matrix(NA,natt,nass);colnames(PdiscrPanelist)=assnames;rownames(PdiscrPanelist)=attnames
	FscalPanelist=matrix(NA,natt,nass);colnames(FscalPanelist)=assnames;rownames(FscalPanelist)=attnames
	PscalPanelist=matrix(NA,natt,nass);colnames(PscalPanelist)=assnames;rownames(PscalPanelist)=attnames
	FdisagPanelist=matrix(NA,natt,nass);colnames(FdisagPanelist)=assnames;rownames(FdisagPanelist)=attnames
	PdisagPanelist=matrix(NA,natt,nass);colnames(PdisagPanelist)=assnames;rownames(PdisagPanelist)=attnames
	FerrorPanelist=matrix(NA,natt,nass);colnames(FerrorPanelist)=assnames;rownames(FerrorPanelist)=attnames
	PerrorPanelist=matrix(NA,natt,nass);colnames(PerrorPanelist)=assnames;rownames(PerrorPanelist)=attnames
	levelPanelist=matrix(NA,natt,nass);colnames(levelPanelist)=assnames;rownames(levelPanelist)=attnames
	meanPanelist=matrix(NA,natt,nass);colnames(meanPanelist)=assnames;rownames(meanPanelist)=attnames
	meanPanel=matrix(NA,natt,nass);colnames(meanPanel)=assnames;rownames(meanPanel)=attnames
	decomposition=ListResultsToUse$decomposition
	multiLevelPanel=mean(decomposition[,"X.mean"],na.rm=TRUE)
	tTestPanelist=matrix(NA,natt,nass);colnames(tTestPanelist)=assnames;rownames(tTestPanelist)=attnames

	listAnova=list()
	for(d in 1:natt)
	{
		decompositionByDescr=decomposition[decomposition[,"Attribute"]==attnames[d],]
		SSprod=t(decompositionByDescr[,"prodEffect"])%*%decompositionByDescr[,"prodEffect"];DFprod=nprod-1
		SSsuj=t(decompositionByDescr[,"sujEffect"])%*%decompositionByDescr[,"sujEffect"];DFsuj=nass-1
		SSscal=t(decompositionByDescr[,"Scaling"])%*%decompositionByDescr[,"Scaling"];if(negativeCorrection){DFscal=nass-1-nNeg[d]}else{DFscal=nass-1}
		SSdisag=t(decompositionByDescr[,"Disag"])%*%decompositionByDescr[,"Disag"];if(modelType!="classic"){if(negativeCorrection){DFdisag=(nprod-2)*(nass-1)+nNeg[d]}else{DFdisag=(nprod-2)*(nass-1)}}else{DFdisag=(nprod-1)*(nass-1)}
		SSerror=t(decompositionByDescr[,"err"])%*%decompositionByDescr[,"err"];DFerror=nprod*nass*(nrep-1)
		avgPanel[d]=mean(decompositionByDescr[,"X.mean"],na.rm=TRUE)
		FdiscrPanel[d]=(SSprod/DFprod)/(SSdisag/DFdisag);PdiscrPanel[d]=pf(FdiscrPanel[d],DFprod,DFdisag,lower.tail=FALSE) 
		FsujPanel[d]=(SSsuj/DFsuj)/(SSdisag/DFdisag);PsujPanel[d]=pf(FsujPanel[d],DFsuj,DFdisag,lower.tail=FALSE) 
	
		FscalPanel[d]=(SSscal/DFscal)/(SSdisag/DFdisag);PscalPanel[d]=pf(FscalPanel[d],DFscal,DFdisag,lower.tail=FALSE)
		FdisagPanel[d]=(SSdisag/DFdisag)/(SSerror/DFerror);PdisagPanel[d]=pf(FdisagPanel[d],DFdisag,DFerror,lower.tail=FALSE)
		PerrPanel[d]=sqrt(SSerror/DFerror)
		
		listAnova[[attnames[d]]]=matrix(NA,5,5)
		rownames(listAnova[[attnames[d]]])=c("Product","Subject","Scaling","Disag","Residuals")
		colnames(listAnova[[attnames[d]]])=c("DF","SS","MS","F","P-value")
	
		listAnova[[attnames[d]]]["Product","DF"]=DFprod
		listAnova[[attnames[d]]]["Product","SS"]=SSprod
		listAnova[[attnames[d]]]["Product","MS"]=SSprod/DFprod
		listAnova[[attnames[d]]]["Product","F"]=FdiscrPanel[d]
		listAnova[[attnames[d]]]["Product","P-value"]=PdiscrPanel[d]
		listAnova[[attnames[d]]]["Subject","DF"]=DFsuj
		listAnova[[attnames[d]]]["Subject","SS"]=SSsuj
		listAnova[[attnames[d]]]["Subject","MS"]=SSsuj/DFsuj
		listAnova[[attnames[d]]]["Subject","F"]=(SSsuj/DFsuj)/(SSdisag/DFdisag)
		listAnova[[attnames[d]]]["Subject","P-value"]=pf(listAnova[[d]]["Subject","F"],df1=DFsuj,df2=DFdisag,lower.tail=FALSE)
		listAnova[[attnames[d]]]["Scaling","DF"]=DFscal
		listAnova[[attnames[d]]]["Scaling","SS"]=SSscal
		listAnova[[attnames[d]]]["Scaling","MS"]=SSscal/DFscal
		 listAnova[[attnames[d]]]["Scaling","F"]=FscalPanel[d]
		 listAnova[[attnames[d]]]["Scaling","P-value"]=PscalPanel[d]
		 listAnova[[attnames[d]]]["Disag","DF"]=DFdisag
		 listAnova[[attnames[d]]]["Disag","SS"]=SSdisag
		 listAnova[[attnames[d]]]["Disag","MS"]=SSdisag/DFdisag
		 listAnova[[attnames[d]]]["Disag","F"]=FdisagPanel[d]
		 listAnova[[attnames[d]]]["Disag","P-value"]=PdisagPanel[d]
		 listAnova[[attnames[d]]]["Residuals","DF"]=DFerror
		 listAnova[[attnames[d]]]["Residuals","SS"]=SSerror
		 listAnova[[attnames[d]]]["Residuals","MS"]=SSerror/DFerror
		
		if(modelType=="overall")
		{	
			decompositionM=ListResultsMAMmultivariate$decomposition
			decompositionByDescrM=decompositionM[decompositionM[,"Attribute"]==attnames[d],]
		}	

		if(panelistPerf)
		{
			overallLevelPanel=rep(NA,nass);names(overallLevelPanel)=assnames
			overallLevelPanelist=rep(NA,nass);names(overallLevelPanelist)=assnames
			sigLevelPanelist=rep(NA,nass);names(sigLevelPanelist)=assnames
			for(i in 1:nass)
			{ 
			# a faire : verifier les degres de libert? quand on selectionne uniquement les significatifs ou quand on a des negatifs
			# Ca devrait marcher: en effet SSscaling=0 dans ces cas.
				scoreOverallPanelist=as.numeric(as.matrix(frame[frame[,1]==assnames[i],4:ncol(frame)]))
				overallLevelPanelist[i]=mean(scoreOverallPanelist)
				scoreOverallPanel=as.numeric(as.matrix(frame[frame[,1]!=assnames[i],4:ncol(frame)]))
				overallLevelPanel[i]=mean(scoreOverallPanel)
				sigLevelPanelist[i]=t.test(scoreOverallPanelist,scoreOverallPanel)$p.value
				decompositionByDescrAndSuj=decompositionByDescr[decompositionByDescr[,"ass"]==assnames[i],]
				effetProdInd=(decompositionByDescrAndSuj[,"prodEffect"]+decompositionByDescrAndSuj[,"int"])
				SSprodi=t(effetProdInd)%*%effetProdInd;DFprodi=nprod-1 
				#anova(lm(X.mean~prod,data=decompositionByDescrAndSuj))
				levelPanelist[d,i]=mean(decompositionByDescrAndSuj[,"X.mean"])-avgPanel[d]
				if(modelType!="classic"){	SSscali=t(decompositionByDescrAndSuj[,"Scaling"])%*%decompositionByDescrAndSuj[,"Scaling"];DFscali=1}
				SSdisagi=t(decompositionByDescrAndSuj[,"Disag"])%*%decompositionByDescrAndSuj[,"Disag"];
				DFdisagi=nprod-2
				if(decompositionByDescrAndSuj[1,"Beta"]<0){DFdisagi=nprod-1}
				if(correctOnlyIfSignificant&(scalPval[d]>limitOfSignificance)){DFdisagi=nprod-1}
				scorePanelist=frame[frame[,1]==assnames[i],attnames[d]]
				scorePanel=frame[frame[,1]!=assnames[i],attnames[d]]
				meanPanelist[d,i]=mean(scorePanelist)
				meanPanel[d,i]=mean(scorePanel)
				tTestPanelist[d,i]=t.test(x=scorePanelist,y=scorePanel)$p.value
			
				if(nrep>1)
				{
					SSerrori=t(decompositionByDescrAndSuj[,"err"])%*%decompositionByDescrAndSuj[,"err"];DFerrori=nprod*(nrep-1)
					if(SSerrori!=0)
					{
						FdiscrPanelist[d,i]=(SSprodi/DFprodi)/(SSerrori/DFerrori);PdiscrPanelist[d,i]=pf(FdiscrPanelist[d,i],DFprodi,DFerrori,lower.tail=FALSE)	
					} 
					else
					{
						FdiscrPanelist[d,i]=NA;PdiscrPanelist[d,i]=NA
					}
					if(modelType!="classic")
					{
						if(SSdisagi!=0)
						{
						FscalPanelist[d,i]=(SSscali/DFscali)/(SSdisagi/DFdisagi);PscalPanelist[d,i]=pf(FscalPanelist[d,i],DFscali,DFdisagi,lower.tail=FALSE)
						}
						else
						{
						FscalPanelist[d,i]=NA;PscalPanelist[d,i]=NA
						}
					}
						FerrorPanelist[d,i]=(SSerrori/DFerrori);
						FerrorTMP=(SSerrori/(nprod*(nrep-1)))/((SSerror-SSerrori)/((nass-1)*nprod*(nrep-1)))
						PerrorPanelist[d,i]=pf(FerrorTMP,df1=nprod*(nrep-1),df2=nprod*(nrep-1)*(nass-1),lower.tail=FALSE)
					
				}
				if(nrep==1)
				{
					SSerrori=t(decompositionByDescrAndSuj[,"int"])%*%decompositionByDescrAndSuj[,"int"];DFerrori=nprod-1
					FdiscrPanelist[d,i]=NA;PdiscrPanelist[d,i]=NA	
					if(modelType!="classic"){	FscalPanelist[d,i]=NA;PscalPanelist[d,i]=NA}
					FerrorPanelist[d,i]=(SSerrori/DFerrori);
					PerrorPanelist[d,i]=NA				
				}
					
				if(modelType=="overall")
				{
					decompositionByDescrAndSujM=decompositionByDescrM[decompositionByDescrM[,"ass"]==assnames[i],]
					SSscali=t(decompositionByDescrAndSujM[,"Scaling"])%*%decompositionByDescrAndSujM[,"Scaling"];DFscali=1
					SSdisagi=t(decompositionByDescrAndSujM[,"Disag"])%*%decompositionByDescrAndSujM[,"Disag"];DFdisagi=nprod-2
					if(modelType!="classic"){	FscalPanelist[d,i]=(SSscali/DFscali)/(SSdisagi/DFdisagi);PscalPanelist[d,i]=pf(FscalPanelist[d,i],DFscali,DFdisagi,lower.tail=FALSE)	}
				}

				if(correlationTest=="none")
				{
					if(SSerrori!=0)
					{
						FdisagPanelist[d,i]=(SSdisagi/DFdisagi)/(SSerrori/DFerrori);PdisagPanelist[d,i]=pf(FdisagPanelist[d,i],DFdisagi,DFerrori,lower.tail=FALSE)
					}
					else
					{
						FdisagPanelist[d,i]=NA;PdisagPanelist[d,i]=NA
					}
				}
				if(correlationTest!="none")
				{
					if(!correlationTest%in%c("kendall","pearson","spearman")){stop("correlationTest should be 'none','kendall','spearman' or 'pearson'")}
					moyennePanelist=rep(NA,nprod)
					moyenneRestPanel=rep(NA,nprod)
					for(z in 1:nprod)
					{
						moyennePanelist[z]=mean(decompositionByDescr[decompositionByDescr[,"ass"]==assnames[i]&decompositionByDescr[,"prod"]==prodnames[z],"X.mean"],na.rm=TRUE)
						moyenneRestPanel[z]=mean(decompositionByDescr[decompositionByDescr[,"ass"]!=assnames[i]&decompositionByDescr[,"prod"]==prodnames[z],"X.mean"],na.rm=TRUE)
					}
					corTest=cor.test(moyennePanelist, moyenneRestPanel,method=correlationTest,alternative="greater")
					FdisagPanelist[d,i]=corTest$estimate
					PdisagPanelist[d,i]=corTest$p.value
				}
			}
		
		}
		
	}
	if(levelOption)

		{
		listTTest=apply(levelPanelist,2,t.test)
		sigOverallTTest=sapply(listTTest,FUN=function(e){return(e$p.value)})
		sigOverallMean=sapply(listTTest,FUN=function(e){return(e$estimate)})
		listPanelistPerf=list(levelPanelist,tTestPanelist,FdiscrPanelist,PdiscrPanelist,FscalPanelist,PscalPanelist,FdisagPanelist,PdisagPanelist,FerrorPanelist,PerrorPanelist,overallLevelPanelist,	sigLevelPanelist,overallLevelPanel,meanPanelist,meanPanel,sigOverallTTest,sigOverallMean,scalingCoefficient,sigMultiBeta,t(UsualBeta))
		names(listPanelistPerf)=c("level","levelTTest","Fdiscr","Pdiscr","Fscal","Pscal","Fdisag","Pdisag","MSError","Perror","overallLevelPanelist","sigLevelPanelist","overallLevelPanel","meanPanelist","meanPanel","sigOverallTTest","sigOverallMean","overallScaling","sigOverallScaling","usualScaling")
		listPanelPerf=list(avgPanel,FdiscrPanel,PdiscrPanel,FsujPanel,PsujPanel,FscalPanel,PscalPanel,FdisagPanel,PdisagPanel,PerrPanel)
		names(listPanelPerf)=c("avg","Fdiscr","Pdiscr","Fsuj","Psuj","Fscal","Pscal","Fdisag","Pdisag","SRMSError")
	
		
		}
		else
		{listPanelistPerf=list(levelPanelist,FdiscrPanelist,PdiscrPanelist,FscalPanelist,PscalPanelist,FdisagPanelist,PdisagPanelist,FerrorPanelist,PerrorPanelist)
			names(listPanelistPerf)=c("avg","Fdiscr","Pdiscr","Fscal","Pscal","Fdisag","Pdisag","MSError","Perror")
			listPanelPerf=list(avgPanel,FdiscrPanel,PdiscrPanel,FscalPanel,PscalPanel,FdisagPanel,PdisagPanel,PerrPanel)
			names(listPanelPerf)=c("avg","Fdiscr","Pdiscr","Fscal","Pscal","Fdisag","Pdisag","SRMSError")
	
		}
	
	L=list(multiPanelPerformances,ListResults$decomposition,PanelDisag,listPanelPerf,listAnova)
	
	
	names(L)=c("multiPanelPerformances","decomposition","matW","listPanelPerf","listAnova")
	
	if(modelType=="overall"||modelType=="mam")
	{	
			L[["Beta"]]=t(scalingCoefficient)	
	}
	if(fisherRatio)
	{
		SSprodPanel=t(decomposition[,"prodEffect"])%*%decomposition[,"prodEffect"];DFprodPanel=nprod-1
		SSscalPanel=t(decomposition[,"Scaling"])%*%decomposition[,"Scaling"];DFscalPanel=nass-1
		SSdisagPanel=t(decomposition[,"Disag"])%*%decomposition[,"Disag"];DFdisagPanel=(nprod-1)*(nass-1)
		SSErrorPanel=t(decomposition[,"err"])%*%decomposition[,"err"];DFerrorPanel=nass*nprod*(nrep-1)
		MSDisagPanel=(SSdisagPanel/nrep)/DFdisagPanel
		MSProdPanel=(SSprodPanel/nass*nrep)/DFprodPanel
		MSScalPanel=(SSscalPanel/nrep)/DFscalPanel
		MSErrorPanel=(SSErrorPanel)/DFerrorPanel
		RapportFisherPanel=rep(NA,3);names(RapportFisherPanel)=c("Disagreement","Discrimination","Scaling")
		RapportFisherPanel[1]=MSDisagPanel/(MSDisagPanel+MSErrorPanel)
		RapportFisherPanel[2]=MSProdPanel/(MSProdPanel+MSDisagPanel)
		RapportFisherPanel[3]=MSScalPanel/(MSScalPanel+MSDisagPanel)
		L[["RapportFisherPanel"]]=RapportFisherPanel
		## Fin de l'essai ]
		## Essai pour retourner les rapport de fisher pour la discrimination au niveau des individus [
		RapportFisherPanelist=vector(length = nass)
		for (i in 1:nass){
		  decompositionByAss=decomposition[decomposition[,"ass"]==assnames[i],]
		  SSprodPanelist=t(decompositionByAss[,"prodEffect"])%*%decompositionByAss[,"prodEffect"];DfprodPanelist=nprod-1
		  SSerrorPanelist=t(decompositionByAss[,"err"])%*%decompositionByAss[,"err"];DFerrPanelist=nprod*(nrep-1)
		  RapportFisherPanelist[i]=(SSprodPanelist/(DfprodPanelist*nrep))/((SSprodPanelist/(DfprodPanelist*nrep))+(SSerrorPanelist/(DFerrPanelist)))
		}
		RapportFisherPanelist=as.vector(RapportFisherPanelist)
		names(RapportFisherPanelist)=assnames
		L[["RapportFisherPanelist"]]=RapportFisherPanelist
	## Fin de l'essai ]
	}
	
	if(panelistPerf){L[["tTest"]]=tTestPanelist;L[["listPanelistPerf"]]=listPanelistPerf;}
	if(modelType=="overall"){L[["CorrectedBeta"]]=t(CorrectedBeta);L[["UsualBeta"]]=t(UsualBeta)}
	return(L)
}


