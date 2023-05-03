#'@param dataFrame data.frame avec en colonnes SubjectCode, ProductCode, Replicate, Y
#' @param model ThreeWayMultiplicativeRepeated, ThreeWayMultiplicative, TwoWayMultiplicative, TwoWayAdditiveByRep, TwoWayAdditiveByProduct, TwoWayAdditiveBySubject, OneWayBySubject, OneWayByProduct
#' @param randomSubject TRUE/FALSE
#' @param correlationStructure non utilis? pour l'instant
#' @param testRep non utilis? pour l'instant
#' @param lsMeansAlpha seuil pour la comparaison de moyennes post-hoc
#' @param lsMeansAdjustment m?thode d'ajustement pour la comparaison de moyennes post-hoc
#' @param ml  si on utilise ou pas le mod?le mixte (it?ratif)

#dataFrame=data orderByF
#colnames(dataFrame)=c("Replicate","ProductCode","SubjectCode","Y")
#'@import lme4
#'@import lsmeans
#'@import sjmisc
#'@importFrom car Anova
#'@import multcomp
#'@export
AnovaV2=function(dataFrame,model,randomSubject=T,correlationStructure="AR1",testRep="EachRepVsPrevious",lsMeansAlpha=0.10, lsMeansAdjustment="Tukey", varianceTest="None", varianceTestAlpha=0.05, normalityTest="None", normalityTestAlpha=0.05, anovaCalculationMode="Ols")
{
	if(anovaCalculationMode=="Ols")
	{
		ml=FALSE
	} else
	{
		ml=TRUE
	}

	if (model=="TwoWayAdditive")
	{
		model="TwoWayAdditiveByRep"
	}

	# conditions for the function
	if(!model %in% c("ThreeWayMultiplicativeRepeated","ThreeWayMultiplicative","TwoWayMultiplicative","TwoWayAdditiveByRep","TwoWayAdditiveByProduct","TwoWayAdditiveBySubject","OneWayBySubject","OneWayByProduct"))
	{
		stop('Please choose a valuable model among "ThreeWayMultiplicativeRepeated","ThreeWayMultiplicative","TwoWayMultiplicative","TwoWayAdditiveByRep","TwoWayAdditiveByProduct","TwoWayAdditiveBySubject","OneWayBySubject","OneWayByProduct"')
	}

	# if(lsMeansAdjustment!="NoGroup")
	# {
	# 	LoadPackage("lsmeans")
	# }
	#
	# LoadPackage("sjmisc")
	# LoadPackage("car")

	dataFrame$Replicate=as.factor(ordered(as.numeric(dataFrame$Replicate)))
	dataFrame$ProductCode=as.factor(dataFrame$ProductCode)
	dataFrame$SubjectCode=as.factor(dataFrame$SubjectCode)

	products=unique(as.character(dataFrame$ProductCode))
	replicates=unique(as.character(dataFrame$Replicate))
	subjects=unique(as.character(dataFrame$SubjectCode))

	if (length(products)<2& !model=="TwoWayAdditiveByProduct"&!model=="OneWayByProduct")
	{
		stop("ANOVA: Min products = 2");
	}
	if (length(subjects)<2 & !model=="TwoWayAdditiveBySubject"&!model=="OneWayBySubject")
	{
		stop(" ANOVA : Min subjects = 2");
	}
	if (length(replicates)<2 && (model=="ThreeWayMultiplicativeRepeated" || model=="ThreeWayMultiplicative" || model=="TwoWayMultiplicative" || model=="TwoWayAdditiveByProduct" || model=="TwoWayAdditiveBySubject"))
	{
		stop(paste("For ",model,", Min replicates = 2",sep=""))
	}

	# Test de normalit? et de variance
	if(!(varianceTest %in% c("Bartlett","Fligner")))
	{
		warnings("Wrong varianceTest, please choose between 'none','Bartlett' or 'Fligner'.");
		varianceTest="None"
	}

	if(!(normalityTest %in% c("ShapiroWilks","KolmogorovSmirnov")))
	{
		warnings("Wrong varianceTest, please choose between 'none','ShapiroWilks' or 'KolmogorovSmirnov'");
		normalityTest="None"
	}

	reslm0=list()
	reslm=list()
	resdata=list()
	resdata[[1]]=dataFrame

	reslsmeans=list()


	# Pour SS type 3
	options(contrasts=c("contr.helmert","contr.poly"))

	# Effets ? tester avec lsmeans, en fonction du mod?le
	lsmeansEffects=""

	if (model=="ThreeWayMultiplicativeRepeated")
	{
		### Sujet+Produit+Rep+Interactions, mesure r?p?t?e
		# Fixe, non test?
		reslm0[[1]]<-lm(Y ~ ProductCode+Replicate+SubjectCode+ProductCode:SubjectCode+ProductCode:Replicate+SubjectCode:Replicate,data=dataFrame)
		# Al?atoire
		if (randomSubject==T)
		{
			LoadPackage("nlme")
			# %SAS : valeurs diff?rentes
			reslm[[1]]<-lme(Y~ProductCode*Replicate, random=~1|SubjectCode/ProductCode, correlation=corAR1(), data=dataFrame, na.action = (na.omit), method = "REML")
			# Cette m?thode teste contre l'interaction double produit*sujet
		}
		if (randomSubject==F)
		{
			# Non test?
			#reslm=lme(Y~ProductCode*SubjectCode, random=~1|Replicate, correlation=corAR1(), data=dataFrame, na.action = (na.omit), method = "REML", weight = varIdent())

		}
		lsmeansEffects=c("ProductCode","Replicate")
	}

	if (model=="ThreeWayMultiplicative")
	{
		### Sujet+Produit+Rep+Interactions
		# Fixe
		reslm0[[1]]<-lm(Y ~ ProductCode+Replicate+SubjectCode+ProductCode:SubjectCode+ProductCode:Replicate+SubjectCode:Replicate,data=dataFrame)
		# Al?atoire
		if (randomSubject==T  && ml==T)
		{
			# %SAS : FProd OK, les 2 autres sont l?g?rement diff?rents
			#reslm[[1]]=lme(Y ~ ProductCode*Replicate, random = ~1|SubjectCode/ProductCode/Replicate,data=dataFrame, na.action = (na.omit), method="REML")
			LoadPackage("lme4")
			reslm[[1]]<-lmer(Y~ProductCode+(1|SubjectCode)+Replicate+ProductCode*Replicate+(1|ProductCode:SubjectCode)+(1|Replicate:SubjectCode),data=dataFrame)
		}
		lsmeansEffects=c("ProductCode","Replicate")
	}

	if (model=="TwoWayMultiplicative")
	{
		### Sujet+Produit+Interaction
		# Fixe
		reslm0[[1]]<-lm(Y ~ ProductCode*SubjectCode,data=dataFrame)
		# Al?atoire
		if (randomSubject==T && ml==T)
		{
			# Non test?
			#reslm[[1]]=lme(Y ~ ProductCode, random = ~1|SubjectCode/ProductCode,data=dataFrame, na.action = (na.omit), method="REML")
			LoadPackage("lme4")
			reslm[[1]]<-lmer(Y ~ ProductCode+(1|SubjectCode)+(1|ProductCode:SubjectCode), data=dataFrame)
		}
		lsmeansEffects=c("ProductCode")
	}

	if (model=="TwoWayAdditiveByRep")
	{
		### Sujet+Produit, par r?p
		for (i in 1:length(replicates))
		{
			subDataFrame=subset(dataFrame,Replicate==replicates[i])
			# Fixe
			reslm0[[i]]<-lm(Y ~ ProductCode+SubjectCode,data=subDataFrame)
			reslm0[[i]][["Name"]]<-paste("Replicate",replicates[i])
			resdata[[i]]=subDataFrame
			# Al?atoire
			if (randomSubject==T && ml==T)
			{
				# Non test?
				#reslm[[i]]=lme(Y ~ ProductCode, random = ~1|SubjectCode,data=subDataFrame, na.action = (na.omit), method="REML")

				reslm[[i]]<-lmer(Y ~ ProductCode+(1|SubjectCode), data=subDataFrame)
			}
		}
		lsmeansEffects=c("ProductCode")
	}

	if (model=="TwoWayAdditiveByProduct")
	{
		### Sujet+Replicate, par produit
		for (i in 1:length(products))
		{
			subDataFrame<-subset(dataFrame,ProductCode==products[i])
			# Fixe
			reslm0[[i]]<-lm(Y ~ Replicate+SubjectCode,data=subDataFrame)
			reslm0[[i]][["Name"]]<-paste("Product",products[i])
			resdata[[i]]=subDataFrame
			# Al?atoire
			if (randomSubject==T && ml==T)
			{
				# Non test?
				#reslm[[i]]=lme(Y ~ Replicate, random = ~1|SubjectCode,data=subDataFrame, na.action = (na.omit), method="REML")
				LoadPackage("lme4")
				reslm[[i]]<-lmer(Y ~ Replicate+(1|SubjectCode), data=subDataFrame)
			}
		}
		lsmeansEffects=c("Replicate")
	}

	if (model=="TwoWayAdditiveBySubject")
	{
		### Produit+Replicate, par sujet
		for (i in 1:length(subjects))
		{
			subDataFrame<-subset(dataFrame,SubjectCode==subjects[i])
			resdata[[i]]=subDataFrame
			# Fixe
			reslm0[[i]]<-lm(Y ~ ProductCode+Replicate,data=subDataFrame)
			reslm0[[i]][["Name"]]<-paste("Subject",subjects[i])
		}
		lsmeansEffects=c("ProductCode","Replicate")
	}

	if (model=="OneWayBySubject")
	{
		### Produit, par sujet
		for (i in 1:length(subjects))
		{
			subDataFrame<-subset(dataFrame,SubjectCode==subjects[i])

			# Fixe
			reslm0[[i]]<-lm(Y ~ ProductCode,data=subDataFrame)
			reslm0[[i]][["Name"]]<-paste("Subject",subjects[i])
			resdata[[i]]=subDataFrame
		}
		lsmeansEffects=c("ProductCode")
	}

	if (model=="OneWayByProduct")
	{
		### Sujet, par produit
		for (i in 1:length(products))
		{
			subDataFrame<-subset(dataFrame,ProductCode==products[i])

			# Fixe
			reslm0[[i]]<-lm(Y ~ SubjectCode,data=subDataFrame)
			reslm0[[i]][["Name"]]<-paste("Product",products[i])
			resdata[[i]]=subDataFrame
		}
		lsmeansEffects=c("SubjectCode")
	}

	diagnosticRep=function(reslm, resLsmeans,dataset, testRep="EachRepVsPrevious",lsMeansAlpha)
	{
		# Contrastes s?quentiels, test si la dur?e de dominance de l'attribut augmente ou diminue au fil des reps quel que soit le produit
		# Sens du test
		alternativeReplicate="greater"
		diagnosticReplicate="+"
		meanReps=resLsmeans[,c("Replicate","lsmean")]
		meanReps=meanReps[order(as.numeric(meanReps$Replicate)),"lsmean"]

		if (meanReps[1]>tail(meanReps,1))
		{
			alternativeReplicate="less"
			diagnosticReplicate="-"
		}

		if (testRep=="EachRepVsPrevious")
		{
			# Contraste : chaque niveau est test? par rapport au niveau pr?c?dent
			n=table(dataset$Replicate)
			contrReplicate=contrMat(n, type = "Sequen")
			res.glht=multcomp::glht(reslm, linfct = mcp(Replicate = contrReplicate),alternative=alternativeReplicate)
			res.summary.glht=summary(res.glht)
			ncomp=length(res.summary.glht$test$coefficients)
			pvaluesReplicate=res.summary.glht$test$pvalues[1:ncomp]
			if (sum(pvaluesReplicate<lsMeansAlpha)<ncomp)
			{
				diagnosticReplicate=""
			}
		}
		if (testRep=="EachRepVsFirst")
		{
			# Contraste : chaque niveau est test? par rapport au premier niveau
			res.glht=multcomp::glht(reslm, linfct = mcp(Replicate = "Tukey"),alternative=alternativeReplicate)
			res.summary.glht=summary(res.glht)
			# Tendance lin?aire si tous les estimates sont dans le m?me sens et contraste(dernier-premier) significatif
			indice=which(names(res.summary.glht$test$tstat)==paste(tail(replicates,1),"-",replicates[1]))
			if (res.summary.glht$test$pvalues[indice]>lsMeansAlpha)
			{
				diagnosticReplicate=""
			}
			if (alternativeReplicate=="greater" && length(which(res.summary.glht$test$coef<=0))>0)
			{
				diagnosticReplicate=""
			}
			if (alternativeReplicate=="less" && length(which(res.summary.glht$test$coef>=0))>0)
			{
				diagnosticReplicate=""
			}
		}
		return (diagnosticReplicate)
	}

	# Tableau de sorties
	output=list()
	for (i in 1:length(reslm0))
	{
		output[[i]]=list()
		if (!is.null(reslm0[[i]][["Name"]]))
		{
			output[[i]][["Name"]]=reslm0[[i]][["Name"]]
		}
		output[[i]][["Data"]]=resdata[[i]]

		# ANOVA type III effets fixes
		if(sum(abs(reslm0[[i]]$residuals))<0.0000001)
		{
			resAnova=NULL
			output[[i]][["FProd"]]=Inf
			output[[i]][["PProd"]]=0
			warning(" Exact fitting in the ANOVA model. Could be due to a too small number of observations compared to the different effects. You should try another ANOVA model")

		} else
		{
			resAnova=car::Anova(reslm0[[i]],type=3)
		}
		if (!is.null(resAnova["ProductCode","F value"]) && !is.na(resAnova["ProductCode","F value"]))
		{
			output[[i]][["FProd"]]=resAnova["ProductCode","F value"]
			output[[i]][["PProd"]]=resAnova["ProductCode","Pr(>F)"]
			if (ml==FALSE && randomSubject==TRUE)
			{
				if (model=="TwoWayAdditiveByRep")
				{
					output[[i]][["FProd"]]=resAnova["ProductCode","F value"]
					output[[i]][["PProd"]]=resAnova["ProductCode","Pr(>F)"]
				}
				if (model=="TwoWayMultiplicative"||model=="ThreeWayMultiplicative")
				{
					output[[i]][["FProd"]]=(resAnova["ProductCode","Sum Sq"]/resAnova["ProductCode","Df"])/(resAnova["ProductCode:SubjectCode","Sum Sq"]/resAnova["ProductCode:SubjectCode","Df"])
					output[[i]][["PProd"]]=pf(output[[i]][["FProd"]], resAnova["ProductCode","Df"], resAnova["ProductCode:SubjectCode","Df"], lower.tail=FALSE)
				}
			}
			output[[i]][["DiagProd"]]=Asterisks(output[[i]][["PProd"]])

			if ("ProductCode" %in% lsmeansEffects && lsMeansAdjustment!="noGroup")

			{

				resLsmeans=lsmeans(reslm0[[i]],~ProductCode, data=reslm0[[i]]$model)
				output[[i]][["MoyProd"]]=cld(resLsmeans,alpha=lsMeansAlpha,adjust=lsMeansAdjustment)
				 if(ml==FALSE && randomSubject==TRUE)
				 {
					if(model=="TwoWayMultiplicative"||model=="ThreeWayMultiplicative")
					{
						#reslm0[[i]]=lm(Y~SubjectCode*ProductCode,data=avgData)
						avgData=aggregate(dataFrame[,"Y"],mean,by=list(dataFrame[,"ProductCode"],dataFrame[,"SubjectCode"]));colnames(avgData)=c("ProductCode","SubjectCode","Y")
						reslmGroup=lm(Y~SubjectCode+ProductCode,data=avgData)
						resLsmeans=lsmeans(reslmGroup,~ProductCode, data=reslmGroup$model)
						output[[i]][["MoyProd"]]=cld(resLsmeans,alpha=lsMeansAlpha,adjust=lsMeansAdjustment,level=1-lsMeansAlpha)
					}

				 }
				 output[[i]][["resLsmeans"]]=resLsmeans
			}
		}

		if (!is.null(resAnova["SubjectCode","F value"]) && !is.na(resAnova["SubjectCode","F value"]))
		{
			if(randomSubject==TRUE&&ml==FALSE)
			{
				if(model=="ThreeWayMultiplicative"||model=="TwoWayMultiplicative")

				{
					output[[i]][["FSubj"]]=(resAnova["SubjectCode","Sum Sq"]/resAnova["SubjectCode","Df"])/(resAnova["ProductCode:SubjectCode","Sum Sq"]/resAnova["ProductCode:SubjectCode","Df"])
					output[[i]][["PSubj"]]=pf(output[[i]][["FSubj"]], resAnova["SubjectCode","Df"], resAnova["ProductCode:SubjectCode","Df"], lower.tail=FALSE)
					output[[i]][["DiagSubj"]]=Asterisks(output[[i]][["PSubj"]])
				}
				else
				{
					output[[i]][["FSubj"]]=resAnova["SubjectCode","F value"]
					output[[i]][["PSubj"]]=resAnova["SubjectCode","Pr(>F)"]
					output[[i]][["DiagSubj"]]=Asterisks(output[[i]][["PSubj"]])
				}
			}
			else
			{
			output[[i]][["FSubj"]]=resAnova["SubjectCode","F value"]
			output[[i]][["PSubj"]]=resAnova["SubjectCode","Pr(>F)"]
			output[[i]][["DiagSubj"]]=Asterisks(output[[i]][["PSubj"]])
			}
			if ("SubjectCode" %in% lsmeansEffects && lsMeansAdjustment!="NoGroup")
			{
				resLsmeans=lsmeans(reslm0[[i]],~ SubjectCode, data=reslm0[[i]]$model)
				output[[i]][["MoySubj"]]=cld(resLsmeans,alpha=lsMeansAlpha,adjust=lsMeansAdjustment,level=1-lsMeansAlpha)
			}
		}

		if (!is.null(resAnova["Replicate","F value"]) && !is.na(resAnova["Replicate","F value"]))
		{
			output[[i]][["FRep"]]=resAnova["Replicate","F value"]
			output[[i]][["PRep"]]=resAnova["Replicate","Pr(>F)"]
			output[[i]][["DiagRep"]]=Asterisks(output[[i]][["PRep"]])
			if(randomSubject==TRUE&&ml==FALSE)
			{
				if(model=="ThreeWayMultiplicative"||model=="TwoWayMultiplicative")
				{
					output[[i]][["FRep"]]=(resAnova["Replicate","Sum Sq"]/resAnova["Replicate","Df"])/(resAnova["Replicate:SubjectCode","Sum Sq"]/resAnova["Replicate:SubjectCode","Df"])
					output[[i]][["PRep"]]=pf(output[[i]][["FRep"]], resAnova["Replicate","Df"], resAnova["Replicate:SubjectCode","Df"], lower.tail=FALSE)
					output[[i]][["DiagRep"]]=Asterisks(output[[i]][["PRep"]])
				}
			}

			if ("Replicate" %in% lsmeansEffects && lsMeansAdjustment!="NoGroup")

			{
				resLsmeans=lsmeans(reslm0[[i]],~ Replicate, data=reslm0[[i]]$model)
				output[[i]][["MoyRep"]]=cld(resLsmeans,alpha=lsMeansAlpha,adjust=lsMeansAdjustment,level=1-lsMeansAlpha)

				# Contrastes s?quentiels, test si la dur?e de dominance de l'attribut augmente ou diminue au fil des reps quel que soit le produit
				if (output[[i]][["PRep"]]<=lsMeansAlpha)
				{
					diagnosticReplicate=diagnosticRep(reslm=reslm0[[i]], resLsmeans=output[[i]][["MoyRep"]],dataset=resdata[[i]], testRep,lsMeansAlpha)
					if (diagnosticReplicate!="")
					{
						output[[i]][["DiagRep"]]=paste(output[[i]][["DiagRep"]],"(",diagnosticReplicate,")",sep="")
					}
				}
			}
		}

		if (!is.null(resAnova["ProductCode:Replicate","F value"]))
		{
			output[[i]][["FProdRep"]]=resAnova["ProductCode:Replicate","F value"]
			output[[i]][["PProdRep"]]=resAnova["ProductCode:Replicate","Pr(>F)"]
			output[[i]][["DiagProdRep"]]=Asterisks(output[[i]][["PProdRep"]])
		}

		if (!is.null(resAnova["Replicate:SubjectCode","F value"]))
		{
			output[[i]][["FSubjRep"]]=resAnova["Replicate:SubjectCode","F value"]
			output[[i]][["PSubjRep"]]=resAnova["Replicate:SubjectCode","Pr(>F)"]
			output[[i]][["DiagSubjRep"]]=Asterisks(output[[i]][["PSubjRep"]])
		}

		if (!is.null(resAnova["ProductCode:SubjectCode","F value"]))
		{
			output[[i]][["FProdSubj"]]=resAnova["ProductCode:SubjectCode","F value"]
			output[[i]][["PProdSubj"]]=resAnova["ProductCode:SubjectCode","Pr(>F)"]
			output[[i]][["DiagProdSubj"]]=Asterisks(output[[i]][["PProdSubj"]])
		}

		# output[[i]][["RMSE"]]=rmse(reslm0[[i]])
		# cette valeur est egale ?  = sqrt(mean(reslm0[[1]]$residuals^2))
		# calcul de la residuelle prenant en compte la perte de degr?s de libert?s
		output[[i]][["RMSE"]]=sqrt(sum(reslm0[[1]]$residuals^2)/reslm0[[1]]$df.residual)
		output[[i]][["Residuals"]]=summary(reslm0[[1]])$residuals

		if (varianceTest!= "None")
		{
			output[[i]][["VarianceTestResult"]]=list()
			if (!is.null(resAnova["ProductCode","F value"]) && !is.na(resAnova["ProductCode","F value"]))
			{
				output[[i]][["VarianceTestResult"]][["Product"]] = FriendlyPValue((VarianceTest(resdata[[i]], y="Y",  x="ProductCode" ,test=varianceTest))$p.value)
			}
			if (!is.null(resAnova["Replicate","F value"]) && !is.na(resAnova["Replicate","F value"]))
			{
				output[[i]][["VarianceTestResult"]][["Replicate"]] = FriendlyPValue((VarianceTest(resdata[[i]], y="Y",  x="Replicate" ,test=varianceTest))$p.value)
			}
			if (!is.null(resAnova["SubjectCode","F value"]) && !is.null(resAnova["SubjectCode","F value"]))
			{
				output[[i]][["VarianceTestResult"]][["Subject"]] = FriendlyPValue((VarianceTest(resdata[[i]], y="Y",  x="SubjectCode" ,test=varianceTest))$p.value)
			}
		}

		if (normalityTest!= "None")
		{
			resNt=TestResidualsNormality((summary(reslm0[[i]]))$residuals, test=normalityTest)
			output[[i]][["NormalityTest"]] = FriendlyPValue(resNt$p.value)
		}

		# ANOVA effets al?atoire
		if (length(reslm)>0)
		{
			res.summary=summary(reslm[[i]])
			if (class(reslm[[i]])[1]=="lme")
			{
				resAnova=anova.lme(reslm[[i]],test="F")
			}
			if (class(reslm[[i]])[1]=="lmerMod")
			{
				resAnova=car::Anova(reslm[[i]],type=3,test="F")
				names(resAnova)=c("F-value","Df","Df.res","p-value")
			}

			if (!is.null(res.summary$modelStruct$corStruct[[1]]))
			{
				# Coefficient de corr?lation entre les observations
				output[[i]][["Phi"]]=res.summary$modelStruct$corStruct[[1]]
			}

			if (!is.null(resAnova["ProductCode","F-value"]))
			{
				output[[i]][["FProd"]]=resAnova["ProductCode","F-value"]
				output[[i]][["PProd"]]=resAnova["ProductCode","p-value"]
				output[[i]][["DiagProd"]]=Asterisks(output[[i]][["PProd"]])

				if ("ProductCode" %in% lsmeansEffects && lsMeansAdjustment!="noGroup")

				{
					resLsmeans=lsmeans::lsmeans(reslm[[i]], ~ ProductCode, data=resdata[[i]])
					output[[i]][["MoyProd"]]=cld(resLsmeans,alpha=lsMeansAlpha,adjust=lsMeansAdjustment,level=1-lsMeansAlpha)
				}
			}

			if (!is.null(resAnova["Replicate","F-value"]))
			{
				output[[i]][["FRep"]]=resAnova["Replicate","F-value"]
				output[[i]][["PRep"]]=resAnova["Replicate","p-value"]
				output[[i]][["DiagRep"]]=Asterisks(output[[i]][["PRep"]])

				if ("Replicate" %in% lsmeansEffects && lsMeansAdjustment!="noGroup")
				{
					resLsmeans=lsmeans(reslm[[i]], ~ Replicate, data=resdata[[i]])
					output[[i]][["MoyRep"]]=cld(resLsmeans,alpha=lsMeansAlpha,adjust=lsMeansAdjustment,level=1-lsMeansAlpha)

					if (output[[i]][["PRep"]]<=lsMeansAlpha)
					{
						diagnosticReplicate=diagnosticRep(reslm=reslm[[i]], resLsmeans=output[[i]][["MoyRep"]], dataset=resdata[[i]],testRep,lsMeansAlpha)
						if (diagnosticReplicate!="")
						{
							output[[i]][["DiagRep"]]=paste(output[[i]][["DiagRep"]],"(",diagnosticReplicate,")",sep="")
						}
					}
				}
			}

			if (!is.null(resAnova["ProductCode:Replicate","F-value"]))
			{
				output[[i]][["FProdRep"]]=resAnova["ProductCode:Replicate","F-value"]
				output[[i]][["PProdRep"]]=resAnova["ProductCode:Replicate","p-value"]
				output[[i]][["DiagProdRep"]]=Asterisks(output[[i]][["PProdRep"]])
			}

			output[[i]][["RMSE"]]=sqrt(mean(summary(reslm[[1]])$residuals^2))
			output[[i]][["Residuals"]]=summary(reslm[[1]])$residuals

			# verifier que c'est bien ce rmse que l'on veut... a priori, on n'ajuste pas les degres de libert? ici, il s'agit juste d'une moyenne des r?sidus du mod?le
			if (!is.null(resAnova["SubjectCode","F value"]))
			{
				output[[i]][["VarianceTestResult"]][["SubjectCode"]] = ""
			}

			if (normalityTest!= "None")
			{
				resNt=TestResidualsNormality((summary(reslm[[i]]))$residuals, test=normalityTest)
				output[[i]][["NormalityTest"]] = FriendlyPValue(resNt$p.value)
			}
		}

		output[[i]][["LSMeansAdjustment"]]=lsMeansAdjustment
		output[[i]][["LSMeansAlpha"]]=lsMeansAlpha
		output[[i]][["Model"]]=model

	}
	return (output)
}