#' @importFrom stats pf
Manova=function(P,R,df2=NULL,test="Hotelling")
{ 
  # P : covariance matrix of the bloc effect
  # R : covariance matrix of the residuals
  # df2 : degree of freedom of the error term (nb.suj*nb.prod-1-nb.prod) si un facteur etc...
  # v=nb.prod*nb.suj*(nb.rep-1) si c'est l'erreur du modele
  # v= (nb.prod-1)*(nb.suj-1) si c'est l'interaction
  
  # pour le scaling
 

    p=qr(P+R)$rank #number of attributes
    q=qr(P)$rank #number of products -1
    s=min(p,q)
    m=(abs(p-q)-1)/2 #m
    v=df2 #error degre de liberte
     n=(v-p-1)/2 #n
	 invertible=TRUE

    invertible= tryCatch({vap=eigen(solve(R)%*%P)$values;s=TRUE},error=function(errors){errors=geterrmessage();print(errors);s=FALSE;return(s)})
	if(invertible)
   { 
    if(test=="Pillai")
    {
      stat=sum(vap/(1+vap))
      fapprox=((2*n+s+1)/(2*m+s+1))*stat/(s-stat)
      pvalue=pf(Re(fapprox),df1=(2*m+s+1)*s,df2=(2*n+s+1)*s,lower.tail=FALSE)
    }
    if(test=="Hotelling")
    { 
      stat=sum(vap)
      b=(p+2*n)*(q+2*n)/(2*(2*n+1)*(n-1))
      c=(2+(p*q+2)/(b-1))/(2*n)
      if(n>0)
      {
        fapprox=(stat/c)*((4+(p*q+2)/(b-1))/(p*q))
         pvalue=pf(Re(fapprox),df1=p*q,df2=4+(p*q+2)/(b-1),lower.tail=FALSE)
      }
      if(n<=0)
      {
        fapprox=(2*(s*n+1)*stat)/(s*s*(2*m+s+1))
        pvalue=pf(Re(fapprox),df1=s*(2*m+s+1),df2=2*(s*n+1),lower.tail=FALSE)
      }
    }
    if(test=="Wilks")
    {
      stat=prod(1/(1+vap))
      r=v-(p-q+1)/2
      u=(p*q-2)/4
      if(p*p+q*q-5>0){t= sqrt((p*p*q*q-4)/(p*p+q*q-5))}else{t=1}
      fapprox=((r*t-2*u)/(p*q))*(1-stat^(1/t))/stat^(1/t)
      pvalue=pf(Re(fapprox),df1=q*p,df2=r*t-2*u,lower.tail=FALSE)
      
    }
    if(test=="Roy")
    {
      stat=max(Re(vap))
      r=max(p,q)
      fapprox=((v-r+q)/r)*stat
      pvalue=pf(Re(fapprox),df1=r,df2=v-r+q,lower.tail=FALSE)
      
    }
    # if(test=="Bartlett")
    # {             
      # stat=-( (nRep-1)*nProd*nSuj-(nAtt+1-(nProd-1)) /2)*log(det(R)/det(R+P))
      # chis=qchisq(p=0.95,df=nAtt*(nProd-1))
      # pvalue=stat>chis
    # }
    # print(pvalue)
  }
  else
  {
    fapprox=NA
    pvalue=NA
    stat=NA
    invertible=FALSE
  }
  L=list(stat,fapprox,pvalue,invertible)
  names(L)=c("stat","f","pvalue","invertible")
  return(L)
}
# 
# # jeu test
# data=transf.data(file="dataProfil.txt",header=TRUE,sep=";",protocol="profile",nom="Profile")
# 
#  profileObject=ReadProfileData(file="dataProfil.txt",header=TRUE,sep=";")
# 
#  dataMFInd=profileObject$ExtendedData

#  dataMF2=dataMFInd[,c(2,1,3,5:dim(dataMFInd)[2])]
#  dataMF2[1,]
# 
# Y=as.matrix(dataMF2[,4:dim(dataMF2)[2]])
#
# Y=apply(Y,2,"scale",center=TRUE,scale=FALSE)
# produit=as.factor(dataMF2[,2])
# nb.prod=length(levels(produit))
# sujet=as.factor(dataMF2[,1])
# nb.suj=length(levels(sujet))
# nb.rep=length(levels(as.factor(dataMF2[,3])))
# 
# ####### CE CODE EST VALIDE POUR LA MANOVA A UN FACTEUR  ##########
# 
# man=manova(Y~produit)
# summary(man,"Hotelling")
# P=summary(man)$SS$produit
# R=summary(man)$SS$Residuals
# MaManova(P,R,nb.prod*nb.suj*nb.rep-nb.prod,"Hotelling")

#  # ceci est issu de l'aide de SAS
# ####### CE CODE EST VALIDE POUR LA MANOVA A UN FACTEUR  ##########
# # ceci est issu de SAS
# ####### CE CODE EST VALIDE POUR LA MANOVA A 2 FACTEURS   ##########
# man=manova(Y~produit+sujet+produit*sujet)
# summary(man)
# P=summary(man)$SS$produit
# R=summary(man)$SS$Residuals
# MaManova(P,R,nb.prod*nb.suj*(nb.rep-1))
# ## VALIDE POUR LA MANOVA EN PRENANT LA RESIDUELLE AVEC SAS ET R SAUF POUR HOTELLING OU ON A UNE NUANCE ENTRE R ET SAS -J AI PRIS SAS
# ## ATTENTIONN, PAR CONTRE, CE N EST PAS le meme resultat que quand on prend la manova sur les notes moyennees ! ! ! ! !
# 
# ########## POUR L INTERACTION ################
# man=manova(Y~produit+sujet+produit*sujet)
# summary(man)
# P=summary(man)$SS$produit
# R=summary(man)$SS$'produit:sujet'
# MaManova(P,R,(nb.prod-1)*(nb.suj-1)) 
# # OK POUR L INTERACTION ON A LA MEME QUE SAS AUSSI
# 
# 
