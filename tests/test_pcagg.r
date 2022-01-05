#=====================
# Test PCAgg et plot
#=====================
data(tds)
# Usual cov and cor
respcagg=PCAgg(tds$durations)
plotPCAgg(respcagg,type="ind",indSup=c("ell","points"))

respcaggCor=PCAgg(tds$durations,option="Correlation")
plotPCAgg(respcaggCor,type="ind",indSup=c("ell","points"))

# with 2 products and missing data
data(df2prod)
df_rsh=reshape(df2prod,direction="long",varying=list(colnames(df2prod)[-c(1,2)]),times=colnames(df2prod)[-c(1:2)])
colnames(df_rsh)=c("product","subject","descriptor","score","id")
df_rsh[,"rep"]=1

respca_cor=PCAgg(df_rsh[,-5],option="Correlation",representation="DistanceBiplot")
respcagg_cov=PCAgg(df_rsh[,-5],representation="DistanceBiplot")

plotPCAgg(respcagg_cov,indSup=c("ell","points"))
plotPCAgg(respca_cor,indSup=c("ell","points"))
plotPCAgg(respca_cor,indSup=c("ell","points"),type="biplot")
plotPCAgg(respcagg_cov,indSup=c("ell","points"),type="biplot")


plotPCAgg(respca_cor,indSup=c("ell","points"),type="cor1",n=20)
plotPCAgg(respca_cor,indSup=c("ell","points"),type="cor2",n=20)

df_rshw_wo_na=df_rsh[!df_rsh[,"descriptor"]%in% c("X.E..Non.2.enal","X2.pentylfuran"),]
df_rshw_wo_na[,"subject"]=c("04","05","07","11","13","14")

  
respca_cor_all=PCAgg(df_rshw_wo_na[,-5],option="Correlation",dataType="raw",representation="DistanceBiplot")
plotPCAgg(respca_cor_all,indSup=c("ell","points"),type="biplot",n=20)

test_that("biplot: ",
          expect_true(
            length(respca_cor_all)==8
                      ))



respcagg1=PCAgg(df=tds$durations,dataType="raw",option="Covariance",representation="TwoMaps")
respcagg2=PCAgg(df=tds$durations,dataType="productMeans",option="Covariance",representation="TwoMaps")
respcagg1c=PCAgg(df=tds$durations,dataType="raw",option="Correlation",representation="TwoMaps")
respcagg2c=PCAgg(df=tds$durations,dataType="productMeans",option="Correlation",representation="TwoMaps")

p1=plotPCAgg(respcagg1c,type="ind",text=TRUE)
p2=plotPCAgg(respcagg1c,type="corCircle")
p1
p2

p1=plotPCAgg(respcagg2c,type="ind",text=TRUE)
p2=plotPCAgg(respcagg2c,type="corCircle",text=FALSE)
p1
p2


p3=plotPCAgg(respcagg1c,type="cor1",n=10)
p4=plotPCAgg(respcagg1c,type="cor2",n=10)
p3
p4


