library(CSUtils)
library(testthat)
data(duration)
library(ggrepel)
# comparison of CVA for extended data with prcomp
extendedData=reshape2::dcast(duration, product+subject+rep~descriptor,mean)
rescva_ow=CVAgg(duration,representation="twoMaps",option="OneWayANOVA")

rescva_tw=CVAgg(duration,representation="twoMaps",option="TwoWayANOVA")
#rescva_tws=CVA(extendedData,representation="twoMaps",option="tws")
rescva_mam=CVAgg(duration,representation="twoMaps",option="MAM")
rescva_overall=CVAgg(duration,representation="twoMaps",option="MultiMAM")
#rescva_1w=CVA(cheeses,representation="twoMaps",option="1w")



rescva_owb=CVAgg(duration,option="OneWayANOVA")
rescva_twb=CVAgg(duration,option="TwoWayANOVA")
#rescva_twsb=CVA(extendedData,option="tws")
rescva_mamb=CVAgg(duration,option="MAM")
rescva_overallb=CVAgg(duration,option="MultiMAM")

plotCVAgg(rescva_twb,type="distanceBiplot",text=TRUE,n=10,colorInd="all",substrVec=c(1,2),axes=c(1,2),indSup=c("ell"),repel=FALSE,revertX=FALSE,revertY=FALSE,sizeText=NULL)
plotCVAgg(rescva_twb,type="distanceBiplot",text=TRUE,n=10,colorInd="all",substrVec=c(1,2),axes=c(1,2),indSup=c("ell","points"),repel=FALSE,revertX=FALSE,revertY=TRUE,sizeText=NULL)


p=  plotCVAgg(rescva_owb,type="distanceBiplot")
plotCVAgg(rescva_twb)
plotCVAgg(rescva_mamb)
plotCVAgg(rescva_overallb)

rescva1=CVA(extendedData,representation="twoMaps")
rescva2=CVA(extendedData,nbDimHotelling=1)
resSum=sum(rescva1$HotellingTable$hotellingTable!=rescva2$HotellingTable$hotellingTable)!=0
rescva1=CVAgg(duration)
rescva2=CVAgg(duration,nbDimHotelling=1)
test_that("cva hotelling",
         expect_true(sum(rescva1$hotelling!=rescva2$hotelling)!=0)
)


plotCVAgg(rescva1)
plotCVAgg(rescva1,type="var")

# Recovering same results as lda
extendedDataRep1=extendedData[extendedData[,"rep"]==1,]
rescva1=CVA(extendedDataRep1,option="OneWayANOVAMod",representation="twoMaps")
library(MASS)
reslda=lda(extendedDataRep1[,-c(1:3)],grouping=extendedDataRep1[,"product"])
test_that("Correlation of the weights of 1 between lda and cva (axis1)",
          expect_true(
            abs(cor(reslda$scaling,rescva1$EigenVectors[,1:2]))[1,1]==1
          ))

test_that("Correlation of the weights of 1 between lda and cva (axis2)",
          expect_true(
           abs( abs(cor(reslda$scaling,rescva1$EigenVectors[,1:2]))[2,2]-1)<1e-15
          ))

predTest<-predict(reslda)
predTest$x
rescva1$IndivCoord
rescva1_gg=turnToCVAgg(rescva1)
test_that("Correlation 1 between individual scores of lda and cva (axis1)",
          expect_true(
abs(cor(predTest$x[,1],rescva1_gg$indSup[,1]))==1
))

test_that("Correlation 1 between individual scores of lda and cva (axis 2)",
          expect_true(
abs(cor(predTest$x[,2],rescva1_gg$indSup[,2]))>1-1e-15
          ))

cor(rescva1$EigenVectors[,1:2])
cor(reslda$scaling)
sum(reslda$scaling[,1]^2)/

sum(rescva1$EigenVectors[,1]^2)

sum(reslda$scaling[,2]^2)/
  sum(rescva1$EigenVectors[,2]^2)

# Comparison between cva 1w et lda
#======================================
reslda=lda(extendedData[,-c(1:3)],grouping=extendedData[,"product"])
test_that("Correlation of the weights of 1 between lda and cva 1w (axis1)",
          expect_true(abs(cor(reslda$scaling,rescva_ow$EigenVectors[,1:2])[1,1])==1)
)
test_that("Correlation of the weights of 1 between lda and cva 1w (axis2)",
          expect_true(round(abs(cor(reslda$scaling,rescva_ow$EigenVectors[,1:2])[2,2])-1,digits=12)==0)
)


# Comparison to older packages
#=================================
rescvaOldB=CVAS::CVA(  data=extendedData,  test = "Hotelling-Lawley",  option = "tw",  hotellingTableBool = TRUE,  nbAxes = "auto",
                    alpha = 0.1,  productName = "product",  subjectName = "subject",  replicateName = "rep",  sessionName = "session",
                    representation = "biplot"
)
rescvaOld=CVAS::CVA(  data=extendedData,  test = "Hotelling-Lawley",  option = "tw",  hotellingTableBool = TRUE,  nbAxes = "auto",
                   alpha = 0.1,  productName = "product",  subjectName = "subject",  replicateName = "rep",  sessionName = "session",
                   representation = "twoMaps"
)


test_that("cva indiv similar",
          expect_true(sum(round(rescva_tw$IndivCoord[,1:2]-rescvaOld$IndivCoord[,1:2],digits=8)!=0)==0)
)
test_that("cva indiv b similar",
          expect_true(sum(round(rescva_twb$IndivCoord[,1:2]-rescvaOldB$IndivCoord[,1:2],digits=8)!=0)==0)
)

test_that("cva var similar",
          expect_true(sum(round(rescva_tw$VarCoord[,1:2]-rescvaOld$VarCoord[,1:2],digits=8)!=0)==0)
)
test_that("cva var b similar",
          expect_true(sum(round(rescva_twb$VarCoord[,1:2]-rescvaOldB$VarCoord[,1:2],digits=8)!=0)==0)
)


