library(CSUtils)
library(testthat)
data(duration)
library(ggrepel)
# comparison of CVA for extended data with prcomp
extendedData=reshape2::dcast(duration, product+subject+rep~descriptor,mean)
rescva_ow=CSUtils::CVA(extendedData,representation="twoMaps",option="OneWayANOVA")
#plotCVAgg(rescva_ow)
rescva_tw=CVA(extendedData,representation="twoMaps",option="TwoWayANOVA")
#rescva_tws=CVA(extendedData,representation="twoMaps",option="tws")
rescva_mam=CVA(extendedData,representation="twoMaps",option="MAM")
rescva_overall=CVA(extendedData,representation="twoMaps",option="MultiMAM")
#rescva_1w=CVA(cheeses,representation="twoMaps",option="1w")

rescva_owb=CVA(extendedData,option="OneWayANOVA")
rescva_twb=CVA(extendedData,option="TwoWayANOVA")
#rescva_twsb=CVA(extendedData,option="tws")
rescva_mamb=CVA(extendedData,option="MAM")
rescva_overallb=CVA(extendedData,option="MultiMAM")
#rescva_1wb=CVA(extendedData,option="1w")

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
identicalUpToTol=function(x,y,tol=1e-12)
{
  return(all(x-y<tol))
}
# Recovering same results as lda
extendedDataRep1=extendedData[extendedData[,"rep"]==1,]
rescva1=CVA(extendedDataRep1,option="OneWayANOVAMod",representation="twoMaps")
library(MASS)
reslda1=lda(extendedDataRep1[,-c(1:3)],grouping=extendedDataRep1[,"product"])
test_that("Correlation of the weights of 1 between lda and cva (axis1)",
          expect_true(
            identicalUpToTol(cor(reslda1$scaling,rescva1$EigenVectors[,1:2])[1,1],-1)
          ))

test_that("Correlation of the weights of 1 between lda and cva (axis2)",
          expect_true(
            identicalUpToTol(cor(reslda1$scaling,rescva1$EigenVectors[,1:2])[2,2],-1)
          ))

predTest<-predict(reslda1)
predTest$x
rescva1$IndivCoord
rescva1_gg=turnToCVAgg(rescva1)
test_that("Correlation 1 between individual scores of lda and cva (axis1)",
          expect_true(
abs(cor(predTest$x[,1],rescva1_gg$indSup[,1]))==1
))

test_that("Correlation 1 between individual scores of lda and cva (axis 2)",
          expect_true(
round(abs(cor(predTest$x[,2],rescva1_gg$indSup[,2])),digits=14)==1
          ))

cor(rescva1$EigenVectors[,1:2])
cor(reslda1$scaling)
sum(reslda1$scaling[,1]^2)/

sum(rescva1$EigenVectors[,1]^2)

sum(reslda1$scaling[,2]^2)/
  sum(rescva1$EigenVectors[,2]^2)

# Comparison between cva 1w et lda
#======================================
reslda=lda(extendedData[,-c(1:3)],grouping=extendedData[,"product"])
test_that("Correlation of the weights of 1 between lda and cva 1w (axis1)",
          expect_true(abs(cor(reslda$scaling,rescva_ow$EigenVectors[,1:2])[1,1])==1)
)
test_that("Correlation of the weights of 1 between lda and cva 1w (axis2)",
          expect_true(round(abs(cor(reslda$scaling,rescva_ow$EigenVectors[,1:2])[2,2]),digits=12)==1)
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


