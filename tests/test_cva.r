library(CSUtils)
library(testthat)
data(duration)
library(ggrepel)
# comparison of CVA for extended data with prcomp
extendedData=reshape2::dcast(duration, product+subject+rep~descriptor,mean)
rescva1=CVA(extendedData)

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
rescva1=CVA(extendedDataRep1,option="OneWayANOVA")
library(MASS)
reslda=lda(extendedDataRep1[,-c(1:3)],grouping=extendedDataRep1[,"product"])
test_that("Correlation of the weights of 1 between lda and cva (axis1)",
          expect_true(
            abs(cor(reslda$scaling,rescva1$EigenVectors[,1:2]))[1,1]==1
          ))

test_that("Correlation of the weights of 1 between lda and cva (axis2)",
          expect_true(
            abs(cor(reslda$scaling,rescva1$EigenVectors[,1:2]))[2,2]==1
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
abs(cor(predTest$x[,2],rescva1_gg$indSup[,2]))==1
          ))

cor(rescva1$EigenVectors[,1:2])
cor(reslda$scaling)
sum(reslda$scaling[,1]^2)/

sum(rescva1$EigenVectors[,1]^2)

sum(reslda$scaling[,2]^2)/
  sum(rescva1$EigenVectors[,2]^2)
