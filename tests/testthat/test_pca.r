# Tests in this scripts
library(CSUtils)
library(testthat)
data(duration)
# Data for tests
#===============
# PCA sur les moyennes des produits
extendedData=reshape2::dcast(duration, product~descriptor,mean)
# PCA sur les moyennes sujet*produit
extendedData2=reshape2::dcast(duration, product+subject~descriptor,mean)
# PCA sur les moyennes sujet*produit*rep
extendedData3=reshape2::dcast(duration, product+subject+rep~descriptor,mean)

# PCA function has several main options: correlation and covariance, raw or productMeans, and two maps and biplot
# The tests are organized as follows:
# - First PCA raw was compared to PCA on product means on extended data
# - PCA raw was compared to usual factominer PCA (for correlation or covariance)
#====================================
# 1. Raw PCA on correlation
#====================================
#   1.a. Similar two maps plot
       respca_tm_cor=CSUtils::PCA(extendedData,representation="twoMaps",option="correlation")
       respca_raw_tm_cor=CSUtils::PCA(extendedData,representation="twoMaps",dataType="raw",option="correlation")
       respca_theo_tm_cor=prcomp(extendedData[,-1],retx=TRUE,scale=TRUE)
       respca_facto_tm_cor=FactoMineR::PCA(extendedData[,-1],scale.unit = T)

       test_that("same result for dataType=raw or not (for only product): ",
                 expect_true(
                   all.equal(respca_tm_cor[1:9],respca_raw_tm_cor[1:9])
                   ))
# Correlation raw PCA similar to factominer's (two maps)
#====================================
#     1.a.i Similar individuals
       test_that("same individuals raw data, correlation",
                 expect_true(
                   sum(round(diag(abs(cor(respca_facto_tm_cor$ind$coor[,1:2],respca_raw_tm_cor$IndivCoord[,1:2])))))==2
                                    ))
#      1.a.ii Similar variables correlations
       test_that("same variable raw data, correlation",
                 expect_true(
                   sum(respca_theo_tm_cor$rotation==respca_raw_tm_cor$EigenVectors)==24
                 ))

       test_that("same variable raw data, correlation",
                 expect_true(
                      cor(respca_facto_tm_cor$var$coord[,1:2],
                      respca_raw_tm_cor$VarCor[,1:2])[1,1]==-1
                 ))
#      1.a.iii Similar eigenvalues
       test_that("similar eigenvalue, raw, correlation",
                 expect_true(
                   cor(respca_theo_tm_cor$sdev^2,respca_tm_cor$EigenValues)==1
                 ))

#====================================
# 2. Raw PCA on covariance - comparison to factominer
#====================================
       respca_tm_cov=CSUtils::PCA(extendedData,representation="twoMaps",option="Covariance")
       respca_raw_tm_cov=CSUtils::PCA(extendedData,representation="twoMaps",dataType="raw",option="Covariance")
       respca_theo_tm_cov=prcomp(extendedData[,-1],retx=TRUE,scale=F)
       respca_facto_tm_cov=FactoMineR::PCA(extendedData[,-1],scale.unit =F)

#   2.a. Similar two maps plot
      #      2.a.i Similar individuals
      test_that(" individual correlation: axe 1 (opt cor): ",
                expect_true(
                  round(cor(respca_raw_tm_cov$IndivCoord[,1],respca_facto_tm_cov$ind$coord[,1]),digits=12)==-1
                  ))
#      2.a.ii Similar variables correlations
        test_that("variable correlation: axe 1 (opt cor): ",
          expect_true(
              sum(round(respca_raw_tm_cov$VarCor[,1]+respca_facto_tm_cov$var$cor[,1]))==0
          ))
        test_that("variable correlation: axe 2 (opt cor): ",
                  expect_true(
                    sum(round(respca_raw_tm_cov$VarCor[,2]-respca_facto_tm_cov$var$cor[,2]))==0
                  ))

#      2.a.iii Similar eigenvalues
        test_that("similar eigen values: ",
                  expect_true(
        100*(respca_raw_tm_cov$EigenValues/sum(respca_raw_tm_cov$EigenValues))[1]== respca_facto_tm_cov$eig[1,2]
                  ))

#====================================
# 4. Covariance PCA on product means
#====================================
#   4.a. Similar two maps plot
        respca_tm_cov=CSUtils::PCA(extendedData2,option="Covariance",dataType ="productMeans")
        respca_facto_tm_cov=FactoMineR::PCA(rbind(extendedData[,-1],extendedData2[,-c(1,2)]),scale=FALSE,ind.sup=4:(nrow(extendedData2)+3))
#        4.a.i Similar individuals
        test_that("same individuals data, cov, productMeans",
                  expect_true(
                    sum(round(diag(abs(cor(respca_facto_tm_cov$ind$coor[,1:2],respca_tm_cov$IndivCoord[,1:2])))))==2
                  ))

#      4.a.ii Similar variables correlations
test_that("variable correlation: axe 1 (opt cov): ",
          expect_true(
            sum(round(respca_facto_tm_cov$var$cor[,1]
                      +respca_tm_cov$VarCor[,1],digits=10e-15))==0
          ))

test_that("variable correlation: axe 2(opt cov): ",
          expect_true(
            sum(  round(respca_facto_tm_cov$var$cor[,2]-respca_tm_cov$VarCor[,2],digits=10e-15))==0
          ))

#      4.a.iii Similar eigenvalues
pcteig=respca_tm_cov$EigenValues/sum(respca_tm_cov$EigenValues)
 test_that("same eigenvalues (up to a factor) (productMeans, cov): ",
           expect_true(
             100*pcteig[1]-respca_facto_tm_cov$eig[1,2]<1e12
           ))


# Correlation product means
 #====================================
 # 4. Correlation PCA on product means
 #====================================
 #   4.a. Similar two maps plot
 respca_tm_cor=CSUtils::PCA(extendedData2,option="Correlation",dataType ="productMeans")
 respca_facto_tm_cor=FactoMineR::PCA(rbind(extendedData[,-1],extendedData2[,-c(1,2)]),scale=TRUE,ind.sup=4:(nrow(extendedData2)+3))
 #        4.a.i Similar individuals
 test_that("same individuals data, cor, productMeans",
           expect_true(
             sum(round(diag(abs(cor(respca_facto_tm_cor$ind$coor[,1:2],respca_tm_cor$IndivCoord[,1:2])))))==2
           ))

 #      4.a.ii Similar variables correlations
 test_that("variable correlation: axe 1 (opt cor): ",
           expect_true(
             sum(round(respca_facto_tm_cor$var$cor[,1]
                       +respca_tm_cor$VarCor[,1],digits=10e-15))==0
           ))

 test_that("variable correlation: axe 2(opt cor): ",
           expect_true(
             sum(  round(respca_facto_tm_cor$var$cor[,2]-respca_tm_cor$VarCor[,2],digits=10e-15))==0
           ))

 #      4.a.iii Similar eigenvalues
 pcteig=respca_tm_cor$EigenValues/sum(respca_tm_cor$EigenValues)
 test_that("same eigenvalues (up to a factor) (productMeans, cor): ",
           expect_true(
             100*pcteig[1]-respca_facto_tm_cov$eig[1,2]<1e12
           ))



#==========
# Tests for biplot : récupération des scores individuels dans tous les cas
#===================

resbiplot=PCA(extendedData,option="Covariance",representation="distanceBiplot")
respca2=PCA(extendedData2,representation="twoMaps")
test_that("Distance Biplot and PCA have same representations when dataType is productMeans",
          expect_true(
            max(respca2$IndivCoord-resbiplot$IndivCoord)<1e-12
          ))


# resbiplotCor=PCA(extendedData,option="Correlation",representation="distanceBiplot")
# respca2cor=PCA(extendedData2,representation="twoMaps",option="Correlation")
# test_that("Distance Biplot and PCA have same representations when dataType is productMeans",
#           expect_true(
#             max(respca2Cor$IndivCoord-resbiplotCor$IndivCoord)<1e-12
#           ))
#






#===========================================================================
# Gestion d'un jeu de données avec deux produits et des valeurs manquantes
#============================================================================
# Test PCA avec deux produits et valeurs manquantes

data(df2prod)
respca=PCA(df2prod)
respca=PCA(df2prod,option="Correlation")

