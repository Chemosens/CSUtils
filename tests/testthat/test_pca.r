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

#      1.a.i Similar individuals
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
#   1.b. Similar covariance biplot
#      1.b.i Similar individuals
#      1.b.ii Similar variables
#      1.b.iii Similar eigenvalues
#   1.c. Similar correlation biplot
#      1.c.i Similar individuals
#      1.c.ii Similar variables
#      1.c.iii Similar eigenvalues
#====================================
# 2. Raw PCA on covariance
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
#   2.b. Similar covariance biplot
        resbiplot_raw=PCA(extendedData,option="Covariance",dataType="raw",representation="distanceBiplot")
#      2.b.i Similar individuals
#      2.b.ii Similar variables
#      2.b.iii Similar eigenvalues
#   2.c. Similar correlation biplot
#      2.c.i Similar individuals
#      2.c.ii Similar variables
#      1.c.iii Similar eigenvalues
#====================================
# 3.Correlation PCA on Product means
#====================================
#   3.a. Similar two maps plot
#      3.a.i Similar individuals
#      3.a.ii Similar variables correlations
#      3.a.iii Similar eigenvalues
#   3.b. Similar covariance biplot
#      3.b.i Similar individuals
#      3.b.ii Similar variables
#      3.b.iii Similar eigenvalues
#   3.c. Similar correlation biplot
#      3.c.i Similar individuals
#      3.c.ii Similar variables
#      3.c.iii Similar eigenvalues
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
# test_that("same eigenvalues (up to a factor) (productMeans, cov): ",
#           pcteig=respca_tm_cov$EigenValues/sum(respca_tm_cov$EigenValues)
#           expect_true(
#             sum(respca_facto_tm_cov$eig[,1]
#                       -100*pcteig[1:2])==0
#           ))
#

#   4.b. Similar covariance biplot
resbiplot=PCA(extendedData,option="Covariance",representation="distanceBiplot")
respca2=PCA(extendedData2,representation="twoMaps")
test_that("Distance Biplot and PCA have same representations when dataType is productMeans",
          expect_true(
            max(respca2$IndivCoord-resbiplot$IndivCoord)<1e-12
          ))
#      4.b.i Similar individuals
#      4.b.ii Similar variables
#      4.b.iii Similar eigenvalues
#   4.c. Similar correlation biplot
#      4.c.i Similar individuals
#      4.c.ii Similar variables
#      4.c.iii Similar eigenvalues#
#7. Further tests

#
# # Equalities for option="Covariance" and for PCA on product means
# #-------------------------------------------
# test_that("raw similar to usual when only product columns",
#           expect_true(
# all.equal(respca[1:9],respca_raw[1:9])
#           ))
#
# test_that("raw similar to usual when only product columns (biplot)",
#           expect_true(
#             all.equal(resbiplot[1:9],resbiplot_raw[1:9])
#           ))
#
# SVD DECOMPOSITION
# Equalities of eigenvectors
#
# test_that("eigen vectors",
#   expect_true(
#     sum(respca_theo$rotation==respca$EigenVectors)==24
#     )
# )

# # Equalities of eigenvalues
# inertiaTheo=(respca_theo$sdev^2/sum(respca_theo$sdev^2))
# inertiaObtained=    (respca$EigenValues/sum(respca$EigenValues))
# diffInertia=round(abs(inertiaTheo-inertiaObtained))<1e-16
# test_that("eigen values",
#   expect_true(
#     sum(diffInertia) ==3
#     )
# )

# VARIABLE COORDINATES

# # Equalities of variables correlations (case of two maps)
# round(respca_theoF$var$cor-respca$VarCoord[,1:2],digits=10e-15)
# cor(respca_theoF$var$coord,
# respca$EigenVectors)
#
# sum(respca_theoF$var$coord[,1]^2)
# sum(apply(extendedData[,-1],2,sd))
# # Equalities of variable coordinates
#
# # biplot case
# test_that("biplot: ",
#           expect_true(
# sum(as.matrix(resbiplot$IndivCoord)%*%t(as.matrix(resbiplot$VarCoord))-resbiplot$B>1e-12)==0
#           ))
# Supplementary individuals
#respca2$IndSup==respca_theoF2

## Equalities for option="Correlation"
#-------------------------------------------
respcaCor=PCA(extendedData,option="Correlation",representation="twoMaps")
respcaCor_raw=PCA(extendedData,option="Correlation",dataType="raw",representation="twoMaps")

respcaCor_theo=prcomp(extendedData[,-1],retx=TRUE,scale.=TRUE)
respcaCor_theoF=FactoMineR::PCA(extendedData[,-1],scale=TRUE)
biplotCor=PCA(extendedData,option="Correlation",representation="distanceBiplot")
biplotCor_raw=PCA(extendedData,option="Correlation",representation="distanceBiplot")


test_that("raw similar to usual when only product columns (cor)",
          expect_true(
            all.equal(respcaCor[1:9],respcaCor_raw[1:9])
          ))

test_that("raw similar to usual when only product columns (distance biplot with cor option)",
          expect_true(
            all.equal(biplotCor[1:9],biplotCor_raw[1:9])
          ))


test_that("distance biplot pca and pca have same individuals:)",
          expect_true(
            all.equal(biplotCor$IndivCoord,respcaCor$IndivCoord)
          ))


test_that("eigen vectors",
          expect_true(
            sum(respcaCor_theo$rotation==respcaCor$EigenVectors)==24
            )
)

# Equalities of eigenvalues
inertiaTheoCor=(respcaCor_theo$sdev^2/sum(respcaCor_theo$sdev^2))
inertiaObtainedCor=    (respcaCor$EigenValues/sum(respcaCor$EigenValues))
diffInertia=round(abs(inertiaTheoCor-inertiaObtainedCor))<1e-16
test_that("eigen values",
          expect_true(
            sum(diffInertia) ==3
            )
)


# Equalities of variables coordinates as FactoMineR (two maps)










#===========================================================================
# Gestion d'un jeu de données avec deux produits et des valeurs manquantes
#============================================================================
# Test PCA avec deux produits et valeurs manquantes

data(df2prod)
respca=PCA(df2prod)
respca=PCA(df2prod,option="Correlation")

