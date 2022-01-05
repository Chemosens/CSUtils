data("tds")

#====================================
# Coherence between PCA and prcomp
#====================================
extendedData=reshape2::dcast(tds$durations, product~descriptor,mean)
extendedData2=reshape2::dcast(tds$durations, product+subject~descriptor,mean)

# Equalities for option="Covariance" 
#-------------------------------------------
respca=PCA(extendedData,representation="twoMaps")
respca_raw=PCA(extendedData,representation="twoMaps",dataType="raw")
respca_theo=prcomp(extendedData[,-1],retx=TRUE)
respca_theoF=FactoMineR::PCA(extendedData[,-1],scale=FALSE)
respca_theoF2=FactoMineR::PCA(rbind(extendedData[,-1],extendedData2[,-c(1,2)]),scale=FALSE,ind.sup=4:(nrow(extendedData2)+3))
resbiplot=PCA(extendedData,option="Covariance",representation="distanceBiplot")
resbiplot_raw=PCA(extendedData,option="Covariance",dataType="raw",representation="distanceBiplot")
respca2=PCA(extendedData2,representation="twoMaps")
apply(respca$VarCoord^2,1,sum)

test_that("raw similar to usual when only product columns",
          expect_true(
all.equal(respca[1:9],respca_raw[1:9])
          ))

test_that("raw similar to usual when only product columns (biplot)",
          expect_true(
            all.equal(resbiplot[1:9],resbiplot_raw[1:9])
          ))

# SVD DECOMPOSITION 
# Equalities of eigenvectors

test_that("eigen vectors",
  expect_true(
    sum(respca_theo$rotation==respca$EigenVectors)==24
    )
)

# Equalities of eigenvalues
inertiaTheo=(respca_theo$sdev^2/sum(respca_theo$sdev^2))
inertiaObtained=    (respca$EigenValues/sum(respca$EigenValues))
diffInertia=round(abs(inertiaTheo-inertiaObtained))<1e-16
test_that("eigen values",
  expect_true(
    sum(diffInertia) ==3
    )
)

# VARIABLE COORDINATES

# Equalities of variables correlations (case of two maps)
round(respca_theoF$var$cor-respca$VarCoord[,1:2],digits=10e-15)

# Equalities of variable coordinates
test_that("variable correlation: axe 1 (opt cov): ",
          expect_true(
            sum(round(respca_theoF$var$cor[,1]+respca$VarCoord[,1],digits=10e-15))==0
          ))
          
test_that("variable correlation: axe 2(opt cov): ",
                    expect_true(
                    sum(  round(respca_theoF$var$cor[,2]-respca$VarCoord[,2],digits=10e-15))==0
 ))

# biplot case
test_that("biplot: ",
          expect_true(
sum(as.matrix(resbiplot$IndivCoord)%*%t(as.matrix(resbiplot$VarCoord))-resbiplot$B>1e-12)==0
          ))
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
test_that("variable correlation: axe 1 (opt cor): ",
          expect_true(
sum(round(respcaCor_theoF$var$cor[,1]+respcaCor$VarCoord[,1],digits=10e-15))==0
          ))

test_that("variable correlation: axe 2: (opt cor)",
          expect_true(
sum(round(respcaCor_theoF$var$cor[,2]-respcaCor$VarCoord[,2],digits=10e-15))==0
      ))

# biplot case








#===========================================================================
# Gestion d'un jeu de données avec deux produits et des valeurs manquantes
#============================================================================
# Test PCA avec deux produits et valeurs manquantes

data(df2prod)
respca=PCA(df2prod)
respca=PCA(df2prod,option="Correlation")

