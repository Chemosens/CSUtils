library(CSUtils)
library(testthat)
data(duration)
library(ggrepel)
# comparison of CVA for extended data with prcomp
extendedData=reshape2::dcast(duration, product+subject+rep~descriptor,mean)
respca=PCA(extendedData)
respcagg=turnToPCAgg(respca)
plotPCAgg(respcagg)




