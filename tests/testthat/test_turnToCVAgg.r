library(CSUtils)
library(testthat)
data(duration)
library(ggrepel)
# comparison of CVA for extended data with prcomp
extendedData=reshape2::dcast(duration, product+subject+rep~descriptor,mean)
rescva_owgg=CVAgg(duration,representation="twoMaps",option="OneWayANOVA")

rescva_ow=CVA(extendedData,option="OneWayANOVA")
rescvagg=turnToCVAgg(rescva_ow)
plotCVAgg(rescvagg)

extendedData2=extendedData[extendedData[,"rep"]==1,]
extendedData2[,"rep"]=as.factor(as.character(extendedData2[,"rep"]))
summary(extendedData2)

rescva_ow=CVA(extendedData2,option="OneWayANOVA")
rescvagg=turnToCVAgg(rescva_ow)
plotCVAgg(rescvagg)


