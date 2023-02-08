library(CSUtils)
library(testthat)
data(duration)
library(ggrepel)
# comparison of CVA for extended data with prcomp
extendedData=reshape2::dcast(duration, product+subject+rep~descriptor,mean)
rescva_ow=CVAgg(duration,representation="twoMaps",option="OneWayANOVA")
rescva_tw=CVA(extendedData,representation="twoMaps",option="OneWayANOVA")
turnToCVAgg(rescva_tw)
