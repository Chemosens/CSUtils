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