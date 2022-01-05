data("tds")
library(ggrepel)
# comparison of CVA for extended data with prcomp
extendedData=reshape2::dcast(tds$durations, product+subject+rep~descriptor,mean)
rescva1=chemosensR:::CVA(extendedData)
rescva2=chemosensR:::CVA(extendedData,nbDimHotelling=1)
resSum=sum(rescva1$HotellingTable$hotellingTable!=rescva2$HotellingTable$hotellingTable)!=0
rescva1=CVAgg(tds$durations)
rescva2=CVAgg(tds$durations,nbDimHotelling=1)
test_that("cva hotelling",
         expect_true(sum(rescva1$hotelling!=rescva2$hotelling)!=0)
)

plotCVAgg(rescva1)
plotCVAgg(rescva1,type="var")