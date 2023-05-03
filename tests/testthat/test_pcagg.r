#=====================
# Test PCAgg et plot
#=====================
library(FactoMineR)
library(CSUtils)
data(duration)
library(testthat)
durationWide=reshape(data=duration,direction="wide",timevar="descriptor",idvar=c("subject","product","rep","period"))
# Usual cov and cor
respcagg12=PCAgg(duration)
respcagg13=PCAgg(duration,axes=list(c(1,3)))
plotPCAgg(respcagg13,type="ind",axes=c(1,3),indSup=c("ell","points"))


plotPCAgg(respcagg13,type="ind",indSup=c("ell","points"))

plotPCAgg(respcagg12,type="ind",indSup=c("ell","points"))
plotPCAgg(respcagg12,type="corCircle")

respcaggCor=PCAgg(duration,option="Correlation")
plotPCAgg(respcaggCor,type="ind",indSup=c("ell","points"))

# with 2 products and missing data
data(df2prod)
df_rsh=reshape(df2prod,direction="long",varying=list(colnames(df2prod)[-c(1,2)]),times=colnames(df2prod)[-c(1:2)])
colnames(df_rsh)=c("product","subject","descriptor","score","id")
df_rsh[,"rep"]=1

respca_cor=PCAgg(df_rsh[,-5],option="Correlation",representation="DistanceBiplot")
respcagg_cov=PCAgg(df_rsh[,-5],representation="DistanceBiplot")
respcagg_cov=PCAgg(df_rsh[,-5],representation="DistanceBiplot")

plotPCAgg(respcagg_cov,indSup=c("ell","points"),expandBiplot=0.2)
plotPCAgg(respca_cor,indSup=c("ell","points"))
plotPCAgg(respca_cor,indSup=c("ell","points"),type="biplot")
plotPCAgg(respcagg_cov,indSup=c("ell","points"),type="biplot")


plotPCAgg(respca_cor,indSup=c("ell","points"),type="cor1",n=20)
plotPCAgg(respca_cor,indSup=c("ell","points"),type="cor2",n=20)

df_rshw_wo_na=df_rsh[!df_rsh[,"descriptor"]%in% c("X.E..Non.2.enal","X2.pentylfuran"),]
df_rshw_wo_na[,"subject"]=c("04","05","07","11","13","14")


respca_cor_all=PCAgg(df_rshw_wo_na[,-5],option="Correlation",dataType="raw",representation="DistanceBiplot")
plotPCAgg(respca_cor_all,indSup=c("ell","points"),type="biplot",n=20)

test_that("biplot: ",
          expect_true(
            length(respca_cor_all)==8
                      ))



respcagg1=PCAgg(df=duration,dataType="raw",option="Covariance",representation="TwoMaps")
respcagg2=PCAgg(df=duration,dataType="productMeans",option="Covariance",representation="TwoMaps")
respcagg1c=PCAgg(df=duration,dataType="raw",option="Correlation",representation="TwoMaps")
respcagg2c=PCAgg(df=duration,dataType="productMeans",option="Correlation",representation="TwoMaps")

p1=plotPCAgg(respcagg1c,type="ind",text=TRUE)
p2=plotPCAgg(respcagg1c,type="corCircle")
p1
p2

p1=plotPCAgg(respcagg2c,type="ind",text=TRUE)
p2=plotPCAgg(respcagg2c,type="corCircle",text=FALSE)
p1
p2


p3=plotPCAgg(respcagg1c,type="cor1",n=10)
p4=plotPCAgg(respcagg1c,type="cor2",n=10)
p3
p4


# tds=tdsA
# tdsA$stdDominances=tdsA$stdDominances[tdsA$stdDominances[,"product"]%in%c("C1","C2","C3","C4"),]
# tdsA$stdDominance[,"product"]=as.factor(as.character(tdsA$stdDominance[,"product"]))
# tdsA$stdDominance[,"descriptor"]=as.factor(as.character(tdsA$stdDominance[,"descriptor"]))
#
# #if(class(tds)=="tds"){df=tds$stdDominances}else{df=tds}
# # Calcul sur temps standardisés
# df=tdsA$stdDominances
# periods=2
# df$period=((df$time-0.01)%/%(1/periods))+1
# df$period=sprintf("%02d", df$period)
# df$product=paste(df$product,df$period,sep="_period")
# levels(factor(df[,"product"]))
# df2=aggregate(score~product+subject+descriptor,df,sum)
# listCoord=PCAgg(df2,representation="DistanceBiplot",expandBiplot=1)
# plotPCAgg(listCoord)
#
# product_period=t(as.data.frame(stringr::str_split(listCoord$indivCoord$name,"_period")))
#
# listCoord$indivCoord$product=product_period[,1]
# listCoord$indivCoord$period=product_period[,2]
# listCoord$indivCoord$name2=listCoord$indivCoord$product
# listCoord$indivCoord$name2[listCoord$indivCoord$period!="1"]=""
#

# color_group <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666",
#                  "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666", "#1B9E77", "#D95F02", "#7570B3",
#                   "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
#                   "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6",
#                   "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2", "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC",
#                   "#CCCCCC", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
#                   "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072",
#                   "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")
#
# individual="arrows"
# fontSizeCex=1
# gg = gMapPlot(listCoord,title=title,facet="grid",cols="axes",type=individual,ellipses=FALSE,output="var", fontSizeCex=fontSizeCex)
# gg = gg
# smooth=FALSE
# if (smooth==FALSE) {
#   gg = gg + ggplot2::geom_point(data = listCoord$indivCoord, ggplot2::aes_string(x = "x", y = "y",col="product"),show.legend = FALSE) +
#     ggplot2::geom_path(data = listCoord$indivCoord,ggplot2::aes_string(x = "x", y = "y",col="product"), arrow=ggplot2::arrow(length = ggplot2::unit(0.2, "cm")))
# } else {
#   gg = gg + ggplot2::geom_smooth(data = listCoord$indivCoord,ggplot2::aes_string(x = "x", y = "y",col="product"),se = FALSE) +
#     ggplot2::update_geom_defaults("smooth", list(size = 0.6))
# }
# gg = gg + ggrepel::geom_text_repel(data = listCoord$indivCoord, ggplot2::aes_string(x = "x", y = "y", label="name",colour="product"), nudge_y = 0.5,size=3*fontSizeCex,segment.color = NA,show.legend = FALSE)
# #ggplot2::scale_colour_manual(values=color_group)
# gg

#bug avec inertie

#result=list()
#result[["output"]][["biplot"]]=gg
#result[["coordinates"]]=listCoord

#
# df2$rep="1"
# df2$period="1"
# formula=paste0("subject+product~","descriptor")
# formula=as.formula(formula)
# extendedData=reshape2::dcast(df2[,c("subject","product","score","descriptor")], formula=formula,function(x){return(mean(x,na.rm=T))},value.var="score",fill=0)
# resPca=PCA(extendedData,dataType="productMeans")
# listCoord=turnToPCAgg(resPca,axes=list(c(1,2)),representation="distanceBiplot",expandBiplot=expandBiplot)
#
#
# print(head(df2))

