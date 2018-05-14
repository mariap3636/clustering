
RunLONGLUST = function(aString1,aString2,aMatrix,OptNumCl){

### the modified version has an higher tolerance for matrix issue


A<-aString1
B<-aString2

#I would put OptNumCl -2; a range is mandatory, it is not possible to have min.nc=max.nc

clus <- longclustEM(aMatrix, OptNumCl-2, OptNumCl, linearMeans=TRUE)



loclu=vector()
longclustClLabels<-apply(clus$zbest, 1, which.max)
loclu<-longclustClLabels

TMP_CluMethRes[[A]][[B]]<-loclu

names(TMP_CluMethRes[[A]][[B]])<-rownames(aMatrix)

return(TMP_CluMethRes[[A]][[B]])



}