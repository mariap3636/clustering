
RunNBCLUST1 = function(aString1,aString2,aMatrix,OptNumCl){

### the modified version has an higher tolerance for matrix issue

source("NbClust.modified.R")

A<-aString1
B<-aString2

# I have decide to explore a small range taking into account that the difference between the minimum and the maximum number of clusters 
#must be at least equal to 2
#so I would put OptNumCl -2; a range is mandatory, it is not possible to have min.nc=max.nc



nbclustSchemesRes=list()
nbclust1=vector()

nbclustSchemesRes[[A]] <- NbClust(aMatrix, diss=NULL, distance = "euclidean", min.nc=OptNumCl-2, max.nc=OptNumCl, method = "kmeans", index = "all")
nbclust1<- nbclustSchemesRes[[A]]$"Best.partition" 

TMP_CluMethRes[[A]][[B]]<-nbclust1

return(TMP_CluMethRes[[A]][[B]])



}