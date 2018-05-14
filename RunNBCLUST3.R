
RunNBCLUST3 = function(aString1,aString2,aMatrix,OptNumCl){

### the modified version has an higher tolerance for matrix issue

source("NbClust.modified.R")

A<-aString1
B<-aString2

# I have decide to explore a small range taking into account that the difference between the minimum and the maximum number of clusters 
#must be at least equal to 2
#so I would put OptNumCl -2; a range is mandatory, it is not possible to have min.nc=max.nc

diss_matrix<- dist(aMatrix, method = "euclidean", diag=FALSE)# dist takes as input scaled data

nbclustSchemesRes=list()
nbclust3=vector()
nbclustSchemesRes[[A]] <- NbClust(aMatrix, diss=diss_matrix, distance = NULL, min.nc=OptNumCl-2, max.nc=OptNumCl, method = "ward.D", index = "all")

nbclust3<- nbclustSchemesRes[[A]]$"Best.partition" 

TMP_CluMethRes[[A]][[B]]<-nbclust3

return(TMP_CluMethRes[[A]][[B]])



}