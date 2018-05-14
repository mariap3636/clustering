
RunTScluCORThc = function(aString1,aString2,aMatrix,OptNumCl){

### pam strategy

A<-aString1
B<-aString2

#using diss mat
#diss mat

IP.dis <- diss(aMatrix, "CORT", k=OptNumCl, deltamethod="DTW")

tscluCORThc=vector()

# hierarchical cluster solution
IP.hclus <- cutree(hclust(IP.dis), k=OptNumCl)
#
tscluCORThc<-IP.hclus

 
TMP_CluMethRes[[A]][[B]]<-tscluCORThc

return(TMP_CluMethRes[[A]][[B]])



}