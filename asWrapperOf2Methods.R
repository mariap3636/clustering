# Rscript --vanilla THIS_SCRIPT.R  INPUT_DATASET.csv  OUTPUT.RData
# EG.: Rscript --vanilla THIS_SCRIPT.R  norm_cellcycle_384_17.csv  OUTPUT.RData

##


args <- commandArgs(trailingOnly = TRUE)

hh <- paste(unlist(args),collapse=' ')
listoptions <- unlist(strsplit(hh,'--'))[-1]
options.args <- sapply(listoptions,function(x){
         unlist(strsplit(x, ' '))[-1]
        })
options.names <- sapply(listoptions,function(x){
  option <-  unlist(strsplit(x, ' '))[1]
})
names(options.args) <- unlist(options.names)
print(options.args)

####### libraries - R packages - local code 
library(plyr);library(NbClust);library(Mfuzz);library(Biobase);library(mclust);library(longclust);library(TSclust)
## modified R code
##fix(NbClust)
source("NbClust.modified.R")
# modify solve(w * statement by using a smaller tolerance, like solve(..., tol = 1e-17)



TMP_CluMethRes=list()


A<-"Cho" # Cho dataset affymetrix

########################################################## default input Cho dataset affymetrix ###########################################################
df = read.table(args[1], header=TRUE)
rownames(df)<-df$"Main"
vec.groundTruth<-df$"Gp"
#
df<-df[,c(-1,-2)]
########################################################## @@@@@@@@@@@@@@@@@@@@@@@@ ###########################################################
# raw dataset needs to be transformed in a matrix or an expression set or in a dataframe according to the specific requirements of each single clustering algorithm
df.mat<-as.matrix(df)


cat("generate mfuzz results","\n")
B<-"Mfuzz"
source("runMfuzz.R")
# df.mat is the input dataset
# 5 is the optimal number of clusters
# TMP_CluMethRes[[A]] is the list R object storing the memberships obtained using as input dataset the one associated to generic string A (raw file is loaded in R by args[1])

TMP_CluMethRes[[A]][[B]]<-RunMfuzz(A,B,df.mat,5)
save(TMP_CluMethRes,file=args[2]) # 

B<-"kmInMfuzz"
source("runKmeansInMfuzz.R")
# df.mat is the input dataset
# 5 is the optimal number of clusters
# TMP_CluMethRes[[A]] is the list R object storing the memberships obtained using as input dataset the one associated to generic string A (raw file is loaded in R by args[1])
TMP_CluMethRes[[A]][[B]]<-RunKmeansInMfuzz(A,B,df.mat,5)
save(TMP_CluMethRes,file=args[2]) #

library(corrplot)
library(mclust)

#finalRes=list()
clustSchemes1<-c("Mfuzz","kmInMfuzz")
#, "SSClust","nbclust1","nbclust3","tscluCORThc","tscCRTpam","loclu")
clustSchemes2<-c("Mfuzz","kmInMfuzz")
#, "SSClust","nbclust1","nbclust3","tscluCORThc","tscCRTpam","loclu")
#"Mfuzz","kmInMfuzz", "SSClust","nbclust1","nbclust3","tscluCORThc","tscCRTpam","loclu"
#"Mfuzz","kmInMfuzz", "SSClust","nbclust1","nbclust3","tscluCORThc","tscCRTpam" ,"loclu"

mAt <- matrix(0, nrow = length(clustSchemes1), ncol =length(clustSchemes2) , byrow = TRUE, dimnames = list(clustSchemes1,clustSchemes2))
temp<-mAt
for(i in 1:length(clustSchemes1)){
for(j in 1:length(clustSchemes2)){
cat(as.character(clustSchemes1[i]),as.character(clustSchemes1[j]),"\n")
temp[rownames(temp)==as.character(clustSchemes1[i]),colnames(temp)==as.character(clustSchemes1[j])] <-adjustedRandIndex(unlist(TMP_CluMethRes[[A]][i]),unlist(TMP_CluMethRes[[A]][j]))#
}
}
temp
#accumulatore<-accumulatore+temp

col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", "cyan", "#007FFF", "blue", "#00007F"))
#corrplot.mixed(aveMatrixBothGenderInsulin,col=col4(150))
corrplot.mixed(temp,col=col4(150))













