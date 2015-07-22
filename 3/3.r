setwd("/home/afk/Codes/Algo/3/")

source("mincutmod.r")

if (!file.exists("kargerMinCut.txt")){
        fileURL<-"http://spark-public.s3.amazonaws.com/algo1/programming_prob/kargerMinCut.txt";
        download.file(fileURL, destfile = "kargerMinCut.txt", method="wget");
}

file<-"kargerMinCut.txt"
ncol <- max(count.fields(file ))
G<-read.table(file, fill = TRUE, header = FALSE,col.names = paste0("V", seq_len(ncol)))

G<-t( t(G)[2:ncol(G),] )
temp<-ncol(G)

for (i in 1:2000){ v<-mincut(G); if (v<temp ) temp<-v  }

###sometimes reading input is bad and there are some prepared actions below

#G<-read.csv("kargerMinCut.txt", sep="\t", header = FALSE)
#G<-read.table("1.txt", fill = TRUE,header=FALSE, flush=TRUE)

# strsplit( as.matrix(G),"  ")
# G<-strsplit( as.matrix(G),"  ")
 
# G<-lapply(G, as.numeric)
##G<-read.csv("2.txt", sep="\t", header = FALSE, fill= TRUE)




