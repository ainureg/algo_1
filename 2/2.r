setwd("/home/afk/Codes/Algo/2/")


source("qsort_mod.r")

if (!file.exists("QuickSort.txt")){
        fileURL<-"http://spark-public.s3.amazonaws.com/algo1/programming_prob/QuickSort.txt";
        download.file(fileURL, destfile = "QuickSort.txt", method="wget");
}
S<-read.table("QuickSort.txt")
S<-S[,]
qsort(S,m=1)[[2]]
qsort(S,m=2)[[2]]
qsort(S,m=3)[[2]]


        