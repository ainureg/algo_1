setwd("/home/afk/Codes/Algo/2/")


source("Quicksort.r")

if (!file.exists("QuickSort.txt")){
        fileURL<-"http://spark-public.s3.amazonaws.com/algo1/programming_prob/QuickSort.txt";
        download.file(fileURL, destfile = "IntegerArray.txt", method="wget");
}
S<-read.table("QuickSort.txt")
S<-S[,]
quicksort(S)

