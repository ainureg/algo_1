 quicksort<- function (S)
{
        ## условие выхода
        if (length(S)>1){
        
        pivot<-S[1]
        c(quicksort(S[S<pivot]), pivot, quicksort(S[S>pivot]))
        }
        else S
}