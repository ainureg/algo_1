qsort <- function(S,n=0,m) {
     
        if (length(S)>1)
        {
#                 m<-Mfunc(S,m)
#               
                M<-Mfunc(S,m)
                
                pivot<-S[M]
                temp<- S[M]
                S[M]<-S[1]
                S[1]<-temp
                
                t<-1
                for (i in 2:(length(S) ))
                {
                        if (S[i]<pivot) 
                        {
                                t<-t+1 
                                temp<- S[i] 
                                S[i]<-S[t]
                                S[t]<-temp
                        }
                }
                
                temp<- S[1]
                S[1]<-S[t]
                S[t]<-temp
                
                if (length(S)==2)
                {
                        n<-n+1
                        list(S,n,m)
                }
                        else if ( ((t)==length(S)) )
                        {
                                l1<-qsort(S[1:(t-1)],n,m)
                                S<-c(l1[[1]], pivot)
                                n<-l1[[2]] +n+length(S)-1
                        }

                        else if (t==1)
                        {
                                l2<-qsort(S[(t+1):length(S)] ,n,m)
                                S<-c(pivot,l2[[1]])
                                n<-n+l2[[2]]+length(S)-1
                        } 

                        else 
                        {
                                l1<-qsort(S[1:(t-1)],n,m)
                                l2<-qsort(S[(t+1):length(S)] ,n,m)
                                S<-c(l1[[1]],pivot,l2[[1]] )
                                n<-n+l1[[2]] +l2[[2]]+length(S)-1
                        }
                list(S,n,m)
                }
        else list(S,n,m)
}


Mfunc <- function(S,m) {
        if (m==1) 
                return (1)
        if (m==2) 
                return (length(S))
        if (m==3)
        {
                temp<- median( c( S[1],S[length(S)], S[ceiling(length(S)/2)] )  )
                if (S[1]== temp)
                        return (1)
                if (S[length(S)]==temp)
                        return (length(S))
                if  (S[ceiling(length(S)/2)]==temp)
                        return (ceiling(length(S)/2 ) )
        }
}        
        
        
        
