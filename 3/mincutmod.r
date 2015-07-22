mincut<-function(G){
        #remove<-list(0)
        l<-list(G[1,][complete.cases(G[1,])])
        for (i in 2:length(G[,1]))
        {
                l<-append(l,list(G[i,][complete.cases(G[i,])]) )        
        }
        remain=(1:length(l))
        
        while (length(remain)!=2 )
        {      a<-sample(remain,1)
               b<-sample(l[[a]],1 )
               remain<-remain [! remain %in% max(c(a,b))]
               l[[min(a,b)]]<-c(l[[a]],l[[b]])[!c(l[[a]],l[[b]]) %in% c(a,b)]
               l<-lapply(l, reasign,min(a,b),max(a,b))
               remove<-append(remove,list(c(a,b)))               
        }
#         if (length(l[[remain[1]]])==1){
#                 print(l[[remain[1]]])
#                 print(l[[remain[2]]])
#                 remove
#         }
#         else
                length(l[[remain[1]]])
        
}
reasign<-function(v,a,b){
        v[v==b]<-a        
        v
}

