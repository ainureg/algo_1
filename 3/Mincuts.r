mincuts<-function(G) {
        remain=(1:length(G[,1]))
        b<-sample(1:length(G[,1]),1)
        a<-sample(1:length(G[,1]),1)
        remain<-remain [! remain %in% c(a,b)]
        while ((length(a)+length(b))!=(length(G[,1])-3) ) 
        {
                t1<-as.vector( G[a,])[complete.cases( as.vector( G[a,]))]
                t2<-as.vector( G[b,])[complete.cases( as.vector( G[b,]))]  
                new_node<-sample(c(t1,t2)[is.element(c(t1,t2),remain)],1)
                remain<-remain [! remain %in% new_node]
                if (sum(t2==new_node) >0 )
                         b<-unique(c(b,new_node))
                else #if (length(setdiff(new_node,a)) <1 &length(setdiff(new_node,b))==1 )
                         a<-unique( c(a,new_node))
#                 else
#                 if (sample(1:2,1)==1)
#                         b<-unique(c(b,new_node))
#                 else
#                         a<-unique( c(a,new_node))
        }
        a<-a[!a %in%0]
        b<-b[!b %in%0]
        
        t1<-as.vector( G[a,])[complete.cases( as.vector( G[a,]))]
        t2<-as.vector( G[b,])[complete.cases( as.vector( G[b,]))]  
        t1<-length(t1[!t1 %in% a] )
        t2<-length(t2[!t2 %in% b])
        
        if (t1==0|t2==0)
                {t<-max(t1,t2)}
        else
                t<-min(t1,t2)
               
        t
}
