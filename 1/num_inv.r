count_inv<-function(){

setwd("/home/afk/Codes/Algo/1/")

        if (!file.exists("IntegerArray.txt")){
                fileURL<-"http://spark-public.s3.amazonaws.com/algo1/programming_prob/IntegerArray.txt";
                download.file(fileURL, destfile = "IntegerArray.txt", method="wget");
        }
        
        
a<-read.table("IntegerArray.txt")
a<-a[,]


merge_sort<- function (up, left=1, right=length(up), n=0)
{
        down<-vector(length = length(up))
        
        ## условие выхода
        if (left == right)
        {
                down[left] <-  up[left]
                return(list(down,n));
        }
        
        middle<-ceiling( ((left + right-1) * 0.5) );
        
        ## разделяй и сортируй
        
        l_l <- merge_sort(up, left, middle,n);
        l_r <- merge_sort(up, middle + 1, right,n );
        l_buff <- l_l[[1]];
        r_buff<- l_r[[1]];
        n<-l_l[[2]]+l_r[[2]];
        ## слияние двух отсортированных половин
        l_cur= left
        r_cur<- middle + 1
        for (i in left:right)
        {       
                if (l_cur <= middle & r_cur <= right)
                { 
                        #print(c(l_l[[1]],11111 ) ) ;
                        if (l_buff[l_cur] < r_buff[r_cur])
                        {
                                down[i] <- l_buff[l_cur];
                                l_cur<-l_cur +1;
                        }
                        else
                        {
                                down[i]<- r_buff[r_cur];
                                r_cur<-r_cur+1;
                                n<- n+middle - l_cur+1;
                        }
                }
                else if (l_cur <= middle)
                {
                        down[i] <- l_buff[l_cur];
                        l_cur  <-  l_cur+1;
                        
                }
                else
                {
                        down[i] <-  r_buff[r_cur];
                        r_cur <-  r_cur+1;
                        n<- n+middle - l_cur+1;
                }
        }
        
        #c(down,n);
        list(down,n)
        
}

merge_sort(a)[[2]]
}