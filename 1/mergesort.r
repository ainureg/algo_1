merge_sort<- function (up, left=1, right=length(up))
{
        down<-vector(length = length(up))
        
        ## условие выхода
        if (left == right)
        {
                down[left] <-  up[left]
                return(down);
        }
                
        middle<-ceiling( ((left + right-1) * 0.5) );
        
        ## разделяй и сортируй
        #l_buff<-vector(length = middle)
        #r_buff<-vector(length = right-middle)
        l_buff = merge_sort(up, left, middle);
        r_buff = merge_sort(up, middle + 1, right);
        
        ## слияние двух отсортированных половин
        l_cur<- left
        r_cur<- middle + 1
        for (i in left:right)
        {       
                if (l_cur <= middle & r_cur <= right)
                {
                        if (l_buff[l_cur] < r_buff[r_cur])
                        {
                                down[i] <- l_buff[l_cur];
                                l_cur<-l_cur +1;
                        }
                        else
                        {
                                down[i]<- r_buff[r_cur];
                                r_cur<-r_cur+1;
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
                       
                }
        }
        
        
        down;
        
        }