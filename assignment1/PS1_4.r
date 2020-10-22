#author: LiYuan
#time: 20200930
#task: 
  ##Here x is an integer randomly selected from 1 to 100. 
  ##Write a function Least_moves to print your results. 
  ##For example, Least_moves(2) should print 1, 
  ##and Least_moves(5) should print 3.

rm(list=ls())
library("gtools")

#设计思路: 
  ##1 计算所有可能的排列组合,最大的组合长度即为x-1;
  ##2 不同的排列组合根据长度进行区分, 存入list()中
  ##3 根据不同长度进行遍历(升序), 只要获取到正确值即结束程序,并输出当前的长度,即最短步骤.
Least_moves <- function(x){#X  money!
  ##求出全排列##
  num <- c(1,0)
  full_condition <- list() #存储所有的情况
  for(i in 1:(x-1)){#i step
    tmp <- permutations(2,i,num,repeats.allowed=TRUE) #获取所有可能的step
    full_condition[[i]] <- tmp
  }
  
  ##进行迭代遍历##
  for(conditions in full_condition){ #遍历1,获取不同长度的集合
    
    #计算不同方式的结果值
    #获取list的行
    rows <- length(conditions[,1]) #获取当前长度的集合数目
    for(condition_index in 1:rows){ #遍历2, 获取相同长度的不同集合
      
      condition <- conditions[condition_index,] #1 ... 16
      tmp_sum <- 1   
      for(step in condition){ #遍历3, 获取相同长度,相同集合的单个step
        
        if(step == 1){ #double 
          tmp_sum = tmp_sum * 2
        }else{ #add another 1
          tmp_sum = tmp_sum + 1
        }
      }
    
      #单个集合全部step结束后,进行判断当前长度是否能够满足
      if(tmp_sum == x  ){
        
        return(paste("money:",x," min_step:",length(condition),sep=" ")) #有最小结果就结束,不考虑多种情况
      }
    }

    
  }
}
#for validation
Least_moves(2) # 1
Least_moves(5) # 1

