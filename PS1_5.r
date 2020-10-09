#author: LiYuan
#time: 20200930
#task: 
  #Write a function Find_expression, which should be able to print 
  #every possible solution that makes the expression 
  #evaluate to a random integer from 1 to 100. 
  #For example, Find_expression(50) should print lines include: [Math Processing Error]
  #and [Math Processing Error]

#设计思路:
  ##1, 计算出符号以及数字的全排列,其中符号的长度应与数字全排列相对应.即8个符号对应9个数字.
      ### 符号长度的范围为1 - 8; 数字长度的范围为 2-9
  ##2, 根据不同长度进行遍历,将符号与数字进行全排列,并计算结果值.

rm(list=ls())
library("gtools")
Find_expression <- function(x){ # x为表达式计算的结果值
  
  ##建立符号与数字的所有组合
  num <- c(1:8)  #origin digital
  sign <- c("+","-") #1 + ; 0 -
  full_sign <- list() #存储所有的sign情况
  
  for(i in 1:8){#i step
    tmp_sign <- permutations(2,i,sign,repeats.allowed=TRUE) #获取所有可能的step
    full_sign[[i]] <- tmp_sign
    
    
  }
  full_digital <- list() #存储所有的digital情况
  index <- c(1:9) #1 + ; 0 -
  for(i in 1:9){ #插入一个符号时,有8个位置;插入两个符号时,有8*7/2个位置;...;插入8个符号时,只有1个位置.
    
    tmp_digital <- combinations(9,2,index,repeats.allowed=FALSE) #获取所有可能的索引,索引之和要为8
    
    full_digital[[i]] <- tmp_digital
    for(j in 1: length(tmp_digital)){
      j <- 1
      tmp_digital[32,]
      tmp_fun(tmp_digital[32,])
    }

  }

  ##计算不同插入方式时的结果值
  #插入1个符号时
  tmp_signs <- full_sign[[1]]
  tmp_indexs <- full_digital[[1]]
  for(tmp_sign in tmp_signs){
    for(tmp_index in tmp_index){
      seq(tmp_index,1,by=1)
      if(tmp_sign == "+"){
        
      }
      }
  }
}


##功能: 通过方法,将数字集合拆分成连续的和非连续的集合
##部分引用: https://bbs.pinggu.org/thread-7605949-1-1.html
##例如: c(1 | 2 | 3,4,5,6 | 7 | 8 | 9) → 1, 2, 3456, 7, 8, 9
tmp_fun <- function(a){
  #a = c(6,8,12,13,14,15,46,1,2,3)
  result = vector("list", 100)
  b = c(a[-1],0)
  c = b - a
  k = 1
  result[[1]] = a[1]
  for(i in 1:length(c))
  {
    
    if(c[i] == 1)
    {
      result[[k]] = c(result[[k]], a[i+1])
    }
    if(c[i] != 1)
    {
      k = k+1
      result[[k]] = a[i+1]
    }
    
  }
  result = result[1:(k-1)]
  tmp_c <- c()
  for(i in result){
    tmp_c <- c(tmp_c,tmp_list_digital(i))
  }
  for(diff in setdiff(1:9,a)){
    tmp_c <- c(tmp_c,tmp_list_digital(diff))
  }
  return(tmp_c)
}
a<-c(1,2,3,4,5,6,8) #1-6, 8
b<-c(2,3,4,5,6,8) #2-6, 8
c<-c(1,3,4,5,6,8) #1,3-6, 8
result <- tmp_fun(c)

##将list类型转换成数值型
  #list(1,2,3,4) → 1234
tmp_list_digital <- function(a){
  a_char <- c()
  for(i in a){
    a_char <- paste(a_char,i,sep="")
  }
  return(as.integer(a_char))
  
}
tmp_list_digital(tmp_a)
