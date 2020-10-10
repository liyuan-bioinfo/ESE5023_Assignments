#author: LiYuan
#time: 20200930
#task: 
  #Write a function Find_expression, which should be able to print 
  #every possible solution that makes the expression 
  #evaluate to a random integer from 1 to 100. 
  #For example, Find_expression(50) should print lines include: [Math Processing Error]
  #and [Math Processing Error]

#设计思路:
  ##1, 计算出符号全排列,再将符号插入数字中
  ##2, 将字符串转化成表达式进行计算即可

rm(list=ls())
library("gtools")
#reference:https://bbs.csdn.net/topics/220069520
Find_expression <- function(x){ # x为表达式计算的结果值
  
  ##建立符号与数字的所有组合
  sign <- c("+","-","") #1 + ; 0 -
  tmp_sign <- permutations(3,8,sign,repeats.allowed=TRUE) #获取所有可能的组合,6561
  
  count_num <-0
  for(i in 1:length(tmp_sign[,1])){
    tmp_sum <- 1
    
    for(j in 2:9){
      tmp_sum <- paste(tmp_sum,tmp_sign[i,j-1],j,sep="")
    }
      #print(tmp_sum)
      #计算表达式的值
      exp_sum <- eval(parse(text=tmp_sum))
      if(exp_sum == x){
        #print(paste(tmp_sum,exp_sum,sep="="))
        count_num <- count_num + 1
      }
    
  }
  return(count_num)
}
count_sum <- Find_expression(50)
print(count_sum)

#[1] "12-3-4-5+67-8-9=50"
# [1] "12-3+45+6+7-8-9=50"
# [1] "12+3+4-56+78+9=50"
# [1] "1-23-4-5-6+78+9=50"
# [1] "1-23+4+5-6+78-9=50"
# [1] "1-2-34-5-6+7+89=50"
# [1] "1-2-3-4-5-6+78-9=50"
# [1] "1-2-3+4+56-7-8+9=50"
# [1] "1-2+34-5-67+89=50"
# [1] "1-2+34+5+6+7+8-9=50"
# [1] "1-2+3-45+6+78+9=50"
# [1] "1+2-34+5-6-7+89=50"
# [1] "1+2-3+4+56+7-8-9=50"
# [1] "1+2+34-56+78-9=50"
# [1] "1+2+34-5-6+7+8+9=50"
# [1] "1+2+3-4+56-7+8-9=50"
# [1] "1+2+3+4-56+7+89=50"
Find_expression(100)
# [1] "123-45-67+89=100"
# [1] "123-4-5-6-7+8-9=100"
# [1] "123+45-67+8-9=100"
# [1] "123+4-5+67-89=100"
# [1] "12-3-4+5-6+7+89=100"
# [1] "12+3-4+5+67+8+9=100"
# [1] "12+3+4+5-6-7+89=100"
# [1] "1+23-4+56+7+8+9=100"
# [1] "1+23-4+5+6+78-9=100"
# [1] "1+2+34-5+67-8+9=100"
# [1] "1+2+3-4+5+6+78+9=100"

#5.2 [5 points] Count the total number of suitable solutions for any integer 
#i from 1 to 100, assign the count to Total_solutions[i]. 
#Plot the vector Total_solutions, 
#so which number(s) yields the maximum and minimum of Total_solutions?
Total_solutions <- c()
for(i in 1:100){
  Total_solutions <- c(Total_solutions,Find_expression(i))
}
plot(1:100,Total_solutions,type = "l",lwd=0.5,col="blue",
     xlab = "1:100",ylab="count of Expression")

which(Total_solutions ==max(Total_solutions))
Total_solutions[1]
Total_solutions[45]
which(Total_solutions ==min(Total_solutions))
Total_solutions[88]
