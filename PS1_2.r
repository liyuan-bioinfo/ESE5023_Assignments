##time:   2020年9月21日23:37:44
##author: LiYuan
##task:    Make two matrices M1 (5 rows and 10 columns ) and M2 (10 rows and 5 columns ); 
#           both are filled with random integers from 0 and 50.

rm(list=ls())
#---------------2.1---------------#
##Make two matrices M1 (5 rows and 10 columns )
M1 <- matrix(sample(c(0:50), 50, replace = TRUE), nrow = 5, ncol = 10)
M2 <- matrix(sample(c(0:50), 50, replace = TRUE), nrow = 10, ncol = 5)

M1 <- matrix(sample(c(0:1), 3, replace = TRUE), nrow = 2, ncol = 3)
M2 <- matrix(sample(c(0:3), 3, replace = TRUE), nrow = 3, ncol = 2)

output <- matrix(nrow=5,ncol=5)
for(i in 1:2){
  
  for(j in 1:3){
    for(jj in 1:2){
    #print(M1[i,j])
    print(M1[i,j] + M2[j,jj]) 
    }
  }
  print("#######")
  
}
#---------------2.2---------------#
##Write a function Matrix_multip to do matrix multiplication, 
##i.e., M1 * M2. Here you are only allowed to use for loop, * operator, and + operator. 
##Check your result with the %*% operator.
Matrix_multip <- function(M1, M2){
  for(i in M1)
}

M1 <- matrix(sample(c(0:1), 3, replace = TRUE), nrow = 2, ncol = 3)
M2 <- matrix(sample(c(0:3), 3, replace = TRUE), nrow = 3, ncol = 2)

M1 %*% M2 #for validate output of calling function
