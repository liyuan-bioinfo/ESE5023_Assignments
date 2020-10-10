##time:   2020年9月21日23:37:44
##author: LiYuan
##task:    Make two matrices M1 (5 rows and 10 columns ) and M2 (10 rows and 5 columns ); 
#           both are filled with random integers from 0 and 50.

rm(list=ls())
#---------------2.1---------------#
##Make two matrices M1 (5 rows and 10 columns )
M1 <- matrix(sample(c(0:50), 50, replace = TRUE), nrow = 5, ncol = 10)
M2 <- matrix(sample(c(0:50), 50, replace = TRUE), nrow = 10, ncol = 5)


#---------------2.2---------------#
##Write a function Matrix_multip to do matrix multiplication, 
##i.e., M1 * M2. Here you are only allowed to use for loop, * operator, and + operator. 
##Check your result with the %*% operator.
Matrix_multip <- function(M1,M2){
  output <- matrix(nrow=5,ncol=5)
  for(i in 1:5){
    for(j in 1:5){
      tmp<-0 #for save line
      for(jj in 1:10){
        tmp = tmp + (M1[i,jj] * M2[jj,j]) 
      }
      output[i,j] <- tmp
    }
    
  }
  print(output)
}
  

 #for validate output of calling function
Matrix_multip(M1,M2)
M1 %*% M2