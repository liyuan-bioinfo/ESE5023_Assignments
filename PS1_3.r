#author: LiYuan
#time: 20200930
#task: 
  ##Write a function Pascal_triangle with an argument k to print the kth line of the Pascal triangle. 
  ##Report Pascal_triangle(100) and Pascal_triangle(200)

rm(list= ls())
Pascal_triangle <- function(k){
  k_line <- c()
  if(k == 1){
    k_line <- k_line <- matrix(c(1),ncol = k)
  }
  else if(k == 2) {
    k_line <- matrix(c(1,1),ncol = k)
  }
  else{
    k_line <- matrix(ncol = k)
    k_line[1]=1;#start
    k_line[k]=1;#end
    for(i in 2:(k-1)){
      before = Pascal_triangle(k-1);#get k-1 line!
      k_line[i]= before[i-1]+before[i];
    }
    
  }
  return(k_line)
}
#for validate in small samples
for(i in 1:10){
  out <- Pascal_triangle(i)
  print(out)
}
#for home work
print(Pascal_triangle(100)) #
print(Pascal_triangle(200)) #
