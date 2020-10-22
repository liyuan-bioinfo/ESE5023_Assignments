##time:   2020年9月21日23:17:02
##author: LiYuan
##task:   Write a function Print_values with arguments a, b, and c to 
##        reflect the following flowchart

#---------------1,function---------------#
Print_values <- function(a, b, c){
  if(a > b){ #step1-T
    if(b > c){ #step2 -T
      print(paste("a:",a," b:",b, " c:",c,sep=""))
    }else{ #step2 -F
      if(a > c){ #step3 -T -public code
        print(paste("a:",a," c:",c, " b:",b,sep=""))
      }else{ #step3 -F -public code
        print(paste("c:",c," a:",a, " b:",b,sep=""))
      }
    }
  }else{ #step1 -F
    if(b > c){ #step2 -T
      if(a > c){ #step3-T -public code
        print(paste("a:",a," c:",c, " b:",b,sep=""))
      }else{ #step3 -F -public code
        print(paste("c:",c," a:",a, " b:",b,sep=""))
      }      
    }else{#step2 -F
      print(paste("c:",c," b:",b, " c:",c,sep=""))
    }
  }
}

#---------------2,output---------------#
group <- runif(3,min=0,max=100) #get random a, b, and c and store these in "group"
Print_values(group[1],group[2],group[3]) #get output

