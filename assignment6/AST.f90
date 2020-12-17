module AST
!http://blog.sina.com.cn/s/blog_5edeeb390100gere.html
implicit none  
contains
        subroutine cal_AST(LST,N,Long,AST_min)
            implicit none
            real(4)::ET,D,PI
            Integer::LSTM
            Integer,intent(in)::N,LST
            real(4),intent(in)::Long
            Integer,intent(out)::AST_min
                PI=3.1415926
                !equation5
                D = 360 * (N-81.)/365 !degree
				D = ANINT(D*10.)/10. !1 digital

				print*,"----------------------------"
				print*,"|equation3-5 local variable|"
				print*,"----------------------------"								
                write(*,*) 'D2: ', D

                ET = 9.87 * sin(2*D*PI/180) - 7.53 * cos(D*PI/180) -1.5*sin(D*PI/180) !min
				ET = ANINT(ET*100.)/100. !2 digital
                write(*,*) 'ET: ', ET
                !equation4
                LSTM = 15 * ANINT(Long / 15) !degree,no digital
                write(*,*) 'LSTM: ', LSTM
				print*,""
                !equation3
                AST_min = ANINT(LST + 4 * (LSTM - Long) +ET) !minute,no digital

        end subroutine cal_AST

end module AST

