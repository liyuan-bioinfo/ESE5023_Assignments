
program Cal_SZA
use Declination_angle
use AST
implicit none
    real(4) ::delta,H,selta_z,belta_1,L    
    Integer::N,LST,AST_min
    real(4)::Long,PI
	
    N=202
	PI=3.1415926
    call cal_dec_angle(N,delta)
	print*,"----------------"
	print*,"|  equation2   |"
	print*,"----------------"
    write(*,*),"D1:",delta
	print*,""

    LST=480 !min
    Long=112 !degree
    call cal_AST(LST,N,Long,AST_min)

	print*,"----------------"
	print*,"| equation3-5  |"
	print*,"----------------"
    write(*,*),"AST_min:",AST_min," min"
    write(*,*),"AST:",floor(AST_min/60.),":",mod(AST_min,60)	!AST date
	print*,""

	print*,"----------------"
	print*,"| equation6-7 |"
	print*,"----------------"
	!equation6
	H= (AST_min - 720) / 4.	 !degree
	L=33.43
	belta_1 = ASIN(cos(L*PI/180)*cos(delta*PI/180)*cos(H*PI/180) +sin(L*PI/180)*sin(delta*PI/180)) *180 / PI
	selta_z = ACOS(cos(L*PI/180)*cos(delta*PI/180)*cos(H*PI/180) +sin(L*PI/180)*sin(delta*PI/180)) *180 / PI
	write(*,*),"L:",L
	write(*,*),"H:",H
	write(*,*),"belta_1:",belta_1
	write(*,*),"selta_z:",selta_z
	print*,""

end program Cal_SZA
