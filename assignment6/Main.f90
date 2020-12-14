program Main
	implicit none

	integer::i,j,ij
	real(4)::line !temp save summary of one line

	real,dimension(4)::a,b,c
	real,dimension(3,4)::M
	real,dimension(3,3)::N,N_t
	real,dimension(4,3)::M_t,O
	
	!read file1,row-4 x col-3
	open(3,file="/work/ese-ouycc/fortran_2/M.dat")!文件的名字是M.dat
	
	do i=1,4
		read(3,*) a(i),b(i),c(i)
		M(1,i)=a(i)
		M(2,i)=b(i)
		M(3,i)=c(i)
	end do
	close(3)

!see the data	
!	do i=1,4
!		print *,M(:,i)
!	end do

	!read file2,row-3 x col-3
	open(4,file="/work/ese-ouycc/fortran_2/N.dat")
	do i=1,3
		read(4,*) a(i),b(i),c(i)
		N(1,i)=a(i)
		N(2,i)=b(i)
		N(3,i)=c(i)
	end do
	close(4)	

	!tran to row x col
	M_t=transpose(M)
	N_t=transpose(N)
	
	!call subrountine
	call Matrix_multip(M_t,N_t,O)

	! Write the values to a new file
	open(5, file='MN.dat', status='replace')		
	do i=1,4
		print *,O(i,:)
		write(5, '(f8.1,f8.1,f8.1)') O(i,:)
	end do
	close(5)

end program Main

		



