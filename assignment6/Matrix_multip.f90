	!---------------------------
	!			subrountine
	!----------------------------
	subroutine Matrix_multip(M,N,O)
		implicit none
		integer::i,j,ij
		real(4)::line !temp save summary of one line
		real,dimension(4,3),intent(in)::M
		real,dimension(3,3),intent(in)::N
		real,dimension(4,3),intent(out)::O	
	!M * N
		do i=1,4	
			do j=1,3
			line=0
				do ij=1,3
					line=line+(M(i,ij) * N(ij,j))
				end do
			O(i,j)=line
			end do
		end do
	end subroutine 	Matrix_multip

