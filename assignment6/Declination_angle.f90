module Declination_angle
!implicit none

contains
        subroutine cal_dec_angle(N,delta)
            implicit none
            real(4)::PI
            Integer,intent(in)::N
	    real(4),intent(out)::delta
            PI=3.1415926
                !equation2
                delta= 23.45 * SIN((N+284.)/365 * 360*PI/180)

        end subroutine cal_dec_angle

end module Declination_angle
