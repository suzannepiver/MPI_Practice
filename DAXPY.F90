!Suzanne Piver
!DAXPY: compute a constant times a vector plus a vector ax+y
!N: number of elements in input vector
!DA:double precidion scalar multiplier
!DX:double precision vector with N elements
!DY:double precision vector with N elements

PROGRAM TEST
IMPLICIT NONE
INTEGER, parameter :: N = 4
INTEGER :: DA = 2
REAL :: DX(4) = (/ 1, 1, 1, 1 /)
REAL :: DY(4) = (/ 1, 1, 1, 1 /)
Call DAXPY(N,DA,DX,DY)

END PROGRAM TEST
Subroutine DAXPY(N,DA,DX,DY)
        IMPLICIT NONE
        INTEGER, intent(in) :: N, DA
        REAL, intent(inout) ::  DX(N), DY(N)
        !REAL, dimension(4) :: X
        INTEGER :: i
           do i=1, N
                DX(i) = DA*DX(i)
                DY(i) = DY(i)+DX(i)
                Print *, DY(i)
           end do
END Subroutine DAXPY


