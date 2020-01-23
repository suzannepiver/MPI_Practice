!SuzannePiver test
!DAXPY: compute a constant times a vector plus a vector ax+y
!N: number of elements in input vector
!DA:double precidion scalar multiplier
!DX:double precision vector with N elements	
!DY:double precision vector with N elements
PROGRAM TEST
IMPLICIT NONE
INTERFACE 
        FUNCTION DAXPY(N, DA,DX,DY)
                REAL, INTENT(IN) :: N, DA, DX, DY
                !INTEGER, INTENT(OUT) :: X
        END FUNCTION DAXPY
END INTERFACE
INTEGER :: N = 4
INTEGER :: DA = 2
REAL, dimension(4) :: DX = (/ 1, 1, 1, 1 /)
REAL :: DY(4) = (/ 1, 1, 1, 1 /)
Call  DAXPY(N,DA,DX,DY)

END PROGRAM TEST
FUNCTION DAXPY(N,DA,DX,DY) RESULT(X)
        IMPLICIT NONE
        INTEGER :: N, DA
        REAL ::  DX(N), DY(N)
        REAL :: X(N)
        INTEGER :: i
           do i=1, N
                DX(i) = DA*DX(i)
                X(i) = DY(i)+DX(i)
                Print X(i)
           end do
END FUNCTION DAXPY
