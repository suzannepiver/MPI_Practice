!Suzanne Piver
!DAXPY: compute a constant times a vector plus a vector ax+y
!N: number of elements in input vector
!DA:double precidion scalar multiplier
!DX:double precision vector with N elements
!DY:double precision vector with N elements

PROGRAM TEST
IMPLICIT NONE
INTEGER, parameter :: N = 4
INTEGER :: alpha = 2
REAL, DIMENSION(N,N) :: DX
REAL, DIMENSION(N,N) :: DY
REAL, DIMENSION(N,N) :: DInv
INTEGER :: i, j

!Populate Matrix
do i=1,N
     do j=1,N
         DX(i,j) = i*j
         DY(i, j)= (i-j)*(j+i)
      end do
end do
Call DAXPY(N,alpha,DX,DY)
write(*, *) "DX"
write(*, *) DX
write(*, *) "DY" 
Write(*, *) DY


END PROGRAM TEST
Subroutine DAXPY(N,alpha,DX,DY)
        IMPLICIT NONE
        INTEGER, intent(in) :: N, alpha
        REAL, intent(inout) ::  DX(N), DY(N)
        INTEGER :: i
           do i=1, N
                DX(i) = alpha*DX(i)
                DY(i) = DY(i)+DX(i)
                Print *, DY(i)
           end do
        !real(dp), dimension(4) :: work  ! work array for LAPACK
        !integer, dimension(4) :: ipiv   ! pivot indices
        !integer :: n, info

        !Call DGETRF(n,n,DY, n, ipiv, info)
        !Call DGETRI(n, DY, n, ipiv, work, n, info)
END Subroutine DAXPY




