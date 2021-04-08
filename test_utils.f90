program test_utils


use mod_utilities
implicit none

integer, parameter :: n=2, m=3
real(8) :: mat(n,m)
integer :: mat2(n,m)


mat(1,:) = [1.0, 2.0, 3.0]
mat(2,:) = [4.0, 5.0, 6.0]

mat2(1,:) = [1, 2, 3]
mat2(2,:) = [4, 5, 6]

call printMatrix(mat,"matrix.txt")
call printMatrix(mat2,"matrix2.txt")



end program test_utils