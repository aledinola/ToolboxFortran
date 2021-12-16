module mod_utilities
	! Description:
	! - Write arrays (of real and integer type) of up to
	!   7 dimensions to text file or binary file columnwise
	! - Print a matrix to a file as (row,col)
	!
	!   Date      Programmer       Description of change
	!   ====      ==========       =====================
	!  20210407   A. Di Nola       Original code
	!  20210701   A. Di Nola       Added read1dim,read2dim
    !  20210725   A. Di Nola       Added writescalar, readscalar
    !  20210829   A. Di Nola       Added print_vector, print_matrix
    !  20210901   A. Di Nola       Added write1dimBinary
	!  20211120   A. Di Nola       Added disp 
    !  20211123   A. Di Nola       Added read3dim
    !  20211216   A. Di Nola       Added writeArr1
    
	! USE other modules
	implicit none
	
	!private !Variables and internal proc are not visible outside of this module   
    
    ! Visible outside 
    !public :: myerror
		
	!Declare module variables

	!Declare generic interface
    
    interface print1dim
		module procedure print1dim_i
		module procedure print1dim_r
    end interface
    
	interface printMatrix
		module procedure printMatrix_i
		module procedure printMatrix_r
    end interface
	
    interface writescalar
		module procedure writescalar_i
		module procedure writescalar_r
    end interface
    
    interface write1dimBinary
		module procedure write1dimBinary_i
		module procedure write1dimBinary_r
    end interface
    
    interface writeArr1
        module procedure writeArr1_i
        module procedure writeArr1_r
    end interface
    
	interface write1dim
		module procedure write1dim_i
		module procedure write1dim_r
	end interface

	interface write2dim
		module procedure write2dim_i
		module procedure write2dim_r
	end interface

	interface write3dim
		module procedure write3dim_i
		module procedure write3dim_r
	end interface

	interface write4dim
		module procedure write4dim_i
		module procedure write4dim_r
	end interface

	interface write5dim
		module procedure write5dim_i
		module procedure write5dim_r  
	end interface

	interface write6dim
		module procedure write6dim_i
		module procedure write6dim_r  
	end interface

	interface write7dim
		module procedure write7dim_i
		module procedure write7dim_r  
    end interface
	
    ! - Read scalar
    interface readscalar
        module procedure readscalar_i
        module procedure readscalar_r
    end interface
    
	! - Read 1-DIM array
    interface read1dim
        module procedure read1dim_i
        module procedure read1dim_r
    end interface
    
    ! - Read 2-DIM array
    interface read2dim
        module procedure read2dim_i
        module procedure read2dim_r
    end interface
    
    ! - Read 3-DIM array
    interface read3dim
        module procedure read3dim_i
        module procedure read3dim_r
    end interface
	
	! - Display vector, matrix or 3-D array (with title)
	interface disp
        module procedure disp_scal, disp_vec, disp_mat, disp_3d, disp_vec_i, disp_mat_i, &
                         disp_scal_title, disp_vec_title, disp_mat_title, disp_3d_title, &
                         disp_vec_title_i, disp_mat_title_i, disp_mat_title_l, disp_mat_l
    end interface disp

	integer, parameter :: rt = 8
    
    contains
    
    !Module procedures
    
  !**********************************************************
  ! PRINT VECTOR BASH
  !**********************************************************  
  subroutine print_vector(v)
  !**********************************************************
  ! PRINT_VECTOR prints a vector in the terminal
  ! Usage: 
  !	call print_vector(v)

  ! INPUTS
  !	v     	: vector to print 
  !  
  !***********************************************************
    !Local
    integer :: i
    
    !Dummy
    real(8), intent(in) :: v(:)

    do i = 1,size(v)
       write(*,*) v(i)
    end do
  end subroutine print_vector

  !**********************************************************
  ! PRINT MATRIX BASH
  !**********************************************************  
  subroutine print_matrix(v)
  !**********************************************************
  ! PRINT_MATRIX prints matrix in the terminal  
  ! Usage: 
  !	call print_matrix(v)
    
  ! INPUTS
  !	v     : matrix to print 
  !
  !***********************************************************
      
    !Local
    integer :: i
    
    !Dummy
    real, intent(in) :: v(:,:)

    do i = 1,size(v,1)
       write(*,*) v(i,:)
    end do
  end subroutine print_matrix

  !**********************************************************
  ! PRINT MATRIX DATA FILE (.dat)
  !**********************************************************  
  subroutine print_matrix_dat(namefile,v)
  !**********************************************************
  ! PRINT_MATRIX_DAT print matrix in a dat file   
  ! Usage: 
  !	call print_matrix_dat(namefile,v)
    
  ! INPUTS
  ! namefile : name of the file    
  !	v        : matrix to print 
  !
  !***********************************************************
    !Local
    integer :: i
    
    !Dummy
    real, intent(in) :: v(:,:)
    character (len=*) :: namefile

    open(1, file=namefile, action='write', status='replace')

    do i = 1,size(v,1)
       write(1,*) v(i,:)
    end do
    close(1)
  end subroutine print_matrix_dat

  !**********************************************************
  ! PRINT VECTOR DATA FILE (.dat)
  !**********************************************************  
  subroutine print_vector_dat(namefile,v) 
  !**********************************************************
  ! PRINT_VECTOR_DAT prints vector in a dat file  
  ! Usage: 
  !	call print_matrix_dat(namefile,v,n)
    
  ! INPUTS
  !	namefile     : name of the dat file 
  !	v            : vector to print  
  !     n            : size of the vector
  !
  !***********************************************************
    !Local
    integer :: i
    
    !Dummy
    real, intent(in) :: v(:)
    character (len=*) :: namefile

    open(1, file=namefile, action='write', status = 'replace')

    do i = 1,size(v)
       write(1,*) v(i)
    end do
    close(1)
    
  end subroutine print_vector_dat
	
    !-----------------------------------------------------------------!
    !   PRINT VECTOR  
    !-----------------------------------------------------------------!
    subroutine print1dim_r(x)
    ! This subroutine prints a vector (1-dim array) x on the screen
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:)
    integer :: i
     
    !Display vector x on screen
    do i = 1,size(x)
        write(*,'(f12.6," ")'), x(i)
        !write(*,'(" ")', advance = 'no')
	enddo
	write(*,"()")  
    write(*,*) " "
    
    end subroutine print1dim_r
    !-----------------------------------------------------------------!
    
    subroutine print1dim_i(x)
    ! This subroutine prints a vector (1-dim array) x on the screen
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:)
    integer :: i
     
    !Display vector x on screen
    do i = 1,size(x)
        write(*,'(I6," ")'), x(i)
        !write(*,'(" ")', advance = 'no')
	enddo
	write(*,"()")  
    write(*,*) " "
    
    end subroutine print1dim_i
    
    !-----------------------------------------------------------------!
    !   PRINT MATRIX 
    !-----------------------------------------------------------------!
    subroutine printMatrix_r(x,file_name)
    ! This subroutine prints matrix (2-dim array) x on file file_name.
    ! If filename is not present, matrix is displayed on the screen.
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:,:)
    character(len=*), intent(in), optional :: file_name
    integer :: unitno, ierr, i, j
    
    if (present(file_name)) then
        !Write matrix x(row,col) into a txt file
        open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
        if (ierr/=0) then
            write(*,*) "Error: printMatrix: cannot open file"
		    pause
		    stop 
        endif
    
        do i = 1,size(x,1)
		    do j = 1,size(x,2)    
			    write(unitno,'(f12.6," ")',advance='no'), x(i,j)
                write(unitno,'(" ")', advance = 'no')
		    enddo
		    write(unitno,"()")  
        enddo
        write(unitno,*) " "
    
        close(unitno)
    
    else
        
        !Display matrix x(row,col) on screen
        do i = 1,size(x,1)
		do j = 1,size(x,2)
			write(*,'(f12.6," ")',advance='no'), x(i,j)
                	write(*,'(" ")', advance = 'no')
		enddo
		write(*,"()")  
        enddo
        write(*,*) " "
        
    endif
    
    end subroutine printMatrix_r
    !-----------------------------------------------------------------!
    subroutine printMatrix_i(x,file_name)
    ! This subroutine prints matrix (2-dim array) x on file file_name.
    ! If filename is not present, matrix is displayed on the screen.
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:,:)
    character(len=*), intent(in), optional :: file_name
    integer :: unitno, ierr, i, j
    
    if (present(file_name)) then
        !Write integer matrix x(row,col) into a txt file
        open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
        if (ierr/=0) then
            write(*,*) "Error: printMatrix: cannot open file"
		    pause
		    stop 
        endif
    
        do i = 1,size(x,1)
		    do j = 1,size(x,2)
			    write(unitno,'(I6)',advance='no'), x(i,j)
		    enddo
		    write(unitno,*) " "
        enddo
        write(unitno,*) " "
    
        close(unitno)
    
    else
        
        !Write integer matrix x(row,col) into a txt file
        do i = 1,size(x,1)
		    do j = 1,size(x,2)
			    write(*,'(I6)',advance='no'), x(i,j)
		    enddo
		    write(*,*) " "
        enddo
        write(*,*) " "
        
    endif
    
    end subroutine printMatrix_i
    
    !-----------------------------------------------------------------!
    !   WRITE SCALARS
    !-----------------------------------------------------------------!
    subroutine writescalar_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x
    character(len=*), intent(in) :: file_name
    integer :: unitno,i, ierr
    
    !Write scalar x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: writescalar: cannot open file"
		pause
		stop 
    endif
    
    write(unitno,*) x

    close(unitno)
    
    end subroutine writescalar_r
    !-----------------------------------------------------------------!
    subroutine writescalar_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x
    character(len=*), intent(in) :: file_name
    integer :: unitno,i, ierr
    
    !Write scalar x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: writescalar: cannot open file"
		pause
		stop 
    endif
    
    write(unitno,*) x

    close(unitno)
    
    end subroutine writescalar_i
    !-----------------------------------------------------------------!
    
    subroutine write1dimBinary_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i, ierr
    
    !Write 1 dim array x into a BINARY file
    !Note the commands FORM="unformatted", ACCESS="stream"
    !and the fact that write(unitno) instead of write(unitno,*)
    OPEN(NEWUNIT=unitno, FILE=file_name, FORM="unformatted", ACCESS="stream", STATUS="unknown", iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write1dimBinary: cannot open file"
		pause
		stop 
    endif
    
    !do i = 1,size(x,dim=1)
    !    write(unitno) x(i)
    !enddo
     
    write(unitno) x
    
    close(unitno)
    
    end subroutine write1dimBinary_r
    !-----------------------------------------------------------------!
    
    subroutine write1dimBinary_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:)
    character(len=*), intent(in) :: file_name
    integer :: unitno, ierr
    
    !Write 1 dim array x into a BINARY file
    !Note the commands FORM="unformatted", ACCESS="stream"
    !and the fact that write(unitno) instead of write(unitno,*)
    OPEN(NEWUNIT=unitno, FILE=file_name, FORM="unformatted", ACCESS="stream", STATUS="unknown", iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write1dimBinary: cannot open file"
		pause
		stop 
    endif
    
    !do i = 1,size(x,dim=1)
    !    write(unitno) x(i)
    !enddo
        
    write(unitno) x
    
    close(unitno)
    
    end subroutine write1dimBinary_i
    !-----------------------------------------------------------------!
    
    !-----------------------------------------------------------------!
    !   WRITE 1-D ARRAY as single line (faster than write1dim) 
    !-----------------------------------------------------------------!
    subroutine writeArr1_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:)
    character(len=*), intent(in) :: file_name
    character(len=50) :: file_format
    integer :: unitno, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write1dim: cannot open file"
		pause
		stop 
    endif
    
   
    !write(unitno,"(330000(F20.16))") x
    write(file_format,*) "(",size(x),"(F,:,','))"
    
    write(unitno,trim(file_format)) x
    
    close(unitno)
    
    end subroutine writeArr1_r
    !-----------------------------------------------------------------!
    
    subroutine writeArr1_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:)
    character(len=*), intent(in) :: file_name
    character(len=50) :: file_format
    integer :: unitno,i, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write1dim: cannot open file"
		pause
		stop 
    endif
    
    !Assume size(x) is equal to 330000
    !write(unitno,"(330000(F20.16))") x
    write(file_format,*) "(",size(x),"(I,:,','))"
    
    write(unitno,trim(file_format)) x
    
    close(unitno)
    
    end subroutine writeArr1_i
    !-----------------------------------------------------------------!
    
    !-----------------------------------------------------------------!
    !   WRITE 1 DIM ARRAYS as text files
    !-----------------------------------------------------------------!
    subroutine write1dim_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:)
    character(len=*), intent(in) :: file_name
    character(len=50) :: file_format
    integer :: unitno, i, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write1dim: cannot open file"
		pause
		stop 
    endif
    
    do i = 1,size(x)
        write(unitno,*) x(i)
    enddo
    
    close(unitno)
    
    end subroutine write1dim_r
    !-----------------------------------------------------------------!
    subroutine write1dim_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:)
    character(len=*), intent(in) :: file_name
    !character(len=*) :: file_format
    integer :: unitno,i, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write1dim: cannot open file"
		pause
		stop 
    endif
    
    !write(file_format,*) size(x)  
    
    do i = 1,size(x)
        write(unitno,*) x(i)
    enddo
    
    close(unitno)
    
    end subroutine write1dim_i
    !-----------------------------------------------------------------!
    !   WRITE 2 DIM ARRAYS
    !-----------------------------------------------------------------!
    subroutine write2dim_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write2dim: cannot open file"
		pause
		stop 
    endif
    
    do i2 = 1,size(x,dim=2)
        do i1 = 1,size(x,dim=1)
            write(unitno,*) x(i1,i2)
        enddo
    enddo
    
    close(unitno)
    
    end subroutine write2dim_r
    !-----------------------------------------------------------------!
    subroutine write2dim_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2,ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write2dim: cannot open file"
		pause
		stop 
    endif
    
    do i2 = 1,size(x,dim=2)
        do i1 = 1,size(x,dim=1)
            write(unitno,*) x(i1,i2)
        enddo
    enddo
    
    close(unitno)
    
    end subroutine write2dim_i
    !-----------------------------------------------------------------!
    !   WRITE 3 DIM ARRAYS
    !-----------------------------------------------------------------!
    subroutine write3dim_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write3dim: cannot open file"
		pause
		stop 
    endif
    
    do i3 = 1,size(x,dim=3)
        do i2 = 1,size(x,dim=2)
            do i1 = 1,size(x,dim=1)
                write(unitno,*) x(i1,i2,i3)
            enddo
        enddo
    enddo
    
    close(unitno)
    
    end subroutine write3dim_r
    !-----------------------------------------------------------------!
    subroutine write3dim_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write3dim: cannot open file"
		pause
		stop 
    endif
    
    do i3 = 1,size(x,dim=3)
        do i2 = 1,size(x,dim=2)
            do i1 = 1,size(x,dim=1)
                write(unitno,*) x(i1,i2,i3)
            enddo
        enddo
    enddo
    
    close(unitno)
    
    end subroutine write3dim_i
    !-----------------------------------------------------------------!
    !   WRITE 4 DIM ARRAYS
    !-----------------------------------------------------------------!
    subroutine write4dim_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write4dim: cannot open file"
		pause
		stop 
    endif
    
    do i4 = 1,size(x,dim=4)
        do i3 = 1,size(x,dim=3)
            do i2 = 1,size(x,dim=2)
                do i1 = 1,size(x,dim=1)
                    write(unitno,*) x(i1,i2,i3,i4)
                enddo
            enddo
        enddo
    enddo
    
    close(unitno)
    
    end subroutine write4dim_r
    !-----------------------------------------------------------------!
    subroutine write4dim_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write4dim: cannot open file"
		pause
		stop 
    endif
    
    do i4 = 1,size(x,dim=4)
        do i3 = 1,size(x,dim=3)
            do i2 = 1,size(x,dim=2)
                do i1 = 1,size(x,dim=1)
                    write(unitno,*) x(i1,i2,i3,i4)
                enddo
            enddo
        enddo
    enddo
    
    close(unitno)
    
    end subroutine write4dim_i
    !-----------------------------------------------------------------!
    
    !-----------------------------------------------------------------!
    !   WRITE 5 DIM ARRAYS
    !-----------------------------------------------------------------!
    subroutine write5dim_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:,:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4,i5, ierr
    
    !Write 5-dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write6dim: cannot open file"
		pause
		stop 
    endif
    
    do i5 = 1,size(x,dim=5)
    do i4 = 1,size(x,dim=4)
    do i3 = 1,size(x,dim=3)
    do i2 = 1,size(x,dim=2)
    do i1 = 1,size(x,dim=1)
        write(unitno,*) x(i1,i2,i3,i4,i5)
    enddo
    enddo
    enddo
    enddo
    enddo
    
    close(unitno)
    
    end subroutine write5dim_r
    !-----------------------------------------------------------------!
    subroutine write5dim_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:,:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4,i5, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write6dim: cannot open file"
		pause
		stop 
    endif
    
    do i5 = 1,size(x,dim=5)
    do i4 = 1,size(x,dim=4)
    do i3 = 1,size(x,dim=3)
    do i2 = 1,size(x,dim=2)
    do i1 = 1,size(x,dim=1)
        write(unitno,*) x(i1,i2,i3,i4,i5)
    enddo
    enddo
    enddo
    enddo
    enddo
    
    close(unitno)
    
    end subroutine write5dim_i
    !-----------------------------------------------------------------!
    
    !-----------------------------------------------------------------!
    !   WRITE 6 DIM ARRAYS
    !-----------------------------------------------------------------!
    subroutine write6dim_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:,:,:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4,i5,i6, ierr
    
    !Write 6-dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write6dim: cannot open file"
		pause
		stop 
    endif
    
    do i6 = 1,size(x,dim=6)
    do i5 = 1,size(x,dim=5)
    do i4 = 1,size(x,dim=4)
    do i3 = 1,size(x,dim=3)
    do i2 = 1,size(x,dim=2)
    do i1 = 1,size(x,dim=1)
        write(unitno,*) x(i1,i2,i3,i4,i5,i6)
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo
    
    close(unitno)
    
    end subroutine write6dim_r
    !-----------------------------------------------------------------!
    subroutine write6dim_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:,:,:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4,i5,i6, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write6dim: cannot open file"
		pause
		stop 
    endif
    
    do i6 = 1,size(x,dim=6)
    do i5 = 1,size(x,dim=5)
    do i4 = 1,size(x,dim=4)
    do i3 = 1,size(x,dim=3)
    do i2 = 1,size(x,dim=2)
    do i1 = 1,size(x,dim=1)
        write(unitno,*) x(i1,i2,i3,i4,i5,i6)
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo
    
    close(unitno)
    
    end subroutine write6dim_i
    !-----------------------------------------------------------------!
    
    !-----------------------------------------------------------------!
    !   WRITE 7 DIM ARRAYS
    !-----------------------------------------------------------------!
    subroutine write7dim_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:,:,:,:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4,i5,i6,i7, ierr
    
    !Write 7 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write7dim: cannot open file"
		pause
		stop 
    endif
    
    do i7 = 1,size(x,dim=7)
    do i6 = 1,size(x,dim=6)
    do i5 = 1,size(x,dim=5)
    do i4 = 1,size(x,dim=4)
    do i3 = 1,size(x,dim=3)
    do i2 = 1,size(x,dim=2)
    do i1 = 1,size(x,dim=1)
        write(unitno,*) x(i1,i2,i3,i4,i5,i6,i7)
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo
    
    close(unitno)
    
    end subroutine write7dim_r
    !-----------------------------------------------------------------!
    subroutine write7dim_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:,:,:,:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4,i5,i6,i7, ierr
    
    !Write 7 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write7dim: cannot open file"
		pause
		stop 
    endif
    
    do i7 = 1,size(x,dim=7)
    do i6 = 1,size(x,dim=6)
    do i5 = 1,size(x,dim=5)
    do i4 = 1,size(x,dim=4)
    do i3 = 1,size(x,dim=3)
    do i2 = 1,size(x,dim=2)
    do i1 = 1,size(x,dim=1)
        write(unitno,*) x(i1,i2,i3,i4,i5,i6,i7)
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo
    
    close(unitno)
    
    end subroutine write7dim_i
    !-----------------------------------------------------------------!
    
    !=======================================================================!
    subroutine readscalar_i(x,file_name)
    
    implicit none
    
    character(len=*), intent(in) :: file_name
    integer, intent(out) :: x
    integer :: unitno,i, ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in readscalar: cannot open file"
		pause
		stop 
    endif
    
    read(unitno,*) x
    
    close(unitno)
    
    end subroutine readscalar_i
    !=======================================================================!
    
    subroutine readscalar_r(x,file_name)
    
    implicit none
    
    character(len=*), intent(in) :: file_name
    real(8), intent(out) :: x
    integer :: unitno,i, ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in readscalar: cannot open file"
		pause
		stop 
    endif
    
    read(unitno,*) x
    
    close(unitno)
    
    end subroutine readscalar_r
    !=======================================================================!
    
    subroutine read1dim_i(x,file_name)
    
    implicit none
    
    character(len=*), intent(in) :: file_name
    integer, intent(out) :: x(:)
    integer :: unitno,i, ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in read1dim: cannot open file"
		pause
		stop 
    endif
    
    do i = 1,size(x)
        read(unitno,*) x(i)
    enddo
    close(unitno)
    
    end subroutine read1dim_i
    !=======================================================================!
    
    subroutine read1dim_r(x,file_name)
    
    implicit none
    
    character(len=*), intent(in) :: file_name
    real(8), intent(out) :: x(:)
    integer :: unitno,i, ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in read1dim: cannot open file"
		pause
		stop 
    endif
    
    do i = 1,size(x)
        read(unitno,*) x(i)
    enddo
    close(unitno)
    
    end subroutine read1dim_r
    !=======================================================================!
    
    subroutine read2dim_i(x,file_name)
    
    implicit none
    
    character(len=*), intent(in) :: file_name
    integer, intent(out) :: x(:,:)
    integer :: unitno,i1,i2, ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in read2dim: cannot open file"
		pause
		stop 
    endif
    
    do i2 = 1,size(x,dim=2)
        do i1 = 1,size(x,dim=1)
            read(unitno,*) x(i1,i2)
        enddo
    enddo
    
    close(unitno)
    
    end subroutine read2dim_i
    !=======================================================================!
    
    subroutine read2dim_r(x,file_name)
    
    implicit none
    
    external getcwd
    
    character(len=*), intent(in) :: file_name
    real(8), intent(out) :: x(:,:)
    integer :: unitno,i1,i2, ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in read2dim: cannot open file"
		pause
		stop 
    endif
    
    do i2 = 1,size(x,dim=2)
        do i1 = 1,size(x,dim=1)
            read(unitno,*) x(i1,i2)
        enddo
    enddo
    
    close(unitno)
    
    end subroutine read2dim_r
    !=======================================================================!
    
    subroutine read3dim_i(x,file_name)
    
    implicit none
    
    character(len=*), intent(in) :: file_name
    integer, intent(out) :: x(:,:,:)
    integer :: unitno,i1,i2,i3, ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in read3dim: cannot open file"
		pause
		stop 
    endif
    
    do i3 = 1,size(x,dim=3)
        do i2 = 1,size(x,dim=2)
            do i1 = 1,size(x,dim=1)
                read(unitno,*) x(i1,i2,i3)
            enddo
        enddo
    enddo
    
    close(unitno)
    
    end subroutine read3dim_i
    !=======================================================================!
    
    subroutine read3dim_r(x,file_name)
    
    implicit none
    
    external getcwd
    
    character(len=*), intent(in) :: file_name
    real(8), intent(out) :: x(:,:,:)
    integer :: unitno,i1,i2,i3, ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in read3dim: cannot open file"
		pause
		stop 
    endif
    
    do i3 = 1,size(x,dim=3)
        do i2 = 1,size(x,dim=2)
            do i1 = 1,size(x,dim=1)
                read(unitno,*) x(i1,i2,i3)
            enddo
        enddo
    enddo
    
    close(unitno)
    
    end subroutine read3dim_r
    !=======================================================================!

    subroutine create_directory( newDirPath )
    ! Author:  Jess Vriesema
    ! Date:    Spring 2011
    ! Purpose: Creates a directory at ./newDirPath

    implicit none

    character(len=*), intent(in) :: newDirPath
    character(len=256)           :: mkdirCmd
    logical                      :: dirExists

    ! Check if the directory exists first
!   inquire( file=trim(newDirPath)//'/.', exist=dirExists )  ! Works with gfortran, but not ifort
    inquire( directory=newDirPath, exist=dirExists )         ! Works with ifort, but not gfortran


    if (dirExists) then
!      write (*,*) "Directory already exists: '"//trim(newDirPath)//"'"
    else
        mkdirCmd = 'mkdir -p '//trim(newDirPath)
        write(*,'(a)') "Creating new directory: '"//trim(mkdirCmd)//"'"
        call system( mkdirCmd )
    endif
    end subroutine create_directory
	
	!=======================================================================!
	! ROUTINES TO DISPLAY SCALARS, VECTORS, MATRICES, 3D ARRAYS (WITH TITLE)
	!=======================================================================!
	subroutine disp_mat_title_l(title,X)
		implicit none
		character(len=*), intent(in) :: title
		logical, dimension(1:,1:), intent(in) :: X    
		write(*,'(A)') title
		call disp_mat_l(X)
	end subroutine 
	
	subroutine disp_mat_l(X)
		implicit none
		logical, dimension(1:,1:), intent(in) :: X    
		integer :: i
		character(len=5) :: dim2
		write(dim2,'(i5)') size(X,2)
		do i = 1,size(X,1)
			write(*,'('//dim2//'L2)') X(i,:)
		end do
	end subroutine disp_mat_l

	subroutine disp_mat_title_i(title,X)
		implicit none
		character(len=*), intent(in) :: title
		integer, dimension(1:,1:), intent(in) :: X    
		write(*,'(A)') title
		call disp_mat_i(X)
		end subroutine 
		subroutine disp_mat_i(X)
		implicit none
		integer, dimension(1:,1:), intent(in) :: X    
		integer :: i
		character(len=5) :: dim2
		write(dim2,'(i5)') size(X,2)
		do i = 1,size(X,1)
			write(*,'('//dim2//'i8)') X(i,:)
		end do
	end subroutine disp_mat_i

	subroutine disp_vec_title_i(title,X)
		implicit none
		character(len=*), intent(in) :: title
		integer, dimension(1:), intent(in) :: X    
		write(*,'(A)') title
		call disp_vec_i(X)
		end subroutine
		subroutine disp_vec_i(X)
		implicit none
		integer, dimension(1:), intent(in) :: X    
		integer :: i
		do i = 1,size(X,1)
			write(*,'(i8)') X(i)
		end do
	end subroutine disp_vec_i

	subroutine disp_vec(X,uid)
		implicit none
		real(rt), dimension(:), intent(in) :: X    
		integer, intent(in), optional :: uid
		if (     present(uid)) call disp_core(X,size(X,1),1,1,'',uid)
		if (.not.present(uid)) call disp_core(X,size(X,1),1,1,'',6)
	end subroutine 
	subroutine disp_mat(X,uid)
		implicit none
		real(rt), dimension(:,:), intent(in) :: X    
		integer, intent(in), optional :: uid
		if (     present(uid)) call disp_core(X,size(X,1),size(X,2),1,'',uid)
		if (.not.present(uid)) call disp_core(X,size(X,1),size(X,2),1,'',6)
	end subroutine 
	subroutine disp_3d(X,uid)
		implicit none
		real(rt), dimension(:,:,:), intent(in) :: X    
		integer, intent(in), optional :: uid
		if (     present(uid)) call disp_core(X,size(X,1),size(X,2),size(X,3),'',uid)
		if (.not.present(uid)) call disp_core(X,size(X,1),size(X,2),size(X,3),'',6)
	end subroutine 
	subroutine disp_vec_title(title,X,uid)
		implicit none
		real(rt), dimension(:), intent(in) :: X    
		character(len=*), intent(in) :: title
		integer, intent(in), optional :: uid
		if (     present(uid)) call disp_core(X,size(X,1),1,1,title,uid)
		if (.not.present(uid)) call disp_core(X,size(X,1),1,1,title,6)
	end subroutine 
	subroutine disp_mat_title(title,X,uid)
		implicit none
		real(rt), dimension(:,:), intent(in) :: X    
		character(len=*), intent(in) :: title
		integer, intent(in), optional :: uid
		if (     present(uid)) call disp_core(X,size(X,1),size(X,2),1,title,uid)
		if (.not.present(uid)) call disp_core(X,size(X,1),size(X,2),1,title,6)
	end subroutine 
	subroutine disp_3d_title(title,X,uid)
		implicit none
		real(rt), dimension(:,:,:), intent(in) :: X    
		character(len=*), intent(in) :: title
		integer, intent(in), optional :: uid
		if (     present(uid)) call disp_core(X,size(X,1),size(X,2),size(X,3),title,uid)
		if (.not.present(uid)) call disp_core(X,size(X,1),size(X,2),size(X,3),title,6)
	end subroutine 

	subroutine disp_core(X,n1,n2,n3,title,uid)
		implicit none
		integer, intent(in) :: n1,n2,n3,uid
		character(len=*), intent(in) :: title
		real(rt), intent(in) :: X(n1,n2,*)
		! local
		integer :: i1,i2,i3

		! If no title or uid is present, then use title='' and uid=6
		do i3 = 1,n3
			if (n3>1) then
				write(uid,'(A,i4,A)') title//'(:,:,',i3,')'
			else
				if (title/='') write(uid,'(A)') title
			end if
			do i1 = 1,n1
				do i2 = 1,n2-1
					write(uid,'(f12.5)',advance='no') X(i1,i2,i3)
				end do
				write(uid,'(f12.5)') X(i1,n2,i3)
			end do
		end do

	end subroutine
	subroutine disp_scal_title(title,X)
		implicit none
		character(len=*), intent(in) :: title
		real(rt), intent(in) :: X    
		write(*,'(A)') title
		call disp_scal(X)
	end subroutine 
	subroutine disp_scal(X)
		implicit none
		real(rt), intent(in) :: X    
		write(*,'(f12.5)') X
	end subroutine disp_scal
	!=======================================================================!
	
	! Acts just like the colon in Matlab
	pure function colon(a,b)
		implicit none
		integer, intent(in) :: a,b
		integer, dimension(1:b-a+1) :: colon
		integer :: i
		do i = a,b
			colon(i-a+1) = i
		end do
	end function colon

    
end module mod_utilities
