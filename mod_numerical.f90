module mod_numerical
    !=========================================================================!
    ! Written by Alessandro Di Nola 
    ! Main sources:
    ! https://github.com/drarnau/Replication_Krusell-et-al-2017/blob/master/Utils.f90
    ! https://sites.google.com/site/greygordon/code
    ! Numerical recipes in F90: http://numerical.recipes/
    ! http://karibzhanov.com/
    ! Giulio Fella: https://github.com/gfell
    !   Date      Programmer       Description of change
	!   ====      ==========       =====================
	!  20210817   A. Di Nola       Original code
	!  20210818   A. Di Nola       Changed lrzcurve
	!  20210830   A. Di Nola       Added function gini
    !=========================================================================!
    
    ! USE other modules 
    implicit none
    
    private !Variables and internal proc are not visible outside of this module   
    
    ! User-defined error function
    public :: myerror
    
    ! Replicate basic Matlab functions
    public :: linspace
    public :: ones, eye, cumsum
    public :: outerprod
    
    ! Sorting subroutines
    public :: QsortC
    
    ! General-purpose routines
    public :: is_monotone, my_closest, grid, mycorr
    
    ! Markov chain:
    public :: my_ss
    
    ! Interpolation:
    public :: linint, locate
    
    ! Discretization:
    public :: discretize_pareto, bddparetocdf, tauchen
    
    ! Percentiles, Gini and Lorenz curve:
    public :: quantili, calculate_quintiles, lrzcurve_basic, lrzcurve, unique, gini
    
    ! Optimization:
    public :: golden_method !similar to fminbnd in Matlab
    
    ! Root-finding:
    public :: rtbis, zbrent !similar to fzero in Matlab
    
   
contains
    
    subroutine myerror(string)

    implicit none
    character(len=*), intent(in) :: string
    write(*,*) "ERROR: ", string
    write(*,*) "Program will terminate.."
    pause
    stop

    end subroutine myerror
    !===============================================================================!
    
    function linspace(my_start, my_stop, n)
    ! Purpose: replicate Matlab function <linspace>
    ! Originally written by Arnau Valladares-Esteban
    implicit none
    ! Inputs:
    integer :: n
    real(8) :: my_start, my_stop
    ! Function result:
    real(8) :: linspace(n)
    ! Locals:
    integer :: i
    real(8) :: step, grid(n)

    if (n == 1) then
        grid(n) = (my_start+my_stop)/2.d0
    elseif (n.ge.2) then
        grid(1) = my_start
        if (n.gt.2) then
            step = (my_stop-my_start)/(real(n-1))
            do i = 2, n-1
                grid(i) = grid(i-1) + step
            end do
        end if
        grid(n) = my_stop
    endif
    linspace = grid
    
    end function linspace
    !===============================================================================!
    
    function ones(n) result(vec)
    ! Purpose: create a column vector of ones
    ! Input/output
    integer, intent(in) :: n
    real(8) :: vec(n)
    !Local variables
    integer :: i
    
    ! Body of ones
    do i = 1,n
        vec(i) = 1.0d0
    end do
    
    end function ones
    !===============================================================================!
    
    function eye(n) result(mat)
    ! Purpose: replicate function <eye> in Matlab
    ! Input/output
    integer, intent(in) :: n
    real(8) :: mat(n,n)
    !Local variables
    integer :: i
    
    ! Body of eye
    mat = 0.0d0
    do i = 1,n
        mat(i,i) = 1.0d0
    end do
    
    end function eye
    !===============================================================================!
    
    function outerprod(a,b)
    ! Computes the outer product of two vectors
    real(8), intent(in) :: a(:), b(:)
    real(8) :: outerprod(size(a),size(b))
    
    outerprod = spread(a,dim=2,ncopies=size(b)) * spread(b,dim=1,ncopies=size(a))

    end function outerprod
    !===============================================================================!
    
    FUNCTION cumsum(arr) RESULT(ans)
    ! Purpose: it replicates matlab <cumsum> function
    real(8), INTENT(IN) :: arr(:)
    ! Note that a Fortran function can return a value with an ALLOCATABLE
    ! attribute. The function result must be declared as ALLOCATABLE
    ! in the calling unit but will not be allocated on entry to the function.
    real(8), allocatable :: ans(:)
    INTEGER :: n,j

    n=size(arr)

    allocate(ans(n))
    ans(1)=arr(1)

    do j=2,n
	    ans(j)=ans(j-1)+arr(j)
    end do
	
    END FUNCTION cumsum
    !===============================================================================!

    function is_monotone(arr) result(ans)
    ! Purpose: Check is a real array is monotonically increasing
    implicit none
    !Declare inputs:
    real(8), intent(in) :: arr(:)
    !Declare function result:
    logical :: ans
    !Declare locals:
    integer :: i,n

    n = size(arr)
    if (n==1) then
        ans = .true.
        return
    endif

    do i=1,n-1
        if ( arr(i)<=arr(i+1) ) then
            ans = .true.
        else
            ans = .false.
            return
        endif
    enddo

    end function is_monotone
    !===============================================================================!
    
    function my_closest(myvector,gp,myvalue)
    ! Purpose: FINDS CLOSEST POSITION OF A REAL IN A VECTOR OF REALS
    ! Originally written by Arnau Valladares-Esteban
    implicit none
    integer, intent(in) :: gp
    real(8), intent(in) :: myvalue
    real(8), dimension(gp), intent(in) :: myvector
    real(8), dimension(gp) :: aux
    ! Function result:
    integer :: my_closest

    aux = abs(myvector-myvalue)

    my_closest = minloc(aux, dim=1)
    end function my_closest
    !===============================================================================!
    
    subroutine unique(x, x_u,ind_u)
	! Purpose: Find unique values in array x 
	! Return a smaller array with only unique values and corresponding (last) indeces 
	! Identical to matlab function 'unique' with 'last' option 
    ! WARNING: The input array x must be sorted in ascending order

	implicit none

	real(8), intent(in) :: x(:)
	real(8), intent(out), allocatable :: x_u(:)
	integer, intent(out), allocatable :: ind_u(:) 
	! Local variables
	integer :: i, n, i_u

	! Execution 

	n = size(x)

	allocate(x_u(n),ind_u(n))

	x_u(1) = x(1)
	i_u = 1

	do i=2,n
		if (x(i)>x(i-1)) then
			ind_u(i_u) = i-1
			x_u(i_u+1) = x(i) 
			i_u = i_u+1
		endif 
	enddo
	
	ind_u(i_u) = n
	
	! Outputs
	x_u = x_u(1:i_u)
	ind_u = ind_u(1:i_u)

    end subroutine unique
    !===============================================================================!
    
    function my_ss(tmatrix,gp)
    ! Purpose: STEADY STATE MARKOV CHAIN
    ! Originally written by Arnau Valladares-Esteban
    implicit none
    integer :: gp
    integer :: row, col, iter
    real(8) :: aux_sum
    real(8), dimension(gp) :: my_ss
    real(8), dimension(gp) :: dist, ndist
    real(8), dimension(gp,gp) :: tmatrix

    ! Initialise distribution
    dist = 1.d0/real(gp)

    do iter = 1, 10000
        ndist = 0.d0
        do col = 1, gp
        do row = 1, gp
            ndist(col) = ndist(col) + (dist(row)*tmatrix(row,col))
        end do
        end do
        aux_sum = sum(abs(ndist-dist))
        dist = ndist
        if (aux_sum.lt.1.0d-10) then
        exit
        end if
    end do

    my_ss = dist
    end function my_ss
    !===============================================================================!
  
    SUBROUTINE grid(x,xmin,xmax,s)
    ! Purpose: Generate grid x on [xmin,xmax] using spacing parameter s set as follows:
    ! s=1		linear spacing
    ! s>1		left skewed grid spacing with power s
    ! 0<s<1		right skewed grid spacing with power s
    ! s<0		geometric spacing with distances changing by a factor -s^(1/(n-1)), (>1 grow, <1 shrink)
    ! s=-1		logarithmic spacing with distances changing by a factor (xmax-xmin+1)^(1/(n-1))
    ! s=0		logarithmic spacing with distances changing by a factor (xmax/xmin)^(1/(n-1)), only if xmax,xmin>0
    IMPLICIT NONE
    REAL(8), DIMENSION(:), INTENT(OUT) :: x
    REAL(8), INTENT(IN) :: xmin,xmax,s
    REAL(8) :: c ! growth rate of grid subintervals for logarithmic spacing
    INTEGER :: n,i
    n=size(x)
    FORALL(i=1:n) x(i)=(i-1)/real(n-1,8)
    IF (s>0.0d0) THEN
	    x=x**s*(xmax-xmin)+xmin
	    !IF (s==1.0d0) THEN
	    !	PRINT '(a,i8,a,f7.3,a,f7.3,a)', 'Using ',n,' equally spaced grid points over domain [',xmin,',',xmax,']'
	    !ELSE
	    !	PRINT '(a,i8,a,f7.3,a,f7.3,a,f7.3,a)', 'Using ',n,' skewed spaced grid points with power ',s,' over domain [',xmin,',',xmax,']'
	    !END IF
    ELSE
	    IF (s==-1.0d0) THEN
		    c=xmax-xmin+1
    !		ELSEIF (s==0.0d0) THEN
    !			IF (xmin>0.0d0) THEN
    !				c=xmax/xmin
    !			ELSE
    !				STOP 'grid: can not use logarithmic spacing for nonpositive values'
    !			END IF
	    ELSE
		    c=-s
	    END IF
	    PRINT '(a,i8,a,f6.3,a,f6.3,a,f6.3,a)', 'Using ',n,' logarithmically spaced grid points with growth rate ',c,' over domain [',xmin,',',xmax,']'
	    x=((xmax-xmin)/(c-1))*(c**x)-((xmax-c*xmin)/(c-1))
    END IF
    END SUBROUTINE grid
    !===============================================================================!

    FUNCTION linint(x,y,xi)
    ! Purpose: linear interpolation of function y on grid x at interpolation point xi
    !          To make it pure, cannot use PRINT or STOP statements
    IMPLICIT NONE
    REAL(8), DIMENSION(:), INTENT(IN) :: x,y
    REAL(8), INTENT(IN) :: xi
    REAL(8) :: linint
    REAL(8) :: a,b,d
    INTEGER :: n,i
    n=size(x)
    IF (size(y)/=n) THEN
	    PRINT *, 'linint: x and y must be of the same size'
        PAUSE
	    STOP 'program terminated by linint'
    END IF
    i=max(min(locate(x,xi),n-1),1)
    d=x(i+1)-x(i)
    !IF (d == 0.0) STOP 'bad x input in splint'
    a=(x(i+1)-xi)/d
    b=(xi-x(i))/d
    linint=a*y(i)+b*y(i+1)
    END FUNCTION linint
    !===============================================================================!
   
    PURE FUNCTION locate(xx,x)
    IMPLICIT NONE
    REAL(8), DIMENSION(:), INTENT(IN) :: xx
    REAL(8), INTENT(IN) :: x
    INTEGER :: locate
    INTEGER :: n,il,im,iu
    n=size(xx)
    il=0
    iu=n+1
    do
	    if (iu-il <= 1) exit
	    im=(iu+il)/2
	    if (x >= xx(im)) then
		    il=im
	    else
		    iu=im
	    end if
    end do
    if (x == xx(1)) then
	    locate=1
    else if (x == xx(n)) then
	    locate=n-1
    else
	    locate=il
    end if
    END FUNCTION locate
    !===============================================================================!
    
    !===============================================================================!
    !subroutine bracket(x, xval, l, r)
    !    ! original code by  John Burkardt
    !    real(8), intent(in), dimension(:) :: x
    !    real(8), intent(in) :: xval
    !    integer, intent(out) :: l, r
    !    integer :: i, n
    !
    !    n=size(x)
    !    do i = 2, n - 1
    !       if ( xval < x(i) ) then
    !          l = i - 1
    !          r = i
    !          return
    !       end if
    !    end do
    !    l = n - 1
    !    r = n
    !end subroutine bracket
    !===============================================================================!
    
    function gini(x_in, y_in)
        ! DESCRIPTION: computes Lorenz curve and Gini coefficient
        ! Notes: written by F.Kindermann
        ! See: https://www.ce-fortran.com/forums/topic/gini-coefficient-and-lorenz-curve/#post-1951
        use toolbox, only: sort, plot, execplot

        real(8), intent(in) :: x_in(:), y_in(:)
        real(8) :: gini
        integer :: n, ic
        real(8), allocatable :: xs(:), ys(:), xcum(:), ycum(:)
        integer, allocatable :: iorder(:)

        ! get array size
        n = size(x_in, 1)

        ! ALLOCATE LARGE ARRAYS TO AVOID SIZE PROBLEMS
        
        ! first deallocate
        if(allocated(xs))deallocate(xs)
        if(allocated(ys))deallocate(ys)
        if(allocated(xcum))deallocate(xcum)
        if(allocated(ycum))deallocate(ycum)
        if(allocated(iorder))deallocate(iorder)
        
        ! then allocate
        allocate(xs(n))
        allocate(ys(n))
        allocate(xcum(0:n))
        allocate(ycum(0:n))
        allocate(iorder(n))
        
        
        ! NOW CALCULATE GINI INDEX

        ! sort array
        xs = x_in
        call sort(xs, iorder)
            
        ! sort y's and normalize to 1
        do ic = 1, n
            ys(ic) = y_in(iorder(ic))
        enddo
        ys = ys/sum(ys)
        

        ! calculate cumulative distributions
        xcum(0) = 0d0
        ycum(0) = 0d0
        do ic = 1, n
            xcum(ic) = xcum(ic-1) + ys(ic)*xs(ic)
            ycum(ic) = ycum(ic-1) + ys(ic)
        enddo
        
        ! now normalize cumulated attributes
        xcum = xcum/xcum(n)
        
        ! plot the Lorenz curve
        call plot(ycum, ycum, linewidth=1d0, color="black", dashtype="--")
        call plot(ycum, xcum)
        call execplot()

        ! determine gini index
        gini = 0d0
        do ic = 1, n
            gini = gini + ys(ic)*(xcum(ic-1) + xcum(ic))
        enddo
        gini = 1d0 - gini
        
    end function gini
    !===============================================================================!
    
    function quantili(x,w,q) result(y)
    ! Purpose: computes quantiles q of x with weights w
    ! w is discrete prob function (or PMF)
    ! need not be sorted or normalized
    ! Sources: Cagetti and De Nardi M-function quantili.m
    ! Note: takes care of repeated values
    ! Dependencies: calls subroutine unique
    use toolbox, only: sort    
    implicit none
    
    !Declare inputs:
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: w(:)
    real(8), intent(in) :: q(:)
    !Declare output:
    real(8) :: y(size(q))

    !Declare locals
    integer :: i, n, istat
    integer, allocatable :: ix(:), ind_u(:)
    real(8), allocatable :: xs(:), ws(:), cums(:)
    real(8), allocatable :: cums_u(:), xs_u(:)

    n = size(x)
    allocate( ix(n), xs(n),ws(n),cums(n),stat=istat )
    if (istat/=0) then
        call myerror("quantili: Allocation failed!")
    endif

    !!Check inputs
    if (size(w)/=n) then
        call myerror("quantili: x (variable) and w (pmf) must be of the same size")
    endif

    !xs is x sorted, ix is the sorting index
    xs = x 
    call sort(xs,ix)
    ws=w(ix)
    ws=ws/sum(ws)
    cums=cumsum(ws)
    
    ![xs_u,ind_u] = unique(xs,'last')
    call unique(xs, xs_u,ind_u)
    cums_u = cums(ind_u)

    y = 0.0d0
    do i=1,size(q)
        if (cums_u(1)<=q(i)) then
            y(i) = linint(cums_u,xs_u,q(i))
        else
            y(i) = xs_u(1)    
        endif
    enddo

    end function quantili
    !===============================================================================!

    subroutine calculate_quintiles(a_input,a_dist,thresholds, quantiles)
    !Source: Kindermann's book pp. 467-469
    !Compare to function "quantili" in this module (see below)

    use toolbox, only: sort    
    implicit none

    !Declare inputs:
    real(8), intent(in) :: a_input(:)
    real(8), intent(inout) :: a_dist(:)
    real(8) :: thresholds(:)

    !Declare outputs:
    real(8), intent(out) :: quantiles(size(thresholds, 1))

    !Declare locals:
    integer :: ia, ic, it, NC, istat
    real(8) :: slope
    integer, allocatable :: iorder(:)
    real(8), allocatable :: a_sort(:), a_cdist(:)

    ! define quantile thresholds (now passed as inputs to subr)
    !thresholds = (/0.05d0, 0.25d0, 0.50d0, 0.75d0, 0.95d0/)
    quantiles = 0d0

    ! K only uses asset levels with pop share of at least 10^(12)
    ! This is similar to getting rid of zeros as I do in lrzcurve
    ! --- SKIP FOR NOW ---!

    NC = size(a_dist,1)

    !!Check inputs
    if (size(a_input)/=NC) then
        call myerror("calculate_quintiles: a (variable) and a_dist (pmf) must be of the same size")
    endif

    allocate( iorder(NC), a_sort(NC), a_cdist(NC), stat=istat )
    if (istat/=0) then
        call myerror("calculate_quintiles: Allocation failed!")
    endif

    ! sort array and distribution
    a_sort = a_input 
    call sort(a_sort(1:NC), iorder(1:NC))

    ! calculate cumulative distribution (attention ordering)
    a_cdist(1) = a_dist(iorder(1))
    do ic = 2, NC
        a_cdist(ic) = a_cdist(ic-1) + a_dist(iorder(ic))
    enddo

    ! get quantiles
    do it = 1, size(thresholds, 1)
        if(thresholds(it) <= a_cdist(1))then
            quantiles(it) = a_sort(1)
        else
            do ic = 2, NC
                if(thresholds(it) < a_cdist(ic)) then
                    slope = (a_sort(ic)-a_sort(ic-1))/(a_cdist(ic)-a_cdist(ic-1))
                    quantiles(it) = a_sort(ic-1) + slope*(thresholds(it)-a_cdist(ic-1))
                    exit
                elseif(ic == NC)then
                    quantiles(it) = a_sort(NC)
                endif
            enddo
        endif
    enddo


    end subroutine calculate_quintiles
    !===============================================================================!

    subroutine lrzcurve_basic(f_in,y,gini)
    ! Purpose: It computes the Gini WITHOUT eliminating the zeros
    ! Source: This is a simplified version of Matlab <lrzcurve>
    use toolbox, only: sort    
    implicit none

    !Declare inputs and outputs:
    real(8), intent(in) :: f_in(:)  !Distribution
    real(8), intent(in) :: y(:)     !Variable of interest
    real(8), intent(out) :: gini    !Gini coefficient

    !Declare local variables:
    real(8) :: minpop
    integer :: i,n, istat
    real(8), allocatable :: f(:),y_sort(:), Scum(:),S(:),f_temp(:)
    integer, allocatable :: key(:)

    n = size(y)

    !Check inputs
    if (size(f_in)/=n) then
        call myerror("lrzcurve_basic: f (distrib.) and y (var. of interest) must be of the same size")
    endif

    allocate(f(n),y_sort(n),Scum(n),S(n),key(n),stat=istat)
    if (istat/=0) then
        call myerror("lrzcurve_basic: Allocation failed!")
    endif

    !I do not want to modify f_in
    f = f_in

    !Sort x in scending order 
    !Sort p
    y_sort = y
    call sort(y_sort,key)

    !f = f(key) !stack overflow occurs here
    f_temp = f
    do i=1,size(f)
        f(i) = f_temp(key(i))
    enddo
    deallocate(f_temp)

    S = y_sort*f
    Scum = cumsum(S) 
    gini = f(1)*Scum(1)

    do i=2,n
        gini = gini + (Scum(i) + Scum(i-1))*f(i)
    enddo

    gini = 1.0d0 - (gini/Scum(n))
    minpop = minval(f)

    ! Normalize the gini (smth not always done...)
    gini = gini/(1.0d0 - minpop)

    end subroutine lrzcurve_basic
    !===============================================================================!
    
    subroutine lrzcurve(p,x,gini_out,fx_out,sx_out,mean_x_out,stdv_x_out)
    
    ! ------------------------- LEGEND --------------------------------!
    ! Purpose: compute Lorenz curve, gini coeff, mean and std
    ! INPUTS:
    ! p is the probability distrib (it must sum to 1)
    ! x is a vector with the values of the variable of interest
    ! OUTPUTS:
    ! fx(:) is share of population
    ! sx(:) is the corresponding share of wealth/income/etc.
    ! E.g. plot Lorenz curve with fx on the x-axis and sx on the y-axis
    ! See wiki: https://en.wikipedia.org/wiki/Lorenz_curve
    ! DEPENDENCIES:
    ! lrzcurve calls a subroutine to sort arrays. It can either be
    ! <sort> from Kindermann's toolbox (and in this case you need to use
    ! the toolbox) or <QsortC> which is stored in this module
    ! -----------------------------------------------------------------!

    !use toolbox, only: sort    
    
    implicit none
    !Declare inputs:
    real(8), intent(in) :: p(:)
    real(8), intent(in) :: x(:)
    !Declare outputs:
    real(8), intent(out) :: gini_out
    real(8), allocatable, intent(out), optional :: fx_out(:)
    real(8), allocatable, intent(out), optional :: sx_out(:)
    real(8), intent(out), optional :: mean_x_out
    real(8), intent(out), optional :: stdv_x_out 

    !Declare locals:
    integer :: n, i, n_valid, istat
    real(8) :: minpop, mean_x, stdv_x, gini 
    real(8), allocatable :: p1(:), x1(:), fx(:), sx(:)
    integer, allocatable :: key(:)

    n = size(x)

    if (size(p)/=n) then
        call myerror("lrzcurve: x and p must have the same size")
    endif
    if (abs(sum(p)-1.0d0)>1d-8) then
        call myerror("lrzcurve: p must sum to one")
    endif

    !Preliminary step
    !Matlab code:
    !%% Eliminate elements with zero probability
    !p1=p; p(p1==0)  = []; x(p1==0)  = [];

    p1 = pack(p,p/=0.0d0)
    x1 = pack(x,p/=0.0d0)
    
    n_valid = count(p/=0.0d0)
    allocate(key(n_valid),stat=istat)
    key = [ (i,i=1,n_valid) ]
    if (istat/=0) then
        call myerror("lrzcurve: Allocation failed!")
    endif

    !Compute mean and standard deviation
    mean_x  = dot_product(p1,x1)
    stdv_x  = dot_product(p1,(x1-mean_x)**2)

    !Sort x1 in ascending order
    !call sort(x1,key)
    call QsortC(x1,key)

    !Sort distribution accordingly
    fx = p1(key)

    !sx(i) is the term x(i)*fx(i)
    sx = (x1*fx)
    sx = sx/mean_x
    
    !Add initial zero
    fx = [0d0, fx]
    sx = [0d0, sx]
    
    !Compute cumulative sums:
    ! sx is the cumulative share of x
    sx = cumsum(sx)

    !Compute Gini coefficient:
    gini = sx(1)*fx(1)
    do i = 2,size(fx)
        gini = gini +(sx(i)+sx(i-1))*fx(i)
    enddo
    gini = 1.0d0-(gini/sx(size(sx)))

    ! Keep the smallest population, needed to normalize the Gini coefficient
    minpop = minval(p1)

    ! Normalize the gini (smth not always done...)
    gini = gini/(1.0d0 - minpop)

    ! Assign outputs
    gini_out = gini
    
    ! Assign optional outputs
    ! Lorenz curve(i) is (fx(i), shareX(i))
    if (present(fx_out)) then
        fx_out = cumsum(fx)
    endif
    if (present(sx_out)) then
        sx_out = sx
    endif
    if (present(mean_x_out)) then
        mean_x_out  = mean_x
    endif
    if (present(stdv_x_out)) then
        stdv_x_out  = stdv_x
    endif

    end subroutine lrzcurve
    !===============================================================================!
    
    subroutine golden_method(f, a, b, x1, f1, mytol, mymaxit)
    ! Purpose: Applies Golden-section search to search for the *maximum* of a function 
    ! in the interval (a, b).
    ! Source: https://en.wikipedia.org/wiki/Golden-section_search
    ! Adapted to Fortran90 from: https://github.com/QuantEcon
    
    !---------------------------------------------------!
    !INPUTS
    interface
        function f(x)
        implicit none
        real(8), intent(in) :: x
        real(8) :: f
        end function f
    end interface
    real(8), intent(in) :: a, b
    !Some optional inputs
    integer,  optional :: mymaxit
    real(8), optional :: mytol
    !OUTPUTS
    real(8), intent(out) :: x1, f1
    !---------------------------------------------------!
    
    !Locals
    integer :: maxit, it
    real(8) :: tol, alpha1, alpha2, d, f2, x2, s
  
    !! Assign default value to maxit if not defined by user
    if (present(mymaxit)) then
        maxit = mymaxit
    else
        maxit = 1000
    end if
    
    ! Assign default value to tol if not defined by user
    if (present(mytol)) then
        tol = mytol
    else
        tol = 1.0d-6
    end if
  
    alpha1 = (3.d0 - sqrt(5.d0)) / 2.d0
    alpha2 = 1.d0 - alpha1
    d = b - a
    x1 = a + alpha1*d
    x2 = a + alpha2*d
    s = 1.d0
    f1 = f(x1)
    f2 = f(x2)
    d = alpha1*alpha2*d
  
    it = 0
  
    do while ((d.gt.tol).and.(it.lt.maxit))
        it = it + 1
        d = d*alpha2
  
        if (f2.gt.f1) then
            x1 = x2
            f1 = f2
            x2 = x1 + s*d
        else
            x2 = x1 - s*d
        end if
  
        s = sign(s, x2-x1)
        f2 = f(x2)
    end do
  
    if (it.ge.maxit) then
        print *, "Golden method: Maximum iterations exceeded"
    end if
  
    if (f2.gt.f1) then
        x1 = x2
        f1 = f2
    end if
    
    end subroutine golden_method
    !===============================================================================!
  
    FUNCTION rtbis(func,x1,x2,xacc,MAXIT)
    !-------------------------------------------------------------!
    ! DESCRIPTION:
    ! Bisection to find root x of a nonlinear function func(x)=0
    ! INPUTS:
    ! func: function to pass to the rootfinder
    ! [x1 x2]: bracketing interval
    ! xacc: accuracy criterion
    ! MAXIT: maximum number of iterations
    ! OUTPUT
    ! rtbis: root of the function in [x1 x2]
    ! SOURCE:
    ! http://numerical.recipes/
    !-------------------------------------------------------------!
 
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: x1,x2,xacc
    INTEGER, INTENT(IN) :: MAXIT
    REAL(8) :: rtbis
    INTERFACE
	    FUNCTION func(x)
	    IMPLICIT NONE
	    REAL(8), INTENT(IN) :: x
	    REAL(8) :: func
	    END FUNCTION func
    END INTERFACE
    !INTEGER, PARAMETER :: MAXIT=40
    INTEGER :: j
    REAL(8) :: dx,f,fmid,xmid
    fmid = func(x2)
    f    = func(x1)
    if (f*fmid >= 0.0d0) then 
	    call myerror("rtbis: root must be bracketed")
    endif
    if (f < 0.0d0) then
	    rtbis=x1
	    dx=x2-x1
    else
	    rtbis=x2
	    dx=x1-x2
    end if
    do j=1,MAXIT
	    dx=dx*0.5d0
	    xmid=rtbis+dx
	    fmid=func(xmid)
	    if (fmid <= 0.0d0) rtbis=xmid
	    if (abs(dx) < xacc .or. fmid == 0.0d0) RETURN
    end do
    write(*,*) "WARNING: rtbis: too many bisections"
    END FUNCTION rtbis
    !===============================================================================!
    
    FUNCTION zbrent(func,x1,x2,tol,ITMAX)
    !-------------------------------------------------------------!
    ! DESCRIPTION:
    ! Brent's method to find root x of a nonlinear function func(x)=0
    ! It is the same as fzero in Matlab. Brent's method is generally
    ! better than bisection.
    ! INPUTS:
    ! func: function to pass to the rootfinder
    ! [x1 x2]: bracketing interval
    ! tol: accuracy criterion
    ! MAXIT: maximum number of iterations ???
    ! OUTPUT
    ! rtbis: root of the function in [x1 x2]
    ! SOURCE:
    ! http://numerical.recipes/
    !-------------------------------------------------------------!
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: x1,x2,tol
    INTEGER, INTENT(IN) :: ITMAX
    REAL(8) :: zbrent
    INTERFACE
	    FUNCTION func(x)
	    !USE mkl95_precision, ONLY: WP => DP
	    IMPLICIT NONE
	    REAL(8), INTENT(IN) :: x
	    REAL(8) :: func
	    END FUNCTION func
    END INTERFACE
    REAL(8), PARAMETER :: EPS=epsilon(x1)
    INTEGER :: iter
    REAL(8) :: a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm
    a=x1
    b=x2
    fa=func(a)
    fb=func(b)
    if ((fa > 0.0 .and. fb > 0.0) .or. (fa < 0.0 .and. fb < 0.0)) then
	    call myerror('root must be bracketed for zbrent')
    endif
    c=b
    fc=fb
    do iter=1,ITMAX
	    if ((fb > 0.0 .and. fc > 0.0) .or. (fb < 0.0 .and. fc < 0.0)) then
		    c=a
		    fc=fa
		    d=b-a
		    e=d
	    end if
	    if (abs(fc) < abs(fb)) then
		    a=b
		    b=c
		    c=a
		    fa=fb
		    fb=fc
		    fc=fa
	    end if
	    tol1=2.0d0*EPS*abs(b)+0.5d0*tol
	    xm=0.5d0*(c-b)
	    if (abs(xm) <= tol1 .or. fb == 0.0) then
		    zbrent=b
		    RETURN
	    end if
	    if (abs(e) >= tol1 .and. abs(fa) > abs(fb)) then
		    s=fb/fa
		    if (a == c) then
			    p=2.0d0*xm*s
			    q=1.0d0-s
		    else
			    q=fa/fc
			    r=fb/fc
			    p=s*(2.0d0*xm*q*(q-r)-(b-a)*(r-1.0d0))
			    q=(q-1.0d0)*(r-1.0d0)*(s-1.0d0)
		    end if
		    if (p > 0.0) q=-q
		    p=abs(p)
		    if (2.0d0*p  <  min(3.0d0*xm*q-abs(tol1*q),abs(e*q))) then
			    e=d
			    d=p/q
		    else
			    d=xm
			    e=d
		    end if
	    else
		    d=xm
		    e=d
	    end if
	    a=b
	    fa=fb
	    b=b+merge(d,sign(tol1,xm), abs(d) > tol1 )
	    fb=func(b)
    end do
    write(*,*) 'WARNING: zbrent: exceeded maximum iterations'
    zbrent=b
    END FUNCTION zbrent
    !===============================================================================!

    function bddparetocdf(emin, emax, shape, eval) result(cdf)
    ! Purpose: calculate the cumulative density (or mass) at eval for a
    ! discretized bounded Pareto distribution over (emin,emax)
    ! Source: https://ideas.repec.org/a/red/issued/17-402.html
    ! Code for paper "Aggregate Consequences of Credit Subsidy Policies"
    ! by Jo and Senga.
    implicit none

    ! Inputs:
    real(8), intent(in) :: emin 
    real(8), intent(in) :: emax
    real(8), intent(in) :: shape
    real(8), intent(in) :: eval
    ! Function result:
    real(8) :: cdf
    ! Locals:
    real(8) :: paretocdf
    
    ! Check inputs:
    if (emin <=0 ) then
        call myerror("bddparetocdf: emin cannot be negative!")
    endif
    
    if (emax <= emin ) then
        call myerror("bddparetocdf: emax cannot be less than emin!")
    endif

    ! Body of Func
    paretocdf = 1.0d0 - (emin/eval)**shape
    cdf       = paretocdf/(1.0d0 - (emin/emax)**shape)

    end function bddparetocdf
    !===============================================================================!
    
    subroutine discretize_pareto(neps, eps, shape, rhoeps, ergoeps, pie)
    !--------------------------------------------------------------------------!
    ! This sub computes the ergodic distribution and the transition matrix of a
    ! bounded Pareto distribution, given grids of epsilon values.
    ! INPUTS:
    ! neps   :: number of grid points
    ! eps    :: grid with endpoints e_min and e_max
    ! shape  :: shape parameter
    ! rhoeps :: persistence
    !--------------------------------------------------------------------------!
    ! Source: https://ideas.repec.org/a/red/issued/17-402.html
    ! Code for paper "Aggregate Consequences of Credit Subsidy Policies"
    ! by Jo and Senga.
    !--------------------------------------------------------------------------!
    implicit none
    ! Inputs:
    integer, intent(in) :: neps
    real(8), intent(in) :: eps(neps)
    real(8), intent(in) :: shape
    real(8), intent(in) :: rhoeps
    ! Outputs
    real(8), intent(out) :: ergoeps(neps), pie(neps,neps)
    ! Locals:
    integer :: i
    real(8) :: emin, emax, mass
    real(8) :: mideps(neps-1)

    ! Check inputs
    if (shape<=0.0d0) then
        call myerror("discretize_pareto: shape param must be positive!")
    endif

    ! Body of discretize_pareto
    ergoeps = 0.0d0
    pie     = 0.0d0

    ! emin and emax are imaginary end points of a continuous distribution
    emin = eps(1) - (eps(2)-eps(1))/2.0d0
    emax = eps(neps) + (eps(neps)-eps(neps-1))/2.0d0

    mideps = 0.0d0
    do i = 1,neps-1
        mideps(i) = (eps(i)+eps(i+1))/2;
    enddo

    ! compute density or mass at each epsilon point
    ergoeps(1) = bddparetocdf(emin, emax, shape, mideps(1))
    do i = 2,neps-1
        mass       = bddparetocdf(emin,emax,shape, mideps(i-1))
        ergoeps(i) = bddparetocdf(emin,emax,shape, mideps(i)) - mass
    enddo
    ergoeps(neps) = 1.0d0 - bddparetocdf(emin, emax, shape, mideps(neps-1))


    ! transition matrix given rhoeps
    do i = 1,neps
        pie(i,i) = rhoeps
        pie(i,:) = pie(i,:) + (1.0d0-rhoeps)*ergoeps
    enddo

    end subroutine discretize_pareto
    !===============================================================================!
	
    subroutine tauchen(rho, sigma, mu, cover, gp, values, trans)
    !-------------------------------------------------------------------------------!
    ! Purpose: approximating first-order autoregressive process with Markov chain
    !
    ! y_t = rho * y_(t-1) + u_t
    !
    ! u_t is a Gaussian white noise process with standard deviation sigma.
    !
    ! cover determines the width of discretized state space, Tauchen uses m=3
    !
    ! gp is the number of possible states chosen to approximate
    ! the y_t process
    !
    ! trans is the transition matrix of the Markov chain
    !
    ! values is the discretized state space of y_t
    !
    ! Adapted from https://github.com/lucaguerrieri/
    ! Written in Fortran by Arnau Valladares-Esteban, 
    ! https://github.com/drarnau/Replication_Krusell-et-al-2017/blob/master/Utils.f90
    ! Modified by Alessandro Di Nola, following also Robert Kirkby's TauchenMethod 
	! https://github.com/vfitoolkit/VFIToolkit-matlab/blob/master/TauchenMethod/TauchenMethod.m
    ! Note: if the process y_t has non-zero mean, i.e.
    ! y_t = mu + rho * y_(t-1) + u_t, ==> E(y) = mu/(1-rho)
    ! Let z_t = y_t - mu/(1-rho)
    ! Apply Tauchen to z_t and then obtain y_t = z_t + mu/(1-rho)
    !-------------------------------------------------------------------------------!
    implicit none
    ! Declare inputs and outputs:
    integer, intent(in) :: gp
    real(8), intent(in) :: rho, sigma, mu, cover
    real(8), dimension(gp), intent(out)    :: values
    real(8), dimension(gp,gp), intent(out) :: trans
    ! Declare locals:
    integer :: j, k
    real(8) :: sd_y, ymin, ymax, w, ystar

    ! standard deviation of y_t
    sd_y = sqrt(sigma**2.d0/(1.d0-rho**2.d0))

    ymax = cover*sd_y   ! upper boundary of state space
    ymin = -ymax        ! lower boundary of state space
    w = (ymax-ymin)/real(gp-1,8) ! length of interval
    
    ystar = mu/(1.0d0-rho) !expected value of y

    values = ystar + linspace(ymin, ymax, gp)

    ! Compute transition matrix
    do j = 1, gp
        do k = 2, gp-1
            trans(j,k) = &
            normcdf(values(k)-rho*values(j)+(values(k+1)-values(k))/2.d0,mu,sigma) &
            - normcdf(values(k)-rho*values(j)-(values(k)-values(k-1))/2.d0,mu,sigma)
        enddo
        ! only subtract half the interval on the right
        trans(j,1) = normcdf(values(1)-rho*values(j)+w/2.d0,mu,sigma)
        ! only subtract half the interval on the left
        trans(j,gp) = 1.d0 - normcdf(values(gp)-rho*values(j)-w/2.d0,mu,sigma)
    enddo
    contains
        
        real(8) function normcdf(x,mu,sigma)
        implicit none
        real(8), intent(in) :: x, mu, sigma

        normcdf = (1.d0+erf((x-mu)/sqrt(2.d0*sigma**2)))/2.d0
        end function normcdf
    
    end subroutine tauchen
    !===============================================================================!
    
    function mycorr(x,y,w) result(corr_weight)
      ! This function computes the correlation coefficient b/w two vectors
      ! X and Y with weights W. 
      ! See wiki: https://en.wikipedia.org/wiki/Correlation_and_dependence
	  implicit none
      
      ! Declare input variables:
      real(8), intent(in) :: x(:), y(:)
      real(8), intent(in) :: w(:)
      
      ! Declare function result:
      real(8) :: corr_weight
      
      ! Declare local variables:
      integer :: i, n
      real(8) :: mean_x, mean_y, sum_covar, sum_weight
      real(8) :: var_x, var_y, cov_xy
      
      
      !-------------------------------!
      ! Check inputs
      if ( size(x,dim=1)/=size(y,dim=1) ) then
          call myerror("Dimension of X and Y do not match!")
      endif
      if ( size(x,dim=1)/=size(w,dim=1) ) then
          call myerror("Dimension of X and weights do not match!")
      endif
      if ( any(w<0.0d0) ) then
          call myerror("Weights must be positive!")
      endif
    
      ! Compute weighted mean of X and Y
      mean_x = weighted_mean(x,w)
      mean_y = weighted_mean(y,w)
      
      ! Compute weighted variance of X and Y
      var_x = weighted_variance(x,w)
      var_y = weighted_variance(y,w)
      
      ! Compute weighted covariance b/w X and Y
      n = size(x,dim=1) !assume x and y have same size
      sum_covar = 0.0d0
      sum_weight= 0.0d0
      do i=1,n
          sum_covar  = sum_covar + w(i)*(x(i)-mean_x)*(y(i)-mean_y)
          sum_weight = sum_weight + w(i)
      enddo
      cov_xy = sum_covar/sum_weight
      
      ! Compute weighted correlation b/w X and Y
      corr_weight = cov_xy/sqrt(var_x*var_y);
    
    contains
    
        function weighted_mean(x,w) result(meanX)
        implicit none
    
        ! Declare inputs
        real(8), intent(in) :: x(:), w(:)
        ! Declare function results
        real(8) :: meanX
        ! Declare locals
        integer :: n, i
        real(8) :: sum, sum_weight

        n = size(w,dim=1)
    
        !assume that weight vector and input vector have same length
        sum=0.0d0
        sum_weight=0.0d0
        do i=1,n
            sum        = sum +        x(i)*w(i)
            sum_weight = sum_weight + w(i)
        enddo
        meanX = sum/sum_weight
    
        end function weighted_mean
        !-----------------------------------------------------------!
        function weighted_variance(x,w) result(varX)
        implicit none
    
        ! Declare inputs
        real(8), intent(in) :: x(:), w(:)
        ! Declare function results
        real(8) :: varX
        ! Declare locals
        integer :: n, i
        real(8) :: sum, sum_weight

        n = size(w,dim=1)
    
        !assume that weight vector and input vector have same length
        sum=0.0d0
        sum_weight=0.0d0
        do i=1,n
            sum = sum + ( ( x(i)- weighted_mean(x,w))**2)*w(i)
            sum_weight = sum_weight + w(i)
        enddo
        varX = sum/sum_weight

        end function weighted_variance
        !-----------------------------------------------------------!
    end function mycorr
    !===============================================================================!
    
    RECURSIVE SUBROUTINE QsortC(A, index_A)
    ! Author: Juli Rew, SCD Consulting (juliana@ucar.edu), 9/03
    ! Based on algorithm from Thomas H. Cormen, Charles E. Leiserson, Ronald L.
    ! Rivest, and Clifford Stein, Introduction to Algorithms, The MIT Press, 1997 edition.
    ! Made F conformant by Walt Brainerd
    ! Modified by Giulio Fella (02/2013) to sort the array index accordingly 
    ! see https://github.com/gfell/gf_qsort/blob/master/mod_gf_qsort.f90
    
    ! Input/output variables
    ! A    	 : real(precision) array to sort
    ! index_A: integer array indexing elements of A
    REAL(8), INTENT(inout), DIMENSION(:) :: A
    INTEGER, INTENT(inout), DIMENSION(SIZE(A)) :: index_A
    INTEGER :: iq
    
    IF(SIZE(A) > 1) THEN
      CALL Partition(A,index_A, iq)
      CALL QsortC(A(:iq-1),index_A(:iq-1))
      CALL QsortC(A(iq:),index_A(iq:))
    ENDIF
    END SUBROUTINE QsortC

    SUBROUTINE Partition(A, index_A, marker)
    REAL(8), INTENT(in out), DIMENSION(:) :: A
    INTEGER, INTENT(in out), DIMENSION(SIZE(A)) :: index_A
    INTEGER, INTENT(out) :: marker
    INTEGER :: i, j
    INTEGER :: index_temp
    REAL(8) :: temp
    REAL(8) :: x      ! pivot point
    x = A(1)
    i= 0
    j= SIZE(A) + 1

    DO
      j = j-1
      DO
        IF (A(j) <= x) EXIT
        j = j-1
      END DO
      i = i+1
      DO
        IF (A(i) >= x) EXIT
        i = i+1
      END DO
      IF (i < j) THEN
        ! exchange A(i) and A(j)
        temp = A(i)
        A(i) = A(j)
        A(j) = temp

        index_temp = index_A(i)
        index_A(i) = index_A(j)
        index_A(j) = index_temp

      ELSEIF (i == j) THEN
        marker = i+1
        RETURN
      ELSE
        marker = i
        RETURN
      ENDIF
    END DO

    END SUBROUTINE Partition
!===============================================================================!

 
 
end module mod_numerical
