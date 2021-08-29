!!!!!!
!RUBC_Brock_Mirman.f90
!
!This program solves, simulates, and computes IRFs for the 
!Brock-Mirman model.
!
!"Really Uncertain Business Cycles"
!Nick Bloom, Max Floetotto, Nir Jaimovich, Itay Saporta-Eksten, Stephen J. Terry
!
!This Version: October 12, 2014
!!!!!!
module globals
implicit none

double precision, parameter :: alpha = 0.33,&
    deltak = 0.09,&
    beta = 0.95, &
    rhoa = 0.9,&
    sigmaa = 0.025,&
    sigma = 1,&
    nstdeva = 3.0,&
    kmin = 2.5,&
    kmax = 4.9,&
    errtol = 1e-8,&
    disttol = 1e-9,&
    shockprob = 1.0
    
integer, parameter :: checkbounds = 1,&
    GEerrorswitch = 4,&
    anum = 25,&
    knum = 1000,&
    numper = 5000,&
    numdiscard = 500,&
    polmaxit=500,&
    accelmaxit = 200,&
    distmaxit=10000,&
    numsimIRF=75000,&
    lengthIRF = 100,&
    shockperIRF = 25,& ! time period when shock hits (t from 1 to lengthIRF)
    !shockval = 120,&   ! 120 if capit destr shock, if tfp it must be in [1 anum] range   
    shockval = 15,&   ! 120 if capit destr shock, if tfp it must be in [1 anum] range   
    !IRFswitch=2  ! 1 if aggr tfp shock, 2 if capital destruction
    IRFswitch=1  ! 1 if aggr tfp shock, 2 if capital destruction

!Integer numbers for loop counters
integer :: act,kct,kprimect, ct,aprimect,polct,accelct,t,distct,simct, i, j

!Integer variables
integer :: seeddim,seedint,polstar,k_previous_max

!Real numbers
double precision :: aval,kval,kprimeval,cval,uval,yval,start,vferr,polerr,disterr,RHSbest,RHStemp

!Allocatable integer arrays
integer, allocatable :: seedarray(:),kprime(:,:),kprimeold(:,:),asimpos(:),Ksimpos(:),asimposIRF(:,:),KsimposIRF(:,:)

!Allocatable real arrays
double precision, allocatable :: asimshocks(:),k0(:),pr_mat_a(:,:),a0(:),RETURNMAT(:,:,:),V(:,:),Vold(:,:),&
    EVmat(:,:),kprimepol(:,:),Ksim(:),asim(:),asimgrid(:),Ysim(:),Csim(:),Isim(:),dist(:,:),distold(:,:),&
    adist(:),kdist(:)

end module globals
!********************************************** END MODULE *******************************************************!
    
program RUBC_Brock_Mirman
use base_lib
use omp_lib
use globals
implicit none


start = omp_get_wtime()
write(*,*) "######################################"
write(*,*) "Starting program for simple RBC model."
write(*,*) " "


!Allocate arrays
allocate(seedarray(seeddim),asimshocks(numper),k0(knum),pr_mat_a(anum,anum),a0(anum),&
    RETURNMAT(anum,knum,knum),kprime(anum,knum),V(anum,knum),Vold(anum,knum),kprimeold(anum,knum),&
    EVmat(anum,knum),kprimepol(anum,knum),asimpos(numper),Ksim(numper),asim(numper),asimgrid(anum),Ksimpos(numper),&
    Ysim(numper),Csim(numper),Isim(numper),dist(anum,knum),distold(anum,knum),adist(anum),kdist(knum))


write(*,*) "available threads = ",omp_get_max_threads()

!$omp parallel
write(*,*) "parallel hello to you."
!$omp end parallel

!set up the capital grid
call linspace(k0,log(kmin),log(kmax),knum); k0=exp(k0);

!set up the agg prod grid
call idio_tauchen(anum,rhoa,sigmaa,pr_mat_a,a0,nstdeva)

!set up the return matrix
RETURNMAT(:,:,:) = 0.0
!RETURNMAT(a,k,k')

!*!$omp parallel private(act,kct,kprimect,kval,kprimeval,aval,cval,uval)
!*!$omp do collapse(3)
!$omp parallel do default(shared) private(act,kct,kprimect,kval,kprimeval,aval,cval,uval)
do act=1,anum
do kct=1,knum
do kprimect=1,knum
    
    kval = k0(kct)
    kprimeval = k0(kprimect)
    aval = a0(act)
    
    yval = yprod(aval,kval)
    
    cval = yval + (1.0-deltak)*kval - kprimeval
    if (cval<=0.0) then
        uval = -huge(0d0)
    else if (cval>0.0) then
        uval = util(cval)
    end if
    
    RETURNMAT(act,kct,kprimect) = uval
    
    
end do !kprimect
end do !kct
end do !act
!$omp end parallel do
!*!$omp end do nowait
!*!$omp end parallel


write(*,*) " "



!now, perform the VFI, using Howard acceleration

!initialize
Vold(:,:) = 0.0
V(:,:) = 0.0

do polct=1,polmaxit

    !construct continuation value EVmat(a,k') 
    EVmat(:,:) = 0.0
    !$omp parallel do default(shared) private(act,kprimect,aprimect) 
    do act=1,anum
    do kprimect=1,knum
        
        do aprimect=1,anum
            EVmat(act,kprimect) = EVmat(act,kprimect) + pr_mat_a(act,aprimect)*beta * Vold(aprimect,kprimect)
        end do !aprimect
        
    end do !kprimect
    end do !act
    !$omp end parallel do
    
    !now, actually do the optimization
    !$omp parallel do default(shared) private(act,kct,kprimect,polstar,RHStemp,RHSbest,k_previous_max)
    do act=1,anum
    do kct=1,knum
        
        !Maximize the RHS of Bellman equation, for given act,kct
        RHSbest = -huge(0d0)
        
        !Monotonicity of policy function
        k_previous_max = 1
        if (kct>1) k_previous_max = kprime(act,kct-1)
        
        do kprimect = k_previous_max,knum
            RHStemp = RETURNMAT(act,kct,kprimect) + EVmat(act,kprimect)
            if (RHStemp>RHSbest) then
                RHSbest = RHStemp
                polstar = kprimect
            else
                exit
            end if
        end do !kprimect
        
        V(act,kct)      = RHSbest
        kprime(act,kct) = polstar
        k_previous_max  = polstar
        
    end do !act
    end do !act
    !$omp end parallel do
    
    !compute errors and end if converged
    vferr = maxval(V-Vold)
    
    write(*,'(a,i4)') "VF iteration ",polct
    write(*,'(a,f10.6)') "VF err ",vferr
    
    !exit if policies have converged
    if (vferr<errtol) exit
    
    !update if not converged
    Vold = V
    
end do !polct

do act=1,anum
do kct=1,knum
    kprimepol(act,kct)=k0(kprime(act,kct))
end do 
end do 


!VF diagnostics
write(*,*) " "
write(*,*) "Done with VFI."
if (minval(kprime)==1) write(*,*) "Max capital policy hits bottom of grid in theory."
if (maxval(kprime)==knum) write(*,*) "Min capital policy hits top of grid in theory."

write(*,'(a,f10.6,a)') "Finished VFI at ",real(omp_get_wtime()-start)," seconds."

write(*,'(a)') 'Çheck some elements of V...'
do i=1,4
    write(*,'(f10.4)') V(2,i)
enddo

pause



!compute unconditional distribution
write(*,*) " "
write(*,*) "Doing ergodic dist calculations."
distold(:,:) = 0.0
distold(anum/2,knum/2) = 1.0

do distct=1,distmaxit
    
    dist(:,:) = 0.0
    
    !loop over states 
    !$omp parallel do default(shared) private(act,kct,kprimect,aprimect) REDUCTION (+: dist)
    do act=1,anum
    do kct=1,knum
        
        kprimect=kprime(act,kct)
        
        !pushforward of weight
        do aprimect=1,anum
            dist(aprimect,kprimect) =  dist(aprimect,kprimect) + distold(act,kct)*pr_mat_a(act,aprimect)
        end do !aprimect
        
    end do !kct
    end do !act
    !$omp end parallel do
    
    disterr =maxval(abs(dist-distold))
    if (mod(distct,25)==1) then
        write(*,'(a,i4)') "Distrib. iter. ", distct
        write(*,'(a,f10.6)') "Distrib. sum ",sum(dist)
        write(*,'(a,f10.6)') "Distrib. error ",disterr
    end if 
    if (disterr<disttol) exit
    
    dist = dist/sum(dist)
    distold = dist
    
    
end do !distct

write(*,'(a)') 'Çheck some elements of dist...'
do i=1,4
    j = 150+i
    write(*,'(f10.8)') dist(2,j)
enddo

!marginal dists
adist(:) = 0.0
do act=1,anum
    adist(act) = sum(dist(act,:))
end do 

kdist(:) = 0.0
do kct=1,knum
    kdist(kct) = sum(dist(:,kct))
end do 

write(*,*) "Done with ergodic dist calculations."
write(*,"(A,F7.4,A,F7.4)") "Weight on min, max of capital grid = ",kdist(1),", ",kdist(knum)

!! Export results in txt files
open(8,file="k0_fortran.txt")
do kct=1,knum
write(8,*) k0(kct)
end do 
close(8)

open(8,file="V_fortran.txt")
do kct=1,knum
do act=1,anum
write(8,*) V(act,kct)
end do 
end do
close(8)

open(8,file="kprimepol_fortran.txt")
do kct=1,knum
do act=1,anum
write(8,*) kprimepol(act,kct)
end do 
end do
close(8)

open(8,file="dist_fortran.txt")
do kct=1,knum
do act=1,anum
write(8,*) dist(act,kct)
end do 
end do
close(8)


pause

write(*,*) " "
!write(*,*) "Finished program for the Brock-Mirman model at ",omp_get_wtime()-start," seconds."
write(*,*) "######################################"

!Deallocate arrays
deallocate(seedarray,asimshocks,k0,pr_mat_a,a0,&
    RETURNMAT,kprime,V,Vold,kprimeold,&
    EVmat,kprimepol,asimpos,Ksim,asim,asimgrid,Ksimpos,&
    Ysim,Csim,Isim,dist,distold,adist,kdist)

    contains

    !--------------------------------------------------------------------------------------------------!
    ! Internal functions
    !--------------------------------------------------------------------------------------------------!
    
double precision function yprod(aval,kval)
implicit none

!this function evaluates output given parameters and states

double precision :: aval,kval

yprod =  aval * (kval ** alpha) 
    
end function

double precision function util(cval)
implicit none

!this function evaluates the utility function

double precision :: cval

!util =  (cval**(1.0-sigma))/(1.0-sigma)
util = log(cval)
    
end function


end program RUBC_Brock_Mirman