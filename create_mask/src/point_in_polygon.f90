!> \file point_in_polygon.f90
!! \brief modified point-in-a-polygon-algorithm
! 
! Removed the tracer `silicate` from binary bio_restart files of HBM-ERGOM. 
! Does not work with the 2018s version of HBM-ERGOM anymore because the 
! alignment of data at the boundaries was changed. Needs a bio_restart file 
! with 13 tracers. Silicate needs to be bio tracer number 4.
! 
! This code is a modified version of Fortran code by Wm. Randolph Franklin 
! (Copyright (c) 1970-2003). The modifications are based on an answer at
! Stackoverflow by the user '4pie0'.
! 
! The original code is available at 
!        https://wrf.ecse.rpi.edu//Research/Short_Notes/pnpoly.html
! 
! The Stackoverflow answer is available at 
!        https://stackoverflow.com/a/32807570/4612235
! 
! This code was written by Daniel Neumann at the Leibniz-Institut for Baltic
! Sea Research Warnemuende (www.io-warnemuende.de). The work was done within'
! the project MeRamo (funded by BMVI, FKZ 50EW1601)
!
!> Module: point_in_polygon
!> \author Daniel Neumann, IOW
!> \date 13.04.2018


module point_in_polygon

  implicit none
  
  real(8), parameter :: eps = 0.0000001


! Adapted C code from
!   https://wrf.ecse.rpi.edu//Research/Short_Notes/pnpoly.html
! 
! Original Version:
!   Copyright (c) 1970-2003, Wm. Randolph Franklin 
! 
! Adapted Version:
!   Daniel Neumann, 2018, daniel.neumann@io-warnemuende.de
! 
! For point-location on the boundary this answer helped:
!   https://stackoverflow.com/a/32807570/4612235

contains

  function pnpoly_bnd(nvert, vertx, verty, testx, testy)

    implicit none
    
    integer,               intent(in) :: nvert         ! number of vertices
    real(8), dimension(:), intent(in) :: vertx, verty  ! coordinates of polygon vertices
    real(8),               intent(in) :: testx, testy  ! coordinates of test points
    
    integer :: i, j           ! iterators
    integer :: stopi          ! stop values for iterator i
    integer :: pnpoly_bnd     ! return value
    real(8) :: linex
    
    pnpoly_bnd = -1
    
    ! test whether the last point of the equals the first point
    if ( (vertx(1) .eq. vertx(nvert)) .and. (verty(1) .eq. verty(nvert)) ) then
      stopi = nvert-1
    else
      stopi = nvert
    end if
    
    ! we connect the last and the first vertex for the first comparison
    j = stopi
    
    do i = 1, stopi
      
      ! test whether point is on polygon points:
      if ( (abs(verty(i) - testy) < eps) .and. (abs(verty(j)-testy) < eps) ) then
        pnpoly_bnd = 1
      end if
      
      ! test whether point is on polygon edge:
      linex = (vertx(j)-vertx(i)) * (testy-verty(i) ) / &
                  (verty(j)-verty(i)) + vertx(i)
      
      if ( ( (verty(i) .gt. testy) .NEQV. (verty(j) .gt. testy) ) .and. &
           ( abs(testx - linex) < eps ) ) then
        pnpoly_bnd = 1
      end if
      
      j = i
      
    end do
    
    if (pnpoly_bnd .eq. -1) then
      pnpoly_bnd = pnpoly(stopi, vertx(1:stopi), verty(1:stopi), testx, testy)
    end if
    
  end function pnpoly_bnd


  function pnpoly(nvert, vertx, verty, testx, testy)

    implicit none
    
    integer,               intent(in) :: nvert         ! number of vertices
    real(8), dimension(:), intent(in) :: vertx, verty  ! coordinates of polygon vertices
    real(8),               intent(in) :: testx, testy  ! coordinates of test points
    
    integer :: i, j           ! iterators
    integer :: stopi          ! stop values for iterator i
    integer :: pnpoly         ! return value
    real(8) :: linex
    
    pnpoly = -1
    
    ! test whether the last point of the equals the first point
    if ( (vertx(1) .eq. vertx(nvert)) .and. (verty(1) .eq. verty(nvert)) ) then
      stopi = nvert-1
    else
      stopi = nvert
    end if
    
    ! we connect the last and the first vertex for the first comparison
    j = stopi
    
    do i = 1, stopi
      
      linex = (vertx(j)-vertx(i)) * (testy-verty(i) ) / &
                        (verty(j)-verty(i)) + vertx(i)
      
      if ( ( (verty(i) .gt. testy) .NEQV. (verty(j) .gt. testy) ) .and. &
           ( testx .lt. linex ) ) then
        pnpoly = -pnpoly
      end if
      
      j = i
      
    end do
    
  end function pnpoly

end module point_in_polygon