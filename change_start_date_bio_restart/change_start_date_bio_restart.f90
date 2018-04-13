!> \file change_start_date_bio_restart.f90
!! \brief change start date in a bio_restart file to 2012010100
! 
! DEPRECATED.
! 
! Please use change_start_date_restart instead. `change_start_date_restart` is
! more rebust. `change_start_date_bio_restart` possibly does not work with the
! current HBM-ERGOM binary bio_restart files.
!
! This code was written by Daniel Neumann at the Leibniz-Institut for Baltic
! Sea Research Warnemuende (www.io-warnemuende.de). The work was done within'
! the project MeRamo (funded by BMVI, FKZ 50EW1601)
!
!> Module: Main Programm
!> \author Daniel Neumann, IOW
!> \date 13.04.2018


program main

  implicit none

  !!!! VARS for TRACERS
  integer(4), parameter :: ntbio = 31     ! number of tagged tracers 
  integer(4), parameter :: ntben = 4      ! number of tagged benthos tracers


  !!!! AREA SPECIFIC VARS
  ! area 1: coarse
  ! area 2: fine

  !~ Checking dimensions of area:    1
    !~ points in N/S direction:    347
    !~ points in W/E direction:    414
    !~ layers:                      36
    !~ z-boundary points:          156
    !~ u-boundary points:           97
    !~ v-boundary points:           45
    !~ sources:                     80
    !~ u-dams:                       0
    !~ v-dams:                       0
     !~ surface wet points:              34086
     !~ wet points:                     616739
  !~ ... dimensions seems OK
  !~ Checking dimensions of area:    2
    !~ points in N/S direction:    387
    !~ points in W/E direction:    630
    !~ layers:                      25
    !~ z-boundary points:          858
    !~ u-boundary points:            0
    !~ v-boundary points:            0
    !~ sources:                      8
    !~ u-dams:                       0
    !~ v-dams:                       0
     !~ surface wet points:             124011
     !~ wet points:                    1117390

  ! from data_coarse.nml
  integer(4), parameter :: EW_1=414, NS_1=347, LAYERS_1=36
  integer(4), parameter :: NZBND_1=156, NUBND_1=97, NVBND_1=45
  ! from log file
  INTEGER(4), PARAMETER :: n2d_1 = 34086, n3d_1 = 616739

  ! from data_fine.nml
  integer(4), parameter :: EW_2=630, NS_2=387, LAYERS_2=25
  integer(4), parameter :: NZBND_2=858, NUBND_2=0, NVBND_2=0
  ! from log file
  INTEGER(4), PARAMETER :: n2d_2 = 124011, n3d_2 = 1117390

  ! derived
  integer(4), parameter :: kmx_main = 36     ! LAYERS_1
  integer(4), parameter :: nz_main = 156     ! NZBND_1



  !!!! simple IO support VARS
  integer(4)     :: unitIn, unitOt   ! input and output file units
  integer(4)     :: iosIn, iosOt     ! input and output file iostat
  character(255) :: fnamIn ! = 'bio_restart_01'
  character(255) :: fnamOt ! = 'bio_restart_ldon_01'
  real(8)        :: dummyReal
  integer(4)     :: dummyInt


  !!!! VAR FOR COPYING DATA
  character(11)    :: starta     ! start date of the restart file
  character(11)    :: starta_new ! start date of the new restart file

  ! cmod_arrays, l.299: allocate(ptc(ia)%p(0:n3d,1:ntracers-2), stat=ierr )
  ! cmod_arrays, l.182: n3d    = iw3(ia)
  REAL(8) :: ptc_1(0:n3d_1,1:ntbio), ptc_2(0:n3d_2,1:ntbio)

  ! cgt_ergom_coupler, l. 25: type(cmr2), allocatable, private, save :: benthos(:)
  ! cgt_ergom_coupler, l.192: allocate( benthos(ia)%p(0:n2d,1:ntben))
  !                           n2d = iw2
  real(8) :: benthos_1(0:n2d_1,1:ntben), benthos_2(0:n2d_2,1:ntben)

  ! cmod_arrays, l.242: bndz(ia)%p(1:ntracers,1:kn,0:nzn)
  ! REAL(8) :: bndzIn(1:(ntbio+2),1:kmx_main,0:nz_main), bndzOt(1:(ntbio+2),1:kmx_main,0:nz_main)
  REAL(8) :: bndz1D(1:(ntbio*kmx_main*(nz_main+1)))

  !!!! COMMAND LINE ARGUMENTS - VARIABLES
  integer(4) :: argc
  
  ! help request?
  logical :: helpme = .false.
  
  
  ! check whether help is requested
  call test_help_string(helpme)
  
  
  if (helpme) then
    
    call print_help()
    
  else

    !!!! GET COMMAND LINE ARGUMENTS
    argc = command_argument_count()
    if (argc /= 2) then
      write(*,'(a56,I4,a9)') 'Bad number of arguments (INFILE and OUTFILE needed) but ', argc, ' provided'
      stop
    end if
    call get_command_argument(1, fnamIn)
    call get_command_argument(2, fnamOt)


    starta_new='V201201.010'

    !!!! WORKING
    !! OPEN FILES
    unitIn = 1
    unitOt = 2
    ! open existing file
    open(unitIn,file=trim(fnamIn),form='unformatted',iostat=iosIn,         &
         action='read',asynchronous='yes',access='stream',status='old')
    ! open new file
    open(unitOt,file=trim(fnamOt),form='unformatted',action='write',   &
         asynchronous='yes',access='stream', iostat=iosOt)

    if(iosIn /= 0) write(*,*) 'error opening input file'
    if(iosOt /= 0) write(*,*) 'error opening output file'


    !! META DATA
    read(unitIn,iostat=iosIn) starta
    write(*,'(A16,A11)') 'Old Start Date: ', starta
    WRITE(unitOt,iostat=iosOt) starta_new
    if(iosIn /= 0) write(*,'(a19,i3)') 'error reading date ', iosIn
    if(iosOt /= 0) write(*,'(a19,i3)') 'error writing date ', iosOt


    !! AREA 1 DATA
    read(unitIn,iostat=iosIn) ptc_1(:,1:ntbio), benthos_1(:,1:ntben)
    write(unitOt,IOSTAT=iosOt) ptc_1(:,1:ntbio), benthos_1(:,1:ntben)
    if(iosIn /= 0) write(*,'(a29,i3)') 'error reading ptc for area 1 ', iosIn
    if(iosOt /= 0) write(*,'(a29,i3)') 'error writing ptc for area 1 ', iosOt
    ! write(*,*) ptcIn_1(1,:)


    !! AREA 2 DATA
    read(unitIn,iostat=iosIn) ptc_2(:,1:ntbio), benthos_2(:,1:ntben)
    write(unitOt,IOSTAT=iosOt) ptc_2(:,1:ntbio), benthos_2(:,1:ntben)
    if(iosIn /= 0) write(*,'(a29,i3)') 'error reading ptc for area 2 ', iosIn
    if(iosOt /= 0) write(*,'(a29,i3)') 'error writing ptc for area 2 ', iosOt


    !! BOUDNARIES (main area = area 1)
    read(unitIn,iostat=iosIn) bndz1D(1:)
   ! read(unitIn,iostat=iosIn) bndzIn(3,1:kmx_main,0:), bndzIn(4,1:kmx_main,0:),   &
   !                      bndzIn(5,1:kmx_main,0:), bndzIn(6,1:kmx_main,0:),   &
   !                      bndzIn(7,1:kmx_main,0:), bndzIn(8,1:kmx_main,0:),   &
   !                      bndzIn(9,1:kmx_main,0:), bndzIn(10,1:kmx_main,0:),  &
   !                      bndzIn(11,1:kmx_main,0:), bndzIn(12,1:kmx_main,0:), &
   !                      bndzIn(13,1:kmx_main,0:), bndzIn(14,1:kmx_main,0:)
    write(unitOt,iostat=iosOt) bndz1D(1:)
   ! WRITE(unitOt,iostat=iosOt) bndzOt(3,1:kmx_main,0:), bndzOt(4,1:kmx_main,0:),   &
   !                            bndzOt(5,1:kmx_main,0:), bndzOt(6,1:kmx_main,0:),   &
   !                            bndzOt(7,1:kmx_main,0:), bndzOt(8,1:kmx_main,0:),   &
   !                            bndzOt(9,1:kmx_main,0:), bndzOt(10,1:kmx_main,0:),  &
   !                            bndzOt(11,1:kmx_main,0:), bndzOt(12,1:kmx_main,0:), &
   !                            bndzOt(13,1:kmx_main,0:), bndzOt(14,1:kmx_main,0:), &
   !                            bndzOt(15,1:kmx_main,0:)
    if(iosIn /= 0) write(*,'(a29,i3)') 'error reading bndz main area ', iosIn
    if(iosOt /= 0) write(*,'(a29,i3)') 'error writing bndz main area ', iosOt

    ! read(unitIn,iostat=iosIn) dummyInt
    ! if(iosIn /= 0) write(*,*) 'error'

    !! CLOSE FILES
    close(unitIn)
    close(unitOt)
  
  end if ! .not. helpme
  
  
contains
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ test_help_string ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine test_help_string(helpme)
    
    logical, intent(out) :: helpme
    
    ! number of command line arguments
    integer(4) :: argc
    
    ! tmp command line argument
    character(len=255) :: tmp_arg
    
    ! read number of command line arguments
    argc = command_argument_count()
    
    ! test for help call
    if (argc > 0) then
      call get_command_argument(1, tmp_arg)
      if ((trim(tmp_arg) .eq. '-h') .or. (trim(tmp_arg) .eq. '--help') .or. &
          (trim(tmp_arg) .eq. '?')) then
        helpme = .true.
      end if
    end if
    
  end subroutine test_help_string
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ test_help_string ~~~~~~~~~~~~~~~~~~~~~~~~~~~

  subroutine print_help()
  
    write(*,'(A)') 'HELP FOR change_start_date_bio_restart                               2018/04/13'
    write(*,'(A)') ''
    write(*,'(A)') 'NAME'
    write(*,'(A)') ''
    write(*,'(A)') '      change start date bio restart - change start date in a bio_restart file'
    write(*,'(A)') '                                      to 2012010100'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'SYNOPSIS'
    write(*,'(A)') '      change_start_date_bio_restart.x bio_restart_in bio_restart_out'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'DESCRIPTION'
    write(*,'(A)') '      Please use change_start_date_restart instead. `change_start_date_restart`'
    write(*,'(A)') '      is more rebust. `change_start_date_bio_restart` possibly does not work '
    write(*,'(A)') '      with the current HBM-ERGOM binary bio_restart files.'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'OPTIONS'
    write(*,'(A)') '      -h, --help'
    write(*,'(A)') '           Print this help.'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'FILE DESCRIPTIONS'
    write(*,'(A)') '         '
    write(*,'(A)') '      bio_restart_in'
    write(*,'(A)') '         '
    write(*,'(A)') '         Binary bio_restart file of HBM-ERGOM. Should be from before late 2017.'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'AUTHORS'
    write(*,'(A)') '      This code was written by Daniel Neumann at the Leibniz-Institut for Baltic'
    write(*,'(A)') '      Sea Research Warnemuende (www.io-warnemuende.de). The work was done within'
    write(*,'(A)') '      the project MeRamo (funded by BMVI, FKZ 50EW1601).'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') '2018/04/13'
  
  end subroutine print_help

end program main
