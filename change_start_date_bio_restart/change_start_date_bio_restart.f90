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

end program main
