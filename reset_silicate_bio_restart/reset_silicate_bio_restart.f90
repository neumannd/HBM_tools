!> \file reset_silicate_bio_restart.f90
!! \brief replace the tracer number 4, which is silicate, of the first input 
!             bio_restart file with silicate of the second input file.
! 
! Resets the tracer `silicate` in binary bio_restart files of HBM-ERGOM to the 
! `silicate` values of another binary bio_restart file. Does not work with the 
! 2018s version of HBM-ERGOM anymore because the alignment of data at the 
! boundaries was changed. The source bio_restart file, from which silicate data
! are copied, needs to have with 13 tracers. The source bio_restart file, from 
! which all other tracers' data are copied, needs to have with 31 tracers 
! (first 13 are equal).  Silicate needs to be bio tracer number 4.
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
  integer(4), parameter :: ntbio = 31      ! number of tagged tracers 
  integer(4), parameter :: ntbio_def = 13  ! number of tracers in default file
  integer(4), parameter :: ntben = 4       ! number of tagged benthos tracers
  integer(4), parameter :: ntben_def = 2   ! number of benthos tracers in default file
     ! idx_sil = 4; idx_dets = 11
     ! bidx_sili = 2


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
  integer(4)     :: argc                    ! number of command line arguments
  integer(4)     :: unitIn, unitDef, unitOt ! units: input, defaults, and output file
  integer(4)     :: iosIn, iosDef, iosOt    ! iostat: input, defaults, and output file
  character(255) :: fnamIn                  ! input/read file       = 'bio_restart_01'
  character(255) :: fnamDef                 ! default silicate file = 'bio_restart_01' other
  character(255) :: fnamOt                  ! output file           = 'bio_restart_resetSilicate_01'
  character(514) :: copyString              ! String to copy files
  logical        :: wait4copy               ! variable indicating whether programm should wait until file is fully copied



  !!!! VAR FOR COPYING DATA
  character(11)    :: starta, start_def     ! start date of the restart file

  ! cmod_arrays, l.299: allocate(ptc(ia)%p(0:n3d,1:ntracers-2), stat=ierr )
  ! cmod_arrays, l.182: n3d    = iw3(ia)
  REAL(8) :: ptc_1(0:n3d_1,1:ntbio),         ptc_2(0:n3d_2,1:ntbio)          ! regular input file
  REAL(8) :: ptc_1_def(0:n3d_1,1:ntbio_def), ptc_2_def(0:n3d_2,1:ntbio_def)  ! silicate default file

  ! cgt_ergom_coupler, l. 25: type(cmr2), allocatable, private, save :: benthos(:)
  ! cgt_ergom_coupler, l.192: allocate( benthos(ia)%p(0:n2d,1:ntben))
  !                           n2d = iw2
  real(8) :: benthos_1(0:n2d_1,1:ntben),     benthos_2(0:n2d_2,1:ntben)      ! regular input file
  real(8) :: benthos_1_def(0:n2d_1,1:ntben_def),  &
             benthos_2_def(0:n2d_2,1:ntben_def)  ! silicate default file

  ! cmod_arrays, l.242: bndz(ia)%p(1:ntracers,1:kn,0:nzn)
  REAL(8) :: bndz3D(1:(ntbio+2),1:kmx_main,0:nz_main)
  REAL(8) :: bndz3D_def(1:(ntbio_def+2),1:kmx_main,0:nz_main)
  
  ! check whether help is requested
  call test_help_string(helpme)


  !!! ~~~~~~~~~~~~~~~~~ END VARIABLE DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~



  !!! ~~~~~~~~~~~~~~~~~ INITIALIZE STUFF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ! initialize 
  wait4copy = .TRUE.
  
  if (helpme) then
    
    call print_help()
    
  else

    !!!! GET COMMAND LINE ARGUMENTS
    argc = command_argument_count()
    if (argc /= 3) then
      write(*,'(a56,I4,a9)') 'Bad number of arguments (INFILE, DEFAULT_FILE and OUTFILE needed) but ', argc, ' provided'
      stop
    end if
    call get_command_argument(1, fnamIn)
    call get_command_argument(2, fnamDef)
    call get_command_argument(3, fnamOt)

    ! check whether 'default file' is unequal other files
    if ( (trim(fnamIn) .eq. trim(fnamDef)) .or. (trim(fnamOt) .eq. trim(fnamDef)) ) then
      write(*,'(a68)') 'STOP: "Default and output" and "default and input" files need to differ.'
      WRITE(*,'(A14)') '  NOTHING DONE'
      stop
    end if

    ! check whether 'input file' and 'output file' are different
    if ( (trim(fnamIn) .eq. trim(fnamOt)) ) then
      write(*,'(a68)') 'STOP: Input and output file need to differ.'
      WRITE(*,'(A14)') '  NOTHING DONE'
      stop
    end if

    WRITE(*,'(A15)') '~~ input file: '
    write(*,'(A2,A)') '  ', fnamIn
    write(*,'(A17)') '~~ default file: '
    write(*,'(A2,A)') '  ', fnamDef
    WRITE(*,'(A16)') '~~ output file: '
    write(*,'(A2,A)') '  ', fnamOt
    WRITE(*,'(A17)') '~~ copy command: '
    write(copyString,'(A3,A,A1,A)') 'cp ', fnamIn, ' ', fnamOt
    write(*,*) copyString
    call execute_command_line(copyString, wait=wait4copy)
    write(*,*) '  copying finished'  

    !!! ~~~~~~~~~~~~~~~~~ END INITIALIZE STUFF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



    !!! ~~~~~~~~~~~~~~~~~ WORK ON FILES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!!! WORKING
    !! OPEN FILES
    unitIn = 1
    unitDef = 2
    unitOt = 3
    ! open existing file
    open(unitIn,file=trim(fnamIn),form='unformatted',iostat=iosIn,         &
         action='read',asynchronous='yes',access='stream',status='old')
    ! open existing file
    open(unitDef,file=trim(fnamDef),form='unformatted',iostat=iosdef,      &
         action='read',asynchronous='yes',access='stream',status='old')
    ! open new file
    open(unitOt,file=trim(fnamOt),form='unformatted',action='write',       &
         asynchronous='yes',access='stream', iostat=iosOt,status='old')

    if(iosIn /= 0) write(*,*) 'error opening input file'
    if(iosDef /= 0) write(*,*) 'error opening input file'
    if(iosOt /= 0) write(*,*) 'error opening output file'


    !! META DATA
    read(unitIn,iostat=iosIn) starta
    read(unitDef,iostat=iosDef) start_def
    write(*,'(A16,A11)') 'Start Date: ', starta
    WRITE(unitOt,iostat=iosOt) starta
    if(iosIn /= 0) write(*,'(a19,i3)') 'error reading date ', iosIn
    if(iosDef /= 0) write(*,'(a34,i3)') 'error reading date (default file) ', iosDef
    if(iosOt /= 0) write(*,'(a19,i3)') 'error writing date ', iosOt


    !! AREA 1 DATA
        ! read data
    read(unitIn,iostat=iosIn) ptc_1(:,1:ntbio), benthos_1(:,1:ntben)
    read(unitDef,iostat=iosDef) ptc_1_def(:,1:ntbio_def), benthos_1_def(:,1:ntben_def)
    if(iosIn /= 0) write(*,'(a50,i3)') 'error reading ptc for area 1 (regular input file) ', iosIn
    if(iosDef /= 0) write(*,'(a53,i3)') 'error reading ptc for area 1 (defaults silicate file) ', iosDef

        ! write data
    write(unitOt,IOSTAT=iosOt) ptc_1(:,1:3), ptc_1_def(:,4), ptc_1(:,5:10),      &
                               ptc_1_def(:,11), ptc_1(:,12:ntbio),               &
                               benthos_1(:,1), benthos_1_def(:,2),               &
                               benthos_1(:,3:ntben)
    if(iosOt /= 0) write(*,'(a29,i3)') 'error writing ptc for area 1 ', iosOt


    !! AREA 2 DATA
        ! read data
    read(unitIn,iostat=iosIn) ptc_2(:,1:ntbio), benthos_2(:,1:ntben)
    read(unitDef,iostat=iosDef) ptc_2_def(:,1:ntbio_def), benthos_2_def(:,1:ntben_def)
    if(iosIn /= 0) write(*,'(a50,i3)') 'error reading ptc for area 2 (regular input file) ', iosIn
    if(iosDef /= 0) write(*,'(a53,i3)') 'error reading ptc for area 2 (default silicate file) ', iosDef

        ! write data
    write(unitOt,IOSTAT=iosOt) ptc_2(:,1:3), ptc_2_def(:,4), ptc_2(:,5:10),      &
                               ptc_2_def(:,11), ptc_2(:,12:ntbio),               &
                               benthos_2(:,1), benthos_2_def(:,2),               &
                               benthos_2(:,3:ntben)
    if(iosOt /= 0) write(*,'(a29,i3)') 'error writing ptc for area 2 ', iosOt

  if (.false.) then

    !! BOUDNARIES (main area = area 1)
    ! sufficient to read and write until index of 4 (respective 6, because it 
    !  starts with 3; salinity and temperature are 1 and 2 (or 2 and 1))
        ! read data
    read(unitIn,iostat=iosIn) bndz3D(3,1:kmx_main,0:), bndz3D(4,1:kmx_main,0:),   &
                              bndz3D(5,1:kmx_main,0:), bndz3D(6,1:kmx_main,0:)
    read(unitDef,iostat=iosDef) bndz3D_def(3,1:kmx_main,0:),   &
                                bndz3D_def(4,1:kmx_main,0:),   &
                                bndz3D_def(5,1:kmx_main,0:),   &
                                bndz3D_def(6,1:kmx_main,0:)
    if(iosIn /= 0) write(*,'(a50,i3)') 'error reading bndz main area (regular input file) ', iosIn
    if(iosDef /= 0) write(*,'(a53,i3)') 'error reading bndz main area (default silicate file) ', iosDef

        ! write data
    ! we do not need to write all tracers but just until all relevant tracers are
    ! written
    WRITE(unitOt,iostat=iosOt) bndz3D(3,1:kmx_main,0:), bndz3D(4,1:kmx_main,0:), &
                           bndz3D(5,1:kmx_main,0:), bndz3D_def(6,1:kmx_main,0:), &
                           bndz3D(7,1:kmx_main,0:), bndz3D(8,1:kmx_main,0:),     &
                           bndz3D(9,1:kmx_main,0:), bndz3D(10,1:kmx_main,0:),    &
                           bndz3D(11,1:kmx_main,0:), bndz3D(12,1:kmx_main,0:),   &
                           bndz3D_def(13,1:kmx_main,0:)

    if(iosOt /= 0) write(*,'(a29,i3)') 'error writing bndz main area ', iosOt

  end if

    !! CLOSE FILES
    close(unitIn)
    close(unitDef)
    close(unitOt)

    !!! ~~~~~~~~~~~~~~~~~ END WORK ON FILES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
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
  
    write(*,'(A)') 'HELP FOR reset_silicate_bio_restart                                 2018/04/13'
    write(*,'(A)') ''
    write(*,'(A)') 'NAME'
    write(*,'(A)') ''
    write(*,'(A)') '      reset silicate bio restart - replace the tracer number 4, which is '
    write(*,'(A)') '                                   silicate, of the first input bio_restart'
    write(*,'(A)') '                                   file with silicate of the second input'
    write(*,'(A)') '                                   file.'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'SYNOPSIS'
    write(*,'(A)') '      reset_silicate_bio_restart.x bio_restart_in bio_restart_silicate \'
    write(*,'(A)') '                                   bio_restart_out'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'DESCRIPTION'
    write(*,'(A)') '      Resets the tracer `silicate` in binary bio_restart files of HBM-ERGOM to '
    write(*,'(A)') '      the `silicate` values of another binary bio_restart file. Does not work '
    write(*,'(A)') '      with the 2018s version of HBM-ERGOM anymore because the alignment of data'
    write(*,'(A)') '      at the boundaries was changed. The source bio_restart file, from which '
    write(*,'(A)') '      silicate data are copied, needs to have with 13 tracers. The source '
    write(*,'(A)') '      bio_restart file, from which all other tracers data are copied, needs to '
    write(*,'(A)') '      have with 31 tracers (first 13 are equal).  Silicate needs to be bio '
    write(*,'(A)') '      tracer number 4.'
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
    write(*,'(A)') '         '
    write(*,'(A)') '         '
    write(*,'(A)') '      bio_restart_silicate'
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
