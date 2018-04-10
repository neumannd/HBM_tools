program main

  implicit none

  !!!! simple IO support VARS
  integer(4)     :: unit    ! input and output file units
  integer(4)     :: ios     ! input and output file iostat
  character(255) :: fnamIn    ! = 'restart_01'
  character(255) :: fnamOt    ! = 'restart_01'
  CHARACTER(516) :: copyString 

  logical        :: wait4copy

  !!!! VAR FOR COPYING DATA
  character(11)    :: starta     ! start date of the restart file
  character(11)    :: starta_new ! start date of the new restart file

  REAL(8)          :: test_next_val(5) ! test whether data were actually 
                                    ! overwritten and not prepended
  CHARACTER(8)     :: test_next_char ! test whether data were actually 
                                    ! overwritten and not prepended

  !!!! COMMAND LINE ARGUMENTS - VARIABLES
  integer(4) :: argc

  starta_new='V201201.010'
  wait4copy=.TRUE.


  !!!! GET COMMAND LINE ARGUMENTS
  argc = command_argument_count()
  if (argc /= 2) then
    write(*,'(a62,I4,a9)') 'STOP: Bad number of arguments (INFILE and OUTFILE needed) but ', argc, ' provided'
    WRITE(*,'(A14)') '  NOTHING DONE'
    stop
  end if
  call get_command_argument(1, fnamIn)
  call get_command_argument(2, fnamOt)

  if (trim(fnamIn) .eq. trim(fnamOt)) then
    write(*,'(a68)') 'STOP: Input and output file need to differ. If input file should be '
    write(*,'(a58)') 'overwritten please set the second argument to "overwrite".'
    WRITE(*,'(A14)') '  NOTHING DONE'
    stop
  end if

  if (trim(fnamOt) .eq. 'overwrite') then
    fnamOt=fnamIn
    WRITE(*,'(A27)') '~~ overwriting input file: '
    write(*,'(A2,A)') '  ', fnamIn
  else
    WRITE(*,'(A15)') '~~ input file: '
    write(*,'(A2,A)') '  ', fnamIn
    WRITE(*,'(A16)') '~~ output file: '
    write(*,'(A2,A)') '  ', fnamOt
    WRITE(*,'(A17)') '~~ copy command: '
    write(copyString,'(A3,A,A1,A)') 'cp ', fnamIn, ' ', fnamOt
    write(*,*) copyString
    call execute_command_line(copyString, wait=wait4copy)
  end if


  !!!! LOOKING
  !! OPEN FILE
  unit = 1
  ! open existing file
  open(unit,file=trim(fnamOt),form='unformatted',iostat=ios,         &
       action='read',asynchronous='yes',access='stream',status='old')
  if(ios /= 0) write(*,'(A20,I5)') 'error opening file: ', ios
  
  WRITE(*,'(A39)') '~~ testing consistency (initial data): '
  
  !! read old date
  read(unit,iostat=ios) starta
  if(ios /= 0) write(*,'(A20,I5)') 'error reading data: ', ios
  write(*,'(A18,A11)') '  Old Start Date: ', starta
  
  !! read test data
  read(unit,iostat=ios) test_next_char
  if(ios /= 0) write(*,'(A20,I5)') 'error reading data: ', ios
  write(*,'(A16,A4)') '  Old next val: ', test_next_char
  
  !! read test data
  read(unit,iostat=ios) test_next_val
  if(ios /= 0) write(*,'(A20,I5)') 'error reading data: ', ios
  write(*,'(A16,F16.10,F16.10,F16.10,F16.10,F16.10)') '  Old next val: ', test_next_val
  
  !! CLOSE FILE
  close(unit)
  WRITE(*,'(A1)') ' '
  
  
  
  !!!! LOOKING
  !! OPEN FILE
  unit = 1
  ! open existing file
  open(unit,file=trim(fnamOt),form='unformatted',iostat=ios,         &
       action='readwrite',asynchronous='yes',access='stream',status='old', &
       position='rewind')
  if(ios /= 0) write(*,'(A20,I5)') 'error opening file: ', ios
  
  write(*,'(A25,A11)') '~~ Write new Start Date: ', starta_new
  
  !! META DATA
  WRITE(unit,iostat=ios) starta_new
  if(ios /= 0) write(*,'(a19,i3)') 'error writing date ', ios
  
  !! CLOSE FILE
  close(unit)
  WRITE(*,'(A1)') ' '
  
  
  
  !!!! LOOKING
  !! OPEN FILE
  unit = 1
  ! open existing file
  open(unit,file=trim(fnamOt),form='unformatted',iostat=ios,         &
       action='read',asynchronous='yes',access='stream',status='old')
  if(ios /= 0) write(*,*) 'error opening file'
  
  WRITE(*,'(A37)') '~~ testing consistency (final data): '
  
  !! META DATA
  read(unit,iostat=ios) starta
  if(ios /= 0) write(*,'(A20,I5)') 'error reading data: ', ios
  write(*,'(A23,A11)') '  Modified Start Date: ', starta
  
  !! read test data
  read(unit,iostat=ios) test_next_char
  if(ios /= 0) write(*,'(A20,I5)') 'error reading data: ', ios
  write(*,'(A16,A4)') '  Old next val: ', test_next_char
  
  !! read test data
  read(unit,iostat=ios) test_next_val
  if(ios /= 0) write(*,'(A20,I5)') 'error reading data: ', ios
  write(*,'(A16,F16.10,F16.10,F16.10,F16.10,F16.10)') '  Old next val: ', test_next_val
  
  !! CLOSE FILE
  close(unit)
  WRITE(*,'(A1)') ' '
  

end program main
