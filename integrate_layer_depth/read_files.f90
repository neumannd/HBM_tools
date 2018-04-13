module read_files

  use check_stat_nonetcdf

  implicit none
  
  private
  
  interface get_filenames
    
    module procedure get_filenames_1
    module procedure get_filenames_2
    module procedure get_filenames_3
    module procedure get_filenames_4
    module procedure get_filenames_5
    module procedure get_filenames_6
    module procedure get_filenames_7
    
  end interface get_filenames
  
  
  public :: read_variables, read_layers, get_filenames, test_help_string
  private :: get_filenames_1, get_filenames_2, get_filenames_3, &
             get_filenames_4, get_filenames_5, get_filenames_6
  
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
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_filenames_1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine get_filenames_1(fname1)
      
      character(len=255), intent(out) :: fname1
      
      ! number command line arguments
      integer(4) :: argc
      
      ! read number of command line arguments
      argc = command_argument_count()
      if (argc /= 1) then
        write(*,'(a45,I4,a9)') 'STOP: Bad number of arguments (1 needed) but ',&
                                argc, ' provided'
        WRITE(*,'(A73)') '  Nothing done. Please use "-h" or "--help" to '//&
                         'display usage information.'
        stop
      end if
      
      call get_command_argument(1, fname1)
      
    end subroutine get_filenames_1
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_filenames_1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_filenames_2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine get_filenames_2(fname1, fname2)
      
      character(len=255), intent(out) :: fname1, fname2
      
      ! number command line arguments
      integer(4) :: argc
      
      ! read number of command line arguments
      argc = command_argument_count()
      if (argc /= 2) then
        write(*,'(a45,I4,a9)') 'STOP: Bad number of arguments (2 needed) but ', argc, ' provided'
        WRITE(*,'(A73)') '  Nothing done. Please use "-h" or "--help" to '//&
                         'display usage information.'
        stop
      end if
      
      call get_command_argument(1, fname1)
      call get_command_argument(2, fname2)
      
    end subroutine get_filenames_2
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_filenames_2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_filenames_3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine get_filenames_3(fname1, fname2, fname3)
      
      character(len=255), intent(out) :: fname1, fname2, fname3
      
      ! number command line arguments
      integer(4) :: argc
      
      ! read number of command line arguments
      argc = command_argument_count()
      if (argc /= 3) then
        write(*,'(a45,I4,a9)') 'STOP: Bad number of arguments (3 needed) but ', argc, ' provided'
        WRITE(*,'(A73)') '  Nothing done. Please use "-h" or "--help" to '//&
                         'display usage information.'
        stop
      end if
      
      call get_command_argument(1, fname1)
      call get_command_argument(2, fname2)
      call get_command_argument(3, fname3)
      
    end subroutine get_filenames_3
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_filenames_3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_filenames_4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine get_filenames_4(fname1, fname2, fname3, fname4)
      
      character(len=255), intent(out) :: fname1, fname2, fname3, fname4
      
      ! number command line arguments
      integer(4) :: argc
      
      ! read number of command line arguments
      argc = command_argument_count()
      if (argc /= 4) then
        write(*,'(a45,I4,a9)') 'STOP: Bad number of arguments (4 needed) but ', argc, ' provided'
        WRITE(*,'(A73)') '  Nothing done. Please use "-h" or "--help" to '//&
                         'display usage information.'
        stop
      end if
      
      call get_command_argument(1, fname1)
      call get_command_argument(2, fname2)
      call get_command_argument(3, fname3)
      call get_command_argument(4, fname4)
      
    end subroutine get_filenames_4
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_filenames_4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_filenames_5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine get_filenames_5(fname1, fname2, fname3, fname4, fname5)
      
      character(len=255), intent(out) :: fname1, fname2, fname3, fname4, &
                                         fname5
      
      ! number command line arguments
      integer(4) :: argc
      
      ! read number of command line arguments
      argc = command_argument_count()
      if (argc /= 5) then
        write(*,'(a45,I4,a9)') 'STOP: Bad number of arguments (5 needed) but ', argc, ' provided'
        WRITE(*,'(A73)') '  Nothing done. Please use "-h" or "--help" to '//&
                         'display usage information.'
        stop
      end if
      
      call get_command_argument(1, fname1)
      call get_command_argument(2, fname2)
      call get_command_argument(3, fname3)
      call get_command_argument(4, fname4)
      call get_command_argument(5, fname5)
      
    end subroutine get_filenames_5
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_filenames_5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_filenames_6 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine get_filenames_6(fname1, fname2, fname3, fname4, fname5, fname6)
      
      character(len=255), intent(out) :: fname1, fname2, fname3, fname4, &
                                         fname5, fname6
      
      ! number command line arguments
      integer(4) :: argc
      
      ! read number of command line arguments
      argc = command_argument_count()
      if (argc /= 6) then
        write(*,'(a45,I4,a9)') 'STOP: Bad number of arguments (6 needed) but ', argc, ' provided'
        WRITE(*,'(A73)') '  Nothing done. Please use "-h" or "--help" to '//&
                         'display usage information.'
        stop
      end if
      
      call get_command_argument(1, fname1)
      call get_command_argument(2, fname2)
      call get_command_argument(3, fname3)
      call get_command_argument(4, fname4)
      call get_command_argument(5, fname5)
      call get_command_argument(6, fname6)
      
    end subroutine get_filenames_6
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_filenames_6 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_filenames_7 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine get_filenames_7(fname1, fname2, fname3, fname4, fname5, fname6, &
                               fname7)
      
      character(len=255), intent(out) :: fname1, fname2, fname3, fname4, &
                                         fname5, fname6, fname7
      
      ! number command line arguments
      integer(4) :: argc
      
      ! read number of command line arguments
      argc = command_argument_count()
      if (argc /= 7) then
        write(*,'(a45,I4,a9)') 'STOP: Bad number of arguments (7 needed) but ', argc, ' provided'
        WRITE(*,'(A73)') '  Nothing done. Please use "-h" or "--help" to '//&
                         'display usage information.'
        stop
      end if
      
      call get_command_argument(1, fname1)
      call get_command_argument(2, fname2)
      call get_command_argument(3, fname3)
      call get_command_argument(4, fname4)
      call get_command_argument(5, fname5)
      call get_command_argument(6, fname6)
      call get_command_argument(7, fname7)
      
    end subroutine get_filenames_7
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_filenames_7 ~~~~~~~~~~~~~~~~~~~~~~~~~~~

end module read_files