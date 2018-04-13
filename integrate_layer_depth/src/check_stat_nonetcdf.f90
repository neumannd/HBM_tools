module check_stat_nonetcdf

  implicit none

contains

  subroutine check_ios_stat(ios, msg)
  
    integer,                    intent(in) :: ios
    character(len=*), optional, intent(in) :: msg
  
    if (ios /= 0) then
      write(*,'(A10,I6)') 'IO Error: ', ios
      if( present(msg)) WRITE(*,*) 'Message: '//msg
      stop
    end if
  
  end subroutine check_ios_stat

end module check_stat_nonetcdf