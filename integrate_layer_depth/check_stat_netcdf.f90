module check_stat_netcdf

  use netcdf
  
  implicit none

contains

  ! ~~~~~ CHECK nf_stat FOR ERRORS (!= 0) ~~~~~
  subroutine check_nf90_stat(nf_stat, err_msg)
  
    implicit none
    
    integer,           intent(in)           :: nf_stat
    character (len=*), intent(in), optional :: err_msg
    
    if (.not.(nf_stat == NF90_NOERR)) then
      write(*,'(A25,I5)') 'error: netCDF error code ', nf_stat
      write(*,'(A15,A)') 'error message: ', err_msg
      stop
    end if
    
  end subroutine check_nf90_stat
  ! ~~~~~ END CHECK nf_stat FOR ERRORS (!= 0) ~~~~~

end module check_stat_netcdf