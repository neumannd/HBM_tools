module text_var_data

  use check_stat_nonetcdf
  
  implicit none
  
  
contains

  subroutine put_text_var_data(fname, vals, n_vals, v_name, v_unit)
    
    character(len=*),      intent(in) :: fname
    real(8), dimension(:), intent(in) :: vals ! size: n_vals
    integer,               intent(in) :: n_vals
    character(len=*),      intent(in) :: v_name, v_unit
    
    ! iterators
    integer :: ival
    
    ! ids
    integer :: funit = 1
    
    ! io status
    integer :: ios
    
    
    open(unit = funit, file = fname, iostat = ios, status = 'replace', &
         action = 'write')
    call check_ios_stat(ios, 'opening file '//fname)
    
    
    write(funit, *, iostat=ios) trim(v_name)//' in '//trim(v_unit)
    call check_ios_stat(ios, 'writing header into '//fname)
    
    
    do ival = 1, n_vals
      
      write(funit, '(F18.6)', iostat=ios) vals(ival)
      call check_ios_stat(ios, 'writing values into '//fname)
      
    end do
    
    close(funit, iostat=ios)
    call check_ios_stat(ios, 'closing file '//fname)
    
  end subroutine put_text_var_data

end module text_var_data