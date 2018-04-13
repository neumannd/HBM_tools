module time_tools

  implicit none

contains

  subroutine generate_time_stamp(str_time_stamp)
  
    character (len=20), intent(out) :: str_time_stamp
  
    ! variables for timestamp setting
    CHARACTER (len=8)        :: str_date
    CHARACTER (len=10)       :: str_time
    CHARACTER (len=5)        :: str_zone
    integer(4), dimension(8) :: int_date_and_time
    character (len=47)       :: fmt_time_stamp 
  
  
    ! GET TIME AND DATE FOR HISTORY ATTRIBUTE and construct time stamp string
    call date_and_time(str_date, str_time, str_zone, int_date_and_time)
    fmt_time_stamp = '(I4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1)'
    write(str_time_stamp, fmt_time_stamp) int_date_and_time(1), '-', &
                                          int_date_and_time(2), '-', &
                                          int_date_and_time(3), ' ', &
                                          int_date_and_time(5), ':', &
                                          int_date_and_time(6), ':', &
                                          int_date_and_time(7), ':'
  
  end subroutine generate_time_stamp

end module time_tools