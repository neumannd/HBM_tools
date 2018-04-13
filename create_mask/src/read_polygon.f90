module read_polygon

  use check_stat_nonetcdf

  implicit none

contains

  subroutine read_coords(fname, xcoords, ycoords, ncoords)
    
    character(len=*), intent(in)                    :: fname
    real(8), dimension(:), allocatable, intent(out) :: xcoords, ycoords
    integer, intent(out) :: ncoords
    
    integer            :: funit = 1, ios, i
    character(len=255) :: tmp_str
    character(len=1)   :: sep
    
    
    ncoords = 0
    
    open(unit = funit, file = fname, iostat = ios, form = 'formatted',  &
         status = 'old', access = 'sequential', action = 'read')
    call check_ios_stat(ios, 'opening file '//fname)
    
    ! read header
    read(funit, '(A)', iostat=ios) tmp_str
    call check_ios_stat(ios, 'reading first line of '//fname)
    
    ! count number of lines
    read(funit, '(A)', iostat=ios) tmp_str
    do while (ios .eq. 0)
      ncoords = ncoords + 1
      read(funit, '(A)', iostat=ios) tmp_str
    end do
    
    ! rewind file to second line
    rewind(unit = funit, IOstat=ios)
    call check_ios_stat(ios, 'rewind after line count; file '//fname)
    read(funit, '(A)', iostat=ios) tmp_str
    call check_ios_stat(ios, 'reading first line of '//fname)
    
    ! allocate data arrays
    allocate(xcoords(ncoords), ycoords(ncoords))
    
    ! read data
    do i = 1, ncoords
      read(funit, '(F6.3,A1,F6.3)', iostat=ios) xcoords(i), sep, ycoords(i)
    end do
  
    close(funit)
    
  end subroutine read_coords

end module read_polygon