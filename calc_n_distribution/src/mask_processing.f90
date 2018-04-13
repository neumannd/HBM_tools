module mask_processing

  use check_stat_netcdf
  use netcdf

  implicit none
  
  integer(1), parameter :: mask_true = 1_1

contains

  subroutine get_mask_window(fname, vname, vals, start, count)
  
    character(len=*),                        intent(in) :: fname, vname
    integer(1), dimension(:,:), allocatable, intent(out) :: vals
    integer,    dimension(2),                intent(out) :: start, count
    
    
    ! netCDF status
    integer :: nf_stat
    
    ! netCDF id
    integer :: ncid
    integer             :: id_d_lon, id_d_lat    ! dimension id_s
    integer             :: id_v_mask             ! further variable id_s
  
    ! netCDF dim sizes
    integer             :: n_lon, n_lat
    
    ! variables for the mask
    integer(1), dimension(:,:), allocatable :: val_v_mask
    
    ! iterators
    integer :: ilon, ilat
    
    ! help variables
    integer, dimension(2) :: last
    
    
    ! open file
    nf_stat = NF90_OPEN(fname, NF90_NOWRITE, ncid)
    call check_nf90_stat(nf_stat, 'opening mask file '//fname)
    
    ! get dimensions
    nf_stat = NF90_INQ_DIMID(ncid, 'lon', id_d_lon)
    call check_nf90_stat(nf_stat, 'inq dim id lon')
    nf_stat = NF90_INQ_DIMID(ncid, 'lat', id_d_lat)
    call check_nf90_stat(nf_stat, 'inq dim id lat')
    
    nf_stat = NF90_INQUIRE_DIMENSION(ncid, id_d_lon, len = n_lon)
    call check_nf90_stat(nf_stat, 'get dim length lon')
    nf_stat = NF90_INQUIRE_DIMENSION(ncid, id_d_lat, len = n_lat)
    call check_nf90_stat(nf_stat, 'get dim length lat')
    
    ! allocate variable
    allocate(val_v_mask(n_lon, n_lat))
    
    ! get variable
    nf_stat = NF90_INQ_VARID(ncid, vname, id_v_mask)
    call check_nf90_stat(nf_stat, 'inq var id '//vname)
    nf_stat = NF90_GET_VAR(ncid, id_v_mask, val_v_mask)
    call check_nf90_stat(nf_stat, 'get var '//vname)
    
    ! close file
    nf_stat = NF90_CLOSE(ncid)
    call check_nf90_stat(nf_stat, 'closing mask file')
    
    ! get start values
    start = (/n_lon+1, n_lat+1/)
    DO ilat = 1, n_lat
      do ilon = 1, n_lon
        if( (val_v_mask(ilon,ilat) .eq. mask_true)) then
          if (ilon .lt. start(1)) start(1) = ilon
          if (ilat .lt. start(2)) start(2) = ilat
        end if
      end do
    end do
    
    ! if no mask was found => ERROR
    if ( (start(1) .eq. n_lon+1) .and. (start(2) .eq. n_lat+1) ) then
      write(*,*) 'NO MASK FOUND'
      STOP
    end if
    
    ! get count values (same as get start; but other way around)
    last = (/0, 0/)
    DO ilat = n_lat, 1, -1
      do ilon = n_lon, 1, -1
        if( (val_v_mask(ilon,ilat) .eq. mask_true)) then
          if (ilon .gt. last(1)) last(1) = ilon
          if (ilat .gt. last(2)) last(2) = ilat
        end if
      end do
    end do
    
    ! if no mask was found => ERROR
    if ( (last(1) .eq. 0) .and. (last(2) .eq. 0) ) then
      write(*,*) 'NO MASK FOUND'
      STOP
    else
      count(1) = last(1) - start(1) + 1
      count(2) = last(2) - start(2) + 1
    end if
    
    allocate(vals(count(1), count(2)))
    
    vals(1:count(1), 1:count(2)) = val_v_mask(start(1):last(1), start(2):last(2))
    
    deallocate(val_v_mask)
    
    
  end subroutine get_mask_window
  
  
  
  subroutine apply_mask(vals_mask, vals, fillval, count)
    
    integer(1), dimension(:,:),     intent(in)    :: vals_mask
    real(8),    dimension(:,:,:,:), intent(inout) :: vals
    real(8),                        intent(in)    :: fillval
    integer,    dimension(4),       intent(in)    :: count
    
    ! iterators
    integer :: i1, i2, i3, i4
    
    ! mask - true value
    integer(1) :: mask_true = 1_1
    
    Do i2 = 1, count(2)
      do i1 = 1, count(1)
        if (vals_mask(i1,i2) .ne. mask_true) then
          vals(i1, i2, 1:count(3), 1:count(4)) = fillval
        end if
      end do
    end do
    
    
  end subroutine apply_mask


end module mask_processing