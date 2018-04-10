program calc_grid_cell_size

  use netcdf
  use omp_lib

  implicit none

  ! Placed as atmos dep in one grid cell of the fine grid. Grid cell data:
  !   phi:   55 deg N
  !   d_phi: 30'' N-S  (1/120 deg)
  !   d_lam: 50'' E-W  (5/360 deg)
  !
  ! Earth Radius:
  !   equator: 6378.1 km (r_max)
  !   pole:    6356.8 km (r_min)
  ! 
  !   dr = 21.3 km
  !
  ! Assume :
  !   r(phi) = r_min + dr * cos(phi)
  !   r(55 deg) = 6365 km
  !
  ! grid cell size in m (dx and dy)
  !   dy = r * d_phi = 6365 km * 1 / 120 * pi/180 = 0.926 km
  !   dx = r * cos(phi) * d_lam = 6365 km * cos(55/180*pi) * 5/360 * pi/180 = 0.885 km
  !
  !   dA = dx * dy = 0.926 km * 0.885 km = 0.8192739 km2 = 819273.9 m2
  
  integer(4)                   :: argc                                       ! commandline argument count
  character (len=255)          :: file_in, file_ot
  
  character (len=1), parameter :: varname_cell_thickness = 'h'
  character (len=9), parameter :: varname_cell_area = 'cell_area'
  character (len=11), parameter :: varname_cell_volume = 'cell_volume'
  character (len=4), parameter :: varname_mask = 'mask'
  
  integer             :: n_lon, n_lat, n_time, n_depth              ! dimension size

  integer             :: id_d_lon, id_d_lat, id_d_time, id_d_depth  ! dimension id_s
  integer             :: id_v_lon, id_v_lat, id_v_time, id_v_depth,    &
                         id_v_h, id_v_cell_area, id_v_mask,            &
                         id_v_cell_volume, id_v_cell_thickness      ! variable id_s
  
  REAL(8), dimension(:), allocatable       :: val_v_lon, val_v_lat,    &
                                              val_v_depth, val_v_time
  REAL(8),    dimension(:,:,:,:), allocatable :: val_v_h
  REAL(8),    dimension(:,:),     allocatable :: val_v_cell_area
  REAL(8),    dimension(:,:,:,:), allocatable :: val_v_cell_volume
  integer(1), dimension(:,:,:,:), ALLOCATABLE :: val_v_mask
  real(8)                                  :: fillval_h
  logical                                  :: fillval_h_bad
  
  real(8),    parameter :: fillval_cell_area = -999.0_8
  real(8),    parameter :: fillval_cell_volume = -999.0_8
  real(8),    parameter :: fillval_cell_thickness = -999.0_8
  integer(1), parameter :: fillval_mask = -1
  
  integer             :: nf_stat         ! for error status of netCDF functions
  integer             :: ncid_in, ncid_ot  ! ncid for input and output
  
  CHARACTER (len=8)        :: str_date
  CHARACTER (len=10)       :: str_time
  CHARACTER (len=5)        :: str_zone
  integer(4), dimension(8) :: int_date_and_time
  character (len=20)       :: str_time_stamp
  character (len=47)       :: fmt_time_stamp 
  
  
  ! GET file_in and file_ot AS INPUT ARGUMENTS
  argc = command_argument_count()
  if (argc /= 2) then
    write(*,'(a62,I4,a9)') 'STOP: Bad number of arguments (INFILE and OUTFILE needed) but ', argc, ' provided'
    WRITE(*,'(A14)') '  NOTHING DONE'
    stop
  end if
  call get_command_argument(1, file_in)
  call get_command_argument(2, file_ot)

  if (trim(file_in) .eq. trim(file_ot)) then
    write(*,*) 'STOP: Input and output file need to differ. NOTHING DONE'
    stop
  end if
  
  
  ! GET TIME AND DATE FOR HISTORY ATTRIBUTE and construct time stamp string
  call date_and_time(str_date, str_time, str_zone, int_date_and_time)
  fmt_time_stamp = '(I4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1)'
  write(str_time_stamp, fmt_time_stamp) int_date_and_time(1), '-', &
                                        int_date_and_time(2), '-', &
                                        int_date_and_time(3), ' ', &
                                        int_date_and_time(5), ':', &
                                        int_date_and_time(6), ':', &
                                        int_date_and_time(7), ':'
  
  
  !! ~~~~~ READ OLD DATA ~~~~~
  ! OPEN FILE
  nf_stat = NF90_OPEN(file_in, NF90_NOWRITE, ncid_in)
  call nf90_check_error(nf_stat)
  
  
  ! GET DIM IDs
  nf_stat = NF90_INQ_DIMID(ncid_in, 'lon', id_d_lon)
  call nf90_check_error(nf_stat, 'error inq dimid lon')
  nf_stat = NF90_INQ_DIMID(ncid_in, 'lat', id_d_lat)
  call nf90_check_error(nf_stat, 'error inq dimid lat')
  nf_stat = NF90_INQ_DIMID(ncid_in, 'depth', id_d_depth)
  call nf90_check_error(nf_stat, 'error inq dimid depth')
  nf_stat = NF90_INQ_DIMID(ncid_in, 'time', id_d_time)
  call nf90_check_error(nf_stat, 'error inq dimid time')
  
  
  ! GET VAR IDs
  nf_stat = NF90_INQ_VARID(ncid_in, varname_cell_thickness, id_v_h)
  call nf90_check_error(nf_stat, 'error inq varid '//varname_cell_thickness)
  nf_stat = NF90_INQ_VARID(ncid_in, 'lon', id_v_lon)
  call nf90_check_error(nf_stat, 'error inq varid lon')
  nf_stat = NF90_INQ_VARID(ncid_in, 'lat', id_v_lat)
  call nf90_check_error(nf_stat, 'error inq varid lat')
  nf_stat = NF90_INQ_VARID(ncid_in, 'depth', id_v_depth)
  call nf90_check_error(nf_stat, 'error inq varid depth')
  nf_stat = NF90_INQ_VARID(ncid_in, 'time', id_v_time)
  call nf90_check_error(nf_stat, 'error inq varid time')
  
  
  ! READ DIM SIZEs
  nf_stat = nf90_inquire_dimension(ncid_in, id_d_lon, len = n_lon)
  call nf90_check_error(nf_stat, 'error get dim lon')
  nf_stat = nf90_inquire_dimension(ncid_in, id_d_lat, len = n_lat)
  call nf90_check_error(nf_stat, 'error get dim lat')
  nf_stat = nf90_inquire_dimension(ncid_in, id_d_depth, len = n_depth)
  call nf90_check_error(nf_stat, 'error get dim depth')
  nf_stat = nf90_inquire_dimension(ncid_in, id_d_time, len = n_time)
  call nf90_check_error(nf_stat, 'error get dim time')
  
  
  ! ALLOCATE ARRAY FOR READ-IN
  allocate(val_v_lon(n_lon), val_v_lat(n_lat), val_v_time(n_time), &
           val_v_depth(n_depth), val_v_h(n_lon, n_lat, n_depth, n_time))
  
  
  ! READ VAR VALUES
  nf_stat = NF90_GET_VAR(ncid_in, id_v_lon, val_v_lon)
  call nf90_check_error(nf_stat, 'error get var lon')
  
  nf_stat = NF90_GET_VAR(ncid_in, id_v_lat, val_v_lat)
  call nf90_check_error(nf_stat, 'error get var lat')
  
  nf_stat = NF90_GET_VAR(ncid_in, id_v_depth, val_v_depth)
  call nf90_check_error(nf_stat, 'error get var depth')
  
  nf_stat = NF90_GET_VAR(ncid_in, id_v_time, val_v_time)
  call nf90_check_error(nf_stat, 'error get var time')
  
  nf_stat = NF90_GET_VAR(ncid_in, id_v_h, val_v_h)
  call nf90_check_error(nf_stat, 'error get var '//varname_cell_thickness)
  
  
  ! READ _FillValue OR missing_value OF VARIABLE 'h'
  fillval_h_bad = .FALSE.
  nf_stat = nf90_get_att(ncid_in, id_v_h, '_FillValue', fillval_h)
  
  if ( nf_stat .eq. -43 ) then
    
    write(*,*) 'Warning: attribute _FillValue not found for variable '//&
                varname_cell_thickness//'. Trying missing_value.'
    nf_stat = nf90_get_att(ncid_in, id_v_h, 'missing_value', fillval_h)
    if ( nf_stat .eq. -43 ) then
      write(*,*) 'Warning: attribute missing_value not found for variable '//&
                 varname_cell_thickness//'. Using no _FillValue'
      fillval_h_bad = .TRUE.
    else
      call nf90_check_error(nf_stat, 'error get att missing_value of var '//varname_cell_thickness)
    end if
    
  else
    
    call nf90_check_error(nf_stat, 'error get att _FillValue of var '//varname_cell_thickness)
    
  end if
  
  
  
  !! ~~~~~ WRITE NEW FILE ~~~~~
  ! tell the user that MASK will not be created
  if (fillval_h_bad) then
    WRITE(*,*) 'No _FillValue or missing_value found in the choosen variable and file. Not '
    write(*,*) 'creating the variable MASK in the output file!'
    write(*,*) '  input file: '//trim(file_in)
    write(*,*) '  input variable: '//trim(varname_cell_thickness)
    write(*,*) '  output file: '//trim(file_ot)
  end if
  
  ! create new file
  nf_stat = NF90_CREATE(file_ot, NF90_HDF5, ncid_ot)
  call nf90_check_error(nf_stat)
  
  
  ! create dimensions
  nf_stat = nf90_def_dim(ncid_ot, 'lon', n_lon, id_d_lon)
  call nf90_check_error(nf_stat, 'error def dim lon')
  
  nf_stat = nf90_def_dim(ncid_ot, 'lat', n_lat, id_d_lat)
  call nf90_check_error(nf_stat, 'error def dim lat')
  
  nf_stat = nf90_def_dim(ncid_ot, 'depth', n_depth, id_d_depth)
  call nf90_check_error(nf_stat, 'error def dim depth')
  
  nf_stat = nf90_def_dim(ncid_ot, 'time', n_time, id_d_time)
  call nf90_check_error(nf_stat, 'error def dim time')
  
  
  ! create dimensional variables
  nf_stat = nf90_def_var(ncid_ot, 'lon', NF90_DOUBLE, (/id_d_lon/), id_v_lon)
  call nf90_check_error(nf_stat, 'error def var lon')
  
  nf_stat = nf90_def_var(ncid_ot, 'lat', NF90_DOUBLE, (/id_d_lat/), id_v_lat)
  call nf90_check_error(nf_stat, 'error def var lat')
  
  nf_stat = nf90_def_var(ncid_ot, 'depth', NF90_DOUBLE, (/id_d_depth/), id_v_depth)
  call nf90_check_error(nf_stat, 'error def var depth')
  
  nf_stat = nf90_def_var(ncid_ot, 'time', NF90_DOUBLE, (/id_d_time/), id_v_time)
  call nf90_check_error(nf_stat, 'error def var time')
  
  
  ! create new value-variables
  nf_stat = nf90_def_var(ncid_ot, varname_cell_area, NF90_DOUBLE, &
                         (/id_d_lon,id_d_lat/), &
                         id_v_cell_area)
  call nf90_check_error(nf_stat, 'error def var cell_area')
  
  nf_stat = nf90_def_var(ncid_ot, varname_cell_volume, NF90_DOUBLE, &
                         (/id_d_lon,id_d_lat, id_d_depth, id_d_time/), &
                         id_v_cell_volume)
  call nf90_check_error(nf_stat, 'error def var cell_volume')
    nf_stat = nf90_def_var_deflate(ncid_ot, id_v_cell_volume, 0, 1, 1)
    call nf90_check_error(nf_stat, 'error def var cell_volume')
  
  nf_stat = nf90_def_var(ncid_ot, varname_cell_thickness, NF90_DOUBLE, &
                         (/id_d_lon,id_d_lat, id_d_depth, id_d_time/), &
                         id_v_cell_thickness)
  call nf90_check_error(nf_stat, 'error def var cell_thickness')
    nf_stat = nf90_def_var_deflate(ncid_ot, id_v_cell_thickness, 0, 1, 1)
    call nf90_check_error(nf_stat, 'error def var cell_thickness')
  
  
  ! create mask variable and deflate
  if (.not. fillval_h_bad) then
    nf_stat = nf90_def_var(ncid_ot, varname_mask, NF90_BYTE, &
                           (/id_d_lon,id_d_lat,id_d_depth,id_d_time/), &
                           id_v_mask)
    call nf90_check_error(nf_stat, 'error def var mask')
    nf_stat = nf90_def_var_deflate(ncid_ot, id_v_mask, 0, 1, 1)
    call nf90_check_error(nf_stat, 'error def var mask')
  end if
  
  
  ! copy attributes:
  nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'lon', 'lon')
  call nf90_check_error(nf_stat, 'error copy atts lon')
  
  nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'lat', 'lat')
  call nf90_check_error(nf_stat, 'error copy atts lat')
  
  nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'time', 'time')
  call nf90_check_error(nf_stat, 'error copy atts time')
  
  nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'depth', 'depth')
  call nf90_check_error(nf_stat, 'error copy atts depth')
  
  nf_stat = nf90_set_atts_cell_area(ncid_ot, varname_cell_area,  &
                                         fillval_cell_area)
  call nf90_check_error(nf_stat, 'error write atts for cell_area')
  
  nf_stat = nf90_set_atts_cell_volume(ncid_ot, varname_cell_volume,  &
                                         fillval_cell_volume)
  call nf90_check_error(nf_stat, 'error write atts for cell_volume')
  
  nf_stat = nf90_set_atts_cell_thickness(ncid_ot, varname_cell_thickness,  &
                                         fillval_cell_thickness)
  call nf90_check_error(nf_stat, 'error write atts for cell_thickness')
  
  if (.not. fillval_h_bad) then
    nf_stat = nf90_set_atts_mask(ncid_ot, varname_mask, fillval_mask)
    call nf90_check_error(nf_stat, 'error write atts for mask')
  end if
  
  
  ! set global attributes
  nf_stat = nf90_copy_global_atts(ncid_in, ncid_ot)
  call nf90_check_error(nf_stat, 'error copy atts global')
  nf_stat = nf90_set_global_atts(ncid_in, ncid_ot, &
                                 str_time_stamp//' calc_grid_cell_size.x '//&
                                 trim(file_in)//' '//trim(file_ot))
  call nf90_check_error(nf_stat, 'error copy atts global')
  
  
  ! leave definition mode
  nf_stat = NF90_ENDDEF(ncid_ot)
  call nf90_check_error(nf_stat, 'error end def mode')
  
  
  ! CALCULATE real_depth
    ! allocate arrays
  allocate(val_v_cell_area(n_lon, n_lat), &
           val_v_cell_volume(n_lon, n_lat, n_depth, n_time), &
           val_v_mask(n_lon, n_lat, n_depth, n_time))
  
  ! calculate cell_area
  call calc_cell_area(val_v_lat, val_v_cell_area, &
                      (/n_lon, n_lat, n_depth/))
  
  ! calculate cell_area
  call convert_fill_value_double(val_v_h, fillval_h, fillval_cell_thickness, &
                                 (/n_lon, n_lat, n_depth, n_time/))
  fillval_h = fillval_cell_thickness
  
  ! calculate cell_area
  call calc_cell_volume(val_v_cell_area, val_v_h, val_v_cell_volume, &
                        fillval_h, (/n_lon, n_lat, n_depth, n_time/))
  
    ! call calculate routine
  if (fillval_h_bad) then
    val_v_mask = fillval_mask
  else
    
    call calc_mask(val_v_h, val_v_mask,            &
                    (/n_lon, n_lat, n_depth, n_time/), &
                    fillval_h)
    
  end if
  
  
  ! write variables
  nf_stat = NF90_PUT_VAR(ncid_ot, id_v_lon, val_v_lon)
  call nf90_check_error(nf_stat, 'error put var lon')
  
  nf_stat = NF90_PUT_VAR(ncid_ot, id_v_lat, val_v_lat)
  call nf90_check_error(nf_stat, 'error put var lat')
  
  nf_stat = NF90_PUT_VAR(ncid_ot, id_v_time, val_v_time)
  call nf90_check_error(nf_stat, 'error put var time')
  
  nf_stat = NF90_PUT_VAR(ncid_ot, id_v_depth, val_v_depth)
  call nf90_check_error(nf_stat, 'error put var real_depth')
  
  nf_stat = NF90_PUT_VAR(ncid_ot, id_v_cell_area, val_v_cell_area)
  call nf90_check_error(nf_stat, 'error put var cell_area')
  
  nf_stat = NF90_PUT_VAR(ncid_ot, id_v_cell_volume, val_v_cell_volume)
  call nf90_check_error(nf_stat, 'error put var cell_volume')
  
  nf_stat = NF90_PUT_VAR(ncid_ot, id_v_cell_thickness, val_v_h)
  call nf90_check_error(nf_stat, 'error put var cell_thickness')
  
  if (.not. fillval_h_bad) then
    nf_stat = NF90_PUT_VAR(ncid_ot, id_v_mask, val_v_mask)
    call nf90_check_error(nf_stat, 'error put var mask')
  end if
  
  
  ! close input file
  nf_stat = NF90_CLOSE(ncid_in)
  call nf90_check_error(nf_stat, 'closing input file')
  
  
  ! close file
  nf_stat = NF90_CLOSE(ncid_ot)
  call nf90_check_error(nf_stat, 'closing output file')
  
  
  ! deallocate arrays
  deallocate(val_v_lon, val_v_lat, val_v_time, val_v_depth, & 
             val_v_mask, val_v_cell_area, val_v_h, &
             val_v_cell_volume)
  
  
  ! ~~~~~ THE END ~~~~~
  
  
  contains
  
    ! ~~~~~ CHECK nf_stat FOR ERRORS (!= 0) ~~~~~
    subroutine nf90_check_error(nf_stat, err_msg)
    
      implicit none
      
      integer,           intent(in)           :: nf_stat
      character (len=*), intent(in), optional :: err_msg
      
      if (.not.(nf_stat == NF90_NOERR)) then
        write(*,'(A25,I5)') 'error: netCDF error code ', nf_stat
        write(*,'(A15,A)') 'error message: ', err_msg
        stop
      end if
      
    end subroutine nf90_check_error
    ! ~~~~~ END CHECK nf_stat FOR ERRORS (!= 0) ~~~~~
  
  
  
    ! ~~~~~ COPY ALL ATTRIBUTES OF THE COORDINATE VARIABLES ~~~~~
    function nf90_copy_all_atts(ncid_in, ncid_ot, varname_in, varname_ot)
      
      implicit none
      
      integer,           intent(in) :: ncid_in, ncid_ot
      character (len=*), intent(in) :: varname_in, varname_ot
      
      integer             :: nf90_copy_all_atts
      integer             :: id_v_in, id_v_ot
      integer             :: nf_stat
      
      
      ! in var id
      nf_stat = NF90_INQ_VARID(ncid_in, varname_in, id_v_in)
      call nf90_check_error(nf_stat, 'inq var_id of var '//varname_in)
      ! out var id
      nf_stat = NF90_INQ_VARID(ncid_ot, varname_ot, id_v_ot)
      call nf90_check_error(nf_stat, 'inq var_id of var '//varname_ot)
      
      
      !! GO INTO DEFINITION MODE
      !! we assume that we are still in the definition mode
      ! nf_stat = nf90_redef(ncid_ot)
      ! call nf90_check_error(nf_stat)
      
      
      ! copy attributes
      nf_stat = nf90_copy_att(ncid_in, id_v_in, 'units', ncid_ot, id_v_ot)
      call nf90_check_error(nf_stat, 'error copy attribute units from var '//&
                            trim(varname_in)//' to var '//trim(varname_ot))
                            
      nf_stat = nf90_copy_att(ncid_in, id_v_in, 'standard_name', ncid_ot, id_v_ot)
      call nf90_check_error(nf_stat, 'error copy attribute standard_name from var '//&
                            trim(varname_in)//' to var '//trim(varname_ot))
                            
      nf_stat = nf90_copy_att(ncid_in, id_v_in, 'long_name', ncid_ot, id_v_ot)
      call nf90_check_error(nf_stat, 'error copy attribute long_name from var '//&
                            trim(varname_in)//' to var '//trim(varname_ot))
                            
      nf_stat = nf90_copy_att(ncid_in, id_v_in, 'axis', ncid_ot, id_v_ot)
      call nf90_check_error(nf_stat, 'error copy attribute axis from var '//&
                            trim(varname_in)//' to var '//trim(varname_ot))
      
      
      if (trim(varname_in) .eq. 'depth') then
        nf_stat = nf90_copy_att(ncid_in, id_v_in, 'positive', ncid_ot, id_v_ot)
        call nf90_check_error(nf_stat, 'error copy attribute positive from var '//&
                              trim(varname_in)//' to var '//trim(varname_ot))
      end if
      
      
      !! LEAVE DEFINITION MODE
      ! nf_stat = nf90_enddef(ncid)
      ! call nf90_check_error(nf_stat)
      
      
      ! SET RETURN VALUE
      nf90_copy_all_atts = NF90_NOERR
      
      
    end function nf90_copy_all_atts
    ! ~~~~~ END COPY ALL ATTRIBUTES OF THE COORDINATE VARIABLES ~~~~~
  
  
  
    ! ~~~~~ COPY ALL ATTRIBUTES OF THE COORDINATE VARIABLES ~~~~~
    function nf90_copy_global_atts(ncid_in, ncid_ot)
      
      implicit none
      
      integer,           intent(in) :: ncid_in, ncid_ot
      
      integer             :: nf90_copy_global_atts
      integer             :: nf_stat
      character (len=255) :: tmp_string
      real(4)             :: tmp_float
      
      
      !! GO INTO DEFINITION MODE
      !! we assume that we are still in the definition mode
      ! nf_stat = nf90_redef(ncid_ot)
      ! call nf90_check_error(nf_stat)
      
      
      ! copy attributes
      nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'longitude_min', ncid_ot, NF90_GLOBAL)
      call nf90_check_error(nf_stat, 'error copy global attribute longitude_min')
                            
      nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'longitude_max', ncid_ot, NF90_GLOBAL)
      call nf90_check_error(nf_stat, 'error copy global attribute longitude_max')
                            
      nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'latitude_min', ncid_ot, NF90_GLOBAL)
      call nf90_check_error(nf_stat, 'error copy global attribute latitude_min')
                            
      nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'latitude_max', ncid_ot, NF90_GLOBAL)
      call nf90_check_error(nf_stat, 'error copy global attribute latitude_max')
                            
      nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'depth_min', ncid_ot, NF90_GLOBAL)
      call nf90_check_error(nf_stat, 'error copy global attribute depth_min')
                            
      nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'depth_max', ncid_ot, NF90_GLOBAL)
      call nf90_check_error(nf_stat, 'error copy global attribute depth_max')
      
      
      !! LEAVE DEFINITION MODE
      ! nf_stat = nf90_enddef(ncid)
      ! call nf90_check_error(nf_stat)
      
      
      ! SET RETURN VALUE
      nf90_copy_global_atts = NF90_NOERR
      
      
    end function nf90_copy_global_atts
    ! ~~~~~ END COPY ALL ATTRIBUTES OF THE COORDINATE VARIABLES ~~~~~
    
    
    
    ! ~~~~~ WRITE ATTS cell_area ~~~~~
    function nf90_set_atts_cell_area(ncid, varname, fillval)
      
      implicit none
      
      integer,           intent(in) :: ncid
      character (len=*), intent(in) :: varname
      real(8),           intent(in) :: fillval
      
      
      integer :: id_v, nf_stat
      integer :: nf90_set_atts_cell_area
      
      
      ! out var id
      nf_stat = NF90_INQ_VARID(ncid, varname, id_v)
      call nf90_check_error(nf_stat, 'inq var_id of var '//varname)
      
      
      ! put attributes
      nf_stat = nf90_put_att(ncid, id_v, 'standard_name', 'cell_area')
      call nf90_check_error(nf_stat, 'error put att standard_name of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'long_name', 'cell_area')
      call nf90_check_error(nf_stat, 'error put att long_name of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'units', 'm2')
      call nf90_check_error(nf_stat, 'error put att units of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'description', 'Cell_area is the '//&
                             'horizontal area of a grid-cell. Cell refers '//&
                             'to a model grid-cell.')
      call nf90_check_error(nf_stat, 'error put att description of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, '_FillValue', fillval)
      call nf90_check_error(nf_stat, 'error put att _FillValue of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'missing_value', fillval)
      call nf90_check_error(nf_stat, 'error put att missing_value of var '//trim(varname))
      
      
      nf90_set_atts_cell_area = NF90_NOERR
      
    end function nf90_set_atts_cell_area
    ! ~~~~~ END WRITE ATTS cell_area ~~~~~
    
    
    
    ! ~~~~~ WRITE ATTS cell_area ~~~~~
    function nf90_set_atts_cell_volume(ncid, varname, fillval)
      
      implicit none
      
      integer,           intent(in) :: ncid
      character (len=*), intent(in) :: varname
      real(8),           intent(in) :: fillval
      
      
      integer :: id_v, nf_stat
      integer :: nf90_set_atts_cell_volume
      
      
      ! out var id
      nf_stat = NF90_INQ_VARID(ncid, varname, id_v)
      call nf90_check_error(nf_stat, 'inq var_id of var '//varname)
      
      
      ! put attributes
      nf_stat = nf90_put_att(ncid, id_v, 'standard_name', 'cell_volumne')
      call nf90_check_error(nf_stat, 'error put att standard_name of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'long_name', 'cell_volume')
      call nf90_check_error(nf_stat, 'error put att long_name of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'units', 'm3')
      call nf90_check_error(nf_stat, 'error put att units of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'description', 'Cell_volume is the '//&
                             '3-dim volume of a grid-cell. Cell refers to a '//&
                             'model grid-cell.')
      call nf90_check_error(nf_stat, 'error put att description of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, '_FillValue', fillval)
      call nf90_check_error(nf_stat, 'error put att _FillValue of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'missing_value', fillval)
      call nf90_check_error(nf_stat, 'error put att missing_value of var '//trim(varname))
      
      
      nf90_set_atts_cell_volume = NF90_NOERR
      
    end function nf90_set_atts_cell_volume
    ! ~~~~~ END WRITE ATTS cell_area ~~~~~
    
    
    
    ! ~~~~~ WRITE ATTS cell_area ~~~~~
    function nf90_set_atts_cell_thickness(ncid, varname, fillval)
      
      implicit none
      
      integer,           intent(in) :: ncid
      character (len=*), intent(in) :: varname
      real(8),           intent(in) :: fillval
      
      
      integer :: id_v, nf_stat
      integer :: nf90_set_atts_cell_thickness
      
      
      ! out var id
      nf_stat = NF90_INQ_VARID(ncid, varname, id_v)
      call nf90_check_error(nf_stat, 'inq var_id of var '//varname)
      
      
      ! put attributes
      nf_stat = nf90_put_att(ncid, id_v, 'standard_name', 'cell_thickness')
      call nf90_check_error(nf_stat, 'error put att standard_name of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'long_name', 'cell_thickness')
      call nf90_check_error(nf_stat, 'error put att long_name of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'units', 'm')
      call nf90_check_error(nf_stat, 'error put att units of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'description', 'Thickness means '//&
                             'the vertical extent of a layer. Cell refers '//&
                             'to a model grid-cell.')
      call nf90_check_error(nf_stat, 'error put att description of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, '_FillValue', fillval)
      call nf90_check_error(nf_stat, 'error put att _FillValue of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'missing_value', fillval)
      call nf90_check_error(nf_stat, 'error put att missing_value of var '//trim(varname))
      
      
      nf90_set_atts_cell_thickness = NF90_NOERR
      
    end function nf90_set_atts_cell_thickness
    ! ~~~~~ END WRITE ATTS cell_area ~~~~~
    
    
    
    ! ~~~~~ WRITE ATTS mask ~~~~~
    function nf90_set_atts_mask(ncid, varname, fillval)
      
      implicit none
      
      integer,           intent(in) :: ncid
      character (len=*), intent(in) :: varname
      integer(1),           intent(in) :: fillval
      
      
      integer :: id_v, nf_stat
      integer :: nf90_set_atts_mask
      
      
      ! out var id
      nf_stat = NF90_INQ_VARID(ncid, varname, id_v)
      call nf90_check_error(nf_stat, 'inq var_id of var '//varname)
      
      
      ! put attributes
      nf_stat = nf90_put_att(ncid, id_v, 'standard_name', 'land_binary_mask')
      call nf90_check_error(nf_stat, 'error put att standard_name of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'long_name', 'land_binary_mask')
      call nf90_check_error(nf_stat, 'error put att long_name of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'units', '1')
      call nf90_check_error(nf_stat, 'error put att units of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'description', 'X_binary_mask has '//&
                             '1 where condition X is met, 0 elsewhere. 1 = '//&
                             'land, 0 = sea.')
      call nf90_check_error(nf_stat, 'error put att description of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, '_FillValue', fillval)
      call nf90_check_error(nf_stat, 'error put att _FillValue of var '//trim(varname))
      
      nf_stat = nf90_put_att(ncid, id_v, 'missing_value', fillval)
      call nf90_check_error(nf_stat, 'error put att missing_value of var '//trim(varname))
      
      
      nf90_set_atts_mask = NF90_NOERR
      
    end function nf90_set_atts_mask
    ! ~~~~~ END WRITE ATTS mask ~~~~~
    
    
    
    ! ~~~~~ SET GLOBAL ATTRIBUTES ~~~~~
    function nf90_set_global_atts(ncid_in, ncid_ot, history)
      
      implicit none
      
      integer,           intent(in) :: ncid_in, ncid_ot
      character (len=*), intent(in) :: history
      
      integer                        :: nf90_set_global_atts
      integer                        :: nf_stat
      character (len=:), allocatable :: tmp_string_dyn
      integer                        :: tmp_int
      
      
      !! go into definition mode
      !nf_stat = nf90_redef(ncid_ot)
      !call nf90_check_error(nf_stat)
      
      
      ! set global attributes
      nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'Conventions', 'CF-1.7')
      call nf90_check_error(nf_stat, 'put global att Conventions')
      
      nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'source', &
                             'HIROMB-BOOS-model input')
      call nf90_check_error(nf_stat, 'put global att source')
      
      nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'title', &
                             'cell area and land-sea mask for HBM model grid')
      call nf90_check_error(nf_stat, 'put global att title')
      
      nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'institution', 'BSH and IOW')
      call nf90_check_error(nf_stat, 'put global att institution')
      
      nf_stat = nf90_inquire_attribute(ncid_in, NF90_GLOBAL, 'history', len=tmp_int)
      call nf90_check_error(nf_stat, 'inq global att history')
      allocate(character(len=tmp_int) :: tmp_string_dyn)
      nf_stat = nf90_get_att(ncid_in, NF90_GLOBAL, 'history', tmp_string_dyn)
      call nf90_check_error(nf_stat, 'get global att history')
      nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'history', trim(tmp_string_dyn)//';   '//history)
      call nf90_check_error(nf_stat, 'put global att history')
      
      nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'comment', &
                             'created by Daniel Neumann (IOW) from HBM layer'//&
                             ' thickness model output data provided by BSH'//&
                             ' and DMI')
      call nf90_check_error(nf_stat, 'put global att comment')
      
      
      !! leave definition mode
      !nf_stat = nf90_enddef(ncid)
      !call nf90_check_error(nf_stat)
      
      nf90_set_global_atts = NF90_NOERR
      
    end function nf90_set_global_atts
    ! ~~~~~ END SET GLOBAL ATTRIBUTES ~~~~~
    
    
    
    ! ~~~~~ CALC CELL AREA ~~~~~
    subroutine calc_cell_area(lat, cell_area, count) 
      
      implicit none
      
      REAL(8), dimension(:),   intent(in)  :: lat
      REAL(8), dimension(:,:), intent(out) :: cell_area
      integer, DIMENSION(2),   intent(in)  :: count
      
      integer :: iLo, iLa, nLo, nLa
      real(8) :: tmp_depth, tmp_h
      logical :: proceed_depth
      
      real(8), parameter :: r_earth_eq = 6378.1_8,         &
                            r_earth_pol = 6356.8_8,        &
                            dr = r_earth_eq - r_earth_pol, &
                            r_min = r_earth_pol,           &
                            pi = 3.141592653589793238_8,   &
                            deg2rad = pi/180.0_8,          &
                            dlon = 5.0_8/360.0_8*deg2rad,  &
                            dlat = 1.0_8/120.0_8*deg2rad
      real(8)            :: tmp_r, tmp_dx, tmp_dy, tmp_cos_lat
      
      nLo = count(1)
      nLa = count(2)

      ! Placed as atmos dep in one grid cell of the fine grid. Grid cell data:
      !   phi:   55 deg N
      !   d_phi: 30'' N-S  (1/120 deg)
      !   d_lam: 50'' E-W  (5/360 deg)
      !
      ! Earth Radius:
      !   equator: 6378.1 km (r_max)
      !   pole:    6356.8 km (r_min)
      ! 
      !   dr = 21.3 km
      !
      ! Assume :
      !   r(phi) = r_min + dr * cos(phi)
      !   r(55 deg) = 6365 km
      !
      ! grid cell size in m (dx and dy)
      !   dy = r * d_phi = 6365 km * 1 / 120 * pi/180 = 0.926 km
      !   dx = r * cos(phi) * d_lam = 6365 km * cos(55/180*pi) * 5/360 * pi/180 = 0.885 km
      !
      !   dA = dx * dy = 0.926 km * 0.885 km = 0.8192739 km2 = 819273.9 m2
      
        
      ! We do not iterate in the proper order in which the data are located
      ! in the memory. This would we TIME, DEPTH, LAT, and LON (TIME outer
      ! loop; DEPTH first inner loop; ...). We do instead TIME, LAT, LON, and
      ! DEPTH. This seems to be reasonable because we can cut the iteration
      ! of DEPTH at a specific horizontal location and time as soon as we 
      ! encounter a missing value. 
!$omp parallel do private(tmp_depth, tmp_h, iLa, iLo, proceed_depth, iZ) shared(depth) num_threads( 4 )
      do iLa = 1, nLa
      
        tmp_cos_lat = cos(lat(iLa)*deg2rad)
        tmp_r = r_min + dr * tmp_cos_lat
        tmp_dx = tmp_r * dlat
        tmp_dy = tmp_r * tmp_cos_lat * dlon
        
        cell_area(1:nLo, iLa) = tmp_dx * tmp_dy * 1000000
          
      end do
!$omp end parallel do
      
    end subroutine calc_cell_area
    ! ~~~~~ END CALC CELL AREA ~~~~~
    
    
    
    ! ~~~~~ CALC MASK ~~~~~
    subroutine calc_mask(dummy, mask, count, fillval_dummy) 
      
      implicit none
      
      REAL(8),    dimension(:,:,:,:), intent(in)  :: dummy
      INTEGER(1), dimension(:,:,:,:), intent(out) :: mask
      integer,    DIMENSION(4),       intent(in)  :: count
      real(8),                        intent(in)  :: fillval_dummy
      
      integer :: iLo, iLa, iZ, iTi, nLo, nLa, nZ, nTi
      logical :: proceed_depth
      
      nLo = count(1)
      nLa = count(2)
      nZ  = count(3)
      nTi = count(4)
      
      mask = 1_1
      
      ! We do not iterate in the proper order in which the data are located
      ! in the memory. This would we TIME, DEPTH, LAT, and LON (TIME outer
      ! loop; DEPTH first inner loop; ...). We do instead TIME, LAT, LON, and
      ! DEPTH. This seems to be reasonable because we can cut the iteration
      ! of DEPTH at a specific horizontal location and time as soon as we 
      ! encounter a missing value. 
!$omp parallel do private(iLa, iLo, iTi, proceed_depth, iZ) shared(mask) num_threads( 4 )
      do iTi = 1, nTi
        do iLa = 1, nLa
          do iLo = 1, nLo
          
            proceed_depth = .true.
            iZ = 1
            do while (proceed_depth .and. (iZ .le. nZ))
              
              if ( dummy(iLo, iLa, iZ, iTi) .eq. fillval_dummy ) then
                proceed_depth = .false.
              else 
                mask(iLo, iLa, iZ, iTi) = 0_1
              endif
              
              iZ = iZ + 1
              
            end do
            
          end do
        end do
      end do
!$omp end parallel do
      
    end subroutine calc_mask
    ! ~~~~~ END CALC MASK ~~~~~
    
    
    
    ! ~~~~~ replace fill values ~~~~~
    subroutine convert_fill_value_double(values, fillval_old, fillval_new, count) 
      
      implicit none
      
      REAL(8),    dimension(:,:,:,:), intent(inOUT) :: values
      integer,    DIMENSION(4),       intent(in)    :: count
      real(8),                        intent(in)    :: fillval_old, fillval_new
      
      integer :: iLo, iLa, iZ, iTi, nLo, nLa, nZ, nTi
      logical :: proceed_depth
      
      nLo = count(1)
      nLa = count(2)
      nZ  = count(3)
      nTi = count(4)
      
!$omp parallel do private(iLa, iLo, iTi, iZ) shared(values) num_threads( 4 )
      do iTi = 1, nTi
        do iZ = 1, nZ
          do iLa = 1, nLa
            do iLo = 1, nLo
            
              if (values(iLo, iLa, iZ, iTi) .eq. fillval_old ) then
                values(iLo, iLa, iZ, iTi) = fillval_new
              end if
              
            end do
          end do
        end do
      end do
!$omp end parallel do
      
    end subroutine convert_fill_value_double
    ! ~~~~~ END replace fill values ~~~~~
    
    
    
    ! ~~~~~ CALC CELL VOLUME ~~~~~
    subroutine calc_cell_volume(area, thickness, volume, fillval, count) 
      
      implicit none
      
      REAL(8),    dimension(:,:),     intent(in)  :: area
      REAL(8),    dimension(:,:,:,:), intent(in)  :: thickness
      REAL(8),    dimension(:,:,:,:), intent(OUT) :: volume
      integer,    DIMENSION(4),       intent(in)  :: count
      real(8),                        intent(in)  :: fillval
      
      integer :: iLo, iLa, iZ, iTi, nLo, nLa, nZ, nTi
      logical :: proceed_depth
      real(8) :: tmp_area
      
      nLo = count(1)
      nLa = count(2)
      nZ  = count(3)
      nTi = count(4)
      
      volume = fillval
      
      ! We do not iterate in the proper order in which the data are located
      ! in the memory. This would we TIME, DEPTH, LAT, and LON (TIME outer
      ! loop; DEPTH first inner loop; ...). We do instead TIME, LAT, LON, and
      ! DEPTH. This seems to be reasonable because we can cut the iteration
      ! of DEPTH at a specific horizontal location and time as soon as we 
      ! encounter a missing value. 
!$omp parallel do private(tmp_area, iLa, iLo, iTi, proceed_depth, iZ) shared(mask) num_threads( 4 )
      do iTi = 1, nTi
        do iLa = 1, nLa
          do iLo = 1, nLo
            tmp_area = area(iLo, iLa)
            
            proceed_depth = .true.
            iZ = 1
            do while (proceed_depth .and. (iZ .le. nZ))
              
              if ( thickness(iLo, iLa, iZ, iTi) .eq. fillval) then
                proceed_depth = .false.
              else 
                volume(iLo, iLa, iZ, iTi) = thickness(iLo, iLa, iZ, iTi) * &
                                             tmp_area
              endif
              
              iZ = iZ + 1
              
            end do
            
          end do
        end do
      end do
!$omp end parallel do
      
    end subroutine calc_cell_volume
    ! ~~~~~ END CALC CELL VOLUME ~~~~~
  


end program calc_grid_cell_size
