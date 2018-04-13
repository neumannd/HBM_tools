program intergrate_layer_depth

  ! use netcdf4_f03
  use netcdf
  use omp_lib
  use nf90_tools
  use check_stat_netcdf
  use read_files
  
  ! NC_ENOTATT not found???? Would be -43

  implicit none
  
  integer(4)          :: argc                                      ! commandline argument count
  character (len=255) :: file_in, file_ot
  integer             :: n_lon, n_lat, n_time, n_depth             ! dimension size

  integer             :: id_d_lon, id_d_lat, id_d_time, id_d_depth ! dimension id_s
  integer             :: id_v_lon, id_v_lat, id_v_time, id_v_depth, id_v_dummy ! variable id_s
  integer             :: id_v_h, id_v_real_depth, id_v_bathy ! further variable id_s
  
  REAL(8), dimension(:), allocatable       :: val_v_lon, val_v_lat,  &
                                              val_v_depth, val_v_time
  REAL(8), dimension(:,:,:,:), allocatable :: val_v_h, val_v_real_depth
  real(8), dimension(:,:,:),   allocatable :: val_v_bathy
  real(8)                                  :: fillval_h
  logical                                  :: fillval_h_bad
  
  integer             :: nf_stat         ! for error status of netCDF functions
  integer             :: ncid_in, ncid_ot  ! ncid for input and output
  
  CHARACTER (len=8)        :: str_date
  CHARACTER (len=10)       :: str_time
  CHARACTER (len=5)        :: str_zone
  integer(4), dimension(8) :: int_date_and_time
  character (len=20)       :: str_time_stamp
  character (len=47)       :: fmt_time_stamp 
  
  ! help request?
  logical :: helpme
  
  
  ! check whether help is requested
  call test_help_string(helpme)
  
  
  if (helpme) then
  
  else
    ! GET file_in and file_ot AS INPUT ARGUMENTS
    call get_filenames(file_in, file_ot)

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
    write(*,*) '~~~~ READING file '//file_in 
    ! OPEN FILE
    nf_stat = NF90_OPEN(file_in, NF90_NOWRITE, ncid_in)
    call check_nf90_stat(nf_stat)
    
    
    ! GET DIM IDs
    nf_stat = NF90_INQ_DIMID(ncid_in, 'lon', id_d_lon)
    call check_nf90_stat(nf_stat, 'error inq dimid lon')
    nf_stat = NF90_INQ_DIMID(ncid_in, 'lat', id_d_lat)
    call check_nf90_stat(nf_stat, 'error inq dimid lat')
    nf_stat = NF90_INQ_DIMID(ncid_in, 'depth', id_d_depth)
    call check_nf90_stat(nf_stat, 'error inq dimid depth')
    nf_stat = NF90_INQ_DIMID(ncid_in, 'time', id_d_time)
    call check_nf90_stat(nf_stat, 'error inq dimid time')
    
    
    ! GET VAR IDs
    nf_stat = NF90_INQ_VARID(ncid_in, 'h', id_v_h)
    call check_nf90_stat(nf_stat, 'error inq varid h')
    nf_stat = NF90_INQ_VARID(ncid_in, 'lon', id_v_lon)
    call check_nf90_stat(nf_stat, 'error inq varid lon')
    nf_stat = NF90_INQ_VARID(ncid_in, 'lat', id_v_lat)
    call check_nf90_stat(nf_stat, 'error inq varid lat')
    nf_stat = NF90_INQ_VARID(ncid_in, 'depth', id_v_depth)
    call check_nf90_stat(nf_stat, 'error inq varid depth')
    nf_stat = NF90_INQ_VARID(ncid_in, 'time', id_v_time)
    call check_nf90_stat(nf_stat, 'error inq varid time')
    
    
    ! READ DIM SIZEs
    nf_stat = nf90_inquire_dimension(ncid_in, id_d_lon, len = n_lon)
    call check_nf90_stat(nf_stat, 'error get dim lon')
    nf_stat = nf90_inquire_dimension(ncid_in, id_d_lat, len = n_lat)
    call check_nf90_stat(nf_stat, 'error get dim lat')
    nf_stat = nf90_inquire_dimension(ncid_in, id_d_depth, len = n_depth)
    call check_nf90_stat(nf_stat, 'error get dim depth')
    nf_stat = nf90_inquire_dimension(ncid_in, id_d_time, len = n_time)
    call check_nf90_stat(nf_stat, 'error get dim time')
    
    
    ! ALLOCATE ARRAY FOR READ-IN
    allocate(val_v_lon(n_lon), val_v_lat(n_lat), val_v_time(n_time), &
             val_v_depth(n_depth), val_v_h(n_lon, n_lat, n_depth, n_time))
    
    
    ! READ VAR VALUES
    nf_stat = NF90_GET_VAR(ncid_in, id_v_lon, val_v_lon)
    call check_nf90_stat(nf_stat, 'error get var lon')
    
    nf_stat = NF90_GET_VAR(ncid_in, id_v_lat, val_v_lat)
    call check_nf90_stat(nf_stat, 'error get var lat')
    
    nf_stat = NF90_GET_VAR(ncid_in, id_v_depth, val_v_depth)
    call check_nf90_stat(nf_stat, 'error get var depth')
    
    nf_stat = NF90_GET_VAR(ncid_in, id_v_time, val_v_time)
    call check_nf90_stat(nf_stat, 'error get var time')
    
    ! nf_stat = NF90_GET_VAR(ncid_in, id_v_h, val_v_h, start = (/1,1,1,1/), count = (/2,2,2,2/))
    nf_stat = NF90_GET_VAR(ncid_in, id_v_h, val_v_h)
    call check_nf90_stat(nf_stat, 'error get var h')
    
    
    ! READ _FillValue OR missing_value OF VARIABLE 'h'
    fillval_h_bad = .FALSE.
    nf_stat = nf90_get_att(ncid_in, id_v_h, '_FillValue', fillval_h)
    
    if ( nf_stat .eq. -43 ) then
      
      write(*,*) 'Warning: attribute _FillValue not found for variable h. '//&
                  'Trying missing_value.'
      nf_stat = nf90_get_att(ncid_in, id_v_h, 'missing_value', fillval_h)
      if ( nf_stat .eq. -43 ) then
        write(*,*) 'Warning: attribute missing_value not found for variable h. '//&
                    'Using no _FillValue'
        fillval_h_bad = .TRUE.
      else
        call check_nf90_stat(nf_stat, 'error get att missing_value of var h')
      end if
      
    else
      
      call check_nf90_stat(nf_stat, 'error get att _FillValue of var h')
      
    end if
    
    
    
    
    !! ~~~~~ WRITE NEW FILE ~~~~~
    write(*,*) '~~~~ CREATING new file '//file_ot
    ! create new file
    nf_stat = NF90_CREATE(file_ot, NF90_HDF5, ncid_ot)
    call check_nf90_stat(nf_stat)
    
    
    ! create dimensions
    nf_stat = nf90_def_dim(ncid_ot, 'lon', n_lon, id_d_lon)
    call check_nf90_stat(nf_stat, 'error def dim lon')
    
    nf_stat = nf90_def_dim(ncid_ot, 'lat', n_lat, id_d_lat)
    call check_nf90_stat(nf_stat, 'error def dim lat')
    
    nf_stat = nf90_def_dim(ncid_ot, 'depth', n_depth, id_d_depth)
    call check_nf90_stat(nf_stat, 'error def dim depth')
    
    nf_stat = nf90_def_dim(ncid_ot, 'time', n_time, id_d_time)
    call check_nf90_stat(nf_stat, 'error def dim time')
    
    
    ! create variables
    nf_stat = nf90_def_var(ncid_ot, 'lon', NF90_DOUBLE, (/id_d_lon/), id_v_lon)
    call check_nf90_stat(nf_stat, 'error def var lon')
    
    nf_stat = nf90_def_var(ncid_ot, 'lat', NF90_DOUBLE, (/id_d_lat/), id_v_lat)
    call check_nf90_stat(nf_stat, 'error def var lat')
    
    ! nf_stat = nf90_def_var(ncid_ot, 'depth', NF90_DOUBLE, (/id_d_depth/), id_v_depth)
    ! call check_nf90_stat(nf_stat, 'error def var depth')
    
    nf_stat = nf90_def_var(ncid_ot, 'time', NF90_DOUBLE, (/id_d_time/), id_v_time)
    call check_nf90_stat(nf_stat, 'error def var time')
    
    ! create depth variable and deflate it!
    nf_stat = nf90_def_var(ncid_ot, 'depth', NF90_FLOAT, &
                           (/id_d_lon,id_d_lat,id_d_depth,id_d_time/), &
                           id_v_real_depth)
    call check_nf90_stat(nf_stat, 'error def var real_depth')
    nf_stat = nf90_def_var_deflate(ncid_ot, id_v_real_depth, 0, 1, 1)
    call check_nf90_stat(nf_stat, 'error def var real_depth')
    
    ! create depth variable and deflate it!
    nf_stat = nf90_def_var(ncid_ot, 'bathymetry', NF90_FLOAT, &
                           (/id_d_lon,id_d_lat,id_d_time/), &
                           id_v_bathy)
    call check_nf90_stat(nf_stat, 'error def var bathymetry')
    nf_stat = nf90_def_var_deflate(ncid_ot, id_v_bathy, 0, 1, 1)
    call check_nf90_stat(nf_stat, 'error def var bathymetry')
    
    ! create dummy_depth variable and deflate it!
    nf_stat = nf90_def_var(ncid_ot, 'dummy_depth', NF90_FLOAT, &
                           (/id_d_lon,id_d_lat,id_d_depth,id_d_time/), &
                           id_v_dummy)
    call check_nf90_stat(nf_stat, 'error def var dummy_depth')
    nf_stat = nf90_def_var_deflate(ncid_ot, id_v_dummy, 0, 1, 1)
    call check_nf90_stat(nf_stat, 'error def var dummy_depth')
    
    
    ! copy attributes:
    nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'lon', 'lon')
    call check_nf90_stat(nf_stat, 'error copy atts lon')
    
    nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'lat', 'lat')
    call check_nf90_stat(nf_stat, 'error copy atts lat')
    
    nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'time', 'time')
    call check_nf90_stat(nf_stat, 'error copy atts time')
    
    nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'depth', 'depth')
    call check_nf90_stat(nf_stat, 'error copy atts time')
    
    nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'depth', 'dummy_depth')
    call check_nf90_stat(nf_stat, 'error copy atts time')
    
    
    ! put fill value
    if ( .not. fillval_h_bad ) then
      nf_stat = nf90_put_att(ncid_ot, id_v_real_depth, '_FillValue', real(fillval_h))
      call check_nf90_stat(nf_stat, 'error set att _FillValue of var depth')
      nf_stat = nf90_put_att(ncid_ot, id_v_real_depth, 'missing_value', real(fillval_h))
      call check_nf90_stat(nf_stat, 'error set att missing_value of var depth')
      !~ nf_stat = nf90_put_att(ncid_ot, id_v_bathy, '_FillValue', fillval_h)
      !~ call check_nf90_stat(nf_stat, 'error set att _FillValue of var bathymetry')
      !~ nf_stat = nf90_put_att(ncid_ot, id_v_bathy, 'missing_value', fillval_h)
      !~ call check_nf90_stat(nf_stat, 'error set att missing_value of var bathymetry')
      nf_stat = nf90_put_att(ncid_ot, id_v_dummy, '_FillValue', real(fillval_h))
      call check_nf90_stat(nf_stat, 'error set att _FillValue of var dummy_depth')
      nf_stat = nf90_put_att(ncid_ot, id_v_dummy, 'missing_value', real(fillval_h))
      call check_nf90_stat(nf_stat, 'error set att missing_value of var dummy_depth')
    end if
    
    ! set other attributes of bathymetry
    nf_stat = nf90_set_atts_bathymetry(ncid_ot, 'bathymetry', fillval_h)
    call check_nf90_stat(nf_stat, 'error write atts for bathymetry')
    
    
    ! set global attributes
    nf_stat = nf90_copy_global_atts(ncid_in, ncid_ot)
    call check_nf90_stat(nf_stat, 'error copy atts global')
    nf_stat = nf90_set_global_atts(ncid_in, ncid_ot, &
                                   str_time_stamp//' integrate_layer_depth.x '//&
                                   trim(file_in)//' '//trim(file_ot))
    call check_nf90_stat(nf_stat, 'error copy atts global')
    
    
    ! leave definition mode
    nf_stat = NF90_ENDDEF(ncid_ot)
    call check_nf90_stat(nf_stat, 'error end def mode')
    
    
    ! CALCULATE real_depth
    write(*,*) '~~~~ CALCULATING new variables for output file'
      ! allocate arrays
    allocate(val_v_real_depth(n_lon, n_lat, n_depth, n_time), &
             val_v_bathy(n_lon, n_lat, n_time))
    
      ! call calculate routine
    if (fillval_h_bad) then
      
      call calc_depth(val_v_h, val_v_real_depth, val_v_bathy, &
                      (/n_lon, n_lat, n_depth, n_time/))
      
    else
      
      call calc_depth(val_v_h, val_v_real_depth, val_v_bathy, &
                      (/n_lon, n_lat, n_depth, n_time/), fillval_h)
      
    end if
    
    
    ! write variables
    write(*,*) '~~~~ WRITE new variables into output file'
    nf_stat = NF90_PUT_VAR(ncid_ot, id_v_lon, val_v_lon)
    call check_nf90_stat(nf_stat, 'error put var lon')
    
    nf_stat = NF90_PUT_VAR(ncid_ot, id_v_lat, val_v_lat)
    call check_nf90_stat(nf_stat, 'error put var lat')
    
    nf_stat = NF90_PUT_VAR(ncid_ot, id_v_time, val_v_time)
    call check_nf90_stat(nf_stat, 'error put var time')
    
    nf_stat = NF90_PUT_VAR(ncid_ot, id_v_real_depth, val_v_real_depth)
    call check_nf90_stat(nf_stat, 'error put var real_depth')
    
    nf_stat = NF90_PUT_VAR(ncid_ot, id_v_bathy, val_v_bathy)
    call check_nf90_stat(nf_stat, 'error put var bathymetry')
    
    nf_stat = NF90_PUT_VAR(ncid_ot, id_v_dummy, val_v_real_depth)
    call check_nf90_stat(nf_stat, 'error put var dummt_depth')
    
    
    write(*,*) 'FINISHED writing; CLOSING input and output files; CLEANING up'
    
    
    ! close input file
    nf_stat = NF90_CLOSE(ncid_in)
    call check_nf90_stat(nf_stat)
    
    
    ! close file
    nf_stat = NF90_CLOSE(ncid_ot)
    call check_nf90_stat(nf_stat)
    
    
    ! deallocate arrays
    deallocate(val_v_lon, val_v_lat, val_v_time, val_v_depth, val_v_h, & 
               val_v_real_depth, val_v_bathy)
  
  end if ! .not. helpme
  
  
  ! ~~~~~ THE END ~~~~~
  
  
contains

  subroutine print_help()
  
    write(*,'(A)') 'HELP FOR integrate_layer_depth                                       2018/04/13'
    write(*,'(A)') ''
    write(*,'(A)') 'NAME'
    write(*,'(A)') ''
    write(*,'(A)') '      integrate layer depth - calculate depth of each layer and bathymetry from'
    write(*,'(A)') '                              HBM output files'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'SYNOPSIS'
    write(*,'(A)') '      integrate_layer_depth.x file_in.nc file_ot.nc'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'DESCRIPTION'
    write(*,'(A)') '      ... TODO ...'
    write(*,'(A)') '      Try it with a file and see what happens!'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'OPTIONS'
    write(*,'(A)') '      -h, --help'
    write(*,'(A)') '           Print this help.'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'FILE DESCRIPTIONS'
    write(*,'(A)') '         '
    write(*,'(A)') '      file_in.nc'
    write(*,'(A)') '         '
    write(*,'(A)') '         The dimensions need to be denoted as --lon--, --lat--, --depth--, and '
    write(*,'(A)') '         --time--. The data variable h needs to be of type --float-- or '
    write(*,'(A)') '         --double--. netCDF-4 files are accepted.'
    write(*,'(A)') '         '
    write(*,'(A)') '         Here is an example CDL output:'
    write(*,'(A)') '         '
    write(*,'(A)') '         ------------------------------------'
    write(*,'(A)') '         netcdf file_in {'
    write(*,'(A)') '         dimensions:'
    write(*,'(A)') '            lon = 414 ;'
    write(*,'(A)') '            lat = 347 ;'
    write(*,'(A)') '            depth = 36 ;'
    write(*,'(A)') '            time = UNLIMITED ; // (5 currently)'
    write(*,'(A)') '         variables:'
    write(*,'(A)') '            double lon(lon) ;'
    write(*,'(A)') '                ...'
    write(*,'(A)') '            double lat(lat) ;'
    write(*,'(A)') '                ...'
    write(*,'(A)') '            double depth(depth) ;'
    write(*,'(A)') '                ...'
    write(*,'(A)') '            double time(time) ;'
    write(*,'(A)') '                ...'
    write(*,'(A)') '            int h(time, depth, lat, lon) ;'
    write(*,'(A)') '                h:standard_name = "layer_thicknesses" ;'
    write(*,'(A)') '                h:units = "m" ;'
    write(*,'(A)') '                h:_FillValue = -2000000000 ;'
    write(*,'(A)') '                h:missing_value = -2000000000 ;'
    write(*,'(A)') '         }'
    write(*,'(A)') '         ------------------------------------'
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
  
  
  
  
  
  ! ~~~~~ CALC DEPTH ~~~~~
  subroutine calc_depth(h, depth, bathy, count, fillval) 
    
    implicit none
    
    REAL(8), dimension(:,:,:,:), intent(in)  :: h
    REAL(8), dimension(:,:,:,:), intent(out) :: depth
    REAL(8), dimension(:,:,:),   intent(out) :: bathy
    integer, DIMENSION(4),       intent(in)  :: count
    real(8), optional,           intent(in)  :: fillval
    
    integer :: iLo, iLa, iZ, iTi, nLo, nLa, nZ, nTi
    real(8) :: prev_depth, prev_h, this_h
    logical :: proceed_depth
    real(8), parameter :: half = 0.5_8
    
    nLo = count(1)
    nLa = count(2)
    nZ  = count(3)
    nTi = count(4)
    
    
    if (.not. present(fillval)) then
      depth(1:nLo, 1:nLa, 1, 1:nTi)  = h(1:nLo, 1:nLa, 1, 1:nTi)
      depth(1:nLo, 1:nLa, iZ, 1:nTi) = depth(1:nLo, 1:nLa, iZ-1, 1:nTi) + &
                                         h(1:nLo, 1:nLa, iZ, 1:nTi)
      bathy(1:nLo, 1:nLa, 1:nTi)     = depth(1:nLo, 1:nLa, nZ, 1:nTi)
    else 
      
      depth = fillval
      bathy = fillval
      
      ! We do not iterate in the proper order in which the data are located
      ! in the memory. This would we TIME, DEPTH, LAT, and LON (TIME outer
      ! loop; DEPTH first inner loop; ...). We do instead TIME, LAT, LON, and
      ! DEPTH. This seems to be reasonable because we can cut the iteration
      ! of DEPTH at a specific horizontal location and time as soon as we 
      ! encounter a missing value. 
      do iTi = 1, nTi
        ! write(*,'(I4,I4)') iTi
!$omp parallel do private(tmp_depth, tmp_h, iLa, iLo, proceed_depth, iZ) shared(depth) num_threads( 4 )
        do iLa = 1, nLa
          do iLo = 1, nLo
            proceed_depth = .true.
            iZ = 1
            prev_depth = 0
            prev_h = 0
            do while (proceed_depth .and. (iZ .le. nZ))
              this_h = h(iLo, iLa, iZ, iTi)
              
              if ( this_h .eq. fillval ) then
                proceed_depth = .false.
                bathy(iLo, iLa, iTi) = prev_depth + prev_h * half
              else 
                prev_depth = prev_depth + this_h * half + prev_h * half
                prev_h = this_h
                depth(iLo, iLa, iZ, iTi) = prev_depth
                iZ = iZ + 1
              endif
              
            end do
            
            if (iZ .eq. 1) then
              bathy(iLo, iLa, iTi) = fillval
            end if
            
          end do
        end do
!$omp end parallel do
      end do
    end if
    
  end subroutine calc_depth
  ! ~~~~~ END CALC DEPTH ~~~~~


end program intergrate_layer_depth