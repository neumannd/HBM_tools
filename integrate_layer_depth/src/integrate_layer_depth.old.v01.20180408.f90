program intergrate_layer_depth

  ! use netcdf4_f03
  use netcdf
  use omp_lib
  
  ! NC_ENOTATT not found???? Would be -43

  implicit none
  
  ! INCLUDE 'netcdf.inc'
  
  integer(4)          :: argc                                      ! commandline argument count
  character (len=255) :: file_in, file_ot
  integer             :: n_lon, n_lat, n_time, n_depth             ! dimension size

! TODO: rename for IN and OUT file!
  integer             :: id_d_lon, id_d_lat, id_d_time, id_d_depth ! dimension id_s
  integer             :: id_v_lon, id_v_lat, id_v_time, id_v_depth ! variable id_s
  integer             :: id_v_h, id_v_real_depth ! further variable id_s
  
  REAL(8), dimension(:), allocatable       :: val_v_lon, val_v_lat,  &
                                              val_v_depth, val_v_time
  REAL(8), dimension(:,:,:,:), allocatable :: val_v_h, val_v_real_depth
  real(8)                                  :: fillval_h
  logical                                  :: fillval_h_bad
  
  integer             :: nf_stat         ! for error status of netCDF functions
  integer             :: ncid_in, ncid_ot  ! ncid for input and output
  
  
  ! get file_in and file_ot as input arguments
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
  
  
  ! READ OLD DATA
  nf_stat = NF90_OPEN(file_in, NF90_NOWRITE, ncid_in)
  call nf90_check_error(nf_stat)
  
  nf_stat = NF90_INQ_DIMID(ncid_in, 'lon', id_d_lon)
  call nf90_check_error(nf_stat, 'error inq dimid lon')
  nf_stat = NF90_INQ_DIMID(ncid_in, 'lat', id_d_lat)
  call nf90_check_error(nf_stat, 'error inq dimid lat')
  nf_stat = NF90_INQ_DIMID(ncid_in, 'depth', id_d_depth)
  call nf90_check_error(nf_stat, 'error inq dimid depth')
  nf_stat = NF90_INQ_DIMID(ncid_in, 'time', id_d_time)
  call nf90_check_error(nf_stat, 'error inq dimid time')
  
  nf_stat = NF90_INQ_VARID(ncid_in, 'h', id_v_h)
  call nf90_check_error(nf_stat, 'error inq varid h')
  nf_stat = NF90_INQ_VARID(ncid_in, 'lon', id_v_lon)
  call nf90_check_error(nf_stat, 'error inq varid lon')
  nf_stat = NF90_INQ_VARID(ncid_in, 'lat', id_v_lat)
  call nf90_check_error(nf_stat, 'error inq varid lat')
  nf_stat = NF90_INQ_VARID(ncid_in, 'depth', id_v_depth)
  call nf90_check_error(nf_stat, 'error inq varid depth')
  nf_stat = NF90_INQ_VARID(ncid_in, 'time', id_v_time)
  call nf90_check_error(nf_stat, 'error inq varid time')
  
  nf_stat = nf90_inquire_dimension(ncid_in, id_d_lon, len = n_lon)
  call nf90_check_error(nf_stat, 'error get dim lon')
  nf_stat = nf90_inquire_dimension(ncid_in, id_d_lat, len = n_lat)
  call nf90_check_error(nf_stat, 'error get dim lat')
  nf_stat = nf90_inquire_dimension(ncid_in, id_d_depth, len = n_depth)
  call nf90_check_error(nf_stat, 'error get dim depth')
  nf_stat = nf90_inquire_dimension(ncid_in, id_d_time, len = n_time)
  call nf90_check_error(nf_stat, 'error get dim time')
  
  ! allocate arrays
  allocate(val_v_lon(n_lon), val_v_lat(n_lat), val_v_time(n_time), &
           val_v_depth(n_depth), val_v_h(n_lon, n_lat, n_depth, n_time))
  
  ! get variable values
  nf_stat = NF90_GET_VAR(ncid_in, id_v_lon, val_v_lon)
  call nf90_check_error(nf_stat, 'error get var lon')
  nf_stat = NF90_GET_VAR(ncid_in, id_v_lat, val_v_lat)
  call nf90_check_error(nf_stat, 'error get var lat')
  nf_stat = NF90_GET_VAR(ncid_in, id_v_depth, val_v_depth)
  call nf90_check_error(nf_stat, 'error get var depth')
  nf_stat = NF90_GET_VAR(ncid_in, id_v_time, val_v_time)
  call nf90_check_error(nf_stat, 'error get var time')
  ! nf_stat = NF90_GET_VAR(ncid_in, id_v_h, val_v_h, start = (/1,1,1,1/), count = (/2,2,2,2/))
  nf_stat = NF90_GET_VAR(ncid_in, id_v_h, val_v_h)
  call nf90_check_error(nf_stat, 'error get var h')
  
  ! get fill value or missing value of variable 'h'
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
      call nf90_check_error(nf_stat, 'error get att missing_value of var h')
    end if
  else
    call nf90_check_error(nf_stat, 'error get att _FillValue of var h')
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
  
  ! create variables
  nf_stat = nf90_def_var(ncid_ot, 'lon', NF90_DOUBLE, (/id_d_lon/), id_v_lon)
  call nf90_check_error(nf_stat, 'error def var lon')
  nf_stat = nf90_def_var(ncid_ot, 'lat', NF90_DOUBLE, (/id_d_lat/), id_v_lat)
  call nf90_check_error(nf_stat, 'error def var lat')
  ! nf_stat = nf90_def_var(ncid_ot, 'depth', NF90_DOUBLE, (/id_d_depth/), id_v_depth)
  ! call nf90_check_error(nf_stat, 'error def var depth')
  nf_stat = nf90_def_var(ncid_ot, 'time', NF90_DOUBLE, (/id_d_time/), id_v_time)
  call nf90_check_error(nf_stat, 'error def var time')
  nf_stat = nf90_def_var(ncid_ot, 'depth', NF90_DOUBLE, &
                         (/id_d_time,id_d_depth,id_d_lat,id_d_lon/), &
                         id_v_real_depth)
  call nf90_check_error(nf_stat, 'error def var real_depth')
  nf_stat = nf90_def_var_deflate(ncid_ot, id_v_real_depth, 0, 1, 1)
  call nf90_check_error(nf_stat, 'error def var real_depth')
  
  ! copy attributes:
  nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'lon')
  call nf90_check_error(nf_stat, 'error copy atts lon')
  nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'lat')
  call nf90_check_error(nf_stat, 'error copy atts lat')
  nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'time')
  call nf90_check_error(nf_stat, 'error copy atts time')
  nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'depth')
  call nf90_check_error(nf_stat, 'error copy atts time')
  
  if ( .not. fillval_h_bad ) then
    nf_stat = nf90_put_att(ncid_ot, id_v_real_depth, '_FillValue', fillval_h)
    call nf90_check_error(nf_stat, 'error set att _FillValue of var depth')
  end if
  
  
  ! set global attributes
  nf_stat = nf90_global_atts(ncid_ot)
  call nf90_check_error(nf_stat, 'error copy atts global')
  
  ! leave definition mode
  nf_stat = NF90_ENDDEF(ncid_ot)
  call nf90_check_error(nf_stat, 'error end def mode')
  
  
  ! calculate real_depth
    ! allocate arrays
  allocate(val_v_real_depth(n_lon, n_lat, n_depth, n_time))
    ! call calculate routine
  if (fillval_h_bad) then
    call calc_depth(val_v_h, val_v_real_depth, &
                    (/n_lon, n_lat, n_depth, n_time/))
  else
    call calc_depth(val_v_h, val_v_real_depth, &
                    (/n_lon, n_lat, n_depth, n_time/), fillval_h)
  end if
  
  
  ! write variables
  nf_stat = NF90_PUT_VAR(ncid_ot, id_v_lon, val_v_lon)
  call nf90_check_error(nf_stat, 'error put var lon')
  nf_stat = NF90_PUT_VAR(ncid_ot, id_v_lat, val_v_lat)
  call nf90_check_error(nf_stat, 'error put var lat')
  nf_stat = NF90_PUT_VAR(ncid_ot, id_v_time, val_v_time)
  call nf90_check_error(nf_stat, 'error put var time')
  ! nf_stat = NF90_PUT_VAR(ncid, id_v_real_depth, )
  ! call nf90_check_error(nf_stat, 'error put var real_depth')
  
  ! close input file
  nf_stat = NF90_CLOSE(ncid_in)
  call nf90_check_error(nf_stat)
  
  ! close file
  nf_stat = NF90_CLOSE(ncid_ot)
  call nf90_check_error(nf_stat)
  
  
  ! deallocate arrays
  deallocate(val_v_lon, val_v_lat, val_v_time, val_v_depth, val_v_h)
  
  
  
  contains
  
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
  
  
  
  
    function nf90_copy_all_atts(ncid_in, ncid_ot, varname)
      
      implicit none
      
      integer,             intent(in) :: ncid_in, ncid_ot
      character (len=*), intent(in) :: varname
      
      integer             :: nf90_copy_all_atts
      integer             :: id_v_in, id_v_ot
      integer             :: nf_stat
      character (len=255) :: tmp_string
      
      ! in var id
      nf_stat = NF90_INQ_VARID(ncid_in, varname, id_v_in)
      call nf90_check_error(nf_stat)
      ! out var id
      nf_stat = NF90_INQ_VARID(ncid_ot, varname, id_v_ot)
      call nf90_check_error(nf_stat)
      
      !! go into definition mode
      !nf_stat = nf90_redef(ncid_ot)
      !call nf90_check_error(nf_stat)
      
      ! copy attributes
      nf_stat = nf90_get_att(ncid_in, id_v_in, 'units', tmp_string)
      call nf90_check_error(nf_stat, 'error copy/read attribute units of var '//trim(varname))
      nf_stat = nf90_put_att(ncid_ot, id_v_ot, 'units', tmp_string)
      call nf90_check_error(nf_stat, 'error copy/write attribute units of var '//trim(varname))
      
      nf_stat = nf90_get_att(ncid_in, id_v_in, 'standard_name', tmp_string)
      call nf90_check_error(nf_stat, 'error copy attribute standard_name of var '//trim(varname))
      nf_stat = nf90_put_att(ncid_ot, id_v_ot, 'standard_name', tmp_string)
      call nf90_check_error(nf_stat, 'error copy attribute standard_name of var '//trim(varname))
      
      nf_stat = nf90_get_att(ncid_in, id_v_in, 'long_name', tmp_string)
      call nf90_check_error(nf_stat, 'error copy attribute long_name of var '//trim(varname))
      nf_stat = nf90_put_att(ncid_ot, id_v_ot, 'long_name', tmp_string)
      call nf90_check_error(nf_stat, 'error copy attribute long_name of var '//trim(varname))
      
      !~ nf_stat = nf90_get_att(ncid_in, id_v_in, 'unit_long', tmp_string)
      !~ call nf90_check_error(nf_stat, 'error copy attribute unit_long of var '//trim(varname))
      !~ nf_stat = nf90_put_att(ncid_ot, id_v_ot, 'unit_long', tmp_string)
      !~ call nf90_check_error(nf_stat, 'error copy attribute unit_long of var '//trim(varname))
      
      nf_stat = nf90_get_att(ncid_in, id_v_in, 'axis', tmp_string)
      call nf90_check_error(nf_stat, 'error copy attribute axis of var '//trim(varname))
      nf_stat = nf90_put_att(ncid_ot, id_v_ot, 'axis', tmp_string)
      call nf90_check_error(nf_stat, 'error copy attribute axis of var '//trim(varname))
      
      if (trim(varname) .eq. 'depth') then
        nf_stat = nf90_get_att(ncid_in, id_v_in, 'positive', tmp_string)
        call nf90_check_error(nf_stat, 'error copy attribute positive of var '//trim(varname))
        nf_stat = nf90_put_att(ncid_ot, id_v_ot, 'positive', tmp_string)
        call nf90_check_error(nf_stat, 'error copy attribute positive of var '//trim(varname))
      end if
      
      !! leave definition mode
      !nf_stat = nf90_enddef(ncid)
      !call nf90_check_error(nf_stat)
      
      nf90_copy_all_atts = NF90_NOERR
      
    end function nf90_copy_all_atts


  
  
    function nf90_global_atts(ncid_ot)
      
      implicit none
      
      integer,             intent(in) :: ncid_ot
      
      integer             :: nf90_global_atts
      integer             :: nf_stat
      character (len=255) :: tmp_string
      
      
      !! go into definition mode
      !nf_stat = nf90_redef(ncid_ot)
      !call nf90_check_error(nf_stat)
      
      nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'TODO', 'CONTENT')
      call nf90_check_error(nf_stat)
      
      !! leave definition mode
      !nf_stat = nf90_enddef(ncid)
      !call nf90_check_error(nf_stat)
      
      nf90_global_atts = NF90_NOERR
      
    end function nf90_global_atts
    
    
    subroutine calc_depth(h, depth, count, fillval) 
      
      implicit none
      
      REAL(8), dimension(:,:,:,:), intent(in)  :: h
      REAL(8), dimension(:,:,:,:), intent(out) :: depth
      integer, DIMENSION(4),       intent(in)  :: count
      real(8), optional,           intent(in)  :: fillval
      
      integer :: iLo, iLa, iZ, iTi, nLo, nLa, nZ, nTi
      real(8) :: tmp_depth, tmp_h
      
      nLo = count(1)
      nLa = count(2)
      nZ  = count(3)
      nTi = count(4)
      
      depth(1:nLo, 1:nLa, 1, 1:nTi) = h(1:nLo, 1:nLa, 1, 1:nTi)
      
      if (.not. present(fillval)) then
        depth(1:nLo, 1:nLa, iZ, 1:nTi) = depth(1:nLo, 1:nLa, iZ-1, 1:nTi) + &
                                           h(1:nLo, 1:nLa, iZ, 1:nTi)
      else 
      
        do iTi = 1, nTi
          write(*,'(I4,I4)') iTi
          do iZ = 2, nZ
            ! write(*,'(I4,I4)') iTi, iZ
!$omp parallel do private(tmp_depth, tmp_h, iLa, iLo) shared(depth) num_threads( 4 )
            do iLa = 1, nLa
              ! write(*,'(I4,I4,I4)') iTi, iZ, iLa
              do iLo = 1, nLo
                ! write(*,'(I4,I4,I4,I4)') iTi, iZ, iLa, iLo
                
                tmp_depth = depth(iLo, iLa, iZ-1, iTi)
                tmp_h = h(iLo, iLa, iZ, iTi)
                
                if ( tmp_depth .eq. fillval ) then 
                  depth(iLo, iLa, iZ, iTi) = fillval
                else if ( tmp_h .eq. fillval ) then
                  depth(iLo, iLa, iZ, iTi) = fillval
                else 
                  depth(iLo, iLa, iZ, iTi) = tmp_depth + tmp_h
                endif
              end do
            end do
!$omp end parallel do
          end do
        end do
      end if
      
    end subroutine calc_depth

end program intergrate_layer_depth