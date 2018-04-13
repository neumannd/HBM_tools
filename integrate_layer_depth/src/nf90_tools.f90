module nf90_tools

  use netcdf
  use check_stat_netcdf
  
  implicit none
  

contains
  
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
    call check_nf90_stat(nf_stat, 'inq var_id of var '//varname_in)
    ! out var id
    nf_stat = NF90_INQ_VARID(ncid_ot, varname_ot, id_v_ot)
    call check_nf90_stat(nf_stat, 'inq var_id of var '//varname_ot)
    
    
    !! GO INTO DEFINITION MODE
    !! we assume that we are still in the definition mode
    ! nf_stat = nf90_redef(ncid_ot)
    ! call check_nf90_stat(nf_stat)
    
    
    ! copy attributes
    nf_stat = nf90_copy_att(ncid_in, id_v_in, 'units', ncid_ot, id_v_ot)
    call check_nf90_stat(nf_stat, 'error copy attribute units from var '//&
                          trim(varname_in)//' to var '//trim(varname_ot))
                          
    nf_stat = nf90_copy_att(ncid_in, id_v_in, 'standard_name', ncid_ot, id_v_ot)
    call check_nf90_stat(nf_stat, 'error copy attribute standard_name from var '//&
                          trim(varname_in)//' to var '//trim(varname_ot))
                          
    nf_stat = nf90_copy_att(ncid_in, id_v_in, 'long_name', ncid_ot, id_v_ot)
    call check_nf90_stat(nf_stat, 'error copy attribute long_name from var '//&
                          trim(varname_in)//' to var '//trim(varname_ot))
                          
    nf_stat = nf90_copy_att(ncid_in, id_v_in, 'axis', ncid_ot, id_v_ot)
    call check_nf90_stat(nf_stat, 'error copy attribute axis from var '//&
                          trim(varname_in)//' to var '//trim(varname_ot))
    
    
    if (trim(varname_in) .eq. 'depth') then
      nf_stat = nf90_copy_att(ncid_in, id_v_in, 'positive', ncid_ot, id_v_ot)
      call check_nf90_stat(nf_stat, 'error copy attribute positive from var '//&
                            trim(varname_in)//' to var '//trim(varname_ot))
    end if
    
    
    !! LEAVE DEFINITION MODE
    ! nf_stat = nf90_enddef(ncid)
    ! call check_nf90_stat(nf_stat)
    
    
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
    ! call check_nf90_stat(nf_stat)
    
    
    ! copy attributes
    nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'longitude_min', ncid_ot, NF90_GLOBAL)
    call check_nf90_stat(nf_stat, 'error copy global attribute longitude_min')
                          
    nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'longitude_max', ncid_ot, NF90_GLOBAL)
    call check_nf90_stat(nf_stat, 'error copy global attribute longitude_max')
                          
    nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'latitude_min', ncid_ot, NF90_GLOBAL)
    call check_nf90_stat(nf_stat, 'error copy global attribute latitude_min')
                          
    nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'latitude_max', ncid_ot, NF90_GLOBAL)
    call check_nf90_stat(nf_stat, 'error copy global attribute latitude_max')
                          
    nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'depth_min', ncid_ot, NF90_GLOBAL)
    call check_nf90_stat(nf_stat, 'error copy global attribute depth_min')
                          
    nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'depth_max', ncid_ot, NF90_GLOBAL)
    call check_nf90_stat(nf_stat, 'error copy global attribute depth_max')
    
    
    !! LEAVE DEFINITION MODE
    ! nf_stat = nf90_enddef(ncid)
    ! call check_nf90_stat(nf_stat)
    
    
    ! SET RETURN VALUE
    nf90_copy_global_atts = NF90_NOERR
    
    
  end function nf90_copy_global_atts
  ! ~~~~~ END COPY ALL ATTRIBUTES OF THE COORDINATE VARIABLES ~~~~~
  
  
  
  ! ~~~~~ WRITE ATTS bathymetry ~~~~~
  function nf90_set_atts_bathymetry(ncid, varname, fillval)
    
    implicit none
    
    integer,           intent(in) :: ncid
    character (len=*), intent(in) :: varname
    real(8),           intent(in) :: fillval
    
    
    integer :: id_v, nf_stat
    integer :: nf90_set_atts_bathymetry
    
    
    ! out var id
    nf_stat = NF90_INQ_VARID(ncid, varname, id_v)
    call check_nf90_stat(nf_stat, 'inq var_id of var '//varname)
    
    
    ! put attributes
    nf_stat = nf90_put_att(ncid, id_v, 'standard_name', 'sea_floor_depth_below_sea_surface')
    call check_nf90_stat(nf_stat, 'error put att standard_name of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'long_name', 'sea_floor_depth_below_sea_surface')
    call check_nf90_stat(nf_stat, 'error put att long_name of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'units', 'm')
    call check_nf90_stat(nf_stat, 'error put att units of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'description', 'The '//&
                           'sea_floor_depth_below_sea_surface is the '//&
                           'vertical distance between the sea surface and '//&
                           'the seabed as measured at a given point in '//&
                           'space including the variance caused by tides '//&
                           'and possibly waves.')
    call check_nf90_stat(nf_stat, 'error put att description of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, '_FillValue', real(fillval))
    call check_nf90_stat(nf_stat, 'error put att _FillValue of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'missing_value', real(fillval))
    call check_nf90_stat(nf_stat, 'error put att missing_value of var '//trim(varname))
    
    
    nf90_set_atts_bathymetry = NF90_NOERR
    
  end function nf90_set_atts_bathymetry
  ! ~~~~~ END WRITE ATTS bathymetry ~~~~~
  
  
  
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
    !call check_nf90_stat(nf_stat)
    
    
    ! set global attributes
    nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'Conventions', 'CF-1.7')
    call check_nf90_stat(nf_stat, 'put global att Conventions')
    
    nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'source', &
                           'HIROMB-BOOS-model input')
    call check_nf90_stat(nf_stat, 'put global att source')
    
    nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'title', &
                           'depth of HBM model grid cells')
    call check_nf90_stat(nf_stat, 'put global att title')
    
    nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'institution', 'BSH and IOW')
    call check_nf90_stat(nf_stat, 'put global att institution')
    
    nf_stat = nf90_inquire_attribute(ncid_in, NF90_GLOBAL, 'history', len=tmp_int)
    call check_nf90_stat(nf_stat, 'inq global att history')
    allocate(character(len=tmp_int) :: tmp_string_dyn)
    nf_stat = nf90_get_att(ncid_in, NF90_GLOBAL, 'history', tmp_string_dyn)
    call check_nf90_stat(nf_stat, 'get global att history')
    nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'history', trim(tmp_string_dyn)//';   '//history)
    call check_nf90_stat(nf_stat, 'put global att history')
    
    nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'comment', &
                           'created by Daniel Neumann (IOW) from HBM layer'//&
                           ' thickness model output data provided by BSH'//&
                           ' and DMI')
    call check_nf90_stat(nf_stat, 'put global att comment')
    
    
    !! leave definition mode
    !nf_stat = nf90_enddef(ncid)
    !call check_nf90_stat(nf_stat)
    
    nf90_set_global_atts = NF90_NOERR
    
  end function nf90_set_global_atts
  ! ~~~~~ END SET GLOBAL ATTRIBUTES ~~~~~

end module nf90_tools