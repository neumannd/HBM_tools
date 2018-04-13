module nf90_tools_calc_grid_cell_size
  
  use check_stat_netcdf
  use netcdf
  
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
    call check_nf90_stat(nf_stat, 'inq var_id of var '//varname)
    
    
    ! put attributes
    nf_stat = nf90_put_att(ncid, id_v, 'standard_name', 'cell_area')
    call check_nf90_stat(nf_stat, 'error put att standard_name of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'long_name', 'cell_area')
    call check_nf90_stat(nf_stat, 'error put att long_name of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'units', 'm2')
    call check_nf90_stat(nf_stat, 'error put att units of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'description', 'Cell_area is the '//&
                           'horizontal area of a grid-cell. Cell refers '//&
                           'to a model grid-cell.')
    call check_nf90_stat(nf_stat, 'error put att description of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, '_FillValue', real(fillval))
    call check_nf90_stat(nf_stat, 'error put att _FillValue of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'missing_value', real(fillval))
    call check_nf90_stat(nf_stat, 'error put att missing_value of var '//trim(varname))
    
    
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
    call check_nf90_stat(nf_stat, 'inq var_id of var '//varname)
    
    
    ! put attributes
    nf_stat = nf90_put_att(ncid, id_v, 'standard_name', 'cell_volumne')
    call check_nf90_stat(nf_stat, 'error put att standard_name of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'long_name', 'cell_volume')
    call check_nf90_stat(nf_stat, 'error put att long_name of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'units', 'm3')
    call check_nf90_stat(nf_stat, 'error put att units of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'description', 'Cell_volume is the '//&
                           '3-dim volume of a grid-cell. Cell refers to a '//&
                           'model grid-cell.')
    call check_nf90_stat(nf_stat, 'error put att description of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, '_FillValue', real(fillval))
    call check_nf90_stat(nf_stat, 'error put att _FillValue of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'missing_value', real(fillval))
    call check_nf90_stat(nf_stat, 'error put att missing_value of var '//trim(varname))
    
    
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
    call check_nf90_stat(nf_stat, 'inq var_id of var '//varname)
    
    
    ! put attributes
    nf_stat = nf90_put_att(ncid, id_v, 'standard_name', 'cell_thickness')
    call check_nf90_stat(nf_stat, 'error put att standard_name of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'long_name', 'cell_thickness')
    call check_nf90_stat(nf_stat, 'error put att long_name of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'units', 'm')
    call check_nf90_stat(nf_stat, 'error put att units of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'description', 'Thickness means '//&
                           'the vertical extent of a layer. Cell refers '//&
                           'to a model grid-cell.')
    call check_nf90_stat(nf_stat, 'error put att description of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, '_FillValue', real(fillval))
    call check_nf90_stat(nf_stat, 'error put att _FillValue of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'missing_value', real(fillval))
    call check_nf90_stat(nf_stat, 'error put att missing_value of var '//trim(varname))
    
    
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
    call check_nf90_stat(nf_stat, 'inq var_id of var '//varname)
    
    
    ! put attributes
    nf_stat = nf90_put_att(ncid, id_v, 'standard_name', 'land_binary_mask')
    call check_nf90_stat(nf_stat, 'error put att standard_name of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'long_name', 'land_binary_mask')
    call check_nf90_stat(nf_stat, 'error put att long_name of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'units', '1')
    call check_nf90_stat(nf_stat, 'error put att units of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'description', 'X_binary_mask has '//&
                           '1 where condition X is met, 0 elsewhere. 1 = '//&
                           'land, 0 = sea.')
    call check_nf90_stat(nf_stat, 'error put att description of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, '_FillValue', fillval)
    call check_nf90_stat(nf_stat, 'error put att _FillValue of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, id_v, 'missing_value', fillval)
    call check_nf90_stat(nf_stat, 'error put att missing_value of var '//trim(varname))
    
    
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
    !call check_nf90_stat(nf_stat)
    
    
    ! set global attributes
    nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'Conventions', 'CF-1.7')
    call check_nf90_stat(nf_stat, 'put global att Conventions')
    
    nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'source', &
                           'HIROMB-BOOS-model input')
    call check_nf90_stat(nf_stat, 'put global att source')
    
    nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'title', &
                           'cell area and land-sea mask for HBM model grid')
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

end module nf90_tools_calc_grid_cell_size