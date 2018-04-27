module nf90_var_data

  use netcdf
  use check_stat_netcdf
  use time_tools
  
  implicit none
  private
  
  
  
  interface get_nf90_var_data
  
    ! if we don't have an open netCDF file
    module procedure get_nf90_var_data_name
    ! if we have an open netCDF file
    module procedure get_nf90_var_data_id
  
  end interface get_nf90_var_data
  
  
  
  interface put_nf90_var_data
  
    ! if we don't have an open netCDF file
    module procedure put_nf90_var_data_name
    ! if we have an open netCDF file
    module procedure put_nf90_var_data_id
  
  end interface put_nf90_var_data
  
  
  
  interface append_NF90_attribute
    module procedure append_NF90_attribute_char
  end interface append_NF90_attribute
  
  
  
  interface get_NF90_fillvalue
    module procedure get_NF90_fillvalue_real8
  end interface get_NF90_fillvalue
  
  
  interface copy_nf90_variable_val
    module procedure copy_nf90_variable_val_start
    module procedure copy_nf90_variable_val_float
    module procedure copy_nf90_variable_val_double
    module procedure copy_nf90_variable_val_byte
    module procedure copy_nf90_variable_val_short
    module procedure copy_nf90_variable_val_integer
  end interface copy_nf90_variable_val
  
  
  PUBLIC :: get_nf90_var_data, put_nf90_var_data, copy_nf90_file_def, &
            append_NF90_attribute, get_NF90_fillvalue, nf90_set_global_atts, &
            nf90_copy_global_atts, copy_nf90_variable_val
  private :: get_nf90_var_data_name, get_nf90_var_data_id,     &
             put_nf90_var_data_name, put_nf90_var_data_id,     &
             copy_nf90_variable_def, copy_nf90_dimensions,     &
             append_NF90_attribute_char, copy_nf90_dim_variable_def, &
             get_NF90_fillvalue_real8, &
             copy_nf90_variable_val_start, copy_nf90_variable_val_float, &
             copy_nf90_variable_val_double, copy_nf90_variable_val_byte, &
             copy_nf90_variable_val_short, copy_nf90_variable_val_integer
  
  
contains

  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_file_def ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine copy_nf90_file_def(fname_in, fname_ot, ncid_in, ncid_ot,     &
                                vars_in, vars_ot, n_vars, start, count,   &
                                leave_open_in, leave_open_ot, var_comment)
    
    character(len=*),                 intent(in)  :: fname_in, fname_ot
    integer,                          intent(out) :: ncid_in, ncid_ot
    character(len=*), dimension(:),   intent(in)  :: vars_in ! size: n_vars(1)
    character(len=*), dimension(:,:), intent(in)  :: vars_ot ! size: n_vars(1) x n_vars(2)
    integer,          dimension(2),   intent(in)  :: n_vars
    integer,          dimension(4),   intent(in)  :: start, count
    logical, optional,                intent(in)  :: leave_open_in, &
                                                      leave_open_ot
    character(len=*), dimension(:),   intent(in)  :: var_comment ! size: n_vars(2)
    
    
    ! iterators
    integer :: ivar
    
    ! netCDF ids
    integer, dimension(:), allocatable :: id_d_ot
    
    ! variables for timestamp setting 
    character (len=20) :: str_time_stamp
    
    ! control parameters
    logical :: leave_open_in_loc, leave_open_ot_loc
    
    ! status
    integer :: nf_stat
    
    
    
    ! set default value for optional arguments
    if (.not. present(leave_open_in)) then
      leave_open_in_loc = .false.
    else
      leave_open_in_loc = leave_open_in
    end if
    
    if (.not. present(leave_open_ot)) then
      leave_open_ot_loc = .false.
    else
      leave_open_ot_loc = leave_open_ot
    end if
    
    ! get time stamp
    call generate_time_stamp(str_time_stamp)
    
    
    ! write(*,*) ' ~~~~ start copying netCDF file ~~~~'
    ! open netCDF input file
    nf_stat = NF90_OPEN(fname_in, NF90_NOWRITE, ncid_in)
    call check_nf90_stat(nf_stat, 'opening input file '//trim(fname_in))
    
    
    ! create new file
    nf_stat = NF90_CREATE(fname_ot, NF90_HDF5, ncid_ot)
    call check_nf90_stat(nf_stat, 'creating output file '//trim(fname_ot))
    
    
    ! copy dimensions
    !! DON'T COPY DIMENSIONS WITH START == -1!!!
    ! write(*,*) ' ~~~~ copy dimensions ~~~~'
    call copy_nf90_dimensions(ncid_in, ncid_ot, start, count, id_d_ot)
    
    
    ! copy variable definitions
    ! write(*,*) ' ~~~~ copy variables ~~~~'
    do ivar = 1, n_vars(1)
      
      ! write(*,*) 'copying variable '//vars_in(ivar)
      call copy_nf90_variable_def(ncid_in, ncid_ot, vars_in(ivar),          &
                                  vars_ot(ivar, :), n_vars(2), id_d_ot,     &
                                  comments = var_comment, new_units = 'mol',&
                                  new_type = NF90_DOUBLE)
    
    end do
    
    
    ! leave definition mode
    ! write(*,*) ' ~~~~ leave definition mode ~~~~'
    nf_stat = NF90_ENDDEF(ncid_ot)
    call check_nf90_stat(nf_stat, 'leaving definition mode of file '//trim(fname_ot))
    
    
    !~ ! write(*,*) ' ~~~~ copy dimensional variables ~~~~'
    !~ call copy_nf90_dimensional_variables(ncid_in, ncid_ot, start, count, id_d_ot)
    
    
    ! sync output file
    nf_stat = NF90_SYNC(ncid_ot)
    call check_nf90_stat(nf_stat, 'syncing file '//trim(fname_ot))
    
    
    ! close input file
    if (leave_open_in) then
      write(*,*) 'NOTE: not closing input file '//trim(fname_in)
    else
      ! write(*,*) 'closing input file '//fname_in
      nf_stat = NF90_CLOSE(ncid_in)
      call check_nf90_stat(nf_stat, 'closing file '//trim(fname_in))
    end if
    
    ! close output file
    if (leave_open_ot) then
      write(*,*) 'NOTE: not closing output file '//trim(fname_ot)
    else
      ! write(*,*) 'closing output file '//fname_ot
      nf_stat = NF90_CLOSE(ncid_ot)
      call check_nf90_stat(nf_stat, 'closing file '//trim(fname_ot))
    end if
    
    
    ! deallocate variables
    deallocate(id_d_ot)
    
    
  end subroutine copy_nf90_file_def
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_file_def ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_nf90_var_data_name ~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine get_nf90_var_data_name(fname, vname, vals, start, count, fillval)
  
    character(len=*),                         intent(in)    :: fname, vname
    real(8), dimension(:,:,:,:), allocatable, intent(out)   :: vals
    integer, dimension(4),                    intent(inout) :: start, count
    real(8), optional,                        intent(out)   :: fillval
    
    
    ! netCDF status
    integer :: nf_stat
    
    ! netCDF id
    integer :: ncid
    
    
    
    ! open file
    nf_stat = NF90_OPEN(fname, NF90_NOWRITE, ncid)
    call check_nf90_stat(nf_stat, 'opening mask file '//fname)
    
    if (present(fillval)) then
      call get_nf90_var_data_id(ncid, vname, vals, start, count, fillval)
    else
      call get_nf90_var_data_id(ncid, vname, vals, start, count)
    end if
    
    ! close file
    nf_stat = NF90_CLOSE(ncid)
    call check_nf90_stat(nf_stat, 'closing mask file')
    
    
  end subroutine get_nf90_var_data_name
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_nf90_var_data_name ~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_nf90_var_data_id ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine get_nf90_var_data_id(ncid, vname, vals, start, count, fillval)
  
    integer,                                  intent(in)    :: ncid
    character(len=*),                         intent(in)    :: vname
    real(8), dimension(:,:,:,:), allocatable, intent(out)   :: vals
    integer, dimension(4),                    intent(inout) :: start, count
    real(8), optional,                        intent(out)   :: fillval
    
    
    ! netCDF status
    integer :: nf_stat
    
    ! netCDF id
    integer :: id_d_lon, id_d_lat, id_d_z, id_d_time ! dimension id_s
    integer :: id_v_data                             ! further variable id_s
  
    ! netCDF dim sizes
    integer :: n_lon, n_lat, n_z, n_time
    
    ! iterators
    integer :: i
    
    ! help variables
    integer, dimension(4) :: last
    
    
    
    ! get dimensions
    nf_stat = NF90_INQ_DIMID(ncid, 'lon', id_d_lon)
    call check_nf90_stat(nf_stat, 'inq dim id lon')
    nf_stat = NF90_INQ_DIMID(ncid, 'lat', id_d_lat)
    call check_nf90_stat(nf_stat, 'inq dim id lat')
    nf_stat = NF90_INQ_DIMID(ncid, 'depth', id_d_z)
    call check_nf90_stat(nf_stat, 'inq dim id depth')
    nf_stat = NF90_INQ_DIMID(ncid, 'time', id_d_time)
    call check_nf90_stat(nf_stat, 'inq dim id time')
    
    nf_stat = NF90_INQUIRE_DIMENSION(ncid, id_d_lon, len = n_lon)
    call check_nf90_stat(nf_stat, 'get dim length lon')
    nf_stat = NF90_INQUIRE_DIMENSION(ncid, id_d_lat, len = n_lat)
    call check_nf90_stat(nf_stat, 'get dim length lat')
    nf_stat = NF90_INQUIRE_DIMENSION(ncid, id_d_z, len = n_z)
    call check_nf90_stat(nf_stat, 'get dim length depth')
    nf_stat = NF90_INQUIRE_DIMENSION(ncid, id_d_time, len = n_time)
    call check_nf90_stat(nf_stat, 'get dim length time')
    
    ! set initial values for 'last'-element array
    !  count = last - start + 1
    last(1) = n_lon
    last(2) = n_lat
    last(3) = n_z
    last(4) = n_time
    
    ! set missing values in start and count
    !  and re-set last
    do i = 1, 4
      ! if missing value in start => set it and that of count
      if(start(i) == -1) then
        start(i) = 1
        count(i) = last(i) - start(i) + 1
      end if
      
      ! if missing value in count => set it
      if(count(i) == -1) then
        count(i) = last(i) - start(i) + 1
      end if
      
      ! set last according to pre-set start and count values
      if (start(i) + count(i) - 1 > last(i)) then
        write(*,'(A40,I1,A18)') "Error: start(i) + count(i) - 1 with i = ", i, " exceeds last(i): "
        write(*,'(A3,I6,A3,I6)') "   ", start(i) + count(i) - 1, " > ", last(i)
        stop
      else 
        last(i) = start(i) + count(i) - 1
      end if
    end do
    
    ! allocate variable
    allocate(vals(count(1), count(2), count(3), count(4)))
    
    ! get variable
    nf_stat = NF90_INQ_VARID(ncid, vname, id_v_data)
    call check_nf90_stat(nf_stat, 'inq var id '//trim(vname))
    nf_stat = NF90_GET_VAR(ncid, id_v_data, vals, start = start, count = count)
    call check_nf90_stat(nf_stat, 'get var '//trim(vname))
    
    ! get fill value
    if (present(fillval)) then
      nf_stat = get_NF90_fillvalue_real8(ncid, id_v_data, fillval)
      call check_nf90_stat(nf_stat, 'get fill value of '//trim(vname))
    end if
    
    
  end subroutine get_nf90_var_data_id
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_nf90_var_data_id ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ put_nf90_var_data_name ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine put_nf90_var_data_name(fname, vname, vals, start, count)
    character(len=*),                       intent(in) :: fname, vname
    real(8), dimension(:,:,:), allocatable, intent(in) :: vals
    integer, dimension(3),                  intent(in) :: start, count
    
    
    ! netCDF status
    integer :: nf_stat
    
    ! netCDF id
    integer :: ncid
    
    
    
    ! open file
    nf_stat = NF90_OPEN(fname, NF90_WRITE, ncid)
    call check_nf90_stat(nf_stat, 'opening mask file '//trim(fname))
    
    call put_nf90_var_data_id(ncid, vname, vals, start, count)
    
    ! close file
    nf_stat = NF90_CLOSE(ncid)
    call check_nf90_stat(nf_stat, 'closing mask file')
    
    
  end subroutine put_nf90_var_data_name
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ put_nf90_var_data_name ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ put_nf90_var_data_id ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine put_nf90_var_data_id(ncid, vname, vals, start, count)
    integer,                                intent(in) :: ncid
    character(len=*),                       intent(in) :: vname
    real(8), dimension(:,:,:), allocatable, intent(in) :: vals
    integer, dimension(3),                  intent(in) :: start, count
    
    
    ! netCDF status
    integer :: nf_stat
    
    ! netCDF ids
    integer :: id_v_data
    
    
    nf_stat = NF90_inq_varid(ncid, vname, id_v_data)
    call check_nf90_stat(nf_stat, 'inquire var id for variable '//trim(vname))
    
    nf_stat = NF90_put_var(ncid, id_v_data, vals, start = start, count = count)
    call check_nf90_stat(nf_stat, 'put var data for variable '//trim(vname))
    
    
  end subroutine put_nf90_var_data_id
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ put_nf90_var_data_id ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_dimensions ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !! DON'T COPY DIMENSIONS WITH START == -1!!!
  subroutine copy_nf90_dimensions(ncid_in, ncid_ot, start, count, id_d_ot)
    
    integer,                            intent(in)  :: ncid_in, ncid_ot
    integer, dimension(4),              intent(in)  :: start, count
    integer, dimension(:), allocatable, intent(out) :: id_d_ot
    
    ! status variables
    integer :: nf_stat
    
    ! dimension information
    character(len=255), dimension(4)              :: dim_names   ! names of dimensions
    integer,            dimension(4)              :: dim_lengths ! length of each dimension
    integer                                       :: n_d_ot      ! number of output dimensions
    integer,            dimension(4)              :: id_d_in2ot
    integer,            dimension(:), allocatable :: id_d_ot2in
    
    ! local modified variables of input parameters
    integer, dimension(4) :: count_loc
    
    ! iterators
    integer :: i_in, i_ot
    
    
    ! copy input parameter
    count_loc = count
    
    
    ! count number of output dimensions and allocate arrays
    n_d_ot = 0
    do i_in = 1, 4
      if (start(i_in) .ne. -1) n_d_ot = n_d_ot + 1
    end do
    allocate(id_d_ot(n_d_ot), id_d_ot2in(n_d_ot))
    
    
    ! get and copy dimensions
    i_ot = 1
    id_d_in2ot = -1
    do i_in = 1, 4
    
      if (start(i_in) .ne. -1) then
        ! fill mapping variables
        id_d_in2ot(i_in) = i_ot
        id_d_ot2in(i_ot) = i_in
      
        ! GET DIMs
        nf_stat = NF90_INQUIRE_DIMENSION(ncid_in, i_in, dim_names(i_in), dim_lengths(i_in))
        call check_nf90_stat(nf_stat, 'error inq dim info')
        
        ! look if count needs to be set
        if (count_loc(i_in) .eq. -1) count_loc(i_in) = dim_lengths(i_in)
        
        ! DEF DIMs
        nf_stat = NF90_DEF_DIM(ncid_ot, dim_names(i_in), count_loc(i_in), id_d_ot(i_ot))
        
        ! increment i_ot
        i_ot = i_ot + 1
      end if
      
    end do
    
    ! get and copy dimensional variables
    do i_ot = 1, n_d_ot
      call copy_nf90_dim_variable_def(ncid_in, ncid_ot,            &
                                      dim_names(id_d_ot2in(i_ot)), &
                                      id_d_in2ot)
      nf_stat = NF90_ENDDEF(ncid_ot)
      call check_nf90_stat(nf_stat, 'leaving definition mode to copy dim '//&
                                    'var '//trim(dim_names(id_d_ot2in(i_ot))))
      call copy_nf90_dim_variable_val(ncid_in, ncid_ot,            &
                                      dim_names(id_d_ot2in(i_ot)), &
                                      id_d_in2ot, start, count)
      nf_stat = NF90_REDEF(ncid_ot)
      call check_nf90_stat(nf_stat, 'leaving definition mode to copy dim '//&
                                    'var '//trim(dim_names(id_d_ot2in(i_ot))))
    end do
    
    
    ! deallocate variables
    deallocate(id_d_ot2in)
    
    
  end subroutine copy_nf90_dimensions
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_dimensions ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  !~ ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_dimensional_variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~ subroutine copy_nf90_dimensional_variables(ncid_in, ncid_ot, start, count, id_d_ot)
  
  
  
  
  
  !~ end subroutine copy_nf90_dimensional_variables
  !~ ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_dimensional_variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_dim_variable_def ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine copy_nf90_dim_variable_def(ncid_in, ncid_ot, dimname, id_d_in2ot)
    
    integer,                        intent(in) :: ncid_in, ncid_ot
    character(len=*),               intent(in) :: dimname
    integer,          dimension(4), intent(in) :: id_d_in2ot
    
    
    ! status
    integer :: nf_stat
    
    ! netCDF ids
    integer                            :: id_v, n_dims_in, n_dims_ot
    integer, dimension(:), allocatable :: id_dims_in, id_dims_ot
    
    ! iterators
    integer :: i1, i2
    
    
    ! get var id
    nf_stat = NF90_INQ_VARID(ncid_in, dimname, id_v)
    call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(dimname))
    
    ! get dim-info of provided variable
    nf_stat = NF90_INQUIRE_VARIABLE(ncid_in, id_v, ndims = n_dims_in)
    call check_nf90_stat(nf_stat, 'error inq dimids of var '//trim(dimname))
    allocate(id_dims_in(n_dims_in))
    nf_stat = NF90_INQUIRE_VARIABLE(ncid_in, id_v, dimids = id_dims_in)
    call check_nf90_stat(nf_stat, 'error inq dimids of var '//trim(dimname))
    
    ! look, which dim-ids the output variable should have 
    n_dims_ot = 0
    do i1 = 1, n_dims_in
      if(id_d_in2ot(id_dims_in(i1)) .ne. -1) n_dims_ot = n_dims_ot + 1
    end do
    
    allocate(id_dims_ot(n_dims_ot))
    
    i2 = 1
    do i1 = 1, n_dims_in
      if(id_d_in2ot(id_dims_in(i1)) .ne. -1) then
        id_dims_ot(i2) = id_d_in2ot(id_dims_in(i1))
        i2 = i2 + 1
      end if
    end do
    
    
    ! copy the dimensional variable
    call copy_nf90_variable_def(ncid_in, ncid_ot, dimname, (/dimname/), 1, id_dims_ot)
    
    
    ! deallocate variables
    deallocate(id_dims_in, id_dims_ot)
    
    
  end subroutine copy_nf90_dim_variable_def
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_dim_variable_def ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_dim_variable_val ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine copy_nf90_dim_variable_val(ncid_in, ncid_ot, dimname, id_d_in2ot, &
                                        start, count)
    
    integer,                        intent(in) :: ncid_in, ncid_ot
    character(len=*),               intent(in) :: dimname
    integer,          dimension(4), intent(in) :: id_d_in2ot, start, count
    
    
    ! status
    integer :: nf_stat
    
    ! netCDF ids
    integer                            :: id_v_in, id_v_ot, n_dims_in, n_dims_ot
    integer, dimension(:), allocatable :: start_ot, count_ot, start_in, count_in
    integer, dimension(:), allocatable :: id_dims_in, id_dims_ot
    
    ! iterators
    integer :: i
    
    ! variable info
    integer :: xtype_in
    
    
    ! get var id
    nf_stat = NF90_INQ_VARID(ncid_in, dimname, id_v_in)
    call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(dimname))
    nf_stat = NF90_INQ_VARID(ncid_ot, dimname, id_v_ot)
    call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(dimname))
    
    ! get dim-info provided variable in input file
    nf_stat = NF90_INQUIRE_VARIABLE(ncid_in, id_v_in, ndims = n_dims_in)
    call check_nf90_stat(nf_stat, 'error inq dimids of var '//trim(dimname))
    allocate(id_dims_in(n_dims_in))
    nf_stat = NF90_INQUIRE_VARIABLE(ncid_in, id_v_in, dimids = id_dims_in)
    call check_nf90_stat(nf_stat, 'error inq dimids of var '//trim(dimname))
    
    ! get dim-info provided variable in output file
    nf_stat = NF90_INQUIRE_VARIABLE(ncid_ot, id_v_ot, ndims = n_dims_ot)
    call check_nf90_stat(nf_stat, 'error inq dimids of var '//trim(dimname))
    allocate(id_dims_ot(n_dims_ot))
    nf_stat = NF90_INQUIRE_VARIABLE(ncid_ot, id_v_ot, dimids = id_dims_ot)
    call check_nf90_stat(nf_stat, 'error inq dimids of var '//trim(dimname))
    
    ! create start and count arrays 
    allocate(start_in(n_dims_in), count_in(n_dims_in), &
              start_ot(n_dims_ot), count_ot(n_dims_ot))
    
    start_ot = 1
    do i = 1, n_dims_ot
      nf_stat = NF90_INQUIRE_DIMENSION(ncid_ot, id_dims_ot(i), len = count_ot(i))
      call check_nf90_stat(nf_stat, 'error inq dim len of one dim of var '//trim(dimname))
    end do
    
    do i = 1, n_dims_in
      start_in(i) = start(id_dims_in(i))
      count_in(i) = count(id_dims_in(i))
      if (count_in(i) .eq. -1) then
        nf_stat = NF90_INQUIRE_DIMENSION(ncid_in, id_dims_in(i), len = count_in(i))
        call check_nf90_stat(nf_stat, 'error inq dim len of one dim of var '//trim(dimname))
      end if
    end do
    
    ! copy the dimensional variable
    call copy_nf90_variable_val(ncid_in, ncid_ot, dimname, (/dimname/), 1, &
                                start_in, count_in, n_dims_in,  &
                                start_ot, count_ot, n_dims_ot)
    
    
  end subroutine copy_nf90_dim_variable_val
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_dim_variable_val ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_def ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine copy_nf90_variable_def(ncid_in, ncid_ot, var_in, vars_ot, &
                                n_vars, id_d_ot, comments, new_units, new_type)
    
    integer,                                  intent(in) :: ncid_in, ncid_ot
    character(len=*),                         intent(in) :: var_in
    character(len=*), dimension(:),           intent(in) :: vars_ot  ! size: n_vars
    integer,                                  intent(in) :: n_vars
    integer,          dimension(:),           intent(in) :: id_d_ot
    character(len=*), dimension(:), optional, intent(in) :: comments ! size: n_vars
    character(len=*),               optional, intent(in) :: new_units
    integer,                        optional, intent(in) :: new_type
    
    
    ! iterators
    integer :: icopy, iatt
    
    ! status
    integer :: nf_stat
    
    ! netCDF ids
    integer :: id_v_in, id_v_ot
    
    ! variable and attribute information
    integer            :: xtype_in, xtype_ot, n_atts, id_comment_att
    character(len=255) :: att_name
    
    
    
    ! inquire variable information
    nf_stat = NF90_INQ_VARID(ncid_in, var_in, id_v_in)
    call check_nf90_stat(nf_stat, 'error inq var id of var '//var_in)
    nf_stat = NF90_INQUIRE_VARIABLE(ncid_in, id_v_in, xtype = xtype_in, nAtts = n_atts)
    call check_nf90_stat(nf_stat, 'error inq var infos of var '//var_in)
    
    xtype_ot = xtype_in
    if (present(new_type)) xtype_ot = new_type
    
    do icopy = 1, n_vars
      
      ! define variable
      nf_stat = NF90_DEF_VAR(ncid_ot, vars_ot(icopy), xtype_ot, id_d_ot, id_v_ot)
      call check_nf90_stat(nf_stat, 'error defining var '//vars_ot(icopy))
      nf_stat = nf90_def_var_deflate(ncid_ot, id_v_ot, 0, 1, 1)
      call check_nf90_stat(nf_stat, 'error def deflate var '//vars_ot(icopy))
      
      ! copy attributes
      do iatt = 1, n_atts
        nf_stat = NF90_INQ_ATTNAME(ncid_in, id_v_in, iatt, att_name)
        call check_nf90_stat(nf_stat, 'error inq att name')
        
        if ( ((trim(att_name) .eq. '_FillValue') .or. &
              (trim(att_name) .eq. 'missing_value')) .and. &
             present(new_type) ) then
          call copy_NF90_att_new_fillval(ncid_in, id_v_in, att_name, &
                                         ncid_ot, id_v_ot, xtype_in, &
                                         new_type)
        else 
          nf_stat = NF90_COPY_ATT(ncid_in, id_v_in, att_name, ncid_ot, id_v_ot)
          call check_nf90_stat(nf_stat, 'error copying att '//trim(att_name))
        end if
      end do
      
      if (present(comments)) then
        call append_NF90_attribute(ncid_ot, id_v_ot, 'comment', comments(icopy))
      end if
      
      if (present(new_units)) then
        nf_stat = NF90_PUT_ATT(ncid_ot, id_v_ot, 'units', new_units)
          call check_nf90_stat(nf_stat, 'error put units '//trim(new_units))
      end if
      
    end do
    
  end subroutine copy_nf90_variable_def
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_def ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_val_start ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine copy_nf90_variable_val_start(ncid_in, ncid_ot, var_in, vars_ot, n_vars, &
                                    start_in, count_in, ndim_in, &
                                    start_ot, count_ot, ndim_ot)
    
    integer,                                  intent(in) :: ncid_in, ncid_ot
    character(len=*),                         intent(in) :: var_in
    character(len=*), dimension(:),           intent(in) :: vars_ot  ! size: n_vars
    integer,                                  intent(in) :: n_vars
    integer,          dimension(:),           intent(in) :: start_in, count_in ! size: ndim_in
    integer,                                  intent(in) :: ndim_in
    integer,          dimension(:),           intent(in) :: start_ot, count_ot ! size: ndim_ot
    integer,                                  intent(in) :: ndim_ot
    
    
    ! iterators
    integer :: icopy
    
    ! status
    integer :: nf_stat
    
    ! netCDF ids
    integer :: id_v_in
    
    ! variable and attribute information
    integer                            :: xtype_in
    
    ! dummy arguments
    integer(1) :: dummy_byte
    integer(2) :: dummy_short
    integer(4) :: dummy_integer
    real(4) :: dummy_float
    real(8) :: dummy_double
    
    
    
    ! get var id
    nf_stat = NF90_INQ_VARID(ncid_in, var_in, id_v_in)
    call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(var_in))
    
    ! get dim-info and type of provided variable
    nf_stat = NF90_INQUIRE_VARIABLE(ncid_in, id_v_in, xtype = xtype_in)
    call check_nf90_stat(nf_stat, 'error inq type of var '//trim(var_in))
    
    ! call function depending on type
    if (xtype_in .eq. NF90_DOUBLE) then
      call copy_nf90_variable_val(ncid_in, ncid_ot, &
                                  var_in, vars_ot, n_vars, &
                                  start_in, count_in, ndim_in, &
                                  start_ot, count_ot, ndim_ot, &
                                  dummy_double)
    else if (xtype_in .eq. NF90_FLOAT) then
      call copy_nf90_variable_val(ncid_in, ncid_ot, &
                                  var_in, vars_ot, n_vars, &
                                  start_in, count_in, ndim_in, &
                                  start_ot, count_ot, ndim_ot, &
                                  dummy_float)
    else if (xtype_in .eq. NF90_INT) then
      call copy_nf90_variable_val(ncid_in, ncid_ot, &
                                  var_in, vars_ot, n_vars, &
                                  start_in, count_in, ndim_in, &
                                  start_ot, count_ot, ndim_ot, &
                                  dummy_integer)
    else if (xtype_in .eq. NF90_SHORT) then
      call copy_nf90_variable_val(ncid_in, ncid_ot, &
                                  var_in, vars_ot, n_vars, &
                                  start_in, count_in, ndim_in, &
                                  start_ot, count_ot, ndim_ot, &
                                  dummy_short)
    else if (xtype_in .eq. NF90_BYTE) then
      call copy_nf90_variable_val(ncid_in, ncid_ot, &
                                  var_in, vars_ot, n_vars, &
                                  start_in, count_in, ndim_in, &
                                  start_ot, count_ot, ndim_ot, &
                                  dummy_byte)
    end if
    
    
  end subroutine copy_nf90_variable_val_start
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_val_start ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_val_double ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine copy_nf90_variable_val_double(ncid_in, ncid_ot, &
                                           var_in, vars_ot, n_vars, &
                                           start_in, count_in, ndim_in, &
                                           start_ot, count_ot, ndim_ot, &
                                           dummy)
    
    integer,                        intent(in) :: ncid_in, ncid_ot
    character(len=*),               intent(in) :: var_in
    character(len=*), dimension(:), intent(in) :: vars_ot  ! size: n_vars
    integer,                        intent(in) :: n_vars
    integer,          dimension(:), intent(in) :: start_in, count_in ! size: ndim_in
    integer,                        intent(in) :: ndim_in
    integer,          dimension(:), intent(in) :: start_ot, count_ot ! size: ndim_ot
    integer,                        intent(in) :: ndim_ot
    real(8),                        intent(in) :: dummy
    
    
    ! iterators
    integer :: icopy
    
    ! status
    integer :: nf_stat
    
    ! netCDF ids
    integer               :: id_v_in
    integer, dimension(:), allocatable :: id_v_ot
    
    ! data arrays
    real(8), dimension(:)      , allocatable :: vals_1d
    real(8), dimension(:,:)    , allocatable :: vals_2d
    real(8), dimension(:,:,:)  , allocatable :: vals_3d
    real(8), dimension(:,:,:,:), allocatable :: vals_4d
    
    
    ! get var ids
    allocate(id_v_ot(n_vars))
    nf_stat = NF90_INQ_VARID(ncid_in, var_in, id_v_in)
    call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(var_in))
    do icopy = 1, n_vars
      nf_stat = NF90_INQ_VARID(ncid_ot, vars_ot(icopy), id_v_ot(icopy))
      call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(vars_ot(icopy)))
    end do
    
    
    ! copy variables
    if (ndim_in .eq. 1) then
      allocate(vals_1d(count_in(1)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_1d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_1d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 2) then
      allocate(vals_2d(count_in(1), count_in(2)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_2d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_2d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 3) then
      allocate(vals_3d(count_in(1), count_in(2), count_in(3)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_3d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_3d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 4) then
      allocate(vals_4d(count_in(1), count_in(2), count_in(3), count_in(4)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_4d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_4d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    end if
    
  end subroutine copy_nf90_variable_val_double
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_val_double ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_val_float ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine copy_nf90_variable_val_float(ncid_in, ncid_ot, &
                                           var_in, vars_ot, n_vars, &
                                           start_in, count_in, ndim_in, &
                                           start_ot, count_ot, ndim_ot, &
                                           dummy)
    
    integer,                        intent(in) :: ncid_in, ncid_ot
    character(len=*),               intent(in) :: var_in
    character(len=*), dimension(:), intent(in) :: vars_ot  ! size: n_vars
    integer,                        intent(in) :: n_vars
    integer,          dimension(:), intent(in) :: start_in, count_in ! size: ndim_in
    integer,                        intent(in) :: ndim_in
    integer,          dimension(:), intent(in) :: start_ot, count_ot ! size: ndim_ot
    integer,                        intent(in) :: ndim_ot
    real(4),                        intent(in) :: dummy
    
    
    ! iterators
    integer :: icopy
    
    ! status
    integer :: nf_stat
    
    ! netCDF ids
    integer               :: id_v_in
    integer, dimension(:), allocatable :: id_v_ot
    
    ! data arrays
    real(4), dimension(:)      , allocatable :: vals_1d
    real(4), dimension(:,:)    , allocatable :: vals_2d
    real(4), dimension(:,:,:)  , allocatable :: vals_3d
    real(4), dimension(:,:,:,:), allocatable :: vals_4d
    
    
    ! get var ids
    allocate(id_v_ot(n_vars))
    nf_stat = NF90_INQ_VARID(ncid_in, var_in, id_v_in)
    call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(var_in))
    do icopy = 1, n_vars
      nf_stat = NF90_INQ_VARID(ncid_ot, vars_ot(icopy), id_v_ot(icopy))
      call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(vars_ot(icopy)))
    end do
    
    
    ! copy variables
    if (ndim_in .eq. 1) then
      allocate(vals_1d(count_in(1)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_1d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_1d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 2) then
      allocate(vals_2d(count_in(1), count_in(2)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_2d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_2d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 3) then
      allocate(vals_3d(count_in(1), count_in(2), count_in(3)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_3d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_3d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 4) then
      allocate(vals_4d(count_in(1), count_in(2), count_in(3), count_in(4)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_4d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_4d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    end if
    
  end subroutine copy_nf90_variable_val_float
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_val_float ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_val_integer ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine copy_nf90_variable_val_integer(ncid_in, ncid_ot, &
                                           var_in, vars_ot, n_vars, &
                                           start_in, count_in, ndim_in, &
                                           start_ot, count_ot, ndim_ot, &
                                           dummy)
    
    integer,                        intent(in) :: ncid_in, ncid_ot
    character(len=*),               intent(in) :: var_in
    character(len=*), dimension(:), intent(in) :: vars_ot  ! size: n_vars
    integer,                        intent(in) :: n_vars
    integer,          dimension(:), intent(in) :: start_in, count_in ! size: ndim_in
    integer,                        intent(in) :: ndim_in
    integer,          dimension(:), intent(in) :: start_ot, count_ot ! size: ndim_ot
    integer,                        intent(in) :: ndim_ot
    integer(4),                     intent(in) :: dummy
    
    
    ! iterators
    integer :: icopy
    
    ! status
    integer :: nf_stat
    
    ! netCDF ids
    integer               :: id_v_in
    integer, dimension(:), allocatable :: id_v_ot
    
    ! data arrays
    integer(4), dimension(:)      , allocatable :: vals_1d
    integer(4), dimension(:,:)    , allocatable :: vals_2d
    integer(4), dimension(:,:,:)  , allocatable :: vals_3d
    integer(4), dimension(:,:,:,:), allocatable :: vals_4d
    
    
    ! get var ids
    allocate(id_v_ot(n_vars))
    nf_stat = NF90_INQ_VARID(ncid_in, var_in, id_v_in)
    call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(var_in))
    do icopy = 1, n_vars
      nf_stat = NF90_INQ_VARID(ncid_ot, vars_ot(icopy), id_v_ot(icopy))
      call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(vars_ot(icopy)))
    end do
    
    
    ! copy variables
    if (ndim_in .eq. 1) then
      allocate(vals_1d(count_in(1)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_1d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_1d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 2) then
      allocate(vals_2d(count_in(1), count_in(2)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_2d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_2d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 3) then
      allocate(vals_3d(count_in(1), count_in(2), count_in(3)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_3d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_3d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 4) then
      allocate(vals_4d(count_in(1), count_in(2), count_in(3), count_in(4)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_4d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_4d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    end if
    
  end subroutine copy_nf90_variable_val_integer
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_val_integer ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_val_short ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine copy_nf90_variable_val_short(ncid_in, ncid_ot, &
                                           var_in, vars_ot, n_vars, &
                                           start_in, count_in, ndim_in, &
                                           start_ot, count_ot, ndim_ot, &
                                           dummy)
    
    integer,                        intent(in) :: ncid_in, ncid_ot
    character(len=*),               intent(in) :: var_in
    character(len=*), dimension(:), intent(in) :: vars_ot  ! size: n_vars
    integer,                        intent(in) :: n_vars
    integer,          dimension(:), intent(in) :: start_in, count_in ! size: ndim_in
    integer,                        intent(in) :: ndim_in
    integer,          dimension(:), intent(in) :: start_ot, count_ot ! size: ndim_ot
    integer,                        intent(in) :: ndim_ot
    integer(2),                     intent(in) :: dummy
    
    
    ! iterators
    integer :: icopy
    
    ! status
    integer :: nf_stat
    
    ! netCDF ids
    integer               :: id_v_in
    integer, dimension(:), allocatable :: id_v_ot
    
    ! data arrays
    integer(2), dimension(:)      , allocatable :: vals_1d
    integer(2), dimension(:,:)    , allocatable :: vals_2d
    integer(2), dimension(:,:,:)  , allocatable :: vals_3d
    integer(2), dimension(:,:,:,:), allocatable :: vals_4d
    
    
    ! get var ids
    allocate(id_v_ot(n_vars))
    nf_stat = NF90_INQ_VARID(ncid_in, var_in, id_v_in)
    call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(var_in))
    do icopy = 1, n_vars
      nf_stat = NF90_INQ_VARID(ncid_ot, vars_ot(icopy), id_v_ot(icopy))
      call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(vars_ot(icopy)))
    end do
    
    
    ! copy variables
    if (ndim_in .eq. 1) then
      allocate(vals_1d(count_in(1)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_1d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_1d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 2) then
      allocate(vals_2d(count_in(1), count_in(2)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_2d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_2d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 3) then
      allocate(vals_3d(count_in(1), count_in(2), count_in(3)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_3d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_3d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 4) then
      allocate(vals_4d(count_in(1), count_in(2), count_in(3), count_in(4)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_4d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_4d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    end if
    
  end subroutine copy_nf90_variable_val_short
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_val_short ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_val_byte ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine copy_nf90_variable_val_byte(ncid_in, ncid_ot, &
                                           var_in, vars_ot, n_vars, &
                                           start_in, count_in, ndim_in, &
                                           start_ot, count_ot, ndim_ot, &
                                           dummy)
    
    integer,                        intent(in) :: ncid_in, ncid_ot
    character(len=*),               intent(in) :: var_in
    character(len=*), dimension(:), intent(in) :: vars_ot  ! size: n_vars
    integer,                        intent(in) :: n_vars
    integer,          dimension(:), intent(in) :: start_in, count_in ! size: ndim_in
    integer,                        intent(in) :: ndim_in
    integer,          dimension(:), intent(in) :: start_ot, count_ot ! size: ndim_ot
    integer,                        intent(in) :: ndim_ot
    integer(1),                     intent(in) :: dummy
    
    
    ! iterators
    integer :: icopy
    
    ! status
    integer :: nf_stat
    
    ! netCDF ids
    integer               :: id_v_in
    integer, dimension(:), allocatable :: id_v_ot
    
    ! data arrays
    integer(1), dimension(:)      , allocatable :: vals_1d
    integer(1), dimension(:,:)    , allocatable :: vals_2d
    integer(1), dimension(:,:,:)  , allocatable :: vals_3d
    integer(1), dimension(:,:,:,:), allocatable :: vals_4d
    
    
    ! get var ids
    allocate(id_v_ot(n_vars))
    nf_stat = NF90_INQ_VARID(ncid_in, var_in, id_v_in)
    call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(var_in))
    do icopy = 1, n_vars
      nf_stat = NF90_INQ_VARID(ncid_ot, vars_ot(icopy), id_v_ot(icopy))
      call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(vars_ot(icopy)))
    end do
    
    
    ! copy variables
    if (ndim_in .eq. 1) then
      allocate(vals_1d(count_in(1)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_1d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_1d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 2) then
      allocate(vals_2d(count_in(1), count_in(2)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_2d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_2d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 3) then
      allocate(vals_3d(count_in(1), count_in(2), count_in(3)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_3d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_3d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    else if (ndim_in .eq. 4) then
      allocate(vals_4d(count_in(1), count_in(2), count_in(3), count_in(4)))
      nf_stat = NF90_GET_VAR(ncid_in, id_v_in, vals_4d, start = start_in, &
                                                        count = count_in)
      call check_nf90_stat(nf_stat, 'error get var '//trim(var_in))
      do icopy = 1, n_vars
        nf_stat = NF90_PUT_VAR(ncid_ot, id_v_ot(icopy), vals_4d, &
                               start = start_ot, count = count_ot)
        call check_nf90_stat(nf_stat, 'error put var '//trim(vars_ot(icopy)))
      end do
    end if
    
  end subroutine copy_nf90_variable_val_byte
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_val_byte ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_NF90_att_new_fillval ~~~~~~~~~~~~~~~~~~~~~~
  subroutine copy_NF90_att_new_fillval(ncid_in, varid_in, att_name, ncid_ot, &
                                       varid_ot, old_type, new_type)
    
    integer,          intent(in) :: ncid_in, varid_in, ncid_ot, varid_ot,    &
                                    old_type, new_type
    character(len=*), intent(in) :: att_name
    
    !  status
    integer :: nf_stat
    
    ! data types:
    !  NF90_BYTE, NF90_CHAR, NF90_SHORT, NF90_INT, NF90_FLOAT, and NF90_DOUBLE
    integer(1) :: byte_in, byte_ot
    integer(2) :: short_in, short_ot
    integer(4) :: int_in, int_ot
    real(4)    :: float_in, float_ot
    real(8)    :: double_in, double_ot, double_tmp
    
    if (old_type .eq. NF90_DOUBLE) then
      nf_stat = nf90_get_att(ncid_in, varid_in, att_name, double_in)
      call check_nf90_stat(nf_stat, 'get double attribte '//trim(att_name))
      double_tmp = double_in
    else if (old_type .eq. NF90_FLOAT) then
      nf_stat = nf90_get_att(ncid_in, varid_in, att_name, float_in)
      call check_nf90_stat(nf_stat, 'get float attribte '//trim(att_name))
      double_tmp = dble(float_in)
    else if (old_type .eq. NF90_INT) then
      nf_stat = nf90_get_att(ncid_in, varid_in, att_name, int_in)
      call check_nf90_stat(nf_stat, 'get int attribte '//trim(att_name))
      double_tmp = dble(int_in)
    else if (old_type .eq. NF90_SHORT) then
      nf_stat = nf90_get_att(ncid_in, varid_in, att_name, short_in)
      call check_nf90_stat(nf_stat, 'get short attribte '//trim(att_name))
      double_tmp = dble(short_in)
    else if (old_type .eq. NF90_BYTE) then
      nf_stat = nf90_get_att(ncid_in, varid_in, att_name, byte_in)
      call check_nf90_stat(nf_stat, 'get byte attribte '//trim(att_name))
      double_tmp = dble(byte_in)
    else 
      write(*,*) 'Fill value of attribute '//trim(att_name)//' not of nice type.'
      stop
    end if
    
    double_tmp = dble(double_in)
    
    if (new_type .eq. NF90_DOUBLE) then
      nf_stat = nf90_put_att(ncid_ot, varid_ot, att_name, double_tmp)
      call check_nf90_stat(nf_stat, 'put double attribte '//trim(att_name))
    else if (new_type .eq. NF90_FLOAT) then
      nf_stat = nf90_put_att(ncid_ot, varid_ot, att_name, real(double_tmp))
      call check_nf90_stat(nf_stat, 'put float attribte '//trim(att_name))
    else if (new_type .eq. NF90_INT) then
      nf_stat = nf90_put_att(ncid_ot, varid_ot, att_name, int(double_tmp))
      call check_nf90_stat(nf_stat, 'put int attribte '//trim(att_name))
    else if (new_type .eq. NF90_SHORT) then
      nf_stat = nf90_put_att(ncid_ot, varid_ot, att_name, int(double_tmp,2))
      call check_nf90_stat(nf_stat, 'put short attribte '//trim(att_name))
    else if (new_type .eq. NF90_BYTE) then
      nf_stat = nf90_put_att(ncid_ot, varid_ot, att_name, int(double_tmp,1))
      call check_nf90_stat(nf_stat, 'put byte attribte '//trim(att_name))
    else 
      write(*,*) 'Fill value of attribute '//trim(att_name)//' not of nice type.'
      stop
    end if
    
    
    
  end subroutine copy_NF90_att_new_fillval
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_NF90_att_new_fillval ~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ append_NF90_attribute_char ~~~~~~~~~~~~~~~~~~~~~~
  subroutine append_NF90_attribute_char(ncid, varid, attname, attvalue)
  
    integer,          intent(in) :: ncid, varid
    character(len=*), intent(in) :: attname, attvalue
    
    
    ! attribute information
    integer                        :: attlen
    character (len=:), allocatable :: tmp_string_dyn
    
    ! status
    integer :: nf_stat
    
  
    nf_stat = nf90_inquire_attribute(ncid, varid, 'history', len=attlen)
    
    ! check, whether attribute exists (then nf_stat == 0) or not (then nf_stat == -43)
    if (nf_stat .eq. -43) then
      nf_stat = NF90_PUT_ATT(ncid, varid, 'comment', attvalue)
      call check_nf90_stat(nf_stat, 'put attribte comment')
    else
      
      call check_nf90_stat(ncid, 'inq att comment')
      allocate(character(len=attlen) :: tmp_string_dyn)
      nf_stat = nf90_get_att(ncid, varid, 'comment', tmp_string_dyn)
      call check_nf90_stat(nf_stat, 'get global att comment')
      nf_stat = nf90_put_att(ncid, varid, 'comment', trim(tmp_string_dyn)//';   '//attvalue)
      call check_nf90_stat(nf_stat, 'put global att comment')
      
    end if
    
    ! deallocate variables
    !! deallocate(tmp_string_dyn)
    ! => produces error
      
  end subroutine append_NF90_attribute_char
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ append_NF90_attribute_char ~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_NF90_fillvalue_real8 ~~~~~~~~~~~~~~~~~~~~~~
  function get_NF90_fillvalue_real8(ncid, varid, fillval)
  
    integer, intent(in) :: ncid, varid
    real(8), intent(out) :: fillval
  
    ! status
    integer :: nf_stat
    
    ! return value
    integer :: get_NF90_fillvalue_real8
    
    
    ! init
    get_NF90_fillvalue_real8 = 0
    
  
    nf_stat = nf90_get_att(ncid, varid, '_FillValue', fillval)
    if ( nf_stat .eq. -43 ) then
      write(*,*) 'Warning: attribute _FillValue not found for variable. '//&
                  'Trying missing_value.'
      nf_stat = nf90_get_att(ncid, varid, 'missing_value', fillval)
      if ( nf_stat .eq. -43 ) then
        write(*,*) 'Warning: attribute missing_value not found for variable. '//&
                    'Using no _FillValue'
        get_NF90_fillvalue_real8 = -43
      else
        call check_nf90_stat(nf_stat, 'error get att missing_value of var')
      end if
    else
      call check_nf90_stat(nf_stat, 'error get att _FillValue of var')
    end if
  
  end function get_NF90_fillvalue_real8
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ get_NF90_fillvalue_real8 ~~~~~~~~~~~~~~~~~~~~~~
    
    
    
  ! ~~~~~ SET GLOBAL ATTRIBUTES ~~~~~
  function nf90_set_global_atts(ncid_ot, history)
    
    implicit none
    
    integer,           intent(in) :: ncid_ot
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
                           'HIROMB-BOOS-model input (land-sea-mask) and '//&
                           'Michael Naumann/IOW (definition of Eastern '//&
                           'Gotland Basin)')
    call check_nf90_stat(nf_stat, 'put global att source')
    
    nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'title', &
                           'summarized data for bottom (below halocline) and top layer (top 5 layers)')
    call check_nf90_stat(nf_stat, 'put global att title')
    
    nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'institution', 'BSH and IOW')
    call check_nf90_stat(nf_stat, 'put global att institution')
    
    nf_stat = nf90_inquire_attribute(ncid_ot, NF90_GLOBAL, 'history', len=tmp_int)
    call check_nf90_stat(nf_stat, 'inq global att history')
    allocate(character(len=tmp_int) :: tmp_string_dyn)
    nf_stat = nf90_get_att(ncid_ot, NF90_GLOBAL, 'history', tmp_string_dyn)
    call check_nf90_stat(nf_stat, 'get global att history')
    nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'history', trim(tmp_string_dyn)//';   '//history)
    call check_nf90_stat(nf_stat, 'put global att history')
    
    nf_stat = nf90_put_att(ncid_ot, NF90_GLOBAL, 'comment', &
                           'created by Daniel Neumann (IOW) from HBM layer'//&
                           ' thickness model output data provided by BSH'//&
                           ' and DMI; definition of the Eastern Gotland'//&
                           ' Basin was provided by Michael Naumann/IOW')
    call check_nf90_stat(nf_stat, 'put global att comment')
    
    
    !! leave definition mode
    !nf_stat = nf90_enddef(ncid)
    !call check_nf90_stat(nf_stat)
    
    nf90_set_global_atts = NF90_NOERR
    
  end function nf90_set_global_atts
  ! ~~~~~ END SET GLOBAL ATTRIBUTES ~~~~~
  
  
  
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
    if (nf_stat .eq. -43) then
      write(*,*) 'Warning: global attribute longitude_min not found'
    else
      call check_nf90_stat(nf_stat, 'error copy global attribute longitude_min')
    end if
                          
    nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'longitude_max', ncid_ot, NF90_GLOBAL)
    if (nf_stat .eq. -43) then
      write(*,*) 'Warning: global attribute longitude_max not found'
    else
      call check_nf90_stat(nf_stat, 'error copy global attribute longitude_max')
    end if
                          
    nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'latitude_min', ncid_ot, NF90_GLOBAL)
    if (nf_stat .eq. -43) then
      write(*,*) 'Warning: global attribute latitude_min not found'
    else
      call check_nf90_stat(nf_stat, 'error copy global attribute latitude_min')
    end if
                          
    nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'latitude_max', ncid_ot, NF90_GLOBAL)
    if (nf_stat .eq. -43) then
      write(*,*) 'Warning: global attribute latitude_max not found'
    else
      call check_nf90_stat(nf_stat, 'error copy global attribute latitude_max')
    end if
                          
    nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'depth_min', ncid_ot, NF90_GLOBAL)
    if (nf_stat .eq. -43) then
      write(*,*) 'Warning: global attribute depth_min not found'
    else
      call check_nf90_stat(nf_stat, 'error copy global attribute depth_min')
    end if
                          
    nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'depth_max', ncid_ot, NF90_GLOBAL)
    if (nf_stat .eq. -43) then
      write(*,*) 'Warning: global attribute depth_max not found'
    else
      call check_nf90_stat(nf_stat, 'error copy global attribute depth_max')
    end if
                          
    nf_stat = nf90_copy_att(ncid_in, NF90_GLOBAL, 'history', ncid_ot, NF90_GLOBAL)
    if (nf_stat .eq. -43) then
      write(*,*) 'Warning: global attribute history not found'
    else
      call check_nf90_stat(nf_stat, 'error copy global attribute history')
    end if
    
    
    !! LEAVE DEFINITION MODE
    ! nf_stat = nf90_enddef(ncid)
    ! call check_nf90_stat(nf_stat)
    
    
    ! SET RETURN VALUE
    nf90_copy_global_atts = NF90_NOERR
    
    
  end function nf90_copy_global_atts
  ! ~~~~~ END COPY ALL ATTRIBUTES OF THE COORDINATE VARIABLES ~~~~~
  
end module nf90_var_data