module nf90_tools_integrate_layer_depth

  use netcdf
  use check_stat_netcdf
  
  implicit none
  
  
  
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
  
  PUBLIC :: get_nf90_dim_infos, get_nf90_var_infos, copy_nf90_dimensions,    &
            copy_nf90_dim_variable_def, copy_nf90_dim_variable_val,          &
            copy_nf90_variable_def, copy_nf90_variable_val,                  &
            nf90_copy_all_atts, nf90_copy_global_atts, nf90_set_global_atts, &
            get_NF90_fillvalue, append_NF90_attribute
  private :: copy_nf90_variable_val_start, copy_nf90_variable_val_float, &
             copy_nf90_variable_val_double, copy_nf90_variable_val_byte, &
             copy_nf90_variable_val_short, copy_nf90_variable_val_integer, &
             get_NF90_fillvalue_real8, append_NF90_attribute_char

contains
  
  
  subroutine get_nf90_dim_infos (ncid, dimnames, ndim, dimids, dimlens)
  
    integer,                                      INTENT(IN)  :: ncid
    CHARACTER (LEN=120), DIMENSION(:),            INTENT(IN)  :: dimnames
    INTEGER, INTENT(IN)                                       :: ndim
    INTEGER, DIMENSION(:), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: dimids, dimlens
    
    
    ! iterators
    integer :: i
    
    ! status
    INTEGER :: nf_stat
    
    
    ! allocate arrays
    allocate(dimids(ndim), dimlens(ndim))
    
    ! iterate dimensions
    DO i = 1, ndim
      
      ! GET DIM IDs
      nf_stat = NF90_INQ_DIMID(ncid, trim(dimnames(i)), dimids(i))
      call check_nf90_stat(nf_stat, 'error inq dimid '//trim(dimnames(i)))
      
      ! GET DIM SIZE
      nf_stat = nf90_inquire_dimension(ncid, dimids(i), len = dimlens(i))
      call check_nf90_stat(nf_stat, 'error get dim '//trim(dimnames(i)))
      
    END DO
    
  end subroutine get_nf90_dim_infos
  
  
  
  subroutine get_nf90_var_infos (ncid, varnames, nvar, varids)
  
    integer,                                      INTENT(IN)  :: ncid
    CHARACTER (LEN=120), DIMENSION(:),            INTENT(IN)  :: varnames
    INTEGER, INTENT(IN)                                       :: nvar
    INTEGER, DIMENSION(:), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: varids
    
    
    ! iterators
    integer :: i
    
    ! status
    INTEGER :: nf_stat
    
    
    ! allocate arrays
    allocate(varids(nvar))
    
    ! iterate dimensions
    DO i = 1, nvar
      
      ! GET DIM IDs
      nf_stat = NF90_INQ_VARID(ncid, trim(varnames(i)), varids(i))
      call check_nf90_stat(nf_stat, 'error inq varid '//trim(varnames(i)))
      
    END DO
    
  end subroutine get_nf90_var_infos
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_dimensions ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !! DON'T COPY DIMENSIONS WITH START == -1!!!
  subroutine copy_nf90_dimensions(ncid_in, ncid_ot, start, count, id_d_ot, &
                                  dim_rename, nrename)
    
    integer,                                    intent(in)  :: ncid_in, ncid_ot
    integer, dimension(4),                      intent(in)  :: start, count
    integer, dimension(:), allocatable,         intent(out) :: id_d_ot
    character(LEN=*), DIMENSION(:,:), OPTIONAL, INTENT(IN)  :: dim_rename ! 1: old name; 2: new name
                           ! size: 2 x nrename
    INTEGER,                          OPTIONAL, INTENT(IN)  :: nrename
    
    ! status variables
    integer :: nf_stat
    
    ! dimension information
    character(len=255), dimension(4)              :: dim_names_in ! names of dimensions
    character(len=255), dimension(4)              :: dim_names_ot ! names of dimensions
    integer,            dimension(4)              :: dim_lengths ! length of each dimension
    integer                                       :: n_d_ot      ! number of output dimensions
    integer,            dimension(4)              :: id_d_in2ot
    integer,            dimension(:), allocatable :: id_d_ot2in
    integer                                       :: id_d_rec ! record dimension ID
    
    ! local modified variables of input parameters
    integer, dimension(4) :: count_loc
    
    ! iterators
    integer :: i_in, i_ot, i_rn
    
    
    if(present(dim_rename)) then
      if(.not. present(nrename)) then
        write(*,'(A25,I5)') 'error: if dim_rename is present nrename also has to be present'
        stop
      end if
    end if
    
    ! copy input parameter
    count_loc = count
    
    ! get the dimid of the record dimension
    nf_stat = nf90_inquire(ncid_in, unlimitedDimId = id_d_rec)
    call check_nf90_stat(nf_stat, 'error getting record dim of input file')
    
    
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
        nf_stat = NF90_INQUIRE_DIMENSION(ncid_in, i_in, dim_names_in(i_in), dim_lengths(i_in))
        call check_nf90_stat(nf_stat, 'error inq dim info')
        
        ! look if count needs to be set
        if (count_loc(i_in) .eq. -1) count_loc(i_in) = dim_lengths(i_in)
        
        ! set output dimnames (look, whether we have to rename some)
        dim_names_ot(i_in) = dim_names_in(i_in)
        if (present(dim_rename)) then
          DO i_rn = 1, nrename
            if ( trim(dim_rename(1,i_rn)) .eq. trim(dim_names_in(i_in)) ) THEN
              dim_names_ot(i_in) = dim_rename(2,i_rn)
            END IF
          END DO
        end if
        
        ! DEF DIMs
        if (i_in .eq. id_d_rec) then
          nf_stat = NF90_DEF_DIM(ncid_ot, dim_names_ot(i_in), NF90_UNLIMITED, id_d_ot(i_ot))
          call check_nf90_stat(nf_stat, 'error defining dim')
        else
          nf_stat = NF90_DEF_DIM(ncid_ot, dim_names_ot(i_in), count_loc(i_in), id_d_ot(i_ot))
          call check_nf90_stat(nf_stat, 'error defining dim')
        END IF
        
        ! increment i_ot
        i_ot = i_ot + 1
      end if
      
    end do
    
    ! get and copy dimensional variables
    do i_ot = 1, n_d_ot
      call copy_nf90_dim_variable_def(ncid_in, ncid_ot,               &
                                      dim_names_in(id_d_ot2in(i_ot)), &
                                      id_d_in2ot,                     &
                                      dim_names_ot(id_d_ot2in(i_ot)))
      nf_stat = NF90_ENDDEF(ncid_ot)
      call check_nf90_stat(nf_stat, 'leaving definition mode to copy dim '//&
                                    'var '//trim(dim_names_in(id_d_ot2in(i_ot))))
      call copy_nf90_dim_variable_val(ncid_in, ncid_ot,               &
                                      dim_names_in(id_d_ot2in(i_ot)), &
                                      id_d_in2ot, start, count,       &
                                      dim_names_ot(id_d_ot2in(i_ot)))
      nf_stat = NF90_REDEF(ncid_ot)
      call check_nf90_stat(nf_stat, 'leaving definition mode to copy dim '//&
                                    'var '//trim(dim_names_in(id_d_ot2in(i_ot))))
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
  subroutine copy_nf90_dim_variable_def(ncid_in, ncid_ot, dimname, id_d_in2ot, new_dimname)
    
    integer,                        intent(in)  :: ncid_in, ncid_ot
    character(len=*),               intent(in)  :: dimname
    integer,          dimension(4), intent(in)  :: id_d_in2ot
    character(len=*), OPTIONAL,     intent(IN)  :: new_dimname
    
    
    ! status
    integer :: nf_stat
    
    ! netCDF ids
    integer                            :: id_v, n_dims_in, n_dims_ot
    integer, dimension(:), allocatable :: id_dims_in, id_dims_ot
    
    ! iterators
    integer :: i1, i2
    
    ! output dimname
    CHARACTER(LEN=255) :: dimname_ot
    
    
    if (present(new_dimname)) THEN
      dimname_ot = new_dimname
    ELSE
      dimname_ot = dimname
    END IF
    
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
    call copy_nf90_variable_def(ncid_in, ncid_ot, dimname, (/dimname_ot/), 1, id_dims_ot)
    
    
    ! deallocate variables
    deallocate(id_dims_in, id_dims_ot)
    
    
  end subroutine copy_nf90_dim_variable_def
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_dim_variable_def ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_dim_variable_val ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine copy_nf90_dim_variable_val(ncid_in, ncid_ot, dimname, id_d_in2ot, &
                                        start, count, new_dimname)
    
    integer,                        intent(in)  :: ncid_in, ncid_ot
    character(len=*),               intent(in)  :: dimname
    integer,          dimension(4), intent(in)  :: id_d_in2ot, start, count
    character(len=*), OPTIONAL,     intent(IN)  :: new_dimname
    
    
    ! status
    integer :: nf_stat
    
    ! netCDF ids
    integer                            :: id_v_in, id_v_ot, n_dims_in, n_dims_ot
    integer, dimension(:), allocatable :: start_ot, count_ot, start_in, count_in
    integer, dimension(:), allocatable :: id_dims_in, id_dims_ot
    integer                            :: id_d_in_rec, id_d_ot_rec ! record dimension ID
    
    ! iterators
    integer :: i
    
    ! variable info
    integer :: xtype_in
    
    ! output dimname
    CHARACTER(LEN=255) :: dimname_ot
    
    
    ! get the dimid of the record dimension
    nf_stat = nf90_inquire(ncid_in, unlimitedDimId = id_d_in_rec)
    call check_nf90_stat(nf_stat, 'error getting record dim of input file')
    nf_stat = nf90_inquire(ncid_ot, unlimitedDimId = id_d_ot_rec)
    call check_nf90_stat(nf_stat, 'error getting record dim of output file')
    
    
    if (present(new_dimname)) THEN
      dimname_ot = new_dimname
    ELSE
      dimname_ot = dimname
    END IF
    
    
    ! get var id
    nf_stat = NF90_INQ_VARID(ncid_in, dimname, id_v_in)
    call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(dimname))
    nf_stat = NF90_INQ_VARID(ncid_ot, dimname_ot, id_v_ot)
    call check_nf90_stat(nf_stat, 'error inq var id of var '//trim(dimname_ot))
    
    ! get dim-info provided variable in input file
    nf_stat = NF90_INQUIRE_VARIABLE(ncid_in, id_v_in, ndims = n_dims_in)
    call check_nf90_stat(nf_stat, 'error inq dimids of var '//trim(dimname))
    allocate(id_dims_in(n_dims_in))
    nf_stat = NF90_INQUIRE_VARIABLE(ncid_in, id_v_in, dimids = id_dims_in)
    call check_nf90_stat(nf_stat, 'error inq dimids of var '//trim(dimname))
    
    ! get dim-info provided variable in output file
    nf_stat = NF90_INQUIRE_VARIABLE(ncid_ot, id_v_ot, ndims = n_dims_ot)
    call check_nf90_stat(nf_stat, 'error inq dimids of var '//trim(dimname_ot))
    allocate(id_dims_ot(n_dims_ot))
    nf_stat = NF90_INQUIRE_VARIABLE(ncid_ot, id_v_ot, dimids = id_dims_ot)
    call check_nf90_stat(nf_stat, 'error inq dimids of var '//trim(dimname_ot))
    
    ! create start and count arrays 
    allocate(start_in(n_dims_in), count_in(n_dims_in), &
              start_ot(n_dims_ot), count_ot(n_dims_ot))
    
    do i = 1, n_dims_in
      start_in(i) = start(id_dims_in(i))
      count_in(i) = count(id_dims_in(i))
      if (count_in(i) .eq. -1) then
        nf_stat = NF90_INQUIRE_DIMENSION(ncid_in, id_dims_in(i), len = count_in(i))
        call check_nf90_stat(nf_stat, 'error inq dim len of one dim of var '//trim(dimname))
      end if
    end do
    
    start_ot = 1
    do i = 1, n_dims_ot
      ! if i is record dimension we cannot get the dimension size => it is currently 0
      IF (id_dims_ot(i) .eq. id_d_ot_rec) THEN
        ! NOTE: we get the record dimension size from the input file
        nf_stat = NF90_INQUIRE_DIMENSION(ncid_in, id_d_in_rec, len = count_ot(i))
        call check_nf90_stat(nf_stat, 'error inq dim len of one dim of var '//trim(dimname))
      ELSE
        nf_stat = NF90_INQUIRE_DIMENSION(ncid_ot, id_dims_ot(i), len = count_ot(i))
        call check_nf90_stat(nf_stat, 'error inq dim len of one dim of var '//trim(dimname_ot))
      END IF
    end do
    
    ! copy the dimensional variable
    call copy_nf90_variable_val(ncid_in, ncid_ot, dimname, (/dimname_ot/), 1, &
                                start_in, count_in, n_dims_in,  &
                                start_ot, count_ot, n_dims_ot)
    
    
  end subroutine copy_nf90_dim_variable_val
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_dim_variable_val ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_nf90_variable_def ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine copy_nf90_variable_def(ncid_in, ncid_ot, var_in, vars_ot, &
                                n_vars, id_d_ot, comments, new_units, new_type, &
                                deflate)
    
    integer,                                  intent(in) :: ncid_in, ncid_ot
    character(len=*),                         intent(in) :: var_in
    character(len=*), dimension(:),           intent(in) :: vars_ot  ! size: n_vars
    integer,                                  intent(in) :: n_vars
    integer,          dimension(:),           intent(in) :: id_d_ot
    character(len=*), dimension(:), optional, intent(in) :: comments ! size: n_vars
    character(len=*),               optional, intent(in) :: new_units
    integer,                        optional, intent(in) :: new_type
    integer,                        optional, intent(in) :: deflate
    
    
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
    
      if (present(deflate)) then
        nf_stat = nf90_def_var_deflate(ncid_ot, id_v_ot, 0, 1, deflate)
        call check_nf90_stat(nf_stat, 'error set deflate for var '//trim(vars_ot(icopy)))
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
      write(*,*) 'Fill value of old attribute '//trim(att_name)//' not of nice type.'
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
      write(*,*) 'Fill value of new attribute '//trim(att_name)//' not of nice type.'
      stop
    end if
    
    
    
  end subroutine copy_NF90_att_new_fillval
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~ copy_NF90_att_new_fillval ~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  SUBROUTINE correct_nf90_attributes_integrate_layer_depths(ncid)
  
    integer, intent(in) :: ncid
    
    ! temp variables
    character(len=255) :: tmp_varname
    integer            :: tmp_varid
    
    ! status
    integer :: nf_stat
    
    
    ! lev
    tmp_varname = 'lev'
    
    nf_stat = nf90_inq_varid(ncid, tmp_varname, tmp_varid)
    Call check_nf90_stat(nf_stat, 'get varid of variable '//trim(tmp_varname))
    
    nf_stat = nf90_del_att(ncid, tmp_varid, 'units')
    nf_stat = nf90_del_att(ncid, tmp_varid, 'units_long')
    nf_stat = nf90_del_att(ncid, tmp_varid, 'unit_long')
    ! nf_stat = nf90_del_att(ncid, tmp_varid, 'standard_name')
    ! nf_stat = nf90_del_att(ncid, tmp_varid, 'long_name')
    
    nf_stat = nf90_put_att(ncid, tmp_varid, 'standard_name', 'model_level_number')
    Call check_nf90_stat(nf_stat, 'set standard_name attribute of var '//trim(tmp_varname))
    
    nf_stat = nf90_put_att(ncid, tmp_varid, 'long_name', 'model_level_number')
    Call check_nf90_stat(nf_stat, 'set long_name attribute of var '//trim(tmp_varname))
    
    nf_stat = nf90_put_att(ncid, tmp_varid, 'axis', 'Z')
    Call check_nf90_stat(nf_stat, 'set axis attribute of var '//trim(tmp_varname))
    
    ! time
    tmp_varname = 'time'
    
    nf_stat = nf90_inq_varid(ncid, tmp_varname, tmp_varid)
    Call check_nf90_stat(nf_stat, 'get varid of variable '//trim(tmp_varname))
    
    nf_stat = nf90_del_att(ncid, tmp_varid, 'cell_methods')
    
    nf_stat = nf90_put_att(ncid, tmp_varid, 'axis', 'T')
    Call check_nf90_stat(nf_stat, 'set axis attribute of var '//trim(tmp_varname))
    nf_stat = nf90_put_att(ncid, tmp_varid, 'calendar', 'gregorian')
    Call check_nf90_stat(nf_stat, 'set calendar attribute of var '//trim(tmp_varname))
    
    ! lon
    tmp_varname = 'lon'
    
    nf_stat = nf90_inq_varid(ncid, tmp_varname, tmp_varid)
    Call check_nf90_stat(nf_stat, 'get varid of variable '//trim(tmp_varname))
    
    nf_stat = nf90_put_att(ncid, tmp_varid, 'axis', 'X')
    Call check_nf90_stat(nf_stat, 'set axis attribute of var '//trim(tmp_varname))
    
    ! lat
    tmp_varname = 'lat'
    
    nf_stat = nf90_inq_varid(ncid, tmp_varname, tmp_varid)
    Call check_nf90_stat(nf_stat, 'get varid of variable '//trim(tmp_varname))
    
    nf_stat = nf90_put_att(ncid, tmp_varid, 'axis', 'Y')
    Call check_nf90_stat(nf_stat, 'set axis attribute of var '//trim(tmp_varname))
    
  
  END SUBROUTINE correct_nf90_attributes_integrate_layer_depths
  
  
  
  SUBROUTINE modify_nf90_attributes_depth(ncid)
  
    integer, intent(in) :: ncid
    
    ! temp variables
    character(len=255) :: varname
    integer            :: varid
    
    ! status
    integer :: nf_stat
    
    
    ! set varname and get varid
    varname = 'depth'
    
    nf_stat = nf90_inq_varid(ncid, varname, varid)
    Call check_nf90_stat(nf_stat, 'get varid of variable '//trim(varname))
    
    nf_stat = nf90_del_att(ncid, varid, 'unit_long')
    
    nf_stat = nf90_put_att(ncid, varid, 'cell_methods', 'time: mean (interval: 1 hour)')
    Call check_nf90_stat(nf_stat, 'set cell_methods attribute of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, varid, 'standard_name', 'depth')
    Call check_nf90_stat(nf_stat, 'set standard_name attribute of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, varid, 'long_name', 'depth')
    Call check_nf90_stat(nf_stat, 'set long_name attribute of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, varid, 'positive', 'down')
    Call check_nf90_stat(nf_stat, 'set positive attribute of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, varid, '_FillValue', 0.0_4)
    call check_nf90_stat(nf_stat, 'error put att _FillValue of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, varid, 'missing_value', 0.0_4)
    call check_nf90_stat(nf_stat, 'error put att missing_value of var '//trim(varname))
    
  END SUBROUTINE modify_nf90_attributes_depth
  
  
  
  SUBROUTINE modify_nf90_attributes_bathymetry(ncid, fillval)
  
    integer, intent(in) :: ncid
    real(4), intent(in) :: fillval
    
    ! temp variables
    character(len=255) :: varname
    integer            :: varid
    
    ! status
    integer :: nf_stat
    
    
    ! set varname and get varid
    varname = 'bathymetry'
    
    nf_stat = nf90_inq_varid(ncid, varname, varid)
    Call check_nf90_stat(nf_stat, 'get varid of variable '//trim(varname))
    
    
    ! put attributes
    nf_stat = nf90_put_att(ncid, varid, 'standard_name', 'sea_floor_depth_below_sea_surface')
    call check_nf90_stat(nf_stat, 'error put att standard_name of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, varid, 'long_name', 'sea_floor_depth_below_sea_surface')
    call check_nf90_stat(nf_stat, 'error put att long_name of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, varid, 'units', 'm')
    call check_nf90_stat(nf_stat, 'error put att units of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, varid, 'positive', 'down')
    Call check_nf90_stat(nf_stat, 'set positive attribute of var '//trim(varname))
    
    nf_stat = nf90_put_att(ncid, varid, 'description', 'The '//&
                           'sea_floor_depth_below_sea_surface is the '//&
                           'vertical distance between the sea surface and '//&
                           'the seabed as measured at a given point in '//&
                           'space including the variance caused by tides '//&
                           'and possibly waves.')
    call check_nf90_stat(nf_stat, 'error put att description of var '//trim(varname))
    
    ! nf_stat = nf90_put_att(ncid, varid, '_FillValue', real(fillval))
    nf_stat = nf90_put_att(ncid, varid, '_FillValue', 0.0_4)
    call check_nf90_stat(nf_stat, 'error put att _FillValue of var '//trim(varname))
    
    ! nf_stat = nf90_put_att(ncid, varid, 'missing_value', real(fillval))
    nf_stat = nf90_put_att(ncid, varid, 'missing_value', 0.0_4)
    call check_nf90_stat(nf_stat, 'error put att missing_value of var '//trim(varname))
    
    
  END SUBROUTINE modify_nf90_attributes_bathymetry
  
  
  
  SUBROUTINE def_nf90_bnds_dimvars(ncid, names, dimids, ndims, dimid_nv, varids)
  
    integer,                                       intent(in)  :: ncid
    character(len=*), dimension(:),              intent(in)  :: names ! size: ndims
    integer,            dimension(:),              intent(in)  :: dimids ! size: ndims
    integer,                                       intent(in)  :: ndims
    integer,                                       intent(in)  :: dimid_nv
    integer,            dimension(:), ALLOCATABLE, intent(OUT) :: varids ! size: ndims
    
    ! iterator
    integer :: i, j
    
    ! status
    integer :: nf_stat
    
    ! tmp vars
    integer                              :: tmp_type, tmp_varid, tmp_ntime
    real(8), dimension(:,:), allocatable :: tmp_time_bnds_dbl
    real(4), dimension(:,:), allocatable :: tmp_time_bnds_flt
    real(8), dimension(:), allocatable   :: tmp_time_dbl
    real(4), dimension(:), allocatable   :: tmp_time_flt
    
    ! allocate output
    ALLOCATE(varids(ndims))
    
    
    DO i = 1, ndims
      
      nf_stat = nf90_inq_varid(ncid, names(i), tmp_varid)
      call check_nf90_stat(nf_stat, 'error inq varid for var '//trim(names(i)))
      nf_stat = nf90_inquire_variable(ncid, tmp_varid, xtype = tmp_type)
      call check_nf90_stat(nf_stat, 'error type for var '//trim(names(i)))
      nf_stat = nf90_def_var(ncid, trim(names(i))//'_bnds', tmp_type, (/dimid_nv, dimids(i)/), varids(i))
      call check_nf90_stat(nf_stat, 'error def var '//trim(names(i))//'_bnds')
      nf_stat = nf90_put_att(ncid, tmp_varid, 'bounds', trim(names(i))//'_bnds')
      call check_nf90_stat(nf_stat, 'error put att bounds of var '//trim(names(i)))
      
      if ((trim(names(i)) .eq. 'time') .and. (tmp_type .eq. NF90_DOUBLE)) then
        nf_stat = nf90_enddef(ncid)
        call check_nf90_stat(nf_stat, 'error leaving definition mode')
        
        nf_stat =  nf90_inquire_dimension(ncid, dimids(i), len = tmp_ntime)
        call check_nf90_stat(nf_stat, 'error inq dim len for dim '//trim(names(i)))
        
        allocate(tmp_time_bnds_dbl(2, tmp_ntime), tmp_time_dbl(tmp_ntime))
        
        nf_stat = nf90_get_var(ncid, tmp_varid, tmp_time_dbl)
        call check_nf90_stat(nf_stat, 'error get values of variable '//trim(names(i)))
        
        DO j = 1, tmp_ntime
          tmp_time_bnds_dbl(1,j) = floor(tmp_time_dbl(j))
          tmp_time_bnds_dbl(2,j) = ceiling(tmp_time_dbl(j))
        end do
        
        nf_stat = nf90_put_var(ncid, varids(i), tmp_time_bnds_dbl)
        call check_nf90_stat(nf_stat, 'error put values of variable '//trim(names(i)))
        
        nf_stat = nf90_redef(ncid)
        call check_nf90_stat(nf_stat, 'error entering definition mode')
        
      end if
      
      if ((trim(names(i)) .eq. 'time') .and. (tmp_type .eq. NF90_FLOAT)) then
        nf_stat = nf90_enddef(ncid)
        call check_nf90_stat(nf_stat, 'error leaving definition mode')
        
        nf_stat =  nf90_inquire_dimension(ncid, dimids(i), len = tmp_ntime)
        call check_nf90_stat(nf_stat, 'error inq dim len for dim '//trim(names(i)))
        
        allocate(tmp_time_bnds_flt(2, tmp_ntime), tmp_time_flt(tmp_ntime))
        
        nf_stat = nf90_get_var(ncid, tmp_varid, tmp_time_flt)
        call check_nf90_stat(nf_stat, 'error get values of variable '//trim(names(i)))
        
        DO j = 1, tmp_ntime
          tmp_time_bnds_flt(1,j) = floor(tmp_time_flt(j))
          tmp_time_bnds_flt(2,j) = ceiling(tmp_time_flt(j))
        end do
        
        nf_stat = nf90_put_var(ncid, varids(i), tmp_time_bnds_flt)
        call check_nf90_stat(nf_stat, 'error put values of variable '//trim(names(i)))
        
        nf_stat = nf90_redef(ncid)
        call check_nf90_stat(nf_stat, 'error entering definition mode')
        
      end if
      
    END DO
    
  
  end SUBROUTINE def_nf90_bnds_dimvars
  
  
  
  SUBROUTINE def_nf90_bnds_var(ncid, varname, dimid_nv, varid, deflate)
  
    integer,            intent(in)  :: ncid
    character(len=*),   intent(in)  :: varname ! size: ndims
    integer,            intent(in)  :: dimid_nv
    integer,            intent(OUT) :: varid ! size: ndims
    integer,  optional, intent(in)  :: deflate
    
    ! iterator
    integer :: i
    
    ! status
    integer :: nf_stat
    
    ! tmp vars
    integer :: tmp_type, tmp_varid
    
    ! output dimids
    integer :: ndims
    INTEGER, dimension(:), allocatable :: dimids, dimids_bnds
      
    nf_stat = nf90_inq_varid(ncid, varname, tmp_varid)
    call check_nf90_stat(nf_stat, 'error inq varid for var '//trim(varname))
    nf_stat = nf90_inquire_variable(ncid, tmp_varid, xtype = tmp_type, ndims = ndims)
    call check_nf90_stat(nf_stat, 'error type for var '//trim(varname))
    allocate(dimids(ndims))
    nf_stat = nf90_inquire_variable(ncid, tmp_varid, xtype = tmp_type, dimids = dimids)
    call check_nf90_stat(nf_stat, 'error type for var '//trim(varname))
    nf_stat = nf90_put_att(ncid, tmp_varid, 'bounds', trim(varname)//'_bnds')
    call check_nf90_stat(nf_stat, 'error put att bounds of var '//trim(varname))
    
    allocate(dimids_bnds(ndims+1))
    dimids_bnds(1) = dimid_nv
    do i = 1, ndims
      dimids_bnds(i+1) = dimids(i)
    end do
    
    nf_stat = nf90_def_var(ncid, trim(varname)//'_bnds', tmp_type, dimids_bnds, varid)
    call check_nf90_stat(nf_stat, 'error def var '//trim(varname)//'_bnds')
    
    if (present(deflate)) then
      nf_stat = nf90_def_var_deflate(ncid, varid, 0, 1, deflate)
      call check_nf90_stat(nf_stat, 'error set deflate for var '//trim(varname)//'_bnds')
    end if
    
    !!!! REMOVED; bounds variables should have no attributes
    !! copy fillvalue
    !!nf_stat = nf90_copy_att(ncid, tmp_varid, '_FillValue', ncid, varid)
    !! call check_nf90_stat(nf_stat, 'error copy att _FillValue of var '//trim(varname))
    !!nf_stat = nf90_copy_att(ncid, tmp_varid, 'missing_value', ncid, varid)
    !! call check_nf90_stat(nf_stat, 'error put att missing_value of var '//trim(varname))
    
    ! nf_stat = nf90_put_att(ncid, tmp_varid, '_FillValue', 0.0_4)
    ! call check_nf90_stat(nf_stat, 'error put att _FillValue of var '//trim(varname))
    ! 
    ! nf_stat = nf90_put_att(ncid, tmp_varid, 'missing_value', 0.0_4)
    ! call check_nf90_stat(nf_stat, 'error put att missing_value of var '//trim(varname))
  
  end SUBROUTINE def_nf90_bnds_var
  
  
end module nf90_tools_integrate_layer_depth