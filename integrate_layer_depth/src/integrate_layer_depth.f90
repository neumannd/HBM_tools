!> \file integrate_layer_depth.f90
!! \brief calculate depth of each layer and bathymetry from HBM output files
! 
! Calculate depth of each layer and bathymetry from HBM output files.
! 
! Try it with a file and see what happens!
! 
! This code was written by Daniel Neumann at the Leibniz-Institut for Baltic
! Sea Research Warnemuende (www.io-warnemuende.de). The work was done within'
! the project MeRamo (funded by BMVI, FKZ 50EW1601)
!
!> Module: Main Programm, integrate_layer_depth
!> \author Daniel Neumann, IOW
!> \date 13.04.2018


program integrate_layer_depth

  ! use netcdf4_f03
  use netcdf
  use omp_lib
  use nf90_tools_integrate_layer_depth
  use check_stat_netcdf
  use read_files
  use time_tools
  
  ! NC_ENOTATT not found???? Would be -43

  implicit none
  
  integer(4)          :: argc                                      ! commandline argument count
  character (len=255) :: file_in, file_ot

  character (len=120), dimension(4)                :: dimnames_in, dimnames_ot      ! dim names
  integer,             DIMENSION(:),   ALLOCATABLE :: id_dims_in, id_dims_ot        ! dim IDs
  integer,             DIMENSION(:),   ALLOCATABLE :: id_vardims_in, id_vardims_ot  ! var-dim IDs
  integer,             DIMENSION(:),   ALLOCATABLE :: id_varbndsdims_ot             ! bnds var-dims IDs
  integer,             DIMENSION(:),   ALLOCATABLE :: len_dims_in, len_dims_ot      ! dim sizes
  character (LEN=120), DIMENSION(:,:), ALLOCATABLE :: dim_rename     ! for renaming dims
  INTEGER                                          :: nrename     ! for renaming dims

  integer             :: id_v_h, id_v_depth, id_v_bathy, id_v_depth_bnds, id_dim_nv ! further variable id_s
  
  REAL(8), dimension(:,:,:,:),   allocatable :: val_v_h, val_v_depth
  REAL(8), dimension(:,:,:,:,:), allocatable :: val_v_depth_bnds
  real(8), dimension(:,:,:),     allocatable :: val_v_bathy
  real(8)                                    :: fillval_h
  
  integer             :: nf_stat         ! for error status of netCDF functions
  integer             :: ncid_in, ncid_ot  ! ncid for input and output
  
  character (len=20)       :: str_time_stamp
  
  ! variable for program call information
  character (len=2048) :: program_call
  
  ! help request?
  logical :: helpme
  
  
  ! check whether help is requested
  helpme = .FALSE.
  call test_help_string(helpme)
  
  
  if (helpme) then
    
    call print_help()
    
  else
    ! GET file_in and file_ot AS INPUT ARGUMENTS
    call get_filenames(file_in, file_ot)

    if (trim(file_in) .eq. trim(file_ot)) then
      write(*,*) 'STOP: Input and output file need to differ. NOTHING DONE'
      stop
    end if
    
    ! get command
    call get_command(program_call)
  
    ! get time stamp
    call generate_time_stamp(str_time_stamp)
    
    
    !! ~~~~~ READ OLD DATA ~~~~~
    write(*,*) '~~~~ READING file '//file_in 
    ! OPEN FILE
    nf_stat = NF90_OPEN(file_in, NF90_NOWRITE, ncid_in)
    call check_nf90_stat(nf_stat)
    
    
    ! initialize some data; in future, one could read them in somehow
    allocate(dim_rename(2,1))
    dimnames_in(1) = 'lon'
    dimnames_in(2) = 'lat'
    dimnames_in(3) = 'depth'
    dimnames_in(4) = 'time'
    dimnames_ot(1) = 'lon'
    dimnames_ot(2) = 'lat'
    dimnames_ot(3) = 'lev'
    dimnames_ot(4) = 'time'
    dim_rename(1,1) = 'depth'
    dim_rename(2,1) = 'lev'
    nrename = 1
    
    
    ! get dimensions and dimension sizes
    call get_nf90_dim_infos (ncid_in, dimnames_in, 4, id_dims_in, len_dims_in)
    call get_nf90_var_infos (ncid_in, dimnames_in, 4, id_vardims_in)
    
    
    ! GET VAR IDs
    nf_stat = NF90_INQ_VARID(ncid_in, 'h', id_v_h)
    call check_nf90_stat(nf_stat, 'error inq varid h')
    
    
    ! ALLOCATE ARRAY FOR READ-IN
    allocate(val_v_h(len_dims_in(1), len_dims_in(2), len_dims_in(3), len_dims_in(4)))
    
    
    
    ! READ VAR VALUES
    ! nf_stat = NF90_GET_VAR(ncid_in, id_v_h, val_v_h, start = (/1,1,1,1/), count = (/2,2,2,2/))
    nf_stat = NF90_GET_VAR(ncid_in, id_v_h, val_v_h)
    call check_nf90_stat(nf_stat, 'error get var h')
    
    ! READ _FillValue OR missing_value OF VARIABLE 'h'
    nf_stat = get_NF90_fillvalue(ncid_in, id_v_h, fillval_h)
    call check_nf90_stat(nf_stat, 'error get att _FillValue of var h')
    
    
    
    !! ~~~~~ WRITE NEW FILE ~~~~~
    write(*,*) '~~~~ CREATING new file '//file_ot
    ! create new file
    nf_stat = NF90_CREATE(file_ot, NF90_HDF5, ncid_ot)
    call check_nf90_stat(nf_stat)
    
    ! copy dimensions
    CALL copy_nf90_dimensions(ncid_in, ncid_ot, (/1,1,1,1/), (/-1,-1,-1,-1/), &
                              id_dims_ot, dim_rename, 1)
    DEALLOCATE(id_dims_ot)
    call get_nf90_dim_infos (ncid_ot, dimnames_ot, 4, id_dims_ot, len_dims_ot)
    call correct_nf90_attributes_integrate_layer_depths(ncid_ot)
    nf_stat = NF90_DEF_DIM(ncid_ot, 'nv', 2, id_dim_nv)
        call check_nf90_stat(nf_stat, 'error defining dim')
    
    ! copy variable definitions
    CALL copy_nf90_variable_def(ncid_in, ncid_ot, 'h', (/'depth'/), 1, id_dims_ot, &
                                new_type = NF90_FLOAT, deflate = 1)
    ! get var ids
    nf_stat = NF90_INQ_VARID(ncid_ot, 'depth', id_v_depth)
    call check_nf90_stat(nf_stat, 'error inq var id of var depth')
    call modify_nf90_attributes_depth(ncid_ot)
    
    
    ! create bathymetry variable and deflate it!
    nf_stat = nf90_def_var(ncid_ot, 'bathymetry', NF90_FLOAT, &
                           (/id_dims_ot(1), id_dims_ot(2), id_dims_ot(4)/), &
                           id_v_bathy)
    call check_nf90_stat(nf_stat, 'error def var bathymetry')
    nf_stat = nf90_def_var_deflate(ncid_ot, id_v_bathy, 0, 1, 1)
    call check_nf90_stat(nf_stat, 'error def var bathymetry')
    call modify_nf90_attributes_bathymetry(ncid_ot, real(fillval_h))
    
    
    ! define bounds variables
    CALL def_nf90_bnds_dimvars(ncid_ot, (/dimnames_ot(4)/), &
                               (/id_dims_ot(4)/), 1, &
                               id_dim_nv, id_varbndsdims_ot)
    CALL def_nf90_bnds_var(ncid_ot, 'depth', id_dim_nv, id_v_depth_bnds, deflate = 1)
    
    
    ! set global attributes
    nf_stat = nf90_copy_global_atts(ncid_in, ncid_ot)
    call check_nf90_stat(nf_stat, 'error copy atts global')
    nf_stat = nf90_set_global_atts(ncid_in, ncid_ot, &
                                   str_time_stamp//' '//program_call)
    call check_nf90_stat(nf_stat, 'error copy atts global')
    
    
    ! leave definition mode
    nf_stat = NF90_ENDDEF(ncid_ot)
    call check_nf90_stat(nf_stat, 'error end def mode')
    
    
    
    ! CALCULATE real_depth
    write(*,*) '~~~~ CALCULATING new variables for output file'
      ! allocate arrays
    allocate(val_v_depth(len_dims_in(1), len_dims_in(2), len_dims_in(3), len_dims_in(4)), &
             val_v_depth_bnds(2, len_dims_in(1), len_dims_in(2), len_dims_in(3), len_dims_in(4)), &
             val_v_bathy(len_dims_in(1), len_dims_in(2), len_dims_in(4)))
    
    ! call calculate routine
    call calc_depth(val_v_h, val_v_depth, val_v_bathy, &
                    len_dims_in, fillval_h, depth_bnds = val_v_depth_bnds)
    
    ! write variables
    write(*,*) '~~~~ WRITE new variables into output file'
    nf_stat = NF90_PUT_VAR(ncid_ot, id_v_depth, val_v_depth)
    call check_nf90_stat(nf_stat, 'error put var depth')
    
    nf_stat = NF90_PUT_VAR(ncid_ot, id_v_bathy, val_v_bathy)
    call check_nf90_stat(nf_stat, 'error put var bathymetry')
    nf_stat = NF90_PUT_VAR(ncid_ot, id_v_depth_bnds, val_v_depth_bnds)
    call check_nf90_stat(nf_stat, 'error put var depth_bnds')
    
    
    write(*,*) 'FINISHED writing; CLOSING input and output files; CLEANING up'
    
    
    ! close input file
    nf_stat = NF90_CLOSE(ncid_in)
    call check_nf90_stat(nf_stat)
    
    
    ! close file
    nf_stat = NF90_CLOSE(ncid_ot)
    call check_nf90_stat(nf_stat)
    
    
    ! deallocate arrays
    deallocate(val_v_h, & 
               val_v_depth, val_v_bathy)
  
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
  subroutine calc_depth(h, depth, bathy, count, fillval, depth_bnds) 
    
    implicit none
    
    REAL(8), dimension(:,:,:,:), intent(in)  :: h
    REAL(8), dimension(:,:,:,:), intent(out) :: depth
    REAL(8), dimension(:,:,:),   intent(out) :: bathy
    integer, DIMENSION(4),       intent(in)  :: count
    real(8), optional,           intent(in)  :: fillval
    REAL(8), dimension(:,:,:,:,:), optional, intent(out) :: depth_bnds
    
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
      do iZ = 2, nZ
        depth(1:nLo, 1:nLa, iZ, 1:nTi) = depth(1:nLo, 1:nLa, iZ-1, 1:nTi) + &
                                           h(1:nLo, 1:nLa, iZ, 1:nTi)
      end do
      bathy(1:nLo, 1:nLa, 1:nTi)     = depth(1:nLo, 1:nLa, nZ, 1:nTi)
      depth_bnds(1, 1:nLo, 1:nLa, 1:nZ, 1:nTi) = depth(1:nLo, 1:nLa, 1:nZ, 1:nTi) - &
                                               h(1:nLo, 1:nLa, 1:nZ, 1:nTi)
      depth_bnds(2, 1:nLo, 1:nLa, 1:nZ, 1:nTi) = depth(1:nLo, 1:nLa, 1:nZ, 1:nTi)
      depth(1:nLo, 1:nLa, 1:nZ, 1:nTi) = depth(1:nLo, 1:nLa, 1:nZ, 1:nTi) - &
                                         h(1:nLo, 1:nLa, 1:nZ, 1:nTi)/2.0_8
      
    else 
      
      ! depth = fillval
      ! depth_bnds = fillval
      ! bathy = fillval
      depth = 0.0_8
      depth_bnds = 0.0_8
      bathy = 0.0_8
      
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
                depth_bnds(1, iLo, iLa, iZ, iTi) = prev_depth - this_h * half
                depth(iLo, iLa, iZ, iTi) = prev_depth
                depth_bnds(2, iLo, iLa, iZ, iTi) = prev_depth + this_h * half
                iZ = iZ + 1
              endif
              
            end do
            
            if (iZ .eq. 1) then
              ! bathy(iLo, iLa, iTi) = fillval
              bathy(iLo, iLa, iTi) = 0.0_8
            end if
            
            if (iZ .eq. nZ+1) then
              bathy(iLo, iLa, iTi) = depth_bnds(2, iLo, iLa, nZ, iTi)
            end if
            
          end do
        end do
!$omp end parallel do
      end do
    end if
    
  end subroutine calc_depth
  ! ~~~~~ END CALC DEPTH ~~~~~


end program integrate_layer_depth