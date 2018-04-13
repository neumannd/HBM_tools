!> \file create_mask.f90
!! \brief Creates a netCDF Mask file from an existing netCDF mask file and a polygon
! 
! TODO
! 
! Try it!
! 
! TODO
! 
! I incorporated a point-in-a-polygon-algorithm (pnply) in this program.
! 
! The used pnply algorith is a modified version of one by Wm. Randolph Franklin 
! (Copyright (c) 1970-2003). The modifications are based on an answer at
! Stackoverflow by the user '4pie0'.
! 
! The original code is available at 
!        https://wrf.ecse.rpi.edu//Research/Short_Notes/pnpoly.html
! 
! The Stackoverflow answer is available at 
!        https://stackoverflow.com/a/32807570/4612235
! 
! This code was written by Daniel Neumann at the Leibniz-Institut for Baltic
! Sea Research Warnemuende (www.io-warnemuende.de). The work was done within'
! the project MeRamo (funded by BMVI, FKZ 50EW1601)
!
!> Module: Main Program, create_mask
!> \author Daniel Neumann, IOW
!> \date 13.04.2018


program create_mask

  use point_in_polygon
  use read_polygon
  use check_stat_netcdf
  use check_stat_nonetcdf
  use time_tools
  use read_files
  use netcdf
  use nf90_tools_create_mask
  
  implicit none
  
  
  
  ! file names
  character(len=255) :: file_polygon, file_in, file_ot
  
  ! polygon vertices
  real(8), dimension(:), allocatable :: vertx, verty
  integer                            :: nvert
  
  ! integers for testing results
  integer    :: pnttest_bnd = 0
  
  ! netCDF ids
  integer             :: ncid_in, ncid_ot
  integer             :: id_d_lon, id_d_lat    ! dimension id_s
  integer             :: id_v_lon, id_v_lat    ! variable id_s 1
  integer             :: id_v_mask             ! further variable id_s
  
  ! netCDF dim sizes
  integer             :: n_lon, n_lat
  
  ! data variables
  real(8),    dimension(:),   allocatable :: val_v_lat, val_v_lon
  integer(1), dimension(:,:), allocatable :: val_v_mask
  integer(1)                              :: fillval_mask
  logical                                 :: bad_fillval_mask
  integer(1)                              :: mask_true = 1_1
  
  ! variable names
  character(len=4)    :: name_v_mask_in = 'mask'
  character(len=4)    :: name_v_mask_ot = 'mask'
  
  ! iterators and status variables
  integer :: ilon, ilat
  integer :: nf_stat
  
  ! variables for timestamp setting
  character (len=20)       :: str_time_stamp
  
  ! variable for program call information
  character (len=2048) :: program_call
  
  ! control flags
  logical :: helpme = .false.
  
  
  
  !! ~~~~~ INITIALIZE ~~~~~
  ! GET TIME AND DATE FOR HISTORY ATTRIBUTE and construct time stamp string
  call generate_time_stamp(str_time_stamp)
  
  ! get command
  call get_command(program_call)
  
  ! test for help string
  call test_help_string(helpme)
  
  if (helpme) then
    
    call print_help()
    
  else ! .not. helpme
  
    ! get filename
    call get_filenames(file_polygon, file_in, file_ot)
    
    ! read polygon coordinates
    call read_coords(file_polygon, vertx, verty, nvert)
    
    
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
    
    ! GET VAR IDs
    nf_stat = NF90_INQ_VARID(ncid_in, 'lon', id_v_lon)
    call check_nf90_stat(nf_stat, 'error inq varid lon')
    nf_stat = NF90_INQ_VARID(ncid_in, 'lat', id_v_lat)
    call check_nf90_stat(nf_stat, 'error inq varid lat')
    nf_stat = NF90_INQ_VARID(ncid_in, name_v_mask_in, id_v_mask)
    call check_nf90_stat(nf_stat, 'error inq varid mask')
    
    ! READ DIM SIZEs
    nf_stat = nf90_inquire_dimension(ncid_in, id_d_lon, len = n_lon)
    call check_nf90_stat(nf_stat, 'error get dim lon')
    nf_stat = nf90_inquire_dimension(ncid_in, id_d_lat, len = n_lat)
    call check_nf90_stat(nf_stat, 'error get dim lat')
    
    ! ALLOCATE ARRAY FOR READ-IN
    allocate(val_v_lon(n_lon), val_v_lat(n_lat), val_v_mask(n_lon, n_lat))
    
    ! READ VAR VALUES
    nf_stat = NF90_GET_VAR(ncid_in, id_v_lon, val_v_lon)
    call check_nf90_stat(nf_stat, 'error get var lon')
    
    nf_stat = NF90_GET_VAR(ncid_in, id_v_lat, val_v_lat)
    call check_nf90_stat(nf_stat, 'error get var lat')
    
    nf_stat = NF90_GET_VAR(ncid_in, id_v_mask, val_v_mask)
    call check_nf90_stat(nf_stat, 'error get var '//name_v_mask_in)
    
    ! READ FILLVALUE
    bad_fillval_mask = .FALSE.
    nf_stat = nf90_get_att(ncid_in, id_v_mask, '_FillValue', fillval_mask)
    
    if ( nf_stat .eq. -43 ) then
      write(*,*) 'Warning: attribute _FillValue not found for variable h. '//&
                  'Trying missing_value.'
      nf_stat = nf90_get_att(ncid_in, id_v_mask, 'missing_value', fillval_mask)
      if ( nf_stat .eq. -43 ) then
        write(*,*) 'Warning: attribute missing_value not found for variable h. '//&
                    'Using no _FillValue'
        bad_fillval_mask = .TRUE.
      else
        call check_nf90_stat(nf_stat, 'error get att missing_value of var h')
      end if
    else
      call check_nf90_stat(nf_stat, 'error get att _FillValue of var h')
    end if
    
    
    
    !! ~~~~~ test coordinates if they are located in the polygon ~~~~~
    write(*,*) '~~~~ CALCULATING new mask'
    DO ilat = 1, n_lat
      do ilon = 1, n_lon
        if (val_v_mask(ilon, ilat) .eq. mask_true) then
          val_v_mask(ilon, ilat) = int((pnpoly_bnd(nvert, vertx, verty, val_v_lon(ilon), val_v_lat(ilat)) + 1)/2, 1)
        end if
      end do
    end do
    
    
    
    
    !! ~~~~~ write output file
    write(*,*) '~~~~ CREATING new file '//file_ot
    ! create new file
    nf_stat = NF90_CREATE(file_ot, NF90_HDF5, ncid_ot)
    call check_nf90_stat(nf_stat)
    
    ! create dimensions
    nf_stat = nf90_def_dim(ncid_ot, 'lon', n_lon, id_d_lon)
    call check_nf90_stat(nf_stat, 'error def dim lon')
    nf_stat = nf90_def_dim(ncid_ot, 'lat', n_lat, id_d_lat)
    call check_nf90_stat(nf_stat, 'error def dim lat')
    
    ! create variables
    nf_stat = nf90_def_var(ncid_ot, 'lon', NF90_DOUBLE, (/id_d_lon/), id_v_lon)
    call check_nf90_stat(nf_stat, 'error def var lon')
    nf_stat = nf90_def_var(ncid_ot, 'lat', NF90_DOUBLE, (/id_d_lat/), id_v_lat)
    call check_nf90_stat(nf_stat, 'error def var lat')
    
    ! create depth variable and deflate it!
    nf_stat = nf90_def_var(ncid_ot, name_v_mask_ot, NF90_BYTE, &
                           (/id_d_lon,id_d_lat/), &
                           id_v_mask)
    call check_nf90_stat(nf_stat, 'error def var '//name_v_mask_ot)
    nf_stat = nf90_def_var_deflate(ncid_ot, id_v_mask, 0, 1, 1)
    call check_nf90_stat(nf_stat, 'error def var '//name_v_mask_ot)
    
    ! copy attributes:
    nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'lon', 'lon')
    call check_nf90_stat(nf_stat, 'error copy atts lon')
    nf_stat = nf90_copy_all_atts(ncid_in, ncid_ot, 'lat', 'lat')
    call check_nf90_stat(nf_stat, 'error copy atts lat')
    
    ! set global attributes
    nf_stat = nf90_copy_global_atts(ncid_in, ncid_ot)
    call check_nf90_stat(nf_stat, 'error copy atts global')
    nf_stat = nf90_set_global_atts(ncid_in, ncid_ot, &
                                   str_time_stamp//' '//trim(program_call))
    call check_nf90_stat(nf_stat, 'error copy atts global')
    
    ! set MASK attributes
    if (.not. bad_fillval_mask) then
      nf_stat = nf90_set_atts_mask(ncid_ot, name_v_mask_ot, fillval_mask)
      call check_nf90_stat(nf_stat, 'error write atts for '//name_v_mask_ot)
    else
      nf_stat = nf90_set_atts_mask(ncid_ot, name_v_mask_ot)
      call check_nf90_stat(nf_stat, 'error write atts for '//name_v_mask_ot)
    end if
    
    ! leave definition mode
    nf_stat = NF90_ENDDEF(ncid_ot)
    call check_nf90_stat(nf_stat, 'error end def mode')
    
    ! write variables
    write(*,*) '~~~~ WRITE new variables into output file'
    nf_stat = NF90_PUT_VAR(ncid_ot, id_v_lon, val_v_lon)
    call check_nf90_stat(nf_stat, 'error put var lon')
    
    nf_stat = NF90_PUT_VAR(ncid_ot, id_v_lat, val_v_lat)
    call check_nf90_stat(nf_stat, 'error put var lat')
    
    nf_stat = NF90_PUT_VAR(ncid_ot, id_v_mask, val_v_mask)
    call check_nf90_stat(nf_stat, 'error put var '//name_v_mask_ot)
    
    
    
    write(*,*) 'FINISHED writing; CLOSING input and output files; CLEANING up'
    
    
    ! close input file
    nf_stat = NF90_CLOSE(ncid_in)
    call check_nf90_stat(nf_stat, 'closing input file')
    
    
    ! close file
    nf_stat = NF90_CLOSE(ncid_ot)
    call check_nf90_stat(nf_stat, 'closing output file')
    
    
    ! DEALLOCATE arrays
    deallocate(val_v_lon, val_v_lat, val_v_mask)
  
  end if ! end .not. helpme
  
  
  
contains

  subroutine print_help()
  
    write(*,'(A)') 'HELP FOR create_mask                                                 2018/04/13'
    write(*,'(A)') ''
    write(*,'(A)') 'NAME'
    write(*,'(A)') ''
    write(*,'(A)') '      create mask file - Creates a netCDF Mask file from an existing netCDF'
    write(*,'(A)') '                         mask file and a polygon.'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'SYNOPSIS'
    write(*,'(A)') '      create_mask.x polygon.txt mask_in.nc mask_ot.nc'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'DESCRIPTION'
    write(*,'(A)') '      Creates a netCDF Mask file from an existing netCDF mask file and a polygon'
    write(*,'(A)') '      '
    write(*,'(A)') '      TODO'
    write(*,'(A)') '      '
    write(*,'(A)') '      Try it!'
    write(*,'(A)') '      '
    write(*,'(A)') '      TODO'
    write(*,'(A)') '      '
    write(*,'(A)') '      I incorporated a point-in-a-polygon-algorithm (pnply) in this program.'
    write(*,'(A)') '      '
    write(*,'(A)') '      The used pnply algorith is a modified version of one by Wm. Randolph '
    write(*,'(A)') '      Franklin (Copyright (c) 1970-2003). The modifications are based on an '
    write(*,'(A)') '      answer at Stackoverflow by the user 4pie0.'
    write(*,'(A)') '      '
    write(*,'(A)') '      The original code is available at '
    write(*,'(A)') '             https://wrf.ecse.rpi.edu//Research/Short_Notes/pnpoly.html'
    write(*,'(A)') '      '
    write(*,'(A)') '      The Stackoverflow answer is available at '
    write(*,'(A)') '             https://stackoverflow.com/a/32807570/4612235'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'OPTIONS'
    write(*,'(A)') '      -h, --help'
    write(*,'(A)') '           Print this help.'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'FILE DESCRIPTIONS'
    write(*,'(A)') ''
    write(*,'(A)') '      polygon.txt'
    write(*,'(A)') '         '
    write(*,'(A)') '         The file needs to have a header row. In each subsequent row one '
    write(*,'(A)') '         set of lon-lat-coordinates needs to be provided. The coordinates'
    write(*,'(A)') '         are align in two columns. Each column has a width of 6. Both columns'
    write(*,'(A)') '         are separated by a semicolon --;--. The coordinates have to be '
    write(*,'(A)') '         formatted as XX.XXX (F6.3). Here is an example:'
    write(*,'(A)') '         '
    write(*,'(A)') '         ------------------------------------'
    write(*,'(A)') '         Xcoord;Ycoord'
    write(*,'(A)') '         18.150;56.183'
    write(*,'(A)') '         18.333;56.500'
    write(*,'(A)') '         18.167;56.917'
    write(*,'(A)') '         18.400;57.600'
    write(*,'(A)') '         19.167;58.000'
    write(*,'(A)') '         19.658;57.656'
    write(*,'(A)') '         20.699;58.104'
    write(*,'(A)') '         22.266;58.235'
    write(*,'(A)') '         22.223;58.104'
    write(*,'(A)') '         22.156;58.010'
    write(*,'(A)') '         22.076;57.914'
    write(*,'(A)') '         21.711;57.556'
    write(*,'(A)') '         21.368;55.809'
    write(*,'(A)') '         19.797;56.049'
    write(*,'(A)') '         18.368;55.342'
    write(*,'(A)') '         17.436;55.584'
    write(*,'(A)') '         18.150;56.183'
    write(*,'(A)') '         ------------------------------------'
    write(*,'(A)') '         '
    write(*,'(A)') '         '
    write(*,'(A)') '      file_mask.nc'
    write(*,'(A)') '         '
    write(*,'(A)') '         The dimensions need to be denoted as --lon-- and --lat--. The mask'
    write(*,'(A)') '         variable needs to be denoted as --mask-- and be of type --byte--.'
    write(*,'(A)') '         netCDF-4 files are accepted.'
    write(*,'(A)') '         '
    write(*,'(A)') '         Here is an example CDL output:'
    write(*,'(A)') '         '
    write(*,'(A)') '         ------------------------------------'
    write(*,'(A)') '         netcdf file_mask {'
    write(*,'(A)') '         dimensions:'
    write(*,'(A)') '            lon = 414 ;'
    write(*,'(A)') '            lat = 347 ;'
    write(*,'(A)') '         variables:'
    write(*,'(A)') '            double lon(lon) ;'
    write(*,'(A)') '                lon:units = "degrees_east" ;'
    write(*,'(A)') '                lon:standard_name = "longitude" ;'
    write(*,'(A)') '                lon:long_name = "longitude" ;'
    write(*,'(A)') '                lon:axis = "x" ;'
    write(*,'(A)') '            double lat(lat) ;'
    write(*,'(A)') '                lat:units = "degrees_north" ;'
    write(*,'(A)') '                lat:standard_name = "latitude" ;'
    write(*,'(A)') '                lat:long_name = "latitude" ;'
    write(*,'(A)') '                lat:axis = "y" ;'
    write(*,'(A)') '            byte mask(lat, lon) ;'
    write(*,'(A)') '                mask:standard_name = "TODO_binary_mask" ;'
    write(*,'(A)') '                mask:long_name = "TODO_binary_mask" ;'
    write(*,'(A)') '                mask:units = "1" ;'
    write(*,'(A)') '                mask:description = "1 = TODO area." ;'
    write(*,'(A)') '                mask:_FillValue = -1b ;'
    write(*,'(A)') '                mask:missing_value = -1b ;'
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
    

end program create_mask