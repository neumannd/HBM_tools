!> \file calc_n_distribution.f90
!! \brief calculate vertical and horizontal average nutrient amounts in specific area
! 
! We calculate the vertical sum over some model layers and write it into 
! the output file --file_ot.nc--. Additionally, we calculate the horizontal
! sum and write each variable into one file into the output directory 
! --dir_ot_text--. The layers, which are considered, are defined in 
! --file_layers.txt--. The variables are read from the input file 
! --file_in.nc--. The variables, which should be considered, are listed in 
! --file_varnames.txt--. The file --file_mask.nc-- provides a horizontal
! mask, on which grid cells are to consider. The file --file_volume.nc--
! contains the volume of each grid cell in order to calculate the amount of
! nutrients from the concentrations of nutrients.
!
! This code was written by Daniel Neumann at the Leibniz-Institut for Baltic
! Sea Research Warnemuende (www.io-warnemuende.de). The work was done within'
! the project MeRamo (funded by BMVI, FKZ 50EW1601)
!
!> Module: Main Program, calc_n_distribution
!> \author Daniel Neumann, IOW
!> \date 13.04.2018


program calc_n_distribution

  use check_stat_netcdf
  use check_stat_nonetcdf
  use read_files
  use mask_processing
  use time_tools
  use netcdf
  use text_var_data
  use process_layer_data
  use nf90_var_data
  
  implicit none
  
  
  
  ! file names
  character(len=255) :: file_varnames, file_layers, file_mask, file_volume, &
                        file_in, file_ot, dir_ot_text
  
  ! variable names
  character(len=4)    :: name_v_mask = 'mask'
  character(len=11)   :: name_v_volume = 'cell_volume'
  
  ! informations on the layers to process
  character(len=32),  dimension(:), Allocatable :: l_prefix  ! size: n_layers
  character(len=255), dimension(:), Allocatable :: l_comment ! size: n_layers
  integer,            dimension(:), allocatable :: l_start, l_end ! size: n_layers
  integer                                       :: n_layers
  
  ! information on variables to process
  character(len=255), dimension(:), Allocatable :: names_v_process ! size: n_varnames_process
  integer                                       :: n_varnames_process
  
  ! information on variables to process x layer
  character(len=255), dimension(:,:), Allocatable :: names_v_process_layer ! size: n_varnames_process x n_layers
  
  ! information on the mask
  integer,    dimension(2)                :: start_mask, count_mask
  integer(1), dimension(:,:), allocatable :: vals_mask
  
  ! information on the volume and model data
  real(8), dimension(:,:,:,:), allocatable :: vals_volume, vals_volume_nofill
  real(8)                                  :: fillval_volume, fillval_data
  integer, dimension(4)                    :: start_data_4d, count_data_4d
  integer, dimension(3)                    :: start_data_3d, count_data_3d
  real(8), dimension(:,:,:,:), allocatable :: vals_data_4d
  real(8), dimension(:,:,:),   allocatable :: vals_data_3d, vals_data_3d_nofill
  real(8), dimension(:),       allocatable :: vals_data_1d
  
  ! iterators
  integer :: ilay, ivar
  
  ! status variables
  integer :: nf_stat
  
  ! variables for timestamp setting 
  character (len=20) :: str_time_stamp
  
  ! variable for program call information
  character (len=2048) :: program_call
  
  ! netCDF ids
  integer :: ncid_in, ncid_ot, id_v_data
  
  ! control flags
  logical :: debug = .false., helpme = .false.
  
  
  
  !! ~~~~~ INITIALIZE ~~~~~
  call generate_time_stamp(str_time_stamp)
  
  ! get command
  call get_command(program_call)
  
  ! test for help string
  call test_help_string(helpme)
  
  if (helpme) then
    
    call print_help()
    
  else ! .not. helpme
    
    write(*,'(A)') '~~~~~ initialize ~~~~~'
  
    ! get filename
    call get_filenames(file_varnames, file_layers, file_mask, file_volume, &
                       file_in, file_ot, dir_ot_text)
    
    ! read variable names to process
    call read_variables(file_varnames, names_v_process, n_varnames_process, &
                        header = .true.)
    
    ! read layers to process
    call read_layers(file_layers, l_prefix, l_start, l_end, l_comment, &
                     n_layers, header = .true.)
    
    ! construct output variable names
    allocate(names_v_process_layer(n_varnames_process, n_layers))
    do ivar = 1, n_varnames_process
      do ilay = 1, n_layers
        write(names_v_process_layer(ivar,ilay),'(A)') trim(l_prefix(ilay))//&
                                                  trim(names_v_process(ivar))
      end do
    end do
    
    
    ! get mask section
    write(*,'(A)') '~~~~~ get mask ~~~~~'
    call get_mask_window(file_mask, name_v_mask, vals_mask, start_mask, &
                         count_mask)
    
    
    ! copy definition of input netcdf to output netcdf file
    write(*,'(A)') '~~~~~ copy variable definition ~~~~~'
    call copy_nf90_file_def(file_in, file_ot, ncid_in, ncid_ot,   &
                            names_v_process, names_v_process_layer, &
                            (/n_varnames_process, n_layers/),       &
                            (/1, 1, -1, 1/), (/-1, -1, -1, -1/),   &
                            .true., .true., l_comment)
    nf_stat = NF90_REDEF(ncid_ot)
    call check_nf90_stat(nf_stat, 'leaving definition mode of file '//file_ot)
    nf_stat = nf90_copy_global_atts(ncid_in, ncid_ot)
    call check_nf90_stat(nf_stat, 'copy global attributes to file '//file_ot)
    nf_stat = nf90_set_global_atts(ncid_ot, program_call)
    call check_nf90_stat(nf_stat, 'write global attributes to file '//file_ot)
    nf_stat = NF90_ENDDEF(ncid_ot)
    call check_nf90_stat(nf_stat, 'leaving definition mode of file '//file_ot)
    nf_stat = NF90_SYNC(ncid_ot)
    call check_nf90_stat(nf_stat, 'syncing file '//file_ot)
    
    write(*,'(A)') '~~~~~ start processing variables ~~~~~'
    ! set start and count values of first two dimensions (lon and lat)
    !  the third dimension is set in the beginning of the next loop and
    !  the fourth dimension is set in 'get_cell_volume'. The '-1' in 
    !  start_data(4) indicates that get_cell_volume should do it.
    start_data_4d(1) = start_mask(1)
    start_data_4d(2) = start_mask(2)
    count_data_4d(1) = count_mask(1)
    count_data_4d(2) = count_mask(2)
    
    start_data_4d(4) = -1
    count_data_4d(4) = -1
    
    do ilay = 1, n_layers
      
      start_data_4d(3) = min(l_start(ilay),l_end(ilay))
      count_data_4d(3) = abs(l_start(ilay)-l_end(ilay)) + 1
      
      call get_nf90_var_data(file_volume, name_v_volume, vals_volume,      &
                             start_data_4d, count_data_4d, fillval_volume)
      call apply_mask(vals_mask, vals_volume, fillval_volume, count_data_4d)
      CALL replace_fillvals2zero(vals_volume, vals_volume_nofill,          &
                                 fillval_volume, count_data_4d)
      
      do ivar = 1, n_varnames_process
        
        if (debug) write(*,*) ' ~~~~ get var '//names_v_process(ivar)//' ~~~~'
        ! get data
        call get_nf90_var_data(ncid_in, names_v_process(ivar), vals_data_4d, &
                               start_data_4d, count_data_4d)
        ! get new fill value
        nf_stat = NF90_INQ_VARID(ncid_ot, names_v_process_layer(ivar, ilay), id_v_data)
        call check_nf90_stat(nf_stat, 'inq var id '//names_v_process(ivar))
        nf_stat = get_NF90_fillvalue(ncid_ot, id_v_data, fillval_data)
        call check_nf90_stat(nf_stat, 'inq fillval '//names_v_process(ivar))
        
        ! vertical sum
        call sum_layer_data(vals_volume_nofill, vals_data_4d,   &
                             vals_data_3d_nofill, vals_data_1d, &
                             count_data_4d)
        
        ! 3d start and count values
        start_data_3d = (/start_data_4d(1), start_data_4d(2), start_data_4d(4)/)
        count_data_3d = (/count_data_4d(1), count_data_4d(2), count_data_4d(4)/)
        
        ! calculate umol to mol
        vals_data_3d_nofill = vals_data_3d_nofill/1000000.0_8
        vals_data_1d = vals_data_1d/1000000.0_8
        
        call reset_fillvals(vals_volume(:,:,1,:), vals_data_3d_nofill, &
                            vals_data_3d, fillval_data, count_data_3d)
        
        call put_nf90_var_data(ncid_ot, names_v_process_layer(ivar, ilay), &
                               vals_data_3d, start_data_3d,    &
                               count_data_3d)
        
        call put_text_var_data(trim(dir_ot_text)//'/'//&
                               trim(names_v_process_layer(ivar, ilay))//'.dat', &
                               vals_data_1d, count_data_4d(4), &
                               names_v_process_layer(ivar, ilay), 'mol')
        
        deallocate(vals_data_4d, vals_data_3d, vals_data_1d)
        
      end do
      
      deallocate(vals_volume)
    
    end do
    
    
    write(*,'(A)') '~~~~~ finished writing; closing input and output files; cleaning up ~~~~~'
    
    ! close input file
    nf_stat = NF90_CLOSE(ncid_in)
    call check_nf90_stat(nf_stat, 'closing input file')
    
    
    ! close file
    nf_stat = NF90_CLOSE(ncid_ot)
    call check_nf90_stat(nf_stat, 'closing output file')
    
    
    ! deallocate
    deallocate(l_prefix, l_comment, l_start, l_end, names_v_process, &
               names_v_process_layer, vals_mask)
  
  end if ! end .not. helpme
  
contains

  subroutine print_help()
  
    write(*,'(A)') 'HELP FOR calc_n_distribution                                         2018/04/13'
    write(*,'(A)') ''
    write(*,'(A)') 'NAME'
    write(*,'(A)') ''
    write(*,'(A)') '      calc_n_distribution - calculate vertical and horizontal average nutrient '
    write(*,'(A)') '                            amounts in specific area'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'SYNOPSIS'
    write(*,'(A)') '      calc_n_distribution.x file_varnames.txt file_layers.txt file_mask.nc \'
    write(*,'(A)') '                            file_volume.nc file_in.nc file_ot.nc dir_ot_text'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'DESCRIPTION'
    write(*,'(A)') '      We calculate the vertical sum over some model layers and write it into '
    write(*,'(A)') '      the output file --file_ot.nc--. Additionally, we calculate the horizontal'
    write(*,'(A)') '      sum and write each variable into one file into the output directory '
    write(*,'(A)') '      --dir_ot_text--. The layers, which are considered, are defined in '
    write(*,'(A)') '      --file_layers.txt--. The variables are read from the input file '
    write(*,'(A)') '      --file_in.nc--. The variables, which should be considered, are listed in '
    write(*,'(A)') '      --file_varnames.txt--. The file --file_mask.nc-- provides a horizontal'
    write(*,'(A)') '      mask, on which grid cells are to consider. The file --file_volume.nc--'
    write(*,'(A)') '      contains the volume of each grid cell in order to calculate the amount of'
    write(*,'(A)') '      nutrients from the concentrations of nutrients.'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'OPTIONS'
    write(*,'(A)') '      -h, --help'
    write(*,'(A)') '           Print this help.'
    write(*,'(A)') ''
    write(*,'(A)') ''
    write(*,'(A)') 'FILE DESCRIPTIONS'
    write(*,'(A)') ''
    write(*,'(A)') '      file_varnames.txt'
    write(*,'(A)') '         '
    write(*,'(A)') '         The file needs to have a header row. In each subsequent row one '
    write(*,'(A)') '         variable name per row needs to be provided. Here is an example:'
    write(*,'(A)') '         '
    write(*,'(A)') '         ------------------------------------'
    write(*,'(A)') '         my variable names'
    write(*,'(A)') '         variable1'
    write(*,'(A)') '         variable2'
    write(*,'(A)') '         variable3'
    write(*,'(A)') '         ------------------------------------'
    write(*,'(A)') '         '
    write(*,'(A)') '         '
    write(*,'(A)') '      file_layers.txt'
    write(*,'(A)') '         '
    write(*,'(A)') '         The file needs to have a header row. In each subsequent row one layer'
    write(*,'(A)') '         definition has to be provided. A layer definition consists of a suffix,'
    write(*,'(A)') '         a minimum layer, a maximum layer, and a comment. The vertical sum will'
    write(*,'(A)') '         be calculated from the min to the max layer. It is no problem if both'
    write(*,'(A)') '         values are exchanged. The prefix is prepended to the variable names in'
    write(*,'(A)') '         the output netCDF file and to the filenames of the output text files.'
    write(*,'(A)') '         The comment has no effect.'
    write(*,'(A)') '         '
    write(*,'(A)') '         NOTE: Please not the exact format specification. Column 1 has a width'
    write(*,'(A)') '         of 32, colums 2 and 3 each a width of 5, and column 4 and maximum width'
    write(*,'(A)') '         of 255. Each column is separated by a semicolon --;--, which is not '
    write(*,'(A)') '         counted to the column width. Thus, the maximum width of one line is'
    write(*,'(A)') '         300 (= 32 + 1 + 5 + 1 + 5 + 1 + 255).'
    write(*,'(A)') '         '
    write(*,'(A)') '         Here is an example:'
    write(*,'(A)') '         '
    write(*,'(A)') '         ------------------------------------'
    write(*,'(A)') '         layer name                      ;min  ;max  ;comment'
    write(*,'(A)') '         sur_                            ;    1;    5; surface layer'
    write(*,'(A)') '         bot_                            ;   28;   36; layers below halocline'
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
    write(*,'(A)') '         '
    write(*,'(A)') '         '
    write(*,'(A)') '      file_volume.nc'
    write(*,'(A)') '         '
    write(*,'(A)') '         The dimensions need to be denoted as --lon--, --lat--, --depth--, and '
    write(*,'(A)') '         --time--. The volume variable needs to be denoted as --cell_volume-- '
    write(*,'(A)') '         and be of type --float-- or --double--. Its unit needs to be equal to'
    write(*,'(A)') '         the volume-unit in the concentration of the variables in the variable'
    write(*,'(A)') '         input --file_in.nc--. netCDF-4 files are accepted.'
    write(*,'(A)') '         '
    write(*,'(A)') '         Here is an example CDL output:'
    write(*,'(A)') '         '
    write(*,'(A)') '         ------------------------------------'
    write(*,'(A)') '         netcdf file_volume {'
    write(*,'(A)') '         dimensions:'
    write(*,'(A)') '            lon = 414 ;'
    write(*,'(A)') '            lat = 347 ;'
    write(*,'(A)') '            depth = 36 ;'
    write(*,'(A)') '            time = 5 ;'
    write(*,'(A)') '         variables:'
    write(*,'(A)') '            double lon(lon) ;'
    write(*,'(A)') '                ...'
    write(*,'(A)') '            double lat(lat) ;'
    write(*,'(A)') '                ...'
    write(*,'(A)') '            double depth(depth) ;'
    write(*,'(A)') '                ...'
    write(*,'(A)') '            double time(time) ;'
    write(*,'(A)') '                ...'
    write(*,'(A)') '            float cell_volume(time, depth, lat, lon) ;'
    write(*,'(A)') '                cell_volume:standard_name = "cell_volumne" ;'
    write(*,'(A)') '                cell_volume:long_name = "cell_volume" ;'
    write(*,'(A)') '                cell_volume:units = "m3" ;'
    write(*,'(A)') '                cell_volume:description = "..." ;'
    write(*,'(A)') '                cell_volume:_FillValue = -999.f ;'
    write(*,'(A)') '                cell_volume:missing_value = -999.f ;'
    write(*,'(A)') '         }'
    write(*,'(A)') '         ------------------------------------'
    write(*,'(A)') '         '
    write(*,'(A)') '         '
    write(*,'(A)') '      file_in.nc'
    write(*,'(A)') '         '
    write(*,'(A)') '         The dimensions need to be denoted as --lon--, --lat--, --depth--, and '
    write(*,'(A)') '         --time--. The data variables need to be of type --float-- or '
    write(*,'(A)') '         --double--. Its volume-unit in the denominator needs to be equal to'
    write(*,'(A)') '         the volume-unit in the grid cell volume variable of the volume file '
    write(*,'(A)') '         --file_volume.nc--. netCDF-4 files are accepted.'
    write(*,'(A)') '         '
    write(*,'(A)') '         Here is an example CDL output:'
    write(*,'(A)') '         '
    write(*,'(A)') '         ------------------------------------'
    write(*,'(A)') '         netcdf p_biodat_coarse_MR_WORKT1_merge_mean_2012_day001to005 {'
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
    write(*,'(A)') '            int variable1(time, depth, lat, lon) ;'
    write(*,'(A)') '                variable1:standard_name = "variable1_in_seawater" ;'
    write(*,'(A)') '                variable1:units = "micromol N/m3" ;'
    write(*,'(A)') '                variable1:_FillValue = -999 ;'
    write(*,'(A)') '                variable1:missing_value = -999 ;'
    write(*,'(A)') '            int variable1(time, depth, lat, lon) ;'
    write(*,'(A)') '                variable2:standard_name = "variable2_in_seawater" ;'
    write(*,'(A)') '                variable2:units = "micromol N/m3" ;'
    write(*,'(A)') '                variable2:_FillValue = -999 ;'
    write(*,'(A)') '                variable2:missing_value = -999 ;'
    write(*,'(A)') '            int variable3(time, depth, lat, lon) ;'
    write(*,'(A)') '                variable3:standard_name = "variable3_in_seawater" ;'
    write(*,'(A)') '                variable3:units = "micromol N/m3" ;'
    write(*,'(A)') '                variable3:_FillValue = -999 ;'
    write(*,'(A)') '                variable3:missing_value = -999 ;'
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
  
end program calc_n_distribution