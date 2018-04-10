#!/bin/bash

echo '~~~~ start OpenMP ~~~~'
date
time ./integrate_layer_depth_wiOpenMP.x /media/neumannd/work_dell/36_Data_Long_Term_Storage/2018__IOW_THREDDS/integrate_layer_depth/test_file/p_z_file_coarse_MR_WORKE_merge_mean_2012_day001_day005.nc /media/neumannd/work_dell/36_Data_Long_Term_Storage/2018__IOW_THREDDS/integrate_layer_depth/test_file/test.nc
date

echo ''
echo '~~~~ start no OpenMP ~~~~'
date
time ./integrate_layer_depth_noOpenMP.x /media/neumannd/work_dell/36_Data_Long_Term_Storage/2018__IOW_THREDDS/integrate_layer_depth/test_file/p_z_file_coarse_MR_WORKE_merge_mean_2012_day001_day005.nc /media/neumannd/work_dell/36_Data_Long_Term_Storage/2018__IOW_THREDDS/integrate_layer_depth/test_file/test.nc
date

echo '~~~~ old with OpenMP ~~~~'
date
time ./integrate_layer_depth.old.v01.20180408_wiOpenMP.x /media/neumannd/work_dell/36_Data_Long_Term_Storage/2018__IOW_THREDDS/integrate_layer_depth/test_file/p_z_file_coarse_MR_WORKE_merge_mean_2012_day001_day005.nc /media/neumannd/work_dell/36_Data_Long_Term_Storage/2018__IOW_THREDDS/integrate_layer_depth/test_file/test.nc
date

