#!/bin/bash

controldir=/media/neumannd/work_dell/69_Tools_Tasks/pub_neumannd2017a/F_calc_n_distribution/control_files
maskdir=/media/neumannd/work_dell/69_Tools_Tasks/pub_neumannd2017a/data_grid
datadir=/media/neumannd/work_dell/69_Tools_Tasks/pub_neumannd2017a/data_test_F_calc_n_distribution
outdirtext=/media/neumannd/work_dell/69_Tools_Tasks/pub_neumannd2017a/data_gotland_basin/time_series

fVar=${controldir}/variables.dat
fLay=${controldir}/layers.dat
fMask=${maskdir}/east_gotland_basin_mask_byte.nc
fVol=${datadir}/p_grddat_coarse_MR_WORKE_merge_mean_2012_day001to005.nc
fIn=${datadir}/p_biodat_coarse_MR_WORKT1_merge_mean_2012_day001to005.nc
fOt=${datadir}/test.nc


./bin/calc_n_distribution.x ${fVar} ${fLay} ${fMask} ${fVol} ${fIn} ${fOt} ${outdirtext}

