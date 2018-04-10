#!/bin/bash

dIn=/gfs1/work/mvkdneum/HBM/RESULTS/EVALUATION/grid_annual
dOt=/gfs1/work/mvkdneum/HBM/RESULTS/EVALUATION/grid_annual

for i1 in 'MR_WORKE' 'PW_WORKE' 'MR_WORKF1' 'MR_WORKF2'; do
  for i2 in 'fine' 'coarse'; do
    fIn="p_z_file_${i2}_${i1}_merge_mean_2012.nc"
    fOt="p_depth_file_${i2}_${i1}_merge_mean_2012.nc"
    echo "~~~~ processing file ${fIn} to ${fOt}"
    ./integrate_layer_depth.x ${dIn}/${fIn} ${dOt}/${dOt}
    echo ""
  done
done
