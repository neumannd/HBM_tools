#!/bin/bash

## -v == verbose

# Start cmod/HBM
#
# @version 1.0
# @date 2017-05-09

## PBS scheduler parameters
#PBS -N mr_wK4
#PBS -j oe

### SMP1 standard Queue ###
#PBS -l feature=prepost1
#PBS -l nodes=1:ppn=1
#PBS -l walltime=23:59:00

#PBS -m ae
#PBS -V

module load netcdf

for iDate in `cat 2_dates.dat`; do
  echo "We ware at ${iDate}."
  for iFile in `ls *${iDate}.nc`; do
    # ncks -O -4 -L 1 ${iFile} ${iFile}
    # nccopy -k nc4 -d 1 ${iFile} p_${iFile}
    nccopy -k nc4 -d 1 -c lat/,lon/,depth/3,time/1 ${iFile} p_${iFile}
  done
done
