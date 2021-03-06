#!/bin/bash

## -v == verbose

# Start cmod/HBM
#
# @author Daniel Neumann
# @version 1.0
# @date 2017-05-09

## PBS scheduler parameters
#PBS -N hbm_z_k1
#PBS -j oe

### SMP1 standard Queue ###
#PBS -l feature=prepost1
# #PBS -l feature=data
#PBS -l nodes=1:ppn=1
# #PBS -l walltime=02:59:00
#PBS -l walltime=23:59:00

#PBS -m ae
#PBS -V

module load nco

WORK='K1'
# RESOL='coarse'
# RESOL='fine'
# FILE='biodat'

for FILE in 'z_file'; do
  for RESOL in 'fine' 'coarse'; do
    ncra -D 2 --mro -d time,0,,24,24 ../p_MR_WORK${WORK}/p_${FILE}_${RESOL}_00_2012*.nc grid_annual/p_${FILE}_${RESOL}_MR_WORK${WORK}_merge_mean_2012.nc
  done
done
