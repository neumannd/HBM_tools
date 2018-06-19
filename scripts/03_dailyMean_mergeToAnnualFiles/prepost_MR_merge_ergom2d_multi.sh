#!/bin/bash

## -v == verbose

# Start cmod/HBM
#
# @author Daniel Neumann
# @version 1.0
# @date 2017-05-09

## PBS scheduler parameters
#PBS -N p_mrMUL2_erg2d
#PBS -j oe

### SMP1 standard Queue ###
# #PBS -l feature=prepost1
#PBS -l feature=data
#PBS -l nodes=1:ppn=1
#PBS -l walltime=02:59:00

#PBS -m ae
#PBS -V

module load nco

# WORK='N1'
# RESOL='coarse'
# RESOL='fine'
# FILE='biodat'

# for WORK in 'OautY1' 'OsumY1' 'OsprY1'; do
for WORK in 'Usum'; do
for RESOL in 'fine' 'coarse'; do
  for FILE in 'secchidat' 'bendat'; do
    ncra -D 2 --mro -d time,0,,24,24 ${WORK2}/HBM/RESULTS/p_MR_WORK${WORK}/p_${FILE}_${RESOL}_00_2012*.nc grid_annual/p_${FILE}_${RESOL}_MR_WORK${WORK}_merge_mean_2012.nc
  done
done
done
