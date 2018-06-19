#!/bin/bash

## -v == verbose

# Start cmod/HBM
#
# @author Daniel Neumann
# @version 1.0
# @date 2017-05-09

## PBS scheduler parameters
#PBS -N hbm_t_n8
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

WORK='Dhbm'
# RESOL='coarse'
# RESOL='fine'
# FILE='biodat'

for FILE in 't_file'; do
  for RESOL in 'fine' 'coarse'; do
    ncra -D 2 --mro -d time,0,,24,24 ${WORK2}/HBM/RESULTS/p_N8_WORK${WORK}/p_${FILE}_${RESOL}_00_2012*.nc grid_annual/p_${FILE}_${RESOL}_N8_WORK${WORK}_merge_mean_2012.nc
  done
done
