#!/bin/bash

# Start cmod/HBM
#
# @author Daniel Neumann
# @version 1.0
# @date 2017-05-09

## PBS scheduler parameters
#PBS -N mean_a2s_E1
#PBS -j oe

### SMP1 standard Queue ###
#PBS -l feature=prepost1
#PBS -l nodes=1:ppn=1
#PBS -l walltime=23:59:00

#PBS -m ae
#PBS -V

module load nco

WORK='A'

cd grid_annual
# for WORK in 'C' 'D' 'K1' 'K2' 'K3' 'K4' 'M1' 'M2' 'M4' 'M4' 'F1' 'F2' 'F3' 'S0' 'R1'; do
# for WORK in 'E1'; do
for WORK in 'T1' 'T2' 'T3' 'T4' 'T5' 'T6' 'T7' 'T8' 'T9'; do
  for iF in `ls p_bendat*MR_WORK${WORK}*.nc`; do
    echo "File: $iF"
    ncra -D 2 -O -d time,91,273 $iF ../mean_apr2sept/hA2S_$iF 
  done
  for iF in `ls p_biodat*MR_WORK${WORK}*.nc`; do
    echo "File: ${iF}"
    ncra -D 2 -O -d time,91,273 $iF ../mean_apr2sept/hA2S_$iF
  done
  for iF in `ls p_chldat_*MR_WORK${WORK}*.nc`; do
    echo "File: ${iF}"
    ncra -D 2 -O -d time,91,273 $iF ../mean_apr2sept/hA2S_$iF
  done
done

