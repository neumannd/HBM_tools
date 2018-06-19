#!/bin/bash

# Start cmod/HBM
#
# @author Daniel Neumann
# @version 1.0
# @date 2017-05-09

## PBS scheduler parameters
#PBS -N mean_mon_Dwe
#PBS -j oe

### SMP1 standard Queue ###
#PBS -l feature=prepost1
#PBS -l nodes=1:ppn=1
#PBS -l walltime=23:59:00

#PBS -m ae
#PBS -V

module load nco

# WORK='Dcgttagrivers_weser_elbe'
# TYPE='N8'
TYPE='MR'

cd grid_annual
# for WORK in 'D' 'E' 'F1' 'F2' 'G1' 'K1' 'L1'; do
for WORK in 'T1' 'T2' 'T3' 'T4' 'T5' 'T6' 'T7' 'T8' 'T9'; do
  for iF in `ls p_biodat*${TYPE}_WORK${WORK}*.nc p_bendat*${TYPE}_WORK${WORK}*.nc p_chldat*${TYPE}_WORK${WORK}*.nc`; do
    echo "File: $iF"
    ncra -D 2 --mro -d time,0,,30,30 $iF ../mean_month/m_$iF 
  done
done
