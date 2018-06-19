#!/bin/bash

rm difflog.log


for iDate in `cat 2_dates.dat`; do
  echo $iDate
  echo $iDate >> difflog.log
  for iFile in `ls p_*${iDate}.nc`; do
    jFile=${iFile:2:256}
    ncdump -h $jFile | tail -n+2 > tmpI.txt
    # ncdump -h $jFile > tmpI.txt
    ncdump -h $iFile | tail -n+2 > tmpJ.txt
  
    diff tmpI.txt tmpJ.txt >> difflog.log
    
    rm tmpI.txt tmpJ.txt
  done
done
