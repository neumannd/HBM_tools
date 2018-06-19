# Description

Script to compress HBM-ERGOM output (netCDF3/classic) to netCDF4 format.

# Needs

   * [NCO](http://nco.sourceforge.net/) (or at GitHub: [NCO](https://github.com/nco/nco))
   * [netCDF](https://www.unidata.ucar.edu/software/netcdf/)

# Usage

Write dates of files to process into a `*.dat` file. Modify the `prepost_batch_1_copy_results.sh` script and run it. To test, whether everythin went well, please modify the `compare_ncdump.sh` script (`*.dat` file name) and run it. The latter script tests whether the ncdump output of the input and output files (compressed and uncompressed) are equal.
