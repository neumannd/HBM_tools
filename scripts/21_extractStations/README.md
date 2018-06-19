# Description

These are scripts to convert HBM-ERGOM output data (annual files of daily means; see [scripts/03_dailyMean_mergeToAnnualFiles](https://github.com/neumannd/HBM_tools/tree/master/scripts/03_dailyMean_mergeToAnnualFiles)) into single-stations files, that can be used by the RShiny Validator by Hagen Radtke (TODO: URL or DOI to Validator).

# Needs

   * [netCDF](https://www.unidata.ucar.edu/software/netcdf/)
   * [R](https://cran.r-project.org/)
   * [ncdf4](https://cran.r-project.org/web/packages/ncdf4/index.html) R package

# Note

There was not enough time to properly document the code and input formats. Therefore, I left some old input files as they are and did not fully clean the code. Hence, some examples are left on how to use the tools. The important R scripts, which start everything, are the `hbm_extract_stations_*.R` scripts in `hbm_extract_stations`.
