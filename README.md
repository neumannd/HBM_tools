# HBM-Tools

These are some tools that I used to process HBM and HBM-ERGOM data.

Please be aware that the format of HBM binary restart files changed over time. Therefore, some tools are only working with specific versions of the HBM.

This code was written by Daniel Neumann at the Leibniz-Institut for Baltic Sea Research Warnemuende (www.io-warnemuende.de/daniel-neumann.html). The work was done within the project MeRamo (funded by BMVI, FKZ 50EW1601).



## Table of Contents

  * [add_tracer_bio_restart](#add_tracer_bio_restart)
  * [calc_grid_cell_size](#calc_grid_cell_size)
  * [calc_n_distribution](#calc_n_distribution)
  * [change_start_date_bio_restart](#change_start_date_bio_restart)
  * [change_start_date_restart](#change_start_date_restart)
  * [create_mask](#create_mask)
  * [integrate_layer_depth](#integrate_layer_depth)
  * [remove_silicate_bio_restart](#remove_silicate_bio_restart)
  * [reset_silicate_bio_restart](#reset_silicate_bio_restart)
  * [switch_order_bnds_bio_restart](#switch_order_bnds_bio_restart)


## add_tracer_bio_restart
  
  DEPRECATED. DON'T USE.
  
  Left here as a source for ideas.
  
-------

[up](#hbm-tools)

## calc_grid_cell_size

See HELP output. TODO: Copy `-h` output here.


-------

[up](#hbm-tools)

## calc_n_distribution

See HELP output. TODO: Copy `-h` output here.


-------

[up](#hbm-tools)

## change_start_date_bio_restart

  Please use [change_start_date_restart](#change_start_date_restart) instead. `change_start_date_restart` is more rebust. `change_start_date_bio_restart` possibly does not work with the current HBM-ERGOM binary bio_restart files.

-------

[up](#hbm-tools)

## change_start_date_restart

TODO

-------

[up](#hbm-tools)

## create_mask

See HELP output. TODO: Copy `-h` output here.


-------

[up](#hbm-tools)

## integrate_layer_depth

See HELP output. TODO: Copy `-h` output here.

-------

[up](#hbm-tools)

## remove_silicate_bio_restart

  Removed the tracer `silicate` from binary bio_restart files of HBM-ERGOM. Does not work with the 2018s version of HBM-ERGOM anymore because the alignment of data at the boundaries was changed. Needs a bio_restart file with 13 tracers. Silicate needs to be bio tracer number 4.

-------

[up](#hbm-tools)

## reset_silicate_bio_restart

  Resets the tracer `silicate` in binary bio_restart files of HBM-ERGOM to the `silicate` values of another binary bio_restart file. Does not work with the 2018s version of HBM-ERGOM anymore because the alignment of data at the boundaries was changed. The source bio_restart file, from which silicate data are copied, needs to have with 13 tracers. The source bio_restart file, from which all other tracers' data are copied, needs to have with 31 tracers (first 13 are equal).  Silicate needs to be bio tracer number 4.

-------

[up](#hbm-tools)

## switch_order_bnds_bio_restart

  Convert a binary bio_restart file of a HBM-ERGOM version of 2017 and before into a binary bio_restart file for a HBM-ERGOM version of late 2017 and later years. The alignment of boundary data was changed in later 2017.

-------

[up](#hbm-tools)

