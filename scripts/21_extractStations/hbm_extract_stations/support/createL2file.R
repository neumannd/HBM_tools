createL2file = function(stations, varnames, varlist, varmapping, dataL2, outdir, grids, suffix = '') {
  
  ncIdOut = list()
  
  copyAtts = c('cell_methods', 'positive', 'negative', 'standard_name')
  
  for (iStat in names(stations)) {
    iG = stations[[iStat]]$grid
    
    all_dims = list()
    all_vars = list()
    
    for (iD in c('lon', 'lat')) {
      all_dims[[iD]] = ncdim_def(iD, grids[[iG]][[iD]]$units, 
                                 vals = stations[[iStat]][[iD]], 
                                 unlim = grids[[iG]][[iD]]$unlim,
                                 longname = grids[[iG]][[iD]]$long_name)
    }
    
    for (iD in c('z', 'sur', 'sed', 'time')) {
      all_dims[[iD]] = ncdim_def(iD, grids[[iG]][[iD]]$units, 
                                 vals = grids[[iG]][[iD]]$values, 
                                 unlim = grids[[iG]][[iD]]$unlim,
                                 longname = grids[[iG]][[iD]]$long_name)
    }
    all_dims$z$vals = rowMeans(dataL2[['depth']]$data[[iStat]], dim = 3)
    
    
    for (iV in varnames$L2) {
      if (iV != 'secchi') {
        tmp_dims = list(all_dims$lon, all_dims$lat, all_dims$z, all_dims$time)
      } else if (3 == 3) {
        if (TRUE) {
          tmp_dims = list(all_dims$lon, all_dims$lat, all_dims$sed, all_dims$time)
        } else {
          tmp_dims = list(all_dims$lon, all_dims$lat, all_dims$sur, all_dims$time)
        }
      } else {
        stop(paste0('Bad dimensions of variable ', iV))
      }
      
      all_vars[[iV]] = ncvar_def(varmapping$L2L2[[iV]], units = varlist[[iV]]$units,
                           dim = tmp_dims, longname = iV, # varlist[[iV]]$longname, 
                           prec = 'float')
    }
    ## time independent depth (averaged)
    # all_vars[['depth2']] = ncvar_def('depth2', units = 'm',
    #                                  dim = list(all_dims$lon, all_dims$lat, all_dims$z), 
    #                                  longname = 'depth2',
    #                                  prec = 'float')
    
    # TODO ITERATE STATIONS
    ncIdTmp = nc_create(paste(outdir, paste(paste0(paste(iStat, 'L2', sep = '_'), suffix), 'nc', sep = '.'), sep = '/'), vars = all_vars)
    
    #' global attributes
    ncatt_put(ncIdTmp, 0, 'contact', "Daniel Neumann", prec = 'text')
    ncatt_put(ncIdTmp, 0, 'contact_email', "daniel.neumann@io-warnemuende.de", prec = 'character')
    ncatt_put(ncIdTmp, 0, 'originator', "Daniel Neumann", prec = 'character')
    ncatt_put(ncIdTmp, 0, 'contributor_name', "Hagen Radtke, Thomas Neumann, Fabian Schwichtenberg, Ina Lorkowski", prec = 'character')
    ncatt_put(ncIdTmp, 0, 'institution', "Leibniz Institute for Baltic Sea Research Warnemuende, Rostock, Germany", prec = 'character')
    ncatt_put(ncIdTmp, 0, 'source', "model: HBM-ERGOM; grid/domain: fine+coarse; boundary conditions: TODO; initial condition: TODO; hardware: HLRN SMP1 Hannover; compiler set: Intel vTODO; meteo: TODO", prec = 'character')
    ncatt_put(ncIdTmp, 0, 'summary', "TODO", prec = 'character')
    ncatt_put(ncIdTmp, 0, 'standard_name_vocabulary', "CF Standard Names 1.6", prec = 'character')
    ncatt_put(ncIdTmp, 0, 'title', "TODO", prec = 'character')
    ncatt_put(ncIdTmp, 0, 'creationTime', "2017-05-02T12:00:00Z", prec = 'character')
    ncatt_put(ncIdTmp, 0, 'date_created', "2017-05-02", prec = 'character')
    ncatt_put(ncIdTmp, 0, 'date_modified', "2017-05-29", prec = 'character')
    ncatt_put(ncIdTmp, 0, 'crs', "TODO", prec = 'character')
    ncatt_put(ncIdTmp, 0, 'Conventions', "CF-1.6", prec = 'character')
    ncatt_put(ncIdTmp, 0, 'history', "", prec = 'character')
    
    abc = 1 + 1;
    
    
    for (iV in varnames$L2) {
      # print(iV)
      if(!is.null(varmapping$L1L0[[iV]])) {
        for (iAtt in intersect(copyAtts,names(dataL2[[varmapping$L1L0[[iV]]]]$attributes))) {
          ncatt_put(ncIdTmp, varmapping$L2L2[[iV]], iAtt, dataL2[[varmapping$L1L0[[iV]]]]$attributes[[iAtt]], prec = 'character')
        }
      }
      ncatt_put(ncIdTmp, varmapping$L2L2[[iV]], 'coordinates', 'lon lat', prec = 'character')
      ncvar_put(ncIdTmp, varmapping$L2L2[[iV]], dataL2[[iV]]$data[[iStat]][1,1,,1:all_dims$time$len])
    }
    
    ## time independent depth (averaged)
    # ncatt_put(ncIdTmp, 'depth2', 'coordinates', 'lon lat', prec = 'character')
    # ncvar_put(ncIdTmp, 'depth2', rowMeans(dataL2[['depth']]$data[[iStat]], dim = 3))
    
    
    nc_sync(ncIdTmp)
    
    ncIdOut[[iStat]] = ncIdTmp
    
    nc_close(ncIdTmp)
  }
  
  return(ncIdOut)
}