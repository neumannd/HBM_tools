read_data_L0 = function(stations, ncIds, varmapping, grids) {
  
  dataL0 = list()
  tmp_var = list()
  
  for (iV in varnames$L0) {
    # print(iV)

    #' temporary variables
    tmp_station_data = list()
    
    #' get ncId
    ncId = ncIds[[varmapping$L0Fi[[iV]]]]
    
    #' set some timing variables
    tmax = grids[[1]]$time$len
    tstart = 1
    deltaT = 10
    
    #' get attributes
    dataL0[[iV]] = list()
    dataL0[[iV]]$attributes = ncatt_get(ncId[[1]], iV)
    
    for(iStat in names(stations)) {
      iG = stations[[iStat]]$grid
      if(varmapping$L0Di[[iV]] == 4) {
        tmp_station_data[[iStat]] = array(0.0, dim = c(1, 1, grids[[iG]]$z$len, tmax))
      } else {
        tmp_station_data[[iStat]] = array(0.0, dim = c(1, 1, 1, tmax))
      }
    }
    
    # for (i1 in 1:1) {
    for (i1 in 1:ceiling(tmax/deltaT)) {
      # print(paste0('>> ', i1))
      tcount = min(deltaT,tmax-tstart+1)
      
      for (iG in names(grids)) {
        #' We loose all of the dimensions with size one.
        #' Therefore, we apply 'array' here.
        if(varmapping$L0Di[[iV]] == 4) {
          tmp_var[[iG]] = array(ncvar_get(ncId[[iG]], iV, start = c(1,1,1,tstart), count = c(-1,-1,-1,tcount)), 
                                dim = c(grids[[iG]]$lon$len, grids[[iG]]$lat$len, grids[[iG]]$z$len, tcount))
        } else {
          tmp_var[[iG]] = array(ncvar_get(ncId[[iG]], iV, start = c(1,1,tstart), count = c(-1,-1,tcount)), 
                                dim = c(grids[[iG]]$lon$len, grids[[iG]]$lat$len, 1, tcount))
        }
      }
      
      for(iStat in names(stations)) {
        jG = stations[[iStat]]$grid
        jX = stations[[iStat]]$xcell
        jY = stations[[iStat]]$ycell
        #' It could happen that we loose some of the dimensions with size one.
        #' Therefore, we apply 'array' here.
        if(varmapping$L0Di[[iV]] == 4) {
          tmp_station_data[[iStat]][1,1,,tstart:(tstart+tcount-1)] = array(tmp_var[[jG]][jX, jY,,1:tcount],
                                                                         dim = c(1, 1, grids[[jG]]$z$len, tcount))
        } else {
          tmp_station_data[[iStat]][1,1,,tstart:(tstart+tcount-1)] = array(tmp_var[[jG]][jX, jY,,1:tcount],
                                                                           dim = c(1, 1, 1, tcount))
          }
      }
      
      tstart = tstart + tcount
    }
    
    dataL0[[iV]]$data = tmp_station_data
  }
  
  return(dataL0)
}