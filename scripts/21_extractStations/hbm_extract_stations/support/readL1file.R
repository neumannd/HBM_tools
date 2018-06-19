readL1file = function(stations, varnames, varmapping, ncIds, grids) {
  dataL1 = list()
  firstFile = TRUE
  stationnames = names(stations)
  
  for (iV in varnames$L1) {
    dataL1[[iV]] = list('attributes'=list(), 'data'=list())
    ncId = ncIds[[stationnames[1]]]
    dataL1[[iV]]$attributes = ncatt_get(ncId, iV)
  }
  
  for (iS in stationnames) {
    ncId = ncIds[[iS]]
    
    for(iV in varnames$L1) {
      iG = stations[[iStat]]$grid
      if(varmapping$L0Di[[iV]] == 4) {
        dataL1[[iV]]$data[[iS]] = array(ncvar_get(ncId, iV), 
                                        dim = c(1, 1, grids[[iG]]$z$len, grids[[iG]]$time$len))
      } else {
        dataL1[[iV]]$data[[iS]] = array(ncvar_get(ncId, iV), 
                                        dim = c(1, 1, 1, grids[[iG]]$time$len))
      }
    }
  }
  
  return(dataL1)
}