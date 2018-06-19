fill_grid_data_base = function(grids, ncIds) {
  
  newGrids = grids
  
  for (iG in names(grids)) {
    ncId = ncIds[[iG]]
    iD = 'time'
    jD = 'time'
    newGrids[[iG]]$time$ntime = ncId$dim$time$len-1
    newGrids[[iG]]$time$units = ncId$dim$time$units
    newGrids[[iG]]$time$len = ncId$dim$time$len-1
    newGrids[[iG]]$time$unlim = ncId$dim$time$unlim
    newGrids[[iG]]$time$values = ncId$dim$time$vals[1:newGrids[[iG]]$time$len]
    
    for (iD in c('lon', 'lat', 'depth')) {
      jD = ifelse(iD=='depth','z',iD)
      newGrids[[iG]][[jD]]$values = ncId$dim[[iD]]$vals
      newGrids[[iG]][[jD]]$unlim = ncId$dim[[iD]]$unlim
      newGrids[[iG]][[jD]]$len = ncId$dim[[iD]]$len
    }
    
    tmp_att = ncatt_get(ncId, 'depth', attname='positive')
    if (tmp_att$hasatt) {
      newGrids[[iG]]$z$positive = tmp_att$value
      newGrids[[iG]]$z$negative = ifelse(tmp_att$value=='down','up','down')
    } else {
      tmp_att = ncatt_get(ncId, 'depth', attname='negative')
      if (tmp_att$hasatt) {
        newGrids[[iG]]$z$negative = tmp_att$value
        newGrids[[iG]]$z$positive = ifelse(tmp_att$value=='down','up','down')
      } else {
        warning(paste0('Neither "positive" nor "negative" attributes set in file ', ncId$filename))
      }
    }
    
    newGrids[[iG]]$sed$negative = newGrids[[iG]]$z$negative
    newGrids[[iG]]$sur$negative = newGrids[[iG]]$z$negative
    
    newGrids[[iG]]$sed$positive = newGrids[[iG]]$z$positive
    newGrids[[iG]]$sur$positive = newGrids[[iG]]$z$positive
    
    newGrids[[iG]]$sed$values = c(newGrids[[iG]]$z$values[newGrids[[iG]]$z$len])
    newGrids[[iG]]$sur$values = c(newGrids[[iG]]$z$values[1])
  }
  
  return(newGrids)
}