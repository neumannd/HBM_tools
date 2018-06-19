
read_stations = function(stationsFileName, grids = list('nogrid'=TRUE)) {
  stations = list()
  tmp_stations = read.csv(file = stationsFileName, sep = ';', header = TRUE, stringsAsFactors = FALSE)
  
  nogrid = FALSE
  if (!is.null(grids$nogrid)) { if(grids$nogrid) nogrid = TRUE }
  
  for (iS in 1:dim(tmp_stations)[1]) {
    iStat = tmp_stations[iS,3]
    stations[[iStat]] = list()
    stations[[iStat]]$name = iStat
    stations[[iStat]]$lon = tmp_stations[iS,2]
    stations[[iStat]]$lat = tmp_stations[iS,1]
    
    if (nogrid) {
      stations[[iStat]]$xcell = -1
      stations[[iStat]]$ycell = -1
    } else {
      tmp_cell = latlon2cell(tmp_stations[iS,1], tmp_stations[iS,2], grids$fine)
      stations[[iStat]]$grid = 'fine'
      if ( tmp_cell$x == -1 || tmp_cell$y == -1 ) {
        tmp_cell = latlon2cell(tmp_stations[iS,1], tmp_stations[iS,2], grids$coarse)
        stations[[iStat]]$grid = 'coarse'
      }
      
      stations[[iStat]]$xcell = tmp_cell$x
      stations[[iStat]]$ycell = tmp_cell$y
    }
  }
  
  return(stations)
}