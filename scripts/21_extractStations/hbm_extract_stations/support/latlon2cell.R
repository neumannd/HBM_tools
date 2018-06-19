latlon2cell = function(lat, lon, grid) {
  x = -1
  y = -1
  
  if ( lon > grid$lon$lonw && lon < grid$lon$lone &&
       lat > grid$lat$lats && lat < grid$lat$latn) {
    x = floor((lon - grid$lon$lonw) / grid$lon$dlon) + 1
    y = grid$lat$nlat - floor((lat - grid$lat$lats) / grid$lat$dlat)
  }
  
  return(list('x' = x, 'y' = y))
}