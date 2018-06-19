open_files = function(fnames) {
  ncIds = list()
  
  for (iF in names(fnames)) {
    ncIds[[iF]] = list()
    for (iG in names(fnames[[iF]])) {
      ncIds[[iF]][[iG]] = nc_open(fnames[[iF]][[iG]])
    }
  }
  
  return(ncIds)
}