close_files = function(ncIds) {
  for (iL in names(ncIds)) {
    if (iL == 'L0') {
      for(iF in names(ncIds[[iL]])) for(iG in names(ncIds[[iL]][[iF]])) {
        # print(iG)
        nc_close(ncFiles[[iL]][[iF]][[iG]])
      }
    } else {
      for(iF in names(ncIds[[iL]])){
        nc_close(ncFiles[[iL]][[iF]])
      }
    }
    
  }
}