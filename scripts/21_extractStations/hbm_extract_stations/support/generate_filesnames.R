generate_filesnames = function(grids, ftypes, prefix, suffix) {
  fnames = list()
  
  for (iM in c('ERGOM', 'HBM')) {
    for (iT in ftypes[[iM]]){
      fnames[[iT]] = list()
      for (iG in names(grids)) {
        fnames[[iT]][[iG]] = paste(inDir, paste(prefix[[iM]], iT, iG, suffix[[iM]], sep = '_'), sep = '/')
      }
    }
  }
  
  return(fnames)
}