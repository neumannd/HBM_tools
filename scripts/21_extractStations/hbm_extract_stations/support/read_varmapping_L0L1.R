

read_varmapping_L0L1 = function(varmappingFileName) {
  varmapping = list('L0L1'=list(), 'L1L0'=list())
  
  tmp_varmapping = read.csv(file = varmappingFileName, sep = ';', header = TRUE, stringsAsFactors = FALSE)
  
  for (iV in 1:dim(tmp_varmapping)[1]) {
    varmapping$L0L1[[tmp_varmapping[iV,1]]] = tmp_varmapping[iV,2]
    varmapping$L1L0[[tmp_varmapping[iV,2]]] = tmp_varmapping[iV,1]
  }
  
  return(varmapping)
}