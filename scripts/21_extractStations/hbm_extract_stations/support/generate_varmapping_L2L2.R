generate_varmapping_L2L2 = function(varnames) {
  mappingL2L2 = list()
  for (iN in varnames) {
    mappingL2L2[[iN]] = paste('MODEL', toupper(iN), sep = '_')
  }
  
  return(list('L2L2' = mappingL2L2))
}