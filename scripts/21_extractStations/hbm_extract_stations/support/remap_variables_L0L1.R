remap_variables_L0L1 = function(stationnames, varnames, varmapping, dataL0) {
  
  dataL1 = list()
  
  for (iV in varnames$L1) {
    # print(iV)
    dataL1[[iV]] = list()
    dataL1[[iV]]$data = list()
    
    #' temporary variables
    oldVarName = varmapping$L1L0[[iV]]
    
    dataL1[[iV]]$attributes = dataL0[[oldVarName]]$attributes
    
    for (iS in stationnames) {
      dataL1[[iV]]$data[[iS]] = dataL0[[oldVarName]]$data[[iS]]
    }
  }
  
  layHeight_L0='h'
  layHeight_L1='lay_thknss'
  depth_L1='depth'
  
  dataL1[[layHeight_L1]] = list('attributes'=dataL0[[layHeight_L0]]$attributes, 'data'=list())
  dataL1[[depth_L1]] = list('attributes'=dataL0[[layHeight_L0]]$attributes, 'data'=list())
  dataL1[[depth_L1]]$attributes$standard_name = 'depth'
  dataL1[[depth_L1]]$attributes$long_name = 'depth'
  for (iS in stationnames) {
    tmp_dims = dim(dataL0[[layHeight_L0]]$data[[iS]])
    nz = tmp_dims[3]
    dataL1[[layHeight_L1]]$data[[iS]] = dataL0[[layHeight_L0]]$data[[iS]]
    dataL1[[depth_L1]]$data[[iS]] = array(NA, dim = tmp_dims)
    dataL1[[depth_L1]]$data[[iS]][,,1,] = dataL0[[layHeight_L0]]$data[[iS]][,,1,]
    if (nz > 1) for (iZ in 2:nz) {
      dataL1[[depth_L1]]$data[[iS]][,,iZ,] = dataL1[[depth_L1]]$data[[iS]][,,iZ-1,] + dataL0[[layHeight_L0]]$data[[iS]][,,iZ,]
    }
  }
  
  return(dataL1)
}

