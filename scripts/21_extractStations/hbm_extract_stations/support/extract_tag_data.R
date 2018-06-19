extract_tag_data = function(dataIn, extractTag, removeTags, sep = '_') {
  dataOut = list()
  
  allNames = names(dataIn)
  removeExpr = paste(removeTags, collapse='|')
  
  removeIdx = grep(removeExpr, allNames)
  extractIdx = grep(extractTag, allNames)
  
  oldNames = allNames[-removeIdx]
  newNames = gsub(paste0(sep, extractTag), '', allNames[extractIdx])
  
  
  keepNames = setdiff(oldNames, newNames)
  
  for (iV in keepNames) {
    dataOut[[iV]] = dataIn[[iV]]
  }
  
  for (iV in newNames) {
    dataOut[[iV]] = dataIn[[paste0(iV, sep, extractTag)]]
  }
  
  return(dataOut)
}