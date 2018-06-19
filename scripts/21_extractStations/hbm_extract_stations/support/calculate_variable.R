calculate_variable = function(expr, dataL1) {
  for (iVar in names(dataL1)) {
    expr = sub(iVar, paste('dataL1$', iVar, '$data$`REPLACE_STATION`', sep = ''),
               expr, fixed = TRUE)
  }
  
  dataL2 = list()
  for(iStat in names(stations)) {
    statExpr = gsub('REPLACE_STATION', iStat, expr, fixed=TRUE)
    # print(statExpr)
    dataL2[[iStat]] = eval(parse(text=statExpr))
  }
  
  return(dataL2)
}
