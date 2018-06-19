# varlistFileName = '/media/neumannd/work_dell/88_MeRamo/64_validator_data_preparation/workflow/control_files/variablelist.csv'


read_varlist = function(varlistFileName) {
   varlist = list()
   
   tmp_varlist = read.csv(file = varlistFileName, sep = ';', header = TRUE, stringsAsFactors = FALSE)
   nVars = dim(tmp_varlist)[1]
   
   for(iV in 1:nVars) {
     varname = trimws(tmp_varlist[iV,1])
     varlist[[varname]] = list()
     varlist[[varname]]$formula = trimws(tmp_varlist[iV,2])
     varlist[[varname]]$units = trimws(tmp_varlist[iV,3])
     varlist[[varname]]$longname = trimws(tmp_varlist[iV,4])
   }
   
  return(varlist)
 }