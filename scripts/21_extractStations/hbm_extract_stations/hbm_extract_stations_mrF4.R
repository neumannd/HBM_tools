library('ncdf4')
source('support/hbm_constants.R')  
source('support/read_varlist.R')  
source('support/read_varmapping_L0L1.R')
source('support/generate_varmapping_L2L2.R')
source('support/createL1file.R')
source('support/createL2file.R')
source('support/get_basic_grid_data.R')
source('support/latlon2cell.R')
source('support/read_stations.R')
source('support/calculate_variable.R')
source('support/generate_filesnames.R')
source('support/open_files.R')
source('support/close_files.R')
source('support/fill_grid_data_base.R')
source('support/get_varmapping_L0Fi_MR.R')
source('support/remap_variables_L0L1.R')
source('support/read_data_L0.R')
source('support/readL1file.R')
source('support/extract_tag_data.R')

readL0=TRUE
calcL1=TRUE
writeL1=TRUE
readL1=FALSE
calcL2=TRUE
writeL2=TRUE
doShip=FALSE
doRiver=FALSE
doAtmos=TRUE
doAgri=TRUE
doPhos=FALSE
doNit=FALSE

# readL0=FALSE
# calcL1=FALSE
# writeL1=TRUE
# readL1=FALSE
# calcL2=FALSE
# writeL2=FALSE
# doPhos=FALSE
# doNit=FALSE

# WORK='B'
# WORK='I'
WORK='F4'

saveData=TRUE
# savePrefix=''
# savePrefix='testing_'
savePrefix='test_'
# savePrefix='hlrn_'

# sea='northsea'
# sea='balticsea'
sea='all'

## load('')
## save('dataL0', 'dataL1', 'dataL2', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', file = paste0('data_mr_work', WORK, '.RData'))
### save('dataL0', 'dataL1', 'dataL2', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', file = paste0('data_northsea_mr_work', WORK, '.RData'))
### save('dataL0', 'dataL1', 'dataL2', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', file = paste0('data_balticsea_mr_work', WORK, '.RData'))
### save('dataL0', 'dataL1', 'dataL2', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', file = paste0('data_', sea, '_mr_work', WORK, '.RData'))
## save('dataL0', 'dataL1', 'dataL2', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', file = paste0(savePrefix, 'data_', sea, '_mr_work', WORK, '.RData'))

# directories ----
varmappingFileName = paste0('../control_files/variablemapping_mr_work', WORK,'.csv')
varlistFileName = '../control_files/variablelist_neumannd3.csv'
# stationsFileName = '/media/neumannd/work_dell/88_MeRamo/64_validator_data_preparation/test_stations.csv'
# stationsFileName = '../control_files/all_stations.csv'
# stationsFileName = '/media/neumannd/work_dell/88_MeRamo/65_validation_data_BSH/northsea_stations.csv'
stationsFileName = paste0('../control_files/',sea,'_stations.csv')
# outdir = paste0('../output/STATIONS_WORK', WORK)
outdir = paste0('/gfs1/work/mvkdneum/HBM/RESULTS/EVALUATION/stations_annual/MR_WORK', WORK)
# inDir = '/silod4/dneumann/HBM/RESULTS/EVALUATION'
# inDir = '/media/neumannd/private_dell/HBM/RESULTS/EVALUATION'
inDir = '/gfs1/work/mvkdneum/HBM/RESULTS/EVALUATION/grid_annual'


# get grid infos ----
if (readL0) {
  grids = get_basic_grid_data()
}


# generate files names and open files ----
ncFiles = list()
filenamePrefix = list('ERGOM'='p', 'HBM'='p')
filenameSuffix = list('ERGOM'=paste0('MR_WORK', WORK, '_merge_mean_2012.nc'),
                      'HBM'='MR_WORKE_merge_mean_2012.nc')
fileTypes      = list('ERGOM'=c('biodat', 'bendat', 'chldat', 'light', 'secchidat'),
                      'HBM' = c('t_file', 'z_file', 'c_file')) 
ncFileNames   = generate_filesnames(grids, fileTypes, filenamePrefix, filenameSuffix)


# read variable and station data ----
varmapping  = list()
varnames    = list()
varmapping  = append(varmapping, get_varmapping_L0Fi(WORK)) 
varmapping  = append(varmapping, read_varmapping_L0L1(varmappingFileName))
varlist     = read_varlist(varlistFileName)
varnames$L0 = names(varmapping$L0Fi)
varnames$L1 = names(varmapping$L1L0)
varnames$L2 = names(varlist)
varmapping  = append(varmapping, generate_varmapping_L2L2(varnames$L2))


# read L0 data ----
if (readL0) {
  print('read data L0')
  ncFiles$L0 = open_files(ncFileNames)
  grids      = fill_grid_data_base(grids, ncFiles$L0$biodat)
  stations    = read_stations(stationsFileName, grids)
  dataL0     = read_data_L0(stations, ncFiles$L0, varmapping, grids)
}


if (saveData && readL0) save('dataL0', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', 
                             file = paste0(savePrefix, 'dataL0_', sea, '_mr_work', WORK, '.RData'))


# remap L0 to L1 ----
if (calcL1) {
  print('convert data L0 -> L1')
  dataL1 = remap_variables_L0L1(names(stations), varnames, varmapping, dataL0)
}


if (saveData && calcL1) save('dataL1', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', 
                             file = paste0(savePrefix, 'dataL1_', sea, '_mr_work', WORK, '.RData'))


# write L1 data ----
if (writeL1) {
  print('write data L1')
  ncFiles$L1 = createL1file(stations, varnames, varmapping, dataL1, outdir, grids)
}


# read L1 data ----
if (readL1) {
  print('read data L1')
  print('Empty')
}


# calculate L2 data ----
if (calcL2) {
  print('convert data L1 -> L2')
  dataL2 = list()
  for(iVar in names(varlist)) {
    dataL2[[iVar]] = list()
    dataL2[[iVar]]$data = calculate_variable(varlist[[iVar]]$formula, dataL1)
  }
}


if (saveData && calcL2) save('dataL2', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', 
                             file = paste0(savePrefix, 'dataL2_', sea, '_mr_work', WORK, '.RData'))
if (saveData) save('dataL0', 'dataL1', 'dataL2', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', 
                   file = paste0(savePrefix, 'data_', sea, '_mr_work', WORK, '.RData'))

varnames$L2 = varnames$L2[!(varnames$L2=="secchi")]
varlist$secchi = NULL

# write L2 data ----
if (writeL2) {
  print('write data L2')
  ncFiles$L2 = createL2file(stations, varnames, varlist, varmapping, dataL2, outdir, grids)
}


# do ship data ----
if (doShip) {
  print('do ship: extract L1')
  dataShipL1 = extract_tag_data(dataL1, 'ship', c('ship', 'river', 'atmos', 'agri'), sep = '_')
  print('do ship: write L1')
  tmpVarnames = list('L1' = intersect(names(dataShipL1), varnames$L1))
  ncFiles$L1 = createL1file(stations, tmpVarnames, varmapping, dataShipL1, outdir, grids, '_ship')
  print('do ship: extract L2')
  dataShipL2 = list()
  for(iVar in names(varlist)) {
    dataShipL2[[iVar]] = list()
    dataShipL2[[iVar]]$data = calculate_variable(varlist[[iVar]]$formula, dataShipL1)
  }
  print('do ship: write L2')
  ncFiles$L2 = createL2file(stations, varnames, varlist, varmapping, dataShipL2, outdir, grids, '_ship')
}

# do river data ----
if (doRiver) {
  print('do river: extract L1')
  dataRiverL1 = extract_tag_data(dataL1, 'river', c('ship', 'river', 'atmos', 'agri'), sep = '_')
  print('do river: write L1')
  tmpVarnames = list('L1' = intersect(names(dataRiverL1), varnames$L1))
  ncFiles$L1 = createL1file(stations, tmpVarnames, varmapping, dataRiverL1, outdir, grids, '_river')
  print('do river: extract L2')
  dataRiverL2 = list()
  for(iVar in names(varlist)) {
    dataRiverL2[[iVar]] = list()
    dataRiverL2[[iVar]]$data = calculate_variable(varlist[[iVar]]$formula, dataRiverL1)
  }
  print('do river: write L2')
  ncFiles$L2 = createL2file(stations, varnames, varlist, varmapping, dataRiverL2, outdir, grids, '_river')
}


# do atmospheric data ----
if (doAtmos) {
  print('do atmospheric: extract L1')
  dataAtmosL1 = extract_tag_data(dataL1, 'atmos', c('ship', 'river', 'atmos', 'agri'), sep = '_')
  print('do atmospheric: write L1')
  tmpVarnames = list('L1' = intersect(names(dataAtmosL1), varnames$L1))
  ncFiles$L1 = createL1file(stations, tmpVarnames, varmapping, dataAtmosL1, outdir, grids, '_atmos')
  print('do atmospheric: extract L2')
  dataAtmosL2 = list()
  for(iVar in names(varlist)) {
    dataAtmosL2[[iVar]] = list()
    dataAtmosL2[[iVar]]$data = calculate_variable(varlist[[iVar]]$formula, dataAtmosL1)
  }
  print('do atmospheric: write L2')
  ncFiles$L2 = createL2file(stations, varnames, varlist, varmapping, dataAtmosL2, outdir, grids, '_atmos')
}


# do agricultural data ----
if (doAgri) {
  print('do agricultural: extract L1')
  dataAgriL1 = extract_tag_data(dataL1, 'agri', c('ship', 'river', 'atmos', 'agri'), sep = '_')
  print('do agricultural: write L1')
  tmpVarnames = list('L1' = intersect(names(dataAgriL1), varnames$L1))
  ncFiles$L1 = createL1file(stations, tmpVarnames, varmapping, dataAgriL1, outdir, grids, '_agri')
  print('do agricultural: extract L2')
  dataAgriL2 = list()
  for(iVar in names(varlist)) {
    dataAgriL2[[iVar]] = list()
    dataAgriL2[[iVar]]$data = calculate_variable(varlist[[iVar]]$formula, dataAgriL1)
  }
  print('do agricultural: write L2')
  ncFiles$L2 = createL2file(stations, varnames, varlist, varmapping, dataAgriL2, outdir, grids, '_agri')
}

# do phosphorus data ----
if (doPhos) {
  print('do phosphorus: extract L1')
  dataPhosL1 = extract_tag_data(dataL1, 'P', c('P', 'N'), sep = '_')
  print('do phosphorus: write L1')
  tmpVarnames = list('L1' = intersect(names(dataPhosL1), varnames$L1))
  ncFiles$L1 = createL1file(stations, tmpVarnames, varmapping, dataPhosL1, outdir, grids, '_P')
  print('do phosphorus: extract L2')
  dataPhosL2 = list()
  for(iVar in names(varlist)) {
    dataPhosL2[[iVar]] = list()
    dataPhosL2[[iVar]]$data = calculate_variable(varlist[[iVar]]$formula, dataPhosL1)
  }
  print('do phosphorus: write L2')
  ncFiles$L2 = createL2file(stations, varnames, varlist, varmapping, dataPhosL2, outdir, grids, '_P')
}

# do nitrogen data ----
if (doNit) {
  print('do nitrogen: extract L1')
  dataNitL1 = extract_tag_data(dataL1, 'N', c('P', 'N'), sep = '_')
  print('do nitrogen: write L1')
  tmpVarnames = list('L1' = intersect(names(dataNitL1), varnames$L1))
  ncFiles$L1 = createL1file(stations, tmpVarnames, varmapping, dataNitL1, outdir, grids, '_N')
  print('do nitrogen: extract L2')
  dataNitL2 = list()
  for(iVar in names(varlist)) {
    dataNitL2[[iVar]] = list()
    dataNitL2[[iVar]]$data = calculate_variable(varlist[[iVar]]$formula, dataNitL1)
  }
  print('do nitrogen: write L2')
  ncFiles$L2 = createL2file(stations, varnames, varlist, varmapping, dataNitL2, outdir, grids, '_N')
}


# close all netCDF files ----
# close_files(ncFiles)
