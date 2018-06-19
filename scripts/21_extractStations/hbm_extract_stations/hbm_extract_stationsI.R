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
source('support/get_varmapping_L0Fi.R')
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
doShip=TRUE
doRiver=TRUE

# WORK='B'
# WORK='I'
WORK='I'

saveData=TRUE
# savePrefix=''
# savePrefix='testing_'
# savePrefix='test_'
savePrefix='hlrn_'

# sea='northsea'
# sea='balticsea'
sea='all'


# load(paste0(savePrefix, 'dataL0_', sea, '_work', WORK, '.RData'))
# load(paste0(savePrefix, 'dataL1_', sea, '_work', WORK, '.RData'))


## load('')
## save('dataL0', 'dataL1', 'dataL2', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', file = paste0('data_work', WORK, '.RData'))
### save('dataL0', 'dataL1', 'dataL2', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', file = paste0('data_northsea_work', WORK, '.RData'))
### save('dataL0', 'dataL1', 'dataL2', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', file = paste0('data_balticsea_work', WORK, '.RData'))
### save('dataL0', 'dataL1', 'dataL2', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', file = paste0('data_', sea, '_work', WORK, '.RData'))
## save('dataL0', 'dataL1', 'dataL2', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', file = paste0(savePrefix, 'data_', sea, '_work', WORK, '.RData'))

# directories ----
varmappingFileName = paste0('../control_files/variablemapping_work', WORK,'.csv')
varlistFileName = '../control_files/variablelist_neumannd3.csv'
# stationsFileName = '/media/neumannd/work_dell/88_MeRamo/64_validator_data_preparation/test_stations.csv'
# stationsFileName = '../control_files/all_stations.csv'
# stationsFileName = '/media/neumannd/work_dell/88_MeRamo/65_validation_data_BSH/northsea_stations.csv'
stationsFileName = paste0('../control_files/',sea,'_stations.csv')
# outdir = paste0('../output/STATIONS_WORK', WORK)
outdir = paste0('/gfs1/work/mvkdneum/HBM/RESULTS/EVALUATION/stations_annual/WORK', WORK)
# inDir = '/silod4/dneumann/HBM/RESULTS/EVALUATION'
inDir = '/gfs1/work/mvkdneum/HBM/RESULTS/EVALUATION/grid_annual'


# get grid infos ----
if (readL0) {
  grids = get_basic_grid_data()
}


# generate files names and open files ----
ncFiles = list()
filenamePrefix = list('ERGOM'='p', 'HBM'='p')
filenameSuffix = list('ERGOM'=paste0('WORK', WORK, '_merge_mean_2012.nc'),
                      'HBM'='WORKE_merge_mean_2012.nc')
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


if (saveData && readL0) save('dataL0', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', file = paste0(savePrefix, 'dataL0_', sea, '_work', WORK, '.RData'))


# remap L0 to L1 ----
if (calcL1) {
  print('convert data L0 -> L1')
  dataL1 = remap_variables_L0L1(names(stations), varnames, varmapping, dataL0)
}


if (saveData && calcL1) save('dataL1', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', file = paste0(savePrefix, 'dataL1_', sea, '_work', WORK, '.RData'))


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


if (saveData) save('dataL2', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', file = paste0(savePrefix, 'dataL2_', sea, '_work', WORK, '.RData'))
if (saveData) save('dataL0', 'dataL1', 'dataL2', 'grids', 'varnames', 'varmapping', 'varlist', 'stations', 'ncFileNames', file = paste0(savePrefix, 'data_', sea, '_work', WORK, '.RData'))

varnames$L2 = varnames$L2[!(varnames$L2=="secchi")]
varlist$secchi = NULL

# write L2 data ----
if (writeL2) {
  print('write data L2')
  ncFiles$L2 = createL2file(stations, varnames, varlist, varmapping, dataL2, outdir, grids)
}


# do ship data ----
if (doShip && (WORK=='B' || WORK=='I')) {
  print('do ship: extract L1')
  dataShipL1 = extract_tag_data(dataL1, 'ship', c('ship', 'river'), sep = '_')
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
if (doRiver && (WORK=='B' || WORK=='I')) {
  print('do river: extract L1')
  dataRiverL1 = extract_tag_data(dataL1, 'river', c('ship', 'river'), sep = '_')
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


# close all netCDF files ----
# close_files(ncFiles)
