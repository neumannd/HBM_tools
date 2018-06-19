get_varmapping_L0Fi = function(WORK) {
  
  FILE_L0 = list()
  FILE_DIM = list()
  L0_FILE = list()
  L0_DIM = list()
  
  FILE_L0$biodat    = c('mole_concentration_of_ammonium_in_seawater',
                        'mole_concentration_of_nitrate_in_seawater',
                        'mole_concentration_of_phosphate_in_seawater',
                        'mole_concentration_of_silicate_in_seawater',
                        'mole_concentration_of_diatoms_expressed_as_nitrogen_in_seawater',
                        'mole_concentration_of_flagellates_expressed_as_nitrogen_in_seawater',
                        'mole_concentration_of_cyanobacteria_expressed_as_nitrogen_in_seawater',
                        'mole_concentration_of_zooplankton_expressed_as_nitrogen_in_seawater',
                        'mole_concentration_of_protozooplankton_expressed_as_nitrogen_in_seawater',
                        'mole_concentration_of_detritus_expressed_as_nitrogen_in_seawater',
                        'mole_concentration_of_detritus_expressed_as_silicate_in_seawater',
                        'mole_concentration_of_labile_dissolved_organic_nitrogen_expressed_as_nitrogen_in_seawater',
                        'mole_concentration_of_oxygen_in_seawater')
  if (WORK == 'B') {
    FILE_L0$biodat  = c(FILE_L0$biodat,
                        'mole_concentration_of_ammonium_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_nitrate_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_diatoms_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_flagellates_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_cyanobacteria_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_zooplankton_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_protozooplankton_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_detritus_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_labile_dissolved_organic_nitrogen_from_anthropogenic_shipping_activity_expressed_as_nitrogen_in_se')
  }
  if (WORK == 'C'  || WORK == 'E' || WORK == 'F1' || WORK == 'F2' || WORK == 'F3' || WORK == 'F4' || WORK == 'G1' || WORK == 'K1' || WORK == 'K2' || WORK == 'K3' || WORK == 'K4') {
    FILE_L0$biodat  = c(FILE_L0$biodat,
                        'mole_concentration_of_ammonium_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_nitrate_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_diatoms_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_flagellates_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_cyanobacteria_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_zooplankton_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_protozooplankton_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_detritus_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                        'mole_concentration_of_labile_dissolved_organic_nitrogen_from_anthropogenic_shipping_activity_expressed_as_nitrogen_in_se',
                        'mole_concentration_of_ammonium_from_riverine_inflow_in_seawater',
                        'mole_concentration_of_nitrate_from_riverine_inflow_in_seawater',
                        'mole_concentration_of_diatoms_expressed_as_nitrogen_from_riverine_inflow_in_seawater',
                        'mole_concentration_of_flagellates_expressed_as_nitrogen_from_riverine_inflow_in_seawater',
                        'mole_concentration_of_cyanobacteria_expressed_as_nitrogen_from_riverine_inflow_in_seawater',
                        'mole_concentration_of_zooplankton_expressed_as_nitrogen_from_riverine_inflow_in_seawater',
                        'mole_concentration_of_protozooplankton_expressed_as_nitrogen_from_riverine_inflow_in_seawater',
                        'mole_concentration_of_detritus_expressed_as_nitrogen_from_riverine_inflow_in_seawater',
                        'mole_concentration_of_labile_dissolved_organic_nitrogen_from_riverine_inflow_expressed_as_nitrogen_in_seawater')
  }
  if (WORK == 'D' || WORK == 'L1' || WORK == 'M1' || WORK == 'M2' || WORK == 'M3' || WORK == 'M4') {
    FILE_L0$biodat  = c(FILE_L0$biodat,
                        'mole_concentration_of_ammonium_from_atmospheric_deposition_in_seawater',
                        'mole_concentration_of_nitrate_from_atmospheric_deposition_in_seawater',
                        'mole_concentration_of_diatoms_expressed_as_nitrogen_from_atmospheric_deposition_in_seawater',
                        'mole_concentration_of_flagellates_expressed_as_nitrogen_from_atmospheric_deposition_in_seawater',
                        'mole_concentration_of_cyanobacteria_expressed_as_nitrogen_from_atmospheric_deposition_in_seawater',
                        'mole_concentration_of_zooplankton_expressed_as_nitrogen_from_atmospheric_deposition_in_seawater',
                        'mole_concentration_of_protozooplankton_expressed_as_nitrogen_from_atmospheric_deposition_in_seawater',
                        'mole_concentration_of_detritus_expressed_as_nitrogen_from_atmospheric_deposition_in_seawater',
                        'mole_concentration_of_labile_dissolved_organic_nitrogen_from_atmospheric_deposition_expressed_as_nitrogen_in_seawater',
                        'mole_concentration_of_ammonium_from_agricultural_activities_in_seawater',
                        'mole_concentration_of_nitrate_from_agricultural_activities_in_seawater',
                        'mole_concentration_of_diatoms_expressed_as_nitrogen_from_agricultural_activities_in_seawater',
                        'mole_concentration_of_flagellates_expressed_as_nitrogen_from_agricultural_activities_in_seawater',
                        'mole_concentration_of_cyanobacteria_expressed_as_nitrogen_from_agricultural_activities_in_seawater',
                        'mole_concentration_of_zooplankton_expressed_as_nitrogen_from_agricultural_activities_in_seawater',
                        'mole_concentration_of_protozooplankton_expressed_as_nitrogen_from_agricultural_activities_in_seawater',
                        'mole_concentration_of_detritus_expressed_as_nitrogen_from_agricultural_activities_in_seawater',
                        'mole_concentration_of_labile_dissolved_organic_nitrogen_from_agricultural_activities_expressed_as_nitrogen_in_seawater')
  }
  if (WORK == 'G' || WORK == 'H' || WORK == 'I' || WORK == 'J') {
    FILE_L0$biodat  = c(FILE_L0$biodat,
                        'mole_concentration_of_phosphate_from_river_P_in_seawater',
                        'mole_concentration_of_diatoms_expressed_as_nitrogen_from_river_P_in_seawater',
                        'mole_concentration_of_flagellates_expressed_as_nitrogen_from_river_P_in_seawater',
                        'mole_concentration_of_cyanobacteria_expressed_as_nitrogen_from_river_P_in_seawater',
                        'mole_concentration_of_zooplankton_expressed_as_nitrogen_from_river_P_in_seawater',
                        'mole_concentration_of_protozooplankton_expressed_as_nitrogen_from_river_P_in_seawater',
                        'mole_concentration_of_detritus_expressed_as_nitrogen_from_river_P_in_seawater',
                        'mole_concentration_of_ammonium_from_river_N_in_seawater',
                        'mole_concentration_of_nitrate_from_river_N_in_seawater',
                        'mole_concentration_of_diatoms_expressed_as_nitrogen_from_river_N_in_seawater',
                        'mole_concentration_of_flagellates_expressed_as_nitrogen_from_river_N_in_seawater',
                        'mole_concentration_of_cyanobacteria_expressed_as_nitrogen_from_river_N_in_seawater',
                        'mole_concentration_of_zooplankton_expressed_as_nitrogen_from_river_N_in_seawater',
                        'mole_concentration_of_protozooplankton_expressed_as_nitrogen_from_river_N_in_seawater',
                        'mole_concentration_of_detritus_expressed_as_nitrogen_from_river_N_in_seawater',
                        'mole_concentration_of_labile_dissolved_organic_nitrogen_from_river_N_expressed_as_nitrogen_in_seawater')
  }
  if (WORK == 'S0' || WORK == 'S1' || WORK == 'S2' || WORK == 'S3' || WORK == 'S4') {
      FILE_L0$biodat    = c('mole_concentration_of_ammonium_in_seawater',
                            'mole_concentration_of_nitrate_in_seawater',
                            'mole_concentration_of_phosphate_in_seawater',
                            'mole_concentration_of_diatoms_expressed_as_nitrogen_in_seawater',
                            'mole_concentration_of_flagellates_expressed_as_nitrogen_in_seawater',
                            'mole_concentration_of_cyanobacteria_expressed_as_nitrogen_in_seawater',
                            'mole_concentration_of_zooplankton_expressed_as_nitrogen_in_seawater',
                            'mole_concentration_of_protozooplankton_expressed_as_nitrogen_in_seawater',
                            'mole_concentration_of_detritus_expressed_as_nitrogen_in_seawater',
                            'mole_concentration_of_labile_dissolved_organic_nitrogen_expressed_as_nitrogen_in_seawater',
                            'mole_concentration_of_oxygen_in_seawater',
                            'mole_concentration_of_ammonium_from_anthropogenic_shipping_activity_in_seawater',
                            'mole_concentration_of_nitrate_from_anthropogenic_shipping_activity_in_seawater',
                            'mole_concentration_of_diatoms_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                            'mole_concentration_of_flagellates_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                            'mole_concentration_of_cyanobacteria_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                            'mole_concentration_of_zooplankton_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                            'mole_concentration_of_protozooplankton_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                            'mole_concentration_of_detritus_expressed_as_nitrogen_from_anthropogenic_shipping_activity_in_seawater',
                            'mole_concentration_of_labile_dissolved_organic_nitrogen_from_anthropogenic_shipping_activity_expressed_as_nitrogen_in_se',
                            'mole_concentration_of_ammonium_from_riverine_inflow_in_seawater',
                            'mole_concentration_of_nitrate_from_riverine_inflow_in_seawater',
                            'mole_concentration_of_diatoms_expressed_as_nitrogen_from_riverine_inflow_in_seawater',
                            'mole_concentration_of_flagellates_expressed_as_nitrogen_from_riverine_inflow_in_seawater',
                            'mole_concentration_of_cyanobacteria_expressed_as_nitrogen_from_riverine_inflow_in_seawater',
                            'mole_concentration_of_zooplankton_expressed_as_nitrogen_from_riverine_inflow_in_seawater',
                            'mole_concentration_of_protozooplankton_expressed_as_nitrogen_from_riverine_inflow_in_seawater',
                            'mole_concentration_of_detritus_expressed_as_nitrogen_from_riverine_inflow_in_seawater',
                            'mole_concentration_of_labile_dissolved_organic_nitrogen_from_riverine_inflow_expressed_as_nitrogen_in_seawater')
  }
  
  
  FILE_L0$bendat    = c('mole_concentration_of_nitrogen_in_the_sediment',
                        'mole_concentration_of_silicate_in_the_sediment')
  if (WORK == 'B') {
    FILE_L0$bendat    = c(FILE_L0$bendat,
                        'mole_concentration_of_nitrogen_from_anthropogenic_shipping_activity_in_the_sediment')
  }
  if (WORK == 'C' || WORK == 'E' || WORK == 'F1' || WORK == 'F2' || WORK == 'F3' || WORK == 'F4' || WORK == 'G1' || WORK == 'K1' || WORK == 'K2' || WORK == 'K3' || WORK == 'K4') {
    FILE_L0$bendat    = c(FILE_L0$bendat,
                        'mole_concentration_of_nitrogen_from_anthropogenic_shipping_activity_in_the_sediment',
                        'mole_concentration_of_nitrogen_from_riverine_inflow_in_the_sediment')
  }
  if (WORK == 'D' || WORK == 'L1' || WORK == 'M1' || WORK == 'M2' || WORK == 'M3' || WORK == 'M4') {
    FILE_L0$bendat    = c(FILE_L0$bendat,
                        'mole_concentration_of_nitrogen_from_atmospheric_deposition_in_the_sediment',
                        'mole_concentration_of_nitrogen_from_agricultural_activities_in_the_sediment')
  }
  if (WORK == 'G' || WORK == 'H' || WORK == 'I' || WORK == 'J') {
    FILE_L0$bendat    = c(FILE_L0$bendat,
                        'mole_concentration_of_nitrogen_from_river_P_in_the_sediment',
                        'mole_concentration_of_nitrogen_from_river_N_in_the_sediment')
  }
  if (WORK == 'S0' || WORK == 'S1' || WORK == 'S2' || WORK == 'S3' || WORK == 'S4') {
    FILE_L0$bendat    = c('mole_concentration_of_nitrogen_in_the_sediment',
                          'mole_concentration_of_nitrogen_from_anthropogenic_shipping_activity_in_the_sediment',
                          'mole_concentration_of_nitrogen_from_riverine_inflow_in_the_sediment')
  }
  
  
  FILE_L0$chldat    = c('mass_concentration_of_chlorophyll_in_seawater')
  FILE_L0$secchidat = c('secchi_depth_of_sea_water')
  FILE_L0$light     = c('photosynthetically_active_radiation')
  FILE_L0$c_file    = c('uvel', 'vvel')
  FILE_L0$t_file    = c('temp', 'salt')
  FILE_L0$z_file    = c('elev', 'h', 'wvel')
  
  FILE_DIM$biodat    = 4
  FILE_DIM$bendat    = 3
  FILE_DIM$chldat    = 4
  FILE_DIM$secchidat = 3
  FILE_DIM$light     = 4
  FILE_DIM$c_file    = 4
  FILE_DIM$t_file    = 4
  FILE_DIM$z_file    = 4
  
  for (iF in names(FILE_L0)) {
    for (iV in FILE_L0[[iF]]) {
      L0_FILE[[iV]] = iF
      L0_DIM[[iV]] = FILE_DIM[[iF]]
    }
  }
  
  L0_DIM$elev = 3
  
  # L0Fi: L0   to File
  # FiL0: File to L0
  # FiDi: File to Dimension (which dimensions does data in these files have)
  # L0Di: File to Dimension (which dimensions does data in these files have)
  return(list('L0Fi' = L0_FILE, 'FiL0' = FILE_L0,
              'L0Di' = L0_DIM, 'FiDi' = FILE_DIM))
}
