

pro blocktest_sav, file

  blocktest

  RESOLVE_ALL

  RESOLVE_ALL, CLASS=['ADT_DHASH','GBM_GEOMETRY', 'FXB_IOS','FGEOM_FRAME', $
                      'FGEOM_SIMPLEPLANE', 'FGEOM_QUATERNION', 'FGEOM_QUADRIL', 'FGCOMPOSITE', $
                      'SOLAR_ARRAY_PLUS', 'SOLAR_ARRAYS', 'SOLAR_ARRAY_MINUS', $
                      'LAT', 'LAT_RADIATOR_PLUS', 'LAT_RADIATOR_MINUS', 'SRC_TABLE'] 
  

  SAVE, /ROUTINES, FILENAME=file

end