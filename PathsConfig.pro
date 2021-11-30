COMMON bto_paths, trigpath, poshpath

trigpath = GETENV('TRIGPATH')

;IF trigpath EQ '' THEN BEGIN
;	trigpath = '/gbm/Catalog/triggerdata/Year2Catalog/level1/trigger/'
;	SETENV,'TRIGPATH='+trigpath
;ENDIF

poshpath = GETENV('POSHPATH')
;IF poshpath EQ '' THEN poshpath = '/gbm/fastcopy/archive/level1/poshist/'