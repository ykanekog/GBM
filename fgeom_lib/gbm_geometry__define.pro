FUNCTION gbm_geometry::init

	self.mountpoint = double([$
						[96.1, 80.4, 107.6],	$
						[101.1, 72.8, 72.1],	$
						[109.0, 58.1, 99.0],	$
						[97.7, -76.3, 102.5],	$
						[109.0, -57.5, 83.6],	$
						[99.6, -49.7, 100.1],	$
						[-95.8, -80.3, 107.1],	$
						[-100.6, -72.5, 71.6],	$
						[-108.4, -57.2, 99.0],	$
						[-97.5, 76.5, 102.5],	$
 						[-108.7, 57.7, 83.7],	$
 						[-99.3, 50.0, 100.2],	$
						[126.05, 0.13, 63.32],	$
						[-126.14, 0.01, 67.22]	$
						])
	
						;Az	, Zen,	1
	self.lookaxis = double([$
						[45.9, 20.6, 1.0], $
						[45.1, 45.3, 1.0], $
						[58.4, 90.2, 1.0], $
						$
						[314.9,45.2, 1.0], $
						[303.2,90.3, 1.0], $
						[3.4,  89.8, 1.0], $
						$
						[224.9,20.4, 1.0], $
						[224.6,46.1, 1.0], $
						[236.6,89.9, 1.0], $
						$
						[135.2,45.5, 1.0], $
						[123.7,90.4, 1.0], $
						[183.7,90.3, 1.0], $
						$
						[0.0,  90.0, 1.0], $
						[180.0,90.0, 1.0] $
					])
						
	RETURN,1
END

PRO gbm_geometry::shift_detector, detector, cm, MOUNT=mount, VIEW=view

	IF N_ELEMENTS(detector) EQ 0 OR N_ELEMENTS(cm) EQ 0 THEN RETURN

	default = NOT ( KEYWORD_SET(view) OR KEYWORD_SET(mount) )

	IF KEYWORD_SET(view) OR default THEN BEGIN
		;Az, Zenith, R
		translation_axis = self.lookaxis[*,detector]
		translation_axis[1] = 90 - translation_axis[1]
		translation_axis[2] = cm
		translation_axis = CV_COORD(FROM_SPHERE=translation_axis,/DEGREES,/TO_RECT)
	END ELSE BEGIN
		translation_axis = self.mountpoint[*,detector]
		translation_axis *= (cm / norm(translation_axis))
	END
	
	self.mountpoint[*,detector] += translation_axis
END

FUNCTION gbm_geometry::number & return, 14 & END

;Returns 3 x n array of detector mountpoints
;detector - a single detector number or array of detector numbers (integers)
FUNCTION gbm_geometry::det_pos, detector
	IF N_ELEMENTS(detector) EQ 0 THEN RETURN,self.mountpoint
	RETURN, self.mountpoint[*,detector]
END

;Returns 3 x n array of detector normal vectors as [Az,Zen,1] or XYZ if /TO_RECT is used
;detector - a single detector number or array of detector numbers (integers)
FUNCTION gbm_geometry::det_normal, detector, TO_RECT=to_rect
	
	lookaxes = self.lookaxis
	
	IF KEYWORD_SET(to_rect) THEN BEGIN
		lookaxes[1,*] = 90 - lookaxes[1,*]
		lookaxes = CV_COORD(FROM_SPHERE=lookaxes, /DEGREES,/TO_RECT)
	ENDIF

	IF N_ELEMENTS(detector) EQ 0 THEN RETURN,lookaxes
	RETURN, lookaxes[*,detector]
END

PRO gbm_geometry__define
	
	struct = {GBM_GEOMETRY,$
			mountpoint: DBLARR(3,14), $
			lookaxis : DBLARR(3,14) }
END