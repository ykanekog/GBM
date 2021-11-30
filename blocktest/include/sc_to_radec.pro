PRO sc_to_radec, info

	IF NOT OBJ_VALID((*info).quat) THEN RETURN

	uprows = (*info).s_set->active()
	sr_sph = (*info).s_set->pull_azzen(/ACTIVE)
	sr_sph[1,*] = 90 - sr_sph[1,*]
	
	sr_rect = CV_COORD(FROM_SPHERE=sr_sph,/DEGREES,/TO_RECT)
	m = (*info).quat->rotator()
	celest_rect = transpose(m) # sr_rect
	radec = CV_COORD(FROM_RECT=celest_rect,/DEGREES,/TO_SPHERE)
	
	(*info).s_set->insert, transpose(radec[0:1,*]), COL=['ra','dec'], ROWS=uprows
	
	*(*info).s_xyz = sr_rect
	
	WIDGET_CONTROL, (*info).radec, GET_VALUE = celest
	celest[*,uprows] = radec[0:1,*]
	WIDGET_CONTROL, (*info).radec, SET_VALUE = celest

;	WIDGET_CONTROL, (*info).radec, SET_VALUE = radec[0:1,*]

END