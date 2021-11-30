PRO radec_to_sc, info, NEWSELECT=newselect

	IF NOT OBJ_VALID((*info).quat) THEN RETURN

	uprows = (*info).s_set->active()
	sr_sph = (*info).s_set->pull_radec(/ACTIVE)
	
	celest_rect = CV_COORD(FROM_SPHERE=sr_sph,/DEGREES,/TO_RECT)
	m = (*info).quat->rotator()
	sr_rect = (m) # celest_rect
	sr_sph = CV_COORD(FROM_RECT=sr_rect,/DEGREES,/TO_SPHERE)
	
	sr_sph[1,*] = 90 - sr_sph[1,*]
	(*info).s_set->insert, transpose(sr_sph[0:1,*]), COL=['az','zen'], ROWS=uprows
	
	*(*info).s_xyz = sr_rect
	
	WIDGET_CONTROL, (*info).table, GET_VALUE = coords
	coords[*,uprows] = sr_sph[0:1,*]
	WIDGET_CONTROL, (*info).table, SET_VALUE = coords
	
	IF N_ELEMENTS(newselect) eq 4 THEN BEGIN
		n = 2
		m = n_elements(rows)
		newcolor = BYTARR(3, n, m)
		color = (*info).colors.text.active
		FOR i=0,2 DO newcolor[i, *, *] = replicate(color[i],n,m)
		WIDGET_CONTROL, (*info).table, SET_TABLE_SELECT = newselect
		WIDGET_CONTROL, (*info).table, FOREGROUND_COLOR = newcolor, /USE_TABLE_SELECT
	ENDIF

;	WIDGET_CONTROL, (*info).table, SET_VALUE = sr_sph[0:1,*]

END