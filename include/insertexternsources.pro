FUNCTION insertexternsources, info, radec, npairs, startrow

	;IF NOT OBJ_VALID((*info).quat) THEN RETURN, -1
	
	tableSize=(*info).s_set->number()
	rows = (INDGEN(npairs) + startrow) mod tableSize
	
	;(*info).s_set->insert, radec, COL=['az','zen'], ROWS=rows
	(*info).s_set->insert, radec, COL=['ra','dec'], ROWS=rows
	
	WIDGET_CONTROL, (*info).radec, GET_VALUE = coords

	coords[*,rows] = (transpose(radec))
	WIDGET_CONTROL, (*info).radec, SET_VALUE = coords
	
	current = (*info).s_set->active()
	(*info).s_set->set_active,ROWS=rows
	
	radec_to_sc, info

	;sc_to_radec, info
	
	RETURN, rows[npairs-1]+1
END