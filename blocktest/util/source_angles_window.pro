PRO source_angles_window, src_table, gbm_geom, DETS=dets, WIN_ID=win, NEW=new, _EXTRA=ex
		
;stop	
	IF N_ELEMENTS(win) EQ 0 THEN win = 0
	
	IF KEYWORD_SET(new) || WIDGET_INFO(win,/VALID_ID) THEN BEGIN
	
		sr_sph = src_table->pull_azzen(_EXTRA=ex)
		sr_sph[1,*] = 90. - sr_sph[1,*]
		sr_sph[2,*] = 1.
		sr_rect = CV_COORD(FROM_SPHERE=sr_sph ,/DEGREES,/TO_RECT)
	
		IF N_ELEMENTS(dets) EQ 0 THEN dets = INDGEN(12)
	
		det_vecs = gbm_geom->det_normal(dets,/TO_RECT)
		dots = transpose(sr_rect) # det_vecs
		;check for imprecision errors & re-normalize before calling trig function
		flterr = where(dots ge 1.,c)
		if c gt 0 then dots[flterr] = 1.0d
	
		table = acos ( dots ) / !dtor
		
		IF KEYWORD_SET(new) THEN BEGIN
			detnames = 'Det '+ strtrim(string(dets),2)
			names = src_table->get(COL='name')
			;rowlabels = SINDGEN( src_table->number() )	
			empty = WHERE(*names EQ '', count)
			col_names = *names
		
			IF count NE 0 THEN col_names[empty] = STRTRIM(STRING(empty),2)
		
			win = WIDGET_BASE(TITLE = "Detector-Source Angles", _EXTRA=ex,/COLUMN)
			tab = WIDGET_TABLE(win, VALUE = table, $
				ROW_LABELS = detnames, COLUMN_LABELS = col_names, _EXTRA = ex)
	
			WIDGET_CONTROL,win,/REALIZE, SET_UVALUE=tab
			ptr_free,names
			
		ENDIF ELSE BEGIN

			WIDGET_CONTROL, win, GET_UVALUE = tab
			WIDGET_CONTROL, tab, SET_VALUE = table, _EXTRA = ex
			
		ENDELSE
		
	ENDIF ELSE win = 0
	
	print, 'Window = ',win
	
END