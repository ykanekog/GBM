PRO source_separation_window, src_table, WIN_ID=win, _EXTRA=ex
	
	sr_sph = src_table->pull_azzen(_EXTRA=ex)
	sr_sph[1,*] = 90. - sr_sph[1,*]
	sr_sph[2,*] = 1.
	sr_rect = CV_COORD(FROM_SPHERE=sr_sph ,/DEGREES,/TO_RECT)
	
	table = acos ( transpose(sr_rect) # sr_rect ) / !dtor
	
	IF N_ELEMENTS(win) EQ 0 THEN win = 0
	
	IF WIDGET_INFO(win,/VALID_ID) THEN BEGIN
	
		WIDGET_CONTROL, win, GET_UVALUE = tab
		WIDGET_CONTROL, tab, SET_VALUE = table, _EXTRA = ex
		
	ENDIF ELSE BEGIN
		
		names = src_table->get(COL='name')
		;rowlabels = SINDGEN( src_table->number() )	
		empty = WHERE(*names EQ '', count)
		col_names = *names
	
		IF count NE 0 THEN col_names[empty] = STRTRIM(STRING(empty),2)
	
	
		win = WIDGET_BASE(TITLE = "Source Separation Angles", _EXTRA=ex,/COLUMN)
		tab = WIDGET_TABLE(win, VALUE = table, $
			ROW_LABELS = col_names, COLUMN_LABELS = col_names, _EXTRA = ex)

		WIDGET_CONTROL,win,/REALIZE, SET_UVALUE=tab
	
		ptr_free,names
	
	ENDElSE
	
END