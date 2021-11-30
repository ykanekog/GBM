;*******************************************************************
;
;	BOCKTEST.PRO
;	
;	Vandiver Chaplin, UAH
;	chapliv@email.uah.edu
;
;	GUI program for studying blockage of GBM sources.
;	Blockage by LAT, +/- Solar Panels, and Radiators.
;
;	Caveats on blockage determinations:
;		LAT and radiator L.O.S. obstruction does not appear to be a problem
;	in photon space because they were simulated in producing GBM's 
;	response.  There may, however, be some small regions of the
;	spacecraft sky where the spatial sampling of the simulation is 
;	insufficient to capture fine structural effects, such as along 
;	the periphary of the radiator, or where the spacecraft model used is inaccurate.
;	While spatially interpolating the response database helps remove such features,
;	it is possible that interpolation is more inaccurate in some small regions.
;	Observations of dropouts using occultation also suggests that other 
;	structures on the spacecraft contribute to shadowing of some detectors.
;	Systematic blockage investigations are ongoing.
;
;	CALL: ` idl> blockest.pro, [nsrc] `
;
;	OPTIONAL INPUT: nsrc - The number of sources allowed in the table
;					(default is 5).
;
;	FEATURES (v1.1):
;		Use GBM trigdat or poshist to convert coordinates
;		Create tables of sources ('SRC_TABLE' objects), and store them
;			as IDL .sav files.
;		
;
;	VERSION: 1.0, August 19, 2009
;			 1.1, August 26, 2009
;			 1.2, February 8, 2010 - requires GBM_GEOM_v26 or later
;
;
;	REQUIREMENTS: IDL licence
;	
;	ENVIRONMENT & DEPENDENCIES: 
;		Set default paths for TRIGDAT and POSHIST in 'PathsConfig.pro'
;		Environment variable $GBM_GEOM = path to 'GBM_GEOM' folder
;		'GBM_GEOM' /-> 	'blocktest'
;						'fgeom_lib'
;	
;
;*******************************************************************

;detlist - list of indices for the currently selected detectors from the GUI widget
;ndets - lenght of the list
;model - spacecraft fgeom model object
;nsrc - number of sources we are testing
;
;RESULT=table - table is output text formatted for the table widget
FUNCTION get_blockage, model, sr_rect, nsrc, detlist, ndets, RESULT=table
	
	geom = model.geom
	

  ;models is an array of fgeom object references. The blockage
  ;for each one is tested using its member method ::intersect_test( .. ).
	models = model.composite->get(/ALL,COUNT=ncolumns)
	table = STRARR(ncolumns,nsrc)
	
	if ndets eq 0 then return, 0
	
	;an array of flags stating whether a particular detector is blocked by a model object for a given source
	;Thus it's in a 3d coordinate:  ( detector # , model #, source # )
	blockage = BYTARR(ndets,ncolumns,nsrc)
	FOR d=0,ndets-1 DO BEGIN
	
		pov = geom->det_pos(detlist[d])
		dstring = STRTRIM(STRING(detlist[d]),2)
		
		m = 0
		FOR n = 0,ncolumns-1 DO BEGIN

			blockage[d,n,*] = models[n]->intersect_test( sr_rect, FROM=pov )
			
			FOR i = 0, nsrc-1 DO IF blockage[d,n,i] THEN $
				table[n,i] = table[n,i] + dstring + ' '
				
			;print, OBJ_CLASS(model_set[m])
			;print, table[m,*]
		ENDFOR
		
	ENDFOR
	
	RETURN, blockage
END



;
;   interface to the xplot3d_custom routine which renders a 3D wireframe drawing
;   
;   
;
PRO plot_block, src_xyx_ptr, A1=aplus, A2=aminu, DET=det, POV=pov, GROUP=base, $
				D_A1 = d_a1, D_A2 = d_a2, MODEL=model, RESULT=table, NOPLOT=noplot,$
				SOURCECOLORS = colors, _REF_EXTRA=ex
	
	ON_ERROR,2
	
	geom = model.geom
	

	saplus = model.sca_p
	saminu = model.sca_n

	plot_frame1 = 1b
	plot_frame2 = 1b
	
	;option to update the solar panel objects with new angles... this effects a rotation
	;of each model in its local coordinate space (a rotation about the y axis)
	IF N_ELEMENTS(aplus) EQ 0 THEN plot_frame1 = 0b ELSE saplus->set_angle, aplus
	IF N_ELEMENTS(aminu) EQ 0 THEN plot_frame2 = 0b ELSE saminu->set_angle, aminu
	
	IF N_ELEMENTS(d_a1) EQ 1 AND N_ELEMENTS(d_a2) EQ 1 THEN BEGIN
		saplus->spin, d_a1
		saminu->spin, d_a2
		plot_frame1 = 1b
		plot_frame2 = 1b
	ENDIF
	
	
	CASE N_ELEMENTS(det) OF
		0: ndets = 0
		1: BEGIN
			CASE det OF
				99:detlist = INDGEN(14)
				-1:ndets = 0
				ELSE:detlist = [det]
			ENDCASE
			END
		ELSE:detlist = det
	ENDCASE
	
	;determine source vectors to plot
	IF PTR_VALID(src_xyx_ptr) THEN BEGIN
		sr_rect = *src_xyx_ptr
		plot_frame1 = 1b
		plot_frame2 = 1b
	END ELSE IF SIZE(src_xyx_ptr, /N_DIMENSIONS) EQ 2 THEN BEGIN
		sr_rect = src_xyx_ptr
		plot_frame1 = 1b
		plot_frame2 = 1b
	END ELSE RETURN
	
	IF SIZE(sr_rect,/N_DIMENSIONS) EQ 1 THEN nsrc = 1 $
	ELSE nsrc = (SIZE(sr_rect,/DIMENSIONS))[1]
	
	ndets = n_elements(detlist)
	;determine which sources are blocked by which detectors... this controls what color is displayed
	blockages = get_blockage( model, sr_rect, nsrc, detlist, ndets, RESULT = table )
	
	IF KEYWORD_SET(noplot) THEN BEGIN
		xplot3d_custom, [0],[0],[0], /OVERPLOT, /KILL
		RETURN
	END
	
	IF plot_frame1 THEN BEGIN
	  ;plot the +y solar panel
		saplus->xplot3d, GROUP = base, XRANGE=[-700,700], YRANGE=[-700,700],ZRANGE=[-500,500], $
						/OVERPLOT, /CLEAR, _EXTRA = ex
		;draw the other spacecraft objectsw
		model.composite->IterateCallPro,'xplot3d',/NORMAL,/OVERPLOT
	ENDIF
		
	IF plot_frame2 THEN BEGIN
	 ;plot the -y panel
		saminu->xplot3d, /OVERPLOT, _EXTRA = ex
	ENDIF
	
	color = LONARR(3)
	
	if ndets eq 0 then begin
		return
	endif
	
	;plot which sources are block w.r.t each selected detector
	FOR d=0,ndets-1 DO BEGIN
		pov = geom->det_pos(detlist[d])
		dstring = STRTRIM(STRING(detlist[d]),2)
		
		scablocks = blockages[d,0,*] OR blockages[d,1,*]
		blocked_RAD = blockages[d,2,*]
		blocked_LAT = blockages[d,3,*]

			
		line = FLTARR(3,2)
		line[*,0] = pov
	
		xplot3d_custom, [0., line[0,0]], [0., line[1,0]], [0., line[2,0]], COLOR=[0,0,0], THICK=1.0, /OVERPLOT
		
		FOR i=0,nsrc-1 DO BEGIN
			;	scablocks = blocked_pos[i] OR blocked_neg[i] ;OR blocked_LAT[i]
				
				line[*,1] = sr_rect[*, i]*400 + pov
				
				IF scablocks[i] THEN BEGIN
					xplot3d_custom, line[0,*], line[1,*], line[2,*], COLOR=colors.sca, THICK=2.0, /OVERPLOT
				END ELSE IF blocked_LAT[i] THEN BEGIN
					xplot3d_custom, line[0,*], line[1,*], line[2,*], COLOR=colors.lat, THICK=2.0, /OVERPLOT
				END ELSE IF blocked_RAD[i] THEN BEGIN
					xplot3d_custom, line[0,*], line[1,*], line[2,*], COLOR=colors.rad, THICK=2.0, /OVERPLOT
				END ELSE xplot3d_custom, line[0,*], line[1,*], line[2,*], COLOR=colors.unblocked, THICK=2.0, /OVERPLOT
		ENDFOR
		
	ENDFOR

END

PRO toggle_plot, ev

	WIDGET_CONTROL, ev.TOP, GET_UVALUE=info

	(*info).noplot = ~ (*info).noplot
	plot_event, ev
	
	CASE (*info).noplot OF
		0 : WIDGET_CONTROL, ev.ID, SET_VALUE = 'Hide Plot'
		1 : WIDGET_CONTRoL, ev.ID, SET_VALUE = 'Show Plot'
	ENDCASE
	
END

PRO plot_event, ev, _REF_EXTRA = ex

	WIDGET_CONTROL, ev.TOP, GET_UVALUE=info
	
	WIDGET_CONTROL,(*info).a1,GET_VALUE=a1
	WIDGET_CONTROL,(*info).a2,GET_VALUE=a2
	
	plot_block, (*info).s_xyz, A1=float(a1), A2=float(a2), $
		DET = *(*info).d_selection, MODEL=(*info).geometry, RESULT=blockage, $
		GROUP=ev.TOP, NOPLOT = (*info).noplot, SOURCECOLORS = (*info).colors.vectors, _EXTRA = ex
		
	rows = (*info).s_set->active(count)
	

	IF count GT 0 THEN BEGIN
		WIDGET_CONTROL,(*info).results,GET_VALUE=blocks
		ncols = (SIZE(blockage,/DIMENSIONS))[0]
		FOR o=0,ncols-1 DO blocks[o,rows] = blockage[o,*]
		WIDGET_CONTROL,(*info).results,SET_VALUE=blocks
	ENDIF
	
END

PRO shift_event, ev, _REF_EXTRA = ex

	WIDGET_CONTROL,ev.TOP,GET_UVALUE=info
	WIDGET_CONTROL,(*info).a1,GET_VALUE=a1
	WIDGET_CONTROL,(*info).a2,GET_VALUE=a2
	WIDGET_CONTROL,(*info).d_a1,GET_VALUE=d_a1
	WIDGET_CONTROL,(*info).d_a2,GET_VALUE=d_a2
	
	plot_block, (*info).s_xyz, A1=float(a1) + d_a1, A2=float(a2) + d_a2, $
		DET = *(*info).d_selection, MODEL=(*info).geometry, RESULT=blockage, $
		GROUP=ev.TOP, NOPLOT = (*info).noplot, SOURCECOLORS = (*info).colors.vectors, _EXTRA = ex
	
	sca_p = (*info).geometry.sca_p
	sca_n = (*info).geometry.sca_n
	
	WIDGET_CONTROL,(*info).a1,SET_VALUE=STRING(sca_p->get_angle(),FORMAT='(F0.1)')
	WIDGET_CONTROL,(*info).a2,SET_VALUE=STRING(sca_n->get_angle(),FORMAT='(F0.1)')
	
	rows = (*info).s_set->active(count)
	IF count GT 0 THEN BEGIN
		WIDGET_CONTROL,(*info).results,GET_VALUE=blocks
		blocks[0,rows] = blockage[0,*]
		blocks[1,rows] = blockage[1,*]
		WIDGET_CONTROL,(*info).results,SET_VALUE=blocks
	ENDIF
END

FUNCTION detbgroup_event,ev 
	
	CASE ev.VALUE OF
		'All':BEGIN
				state = BYTARR(16)
				state[0:14] = 1
				state[14:15] = [0,0]
				WIDGET_CONTROL,ev.ID, SET_VALUE = state
				selections = 99
				END
		'None':BEGIN
				state = BYTARR(16)
				WIDGET_CONTROL,ev.ID, SET_VALUE = state
				selections = -1
				END
		ELSE: BEGIN
				WIDGET_CONTROL,ev.ID, GET_VALUE = state, GET_UVALUE = buttonNames
				selections = WHERE(state[0:14],count)
				END
	ENDCASE
	
	WIDGET_CONTROL, ev.TOP, GET_UVALUE=info
	*(*info).d_selection = selections

	plot_event, ev

	RETURN,1
END

PRO detlist_ev,ev
	WIDGET_CONTROL, ev.TOP, GET_UVALUE=info
	WIDGET_CONTROL, ev.id, GET_VALUE=value, GET_UVALUE = hash
	
	*((*info).d_selection) = hash->get(value[ev.INDEX])
	
	plot_event, ev
END

PRO source_enter, ev
	ON_ERROR,2

	WIDGET_CONTROL,ev.TOP,GET_UVALUE=info
	WIDGET_CONTROL,ev.ID, GET_VALUE = sources
	
	inf = WIDGET_INFO(ev.ID, /TABLE_SELECT)

	rows = indgen(inf[3] - inf[1] + 1) + inf[1]
	updates = FLTARR(3, n_elements(rows) )
	updates[0:1,*] = sources[*,rows]
	updates[2, *] = REPLICATE(1b, n_elements(rows) )
	
	;print,'test',((*info).datamode AND (64b + 128b))
	
	n = 2
	m = n_elements(rows)
	newcolor = BYTARR(3, n, m)
	color = (*info).colors.rows.active
	FOR i=0,2 DO newcolor[i, *, *] = replicate(color[i],n,m)
	
	nc = (*info).geometry.ncols
	
;	WIDGET_CONTROL, ev.ID, SET_TABLE_SELECT = [0,inf[1],1,inf[3] ]
	WIDGET_CONTROL, ev.ID, BACKGROUND_COLOR= newcolor, USE_TABLE_SELECT = [0,inf[1],1,inf[3]]
	
	WIDGET_CONTROL, (*info).results, BACKGROUND_COLOR= newcolor, USE_TABLE_SELECT = [0,inf[1],nc-1,inf[3]]
	
	IF NOT OBJ_VALID((*info).quat) THEN BEGIN
		(*info).s_set->insert, transpose(updates), COL=['az','zen','active'], ROWS=rows
		sync,info
	ENDIF ELSE BEGIN
		CASE ev.ID OF
		(*info).table: BEGIN
					(*info).s_set->insert, transpose(updates), $
							COL=['az','zen','active'], ROWS=rows
					
					sc_to_radec, info
					
					src_checkedit_conflict,info, ROWS=rows
					
				;	WIDGET_CONTROL,(*info).radec, SET_TABLE_SELECT = [0,inf[1],1,inf[3] ]
					WIDGET_CONTROL,(*info).radec, BACKGROUND_COLOR= newcolor, $
						USE_TABLE_SELECT = [0,inf[1],1,inf[3] ]
					
					END
		(*info).radec: BEGIN
					(*info).s_set->insert, transpose(updates), $
							COL=['ra','dec','active'], ROWS=rows
					radec_to_sc, info
					
					src_checkedit_conflict,info, ROWS=rows
					
				;	WIDGET_CONTROL,(*info).table, SET_TABLE_SELECT = [0,inf[1],1,inf[3] ]
					WIDGET_CONTROL,(*info).table, BACKGROUND_COLOR= newcolor, $
						USE_TABLE_SELECT = [0,inf[1],1,inf[3] ]
					
					END
		ENDCASE
	ENDELSE

	plot_event,ev
END

PRO quit_test, ev
	WIDGET_CONTROL,ev.TOP,/DESTROY
END

PRO quit_HashDelete, detlist_wID
	WIDGET_CONTROL,detlist_wID,GET_UVALUE=hash
	OBJ_DESTROY,hash
END

PRO quit_CollectGarbage, topID, STRUCT=struct

	IF N_ELEMENTS(struct) EQ 0 THEN WIDGET_CONTROL,topID, GET_UVALUE=struct
	
	OBJ_DESTROY,(*struct).geometry.composite
	HEAP_FREE,struct
END

PRO plot_select, ev

	WIDGET_CONTROL,ev.TOP, GET_UVALUE=info
	
	WIDGET_CONTROL,(*info).results, GET_VALUE=result_str
	WIDGET_CONTROL,(*info).table, GET_VALUE=sr_sph
	
	inf = WIDGET_INFO((*info).table, /TABLE_SELECT)
	
	rows = indgen(inf[3] - inf[1] + 1) + inf[1]
	
	nc = (*info).geometry.ncols
	n = 2
	m = (SIZE(sr_sph,/DIMENSIONS))[1]
	newcolor = BYTARR(3, n, m)
	active = (*info).colors.rows.active
	inactive = (*info).colors.rows.inactive
	
	obstrcol = BYTARR(3, nc, m)
	FOR i=0,2 DO BEGIN
		newcolor[i, *, *] = replicate(inactive[i],n, m)
		obstrcol[i, *, *] = replicate(inactive[i],nc,m)
	ENDFOR
	FOR i=0,2 DO BEGIN
		newcolor[i, *, rows] = replicate(active[i],n, n_elements(rows) )
		obstrcol[i, *, rows] = replicate(active[i],nc,n_elements(rows) )
	ENDFOR
	
	WIDGET_CONTROL,(*info).table,BACKGROUND_COLOR=newcolor
	
	
	WIDGET_CONTROL, (*info).results, BACKGROUND_COLOR = obstrcol
	
	IF OBJ_VALID((*info).quat) THEN BEGIN
	;	WIDGET_CONTROL,(*info).radec,SET_TABLE_SELECT=[0,inf[1],1,inf[3]]
		WIDGET_CONTROL,(*info).radec,BACKGROUND_COLOR=newcolor
	ENDIF
	
	(*info).s_set->set_active, ROWS=rows
	sync,info
	
	plot_event,ev
END

pro reselect_all, ev
	WIDGET_CONTROL,ev.TOP, GET_UVALUE=info
	WIDGET_CONTROL,(*info).table,BACKGROUND_COLOR=(*info).colors.rows.active
	WIDGET_CONTROL,(*info).radec,BACKGROUND_COLOR=(*info).colors.rows.active
	WIDGET_CONTROL,(*info).results, BACKGROUND_COLOR = (*info).colors.rows.active
	
	(*info).s_set->set_active
	sync, info
	plot_event,ev
END

PRO sync, info

	sr_sph = (*info).s_set->pull_azzen(/ACTIVE)
	sr_sph[1,*] = 90 - sr_sph[1,*]
	
	sr_rect = FLTARR(3, n_elements(sr_sph[0,*]))
	sr_rect = CV_COORD(FROM_SPHERE=sr_sph,/DEGREES,/TO_RECT)
	*(*info).s_xyz = sr_rect
	print,(*info).extrawin.angles
	IF (*info).extrawin.angles NE 0 THEN call_angles_window, ev, INFO=info
	
END

FUNCTION read_trigdat, info, FILE=result
	ON_ERROR,2
	
	IF OBJ_VALID((*info).fs) THEN OBJ_DESTROY, (*info).fs
	
	(*info).fs = OBJ_NEW('FXB_IOS')
	(*info).extnum = 5
	err=''
	status = -1
	(*info).datamode = 0
	status = (*info).fs->open(result, (*info).extnum, ERRMSG=err)
	
	IF status NE 0 THEN BEGIN
		re = DIALOG_MESSAGE("Could not open file: "+err, /ERROR)
		(*info).datamode = (*info).datamode AND (64b + 128b)
		OBJ_DESTROY, (*info).fs
		RETURN, status
	ENDIF
	
	(*info).fs->keyByCol, 1
	ttime = (*info).fs->fxpar('TRIGTIME', status)
	
	(*info).refrow.row = (*info).fs->nearest_row(ttime)
	(*info).refrow.time = ttime
	(*info).fs->impute_ref, ttime
	
	FXBREAD, (*info).fs->lun(), quat, 3, (*info).refrow.row
	
	;print,quat
	
	Qs = quat[3]
	Qxyz = quat[0:2]
	
	;Qs = quat[0]
	;Qxyz = quat[1:3]
	
	IF OBJ_VALID((*info).quat) THEN (*info).quat->set, Q = [Qs, Qxyz] $
	ELSE (*info).quat = OBJ_NEW('FGEOM_QUATERNION', Q = [Qs, Qxyz])
	
	WIDGET_CONTROL, (*info).file, SET_VALUE = (*info).fs->file()
	WIDGET_CONTROL, (*info).reflab, SET_VALUE = STRING(ttime, FORMAT='(F0.6)')
	WIDGET_CONTROL, (*info).refutc, SET_VALUE = met_to_utc(ttime, /ISO)
	;WIDGET_CONTROL, (*info).refoff, SET_VALUE = STRING(0.0, FORMAT='(F0.3)'),/SENSITIVE
	WIDGET_CONTROL, (*info).rowlab, SET_VALUE = STRTRIM(STRING((*info).refrow.row),2)
	WIDGET_CONTROL, (*info).radec, /SENSITIVE, /EDITABLE, FOREGROUND_COLOR=[0,0,0]
	
	updateOffsetDisplay, info, ttime, /SENSITIVE
	
	IF (*info).datamode EQ 0 THEN sc_to_radec, info $
	ELSE radec_to_sc, info
	
	(*info).datamode = 1b OR ((*info).datamode AND (64b + 128b))
	
	;r = insertExternSources(info, [ [10,20,30],[40,50,60] ], 3, 5)
	
	RETURN,0
	
END

FUNCTION read_poshist, info, FILE=result
	ON_ERROR,2
	
	IF OBJ_VALID((*info).fs) THEN OBJ_DESTROY, (*info).fs
	
	(*info).fs = OBJ_NEW('FXB_IOS')
	err=''
	status = -1
	(*info).extnum = 1
	(*info).datamode = 0
	
	status = (*info).fs->open(result, (*info).extnum, ERRMSG=err)
	
	IF status NE 0 THEN BEGIN
		re = DIALOG_MESSAGE("Could not open file: "+err, /ERROR)
		OBJ_DESTROY, (*info).fs
		(*info).datamode = (*info).datamode AND (64b + 128b)
		RETURN, status
	ENDIF
	
	(*info).fs->keyByCol, 1
	
;	FXBREADM, (*info).fs->lun(), [1,2,3,4,5], ROW=1, PASS_METHOD='POINTER', POINTERS=ptr_arr
;	time = *(ptr_arr[0])
;	Qx = *(ptr_arr[1])
;	Qy = *(ptr_arr[2])
;	Qz = *(ptr_arr[3])
;	Qs = *(ptr_arr[4])
;	ptr_free,ptr_arr
	time = 0.0
	FXBREAD, (*info).fs->lun(), time, 1, 1
	FXBREAD, (*info).fs->lun(), Qx, 2, 1
  FXBREAD, (*info).fs->lun(), Qy, 3, 1
  FXBREAD, (*info).fs->lun(), Qz, 4, 1
  FXBREAD, (*info).fs->lun(), Qs, 5, 1
	
	;FXBREADM, (*info).fs->lun(), [1,2,3,4,5], time, Qs, Qx, Qy, Qz, ROW=1
	
	(*info).refrow.time = time
	(*info).refrow.row = 1
	
	IF OBJ_VALID((*info).quat) THEN (*info).quat->set, Q = [Qs, Qx, Qy, Qz] $
	ELSE (*info).quat = OBJ_NEW('FGEOM_QUATERNION', Q = [Qs, Qx, Qy, Qz])
	
	IF (*info).datamode EQ 0 THEN sc_to_radec, info $
	ELSE radec_to_sc, info
	(*info).datamode = 2b OR ( (*info).datamode AND (64b + 128b) )
	
	FXBFIND, (*info).fs->lun(), 'TTYPE', cols, values, n_found
	
	inds = WHERE(STRMATCH(values, 'SC_L*'), count)
	IF count EQ 2 THEN BEGIN
		(*info).datamode = (*info).datamode OR 4b
	ENDIF
	inds = WHERE(STRMATCH(values, 'SADA*'), count2)
	IF count2 EQ 2 THEN BEGIN
		(*info).datamode = (*info).datamode OR 8b
		read_angles, info
	ENDIF
	
	WIDGET_CONTROL, (*info).file, SET_VALUE = (*info).fs->file()
	WIDGET_CONTROL, (*info).reflab, SET_VALUE = STRING(time, FORMAT='(F0.6)')
	WIDGET_CONTROL, (*info).refutc, SET_VALUE = met_to_utc(time, /ISO)
	WIDGET_CONTROL, (*info).rowlab, SET_VALUE = STRTRIM(STRING((*info).refrow.row),2)
	WIDGET_CONTROL, (*info).radec, /SENSITIVE, /EDITABLE, FOREGROUND_COLOR=[0,0,0]
	
	updateOffsetDisplay, info, time, /SENSITIVE
	
	RETURN,0
	
END

PRO select_trigdat, ev
	COMMON bto_paths, trigpath, poshpath
	
	result = DIALOG_PICKFILE(/READ,FILTER=['*trigdat*.fit'],PATH=trigpath)
	IF result EQ '' THEN RETURN
	
	WIDGET_CONTROL,ev.TOP, GET_UVALUE=info
	
	status = read_trigdat( info, FILE=result )
	
	IF NOT status THEN BEGIN
		color = WIDGET_INFO( (*info).table, /TABLE_BACKGROUND_COLOR )
		WIDGET_CONTROL, (*info).radec, BACKGROUND_COLOR = color
		;WIDGET_CONTROL, (*info).results, BACKGROUND_COLOR = color
	ENDIF
	
	plot_event,ev
END

PRO select_poshist, ev
	COMMON bto_paths, trigpath, poshpath
	
	result = DIALOG_PICKFILE(/READ,FILTER=['*poshist*.fit'],PATH=poshpath)
	IF result EQ '' THEN RETURN
	
	WIDGET_CONTROL,ev.TOP, GET_UVALUE=info
	
	status = read_poshist( info, FILE=result )
	
	IF NOT status THEN BEGIN
		color = WIDGET_INFO( (*info).table, /TABLE_BACKGROUND_COLOR )
		WIDGET_CONTROL, (*info).radec, BACKGROUND_COLOR = color
		;WIDGET_CONTROL, (*info).results, BACKGROUND_COLOR = color
	ENDIF
	
	plot_event,ev
END

PRO read_row, info

	IF ((*info).datamode AND 1b) EQ 1 THEN BEGIN
			
			FXBREAD, (*info).fs->lun(), quat, 3, (*info).refrow.row
			Qs = quat[3]
			Qxyz = quat[0:2]
			;print,quat
			;Qs = quat[0]
			;Qxyz = quat[1:3]
			(*info).quat->set, Q = [Qs, Qxyz]
			radec_to_sc, info
	END ELSE $
	IF (*info).datamode GE 2 THEN BEGIN
			Qx = 0.0
			Qy = Qx
			Qz = Qx
			Qs = Qx
			;FXBREADM, (*info).fs->lun(), [2,3,4,5], Qx, Qy, Qz, Qs, ROW=(*info).refrow.row
			;FXBREADM, (*info).fs->lun(), [2,3,4,5], ROW=(*info).refrow.row, PASS_METHOD='POINTER', POINTERS=ptr_arr
			
			FXBREAD, (*info).fs->lun(), Qx, 2, (*info).refrow.row
			FXBREAD, (*info).fs->lun(), Qy, 3, (*info).refrow.row
			FXBREAD, (*info).fs->lun(), Qz, 4, (*info).refrow.row
			FXBREAD, (*info).fs->lun(), Qs, 5, (*info).refrow.row
			
;			 Qx = *(ptr_arr[0])
;       Qy = *(ptr_arr[1])
;       Qz = *(ptr_arr[2])
;       Qs = *(ptr_arr[3])
;  
;      ptr_free,ptr_arr
  
			(*info).quat->set, Q = [Qs, Qx, Qy, Qz]
			radec_to_sc, info
		IF ((*info).datamode AND 8b) EQ 8b THEN read_angles, info	
	END
	
END

PRO read_angles, info

  ; fxbreadm calls EXECUTE() with multiple columns, which cant be used in IDLvm. stupid stupid.  
	;FXBREADM, (*info).fs->lun(), ['SADA_PY','SADA_NY'], ROW=(*info).refrow.row, PASS_METHOD='POINTER', POINTERS=a_ptr
	
	;FXBREADM, (*info).fs->lun(), 'SADA_PY', a1, ROW=(*info).refrow.row
	;FXBREADM, (*info).fs->lun(), 'SADA_NY', a2, ROW=(*info).refrow.row

	FXBREAD, (*info).fs->lun(), a1, 'SADA_PY', (*info).refrow.row
	FXBREAD, (*info).fs->lun(), a2, 'SADA_NY', (*info).refrow.row
	
	WIDGET_CONTROL,(*info).a1,SET_VALUE=STRING(a1,FORMAT='(F0.1)')
	WIDGET_CONTROL,(*info).a2,SET_VALUE=STRING(a2,FORMAT='(F0.1)')
END

PRO data_offset, ev
	ON_ERROR,2
	WIDGET_CONTROL,ev.TOP, GET_UVALUE=info
	
	IF (*info).datamode EQ 0 THEN RETURN
	
	WIDGET_CONTROL,ev.ID, GET_VALUE = val
	
	CASE val OF
		'Fwd ':	BEGIN
					WIDGET_CONTROL,(*info).rowinc, GET_UVALUE=delta
					(*info).refrow.row += delta	
					numrows = (*info).fs->numrows()
					IF (*info).refrow.row LT 1 THEN (*info).refrow.row = 1 $
					ELSE IF (*info).refrow.row GT numrows THEN (*info).refrow.row = numrows
					FXBREAD, (*info).fs->lun(), row_time, 1, (*info).refrow.row
				END
		'Back':	BEGIN
					WIDGET_CONTROL,(*info).rowinc, GET_UVALUE=delta
					(*info).refrow.row -= delta	
					numrows = (*info).fs->numrows()
					IF (*info).refrow.row LT 1 THEN (*info).refrow.row = 1 $
					ELSE IF (*info).refrow.row GT numrows THEN (*info).refrow.row = numrows
					FXBREAD, (*info).fs->lun(), row_time, 1, (*info).refrow.row
				END
		ELSE:	BEGIN
				val = DOUBLE(val[0])
				(*info).refrow.row = $
					(*info).fs->nearest_row( (*info).refrow.time + val, $
									VALUE=row_time)
				END
	ENDCASE
	
	read_row, info
	
	time = row_time - (*info).refrow.time
	updateOffsetDisplay, info, row_time
	WIDGET_CONTROL,(*info).rowlab, SET_VALUE = STRTRIM(STRING((*info).refrow.row),2)
	
	sync, info
	
	plot_event, ev
END

PRO updateOffsetDisplay, info, row_time, _REF_EXTRA=ex
	offset = row_time - (*info).refrow.time
	IF ABS(offset) LT 0.001 THEN offset = 0.0
	WIDGET_CONTROL,(*info).refoff, SET_VALUE = STRING(offset, FORMAT='(F0.3)'), _EXTRA=['SENSITIVE']
	WIDGET_CONTROL,(*info).offutc, SET_VALUE = met_to_utc(row_time, /ISO)
	WIDGET_CONTROL,(*info).offmet, SET_VALUE = STRING(row_time, FORMAT='(F0.6)')
END

PRO valtouvalf, ev
	WIDGET_CONTROL, ev.ID, GET_VALUE=val
	WIDGET_CONTROL, ev.ID, SET_UVALUE=float(val)
END


PRO src_checkedit_conflict, info, ROWS=rows

	IF ((*info).datamode AND 64b) EQ 0b THEN RETURN
					
	names = (*info).s_set->get(COL='name')
	newLabels = (*names)[*]
	conflict = WHERE( (newLabels[rows] NE '') AND (STREGEX(newLabels[rows],'^[^*]',/BOOLEAN)) EQ 1, count)
	
	IF count EQ 0 THEN BEGIN
		PTR_FREE,names
		RETURN
	ENDIF
	newLabels[rows[conflict]] = '*' + newLabels[rows[conflict]]
	
	insert_src_names, info, newLabels
	
	IF ((*info).datamode AND 128b) NE 0 THEN BEGIN
		(*info).datamode = (*info).datamode AND NOT 128b
		WIDGET_CONTROL, (*info).setFile, GET_VALUE=file
		WIDGET_CONTROL, (*info).setFile, SET_VALUE='*'+file
	ENDIF
	
	PTR_FREE,names
END

PRO src_checkedit_confirm, info

	IF ((*info).datamode AND 64b) EQ 0b THEN RETURN
					
	names = (*info).s_set->get(COL='name')
	newLabels = (*names)[*]
	conflict = WHERE( STREGEX(newLabels,'^\*',/BOOLEAN), count)
	
	IF count EQ 0 THEN BEGIN
		PTR_FREE,names
		RETURN
	ENDIF
	newLabels[conflict] = STREGEX(newLabels[conflict],'[^*]+',/EXTRACT)
	
	insert_src_names, info, newLabels
	
	IF ((*info).datamode AND 128b) NE 0 THEN BEGIN
		(*info).datamode = (*info).datamode AND NOT 128b
		WIDGET_CONTROL, (*info).setFile, GET_VALUE=file
		WIDGET_CONTROL, (*info).setFile, SET_VALUE=STREGEX(file,'[^*]+',/EXTRACT)
	ENDIF
	
	PTR_FREE,names
END

PRO insert_src_names, info, sourceNames, _REF_EXTRA=ex
	
	rowlabels = STRARR( (*info).s_set->number() )
	
	empty = WHERE(sourceNames EQ '', count)
	rowlabels = sourceNames
	
	IF count NE 0 THEN rowlabels[empty] = STRTRIM(STRING(empty),2)

	(*info).s_set->insert, sourceNames, COL='name', _EXTRA=ex
	
	WIDGET_CONTROL, (*info).table, ROW_LABELS = rowlabels
	WIDGET_CONTROL, (*info).radec, ROW_LABELS = rowlabels
	WIDGET_CONTROL, (*info).results, ROW_LABELS = rowlabels

END

PRO edit_src_rows, ev
	ON_ERROR,2
	WIDGET_CONTROL,ev.TOP,GET_UVALUE=info
	
	names = (*info).s_set->get(COL='name')
	
	tempBase = WIDGET_BASE(GROUP_LEADER=ev.TOP, /COLUMN, /MODAL, /BASE_ALIGN_CENTER)
	nameSet = WIDGET_TABLE(tempBase, VALUE = transpose(*names), $
		COLUMN_LABELS = ['Source Name'], COLUMN_WIDTHS=110, /EDITABLE, /ALL_EVENTS)
	close = WIDGET_BUTTON(tempBase, VALUE='OK')
	cancel = WIDGET_BUTTON(tempBase, VALUE='Cancel')
	
	WIDGET_CONTROL,tempBase,/REALIZE
	
	emptyset = REPLICATE('', (*info).s_set->number() )
	
	REPEAT BEGIN
		ev = WIDGET_EVENT(tempBase)
	ENDREP UNTIL (ev.ID EQ close OR ev.ID EQ cancel)
	
	IF ev.ID EQ cancel THEN BEGIN
		WIDGET_CONTROL,tempBase,/DESTROY
		PTR_FREE,names
		RETURN
	ENDIF
	
	WIDGET_CONTROL, nameSet, GET_VALUE = newSet
	
	insert_src_names, info, newSet[*]
	
	WIDGET_CONTROL,tempBase,/DESTROY
	PTR_FREE,names
END


PRO load_set,ev
	WIDGET_CONTROL,ev.TOP, GET_UVALUE=info
	
	sc = PATH_SEP()
	path = (*info).geom_dir + sc + 'blocktest' + sc + 'SourceSets'
	infile = DIALOG_PICKFILE(/READ,PATH = path, FILTER=['*.sav'])
	IF infile EQ '' THEN RETURN
	
	RESTORE, infile, /VERBOSE
	
	OBJ_DESTROY, (*info).s_set
	loadstate = s_set->active()
	s_set->set_active
	
	(*info).s_set = s_set
	
	sr_radec = s_set->pull_radec()
	
	WIDGET_CONTROL, (*info).radec, SET_VALUE = sr_radec[0:1,*]
	
	IF (*info).datamode NE 0 THEN radec_to_sc, info $
	ELSE BEGIN
		;(*info).datamode = (*info).datamode OR (64b + 128b)
	
		sr_scsph = s_set->pull_azzen()
		sr_scsph[1,*] = 90 - sr_scsph[1,*]
		
		sr_rect = FLTARR(3, n_elements(sr_scsph[0,*]))
		sr_rect = CV_COORD(FROM_SPHERE=sr_scsph,/DEGREES,/TO_RECT)
		;*(*info).s_xyz = sr_rect
		
		WIDGET_CONTROL, (*info).table, SET_VALUE = sr_scsph[0:1,*]
	ENDELSE
	
	(*info).datamode = (*info).datamode OR (64b + 128b)
	
	WIDGET_CONTROL, (*info).setFile, SET_VALUE = FILE_BASENAME(infile)
	
	names = s_set->get(COL='name')
	
	insert_src_names, info, (*names)[*]
	s_set->set_active, ROWS=loadstate
	
	PTR_FREE,names
	sync,info
	
	WIDGET_CONTROL, (*info).table, $
		SET_TABLE_SELECT = [0,loadstate[0],1,max(loadstate)]
	WIDGET_CONTROL, (*info).radec, $
		SET_TABLE_SELECT = [0,loadstate[0],1,max(loadstate)]
	
	plot_event,ev
END

PRO save_set,ev

	WIDGET_CONTROL,ev.TOP, GET_UVALUE=info
	
	sc = PATH_SEP()
	path = (*info).geom_dir + sc + 'blocktest' + sc + 'SourceSets'
	outfile = DIALOG_PICKFILE(/WRITE,/OVERWRITE_PROMPT,PATH = path, FILTER=['*.sav'])
	IF outfile EQ '' THEN RETURN
	
	s_set = (*info).s_set
	r = HEAP_SAVE(s_set, 1)
	SAVE,s_set, FILE=outfile
	
	fname = FILE_BASENAME(outfile)
	
	IF ((*info).datamode AND 128b) EQ 0 THEN src_checkedit_confirm, info
	
	WIDGET_CONTROL, (*info).setFile, SET_VALUE = fname
	
END

PRO call_angles_window, ev, INFO=info
	
	IF n_elements(info) eq 0 && n_elements(ev) ne 0 THEN BEGIN
		WIDGET_CONTROL,ev.TOP, GET_UVALUE=info
		win = (*info).extrawin.angles
		source_angles_window, (*info).s_set, (*info).geometry.geom, $
			WIN_ID = win, GROUP_LEADER = ev.TOP, NEW=~(WIDGET_INFO(win,/VALID_ID))
	ENDIF ELSE BEGIN
		win = (*info).extrawin.angles
		source_angles_window, (*info).s_set, (*info).geometry.geom, $
			WIN_ID = win
	ENDELSE
	
	(*info).extrawin.angles = win
END

PRO call_sep_window, ev, INFO=info

	IF n_elements(ev) ne 0 THEN BEGIN
		WIDGET_CONTROL,ev.TOP, GET_UVALUE=info
		win = (*info).extrawin.separations
		source_separation_window, (*info).s_set, $
			WIN_ID = win, GROUP_LEADER = ev.TOP,/NEW
	ENDIF ELSE BEGIN
		win = (*info).extrawin.separations
		source_separation_window, (*info).s_set, $
			WIN_ID = win
	ENDELSE
	
	(*info).extrawin.separations = win
	
END
	

PRO interrupt, ev
	WIDGET_CONTROL,ev.TOP, GET_UVALUE=info
	o = (*info).fs
	kt = (*info).refrow.time
	lun = o->lun()
	STOP
END

PRO buckets, ev
	WIDGET_CONTROL,ev.TOP, GET_UVALUE=info
	IF NOT OBJ_VALID((*info).fs) THEN RETURN
	bu = (*info).fs->buckets(nb)
	FOR i=0,nb-1 DO print,i,bu[i].row,bu[i].key - (*info).refrow.time
END

PRO blocktest_event,ev

	IF ev.ID EQ ev.TOP THEN	WIDGET_CONTROL,ev.TOP,MAP=0

END

PRO blocktest, nsrc, GROUP_LEADER=tlb_id, SRC_TABLE=table_obj, NOPLOT=noplot, CLIENT_ID=client_base,$
	INTERFACE=object, AUTOKILL=autokill, TRIGDAT_INIT=trigdat, POSHIST_INIT=poshist, CATTEST=cattest

	geom_dir = GETENV('GBM_GEOM')
	
	IF geom_dir EQ '' THEN BEGIN
		;print, 'BLOCKTEST.pro: ERROR, Environment variable GBM_GEOM not found'
		geom_dir='.'
		;RETURN
	ENDIF
	
	@PathsConfig
	
	;Construct spacecraft model
	geom = OBJ_NEW('GBM_GEOMETRY')
	saplus = OBJ_NEW('SOLAR_ARRAY_PLUS')
	saminu = OBJ_NEW('SOLAR_ARRAY_MINUS')
	lat = OBJ_NEW('LAT')
	rad_p = OBJ_NEW('LAT_RADIATOR_PLUS')
	rad_n = OBJ_NEW('LAT_RADIATOR_MINUS')
	
	radiators = OBJ_NEW('fgcomposite')
	radiators->Add, [rad_p, rad_n]
	
	container = OBJ_NEW('fgcomposite')				
	
	if keyword_set(cattest) then begin
		glast = OBJ_NEW('fgcomposite')
		
		maskGroup = OBJ_NEW('fgcomposite', /COMP_NOT,/COMP_AND)
		testGroup = OBJ_NEW('fgcomposite')
		
		pluspanels = OBJARR(20)
		minuspanels = OBJARR(20)
		for a=0,19 do begin
			p = OBJ_NEW('SOLAR_ARRAY_PLUS')
			n = OBJ_NEW('SOLAR_ARRAY_MINUS')
			p->set_angle,a*19.0/90.0
			n->set_angle,a*19.0/90.0
			pluspanels[a] = p
			minuspanels[a] = n
		endfor
		
		testGroup->Add, OBJ_NEW('LAT_RADIATOR_PLUS')
		testGroup->Add, OBJ_NEW('LAT_RADIATOR_MINUS')
		maskGroup->Add, [ OBJ_NEW('LAT'), pluspanels, minuspanels]
		
		glast->Add, [maskGroup, testGroup]
		
		container->Add, [saplus, saminu, radiators, glast]
		model_columns = ['+y SCA', '-y SCA', '+/- Rad.','CAT']
		model = {geom:geom, composite:container, sca_p:saplus, sca_n:saminu, ncols:4}
	endif else begin
		container->Add, [saplus, saminu, radiators, lat]
		model_columns = ['+y SCA', '-y SCA', '+/- Rad.','LAT']
		model = {geom:geom, composite:container, sca_p:saplus, sca_n:saminu, ncols:4}
	end
		
	
	
	
	colors = {  vectors : {lat:[125,250,0], rad:[175,125,0], sca:[200,0,0], unblocked:[0,100,0] },$
				text : {active: [0,0,0], inactive:[130,130,130]}, $
				rows : {active: [255,255,100], inactive:[255,255,255]} $
			}
	resultsTableBG = $		
	[ [colors.vectors.sca], [colors.vectors.sca], [colors.vectors.rad], [colors.vectors.lat] ]
	
	;geom->shift_detector, 2, 2, /VIEW
	
	;Create or use input source table
	haveTable = OBJ_VALID(table_obj) && OBJ_ISA(table_obj,'SRC_TABLE')
	IF haveTable THEN BEGIN
		s_set = table_obj
		nsrc = s_set->number()
	END ELSE BEGIN
	
		IF N_ELEMENTS(nsrc) EQ 0 THEN nsrc = 5
	
		s_set = OBJ_NEW('SRC_TABLE', nsrc)
		
		init_blocktable = STRARR(4,nsrc)
		s_set->insert, [ [0],	$
						[0]	$
					],  COL=['az','zen']
		s_set->insert, REPLICATE(1b, 1), COL='active'
	END
	show = s_set->active(count)
	sr_sph = s_set->pull_azzen()	
	sr_celest = s_set->pull_radec()

	;print,show

	sr_sph[1,*] = 90 - sr_sph[1,*]
	sr_rect = CV_COORD(FROM_SPHERE=sr_sph[*,show] ,/DEGREES,/TO_RECT)
	sr_sph[1,*] = 90 - sr_sph[1,*]
	
	srcInitTextColor = BYTARR(3,2,nsrc)
	
	FOR i=0,2 DO srcInitTextColor[i,*,*] = REPLICATE(colors.rows.inactive[i], 2, nsrc)
	FOR i=0,2 DO srcInitTextColor[i,*,show] = REPLICATE(colors.rows.active[i], 2, count)
	
	
	;Construct hash object for the detector select list
	dethash = OBJ_NEW('ADT_DHASH', 30)
	dlist = STRARR(16)
	FOR d=0,11 DO BEGIN
		key = 'NaI '+STRTRIM(STRING(d),2)
		key = STRTRIM(key,2)
		dethash->set, key, d
		dlist[d] = key
	ENDFOR
	FOR d=0,1 DO BEGIN
		key = 'BGO '+STRTRIM(STRING(d),2)
		key = STRTRIM(key,2)
		dethash->set, key, 12+d
		dlist[d+12] = key
	ENDFOR
	dlist[14:15] = ['All', 'None']
	dethash->set, dlist[14], 99
	dethash->set, dlist[15], -1
	
	
	
	
;	|------blocktest------------|
;	| I		|		II			|
;	|		||		II.Top     	||
;	|		||<----II.Tables-->	||
;	| I.a  	||	II.s | II.c		||
;	| 	   	||<----II.Buttons-->||
;	| ***	|<-----II.Bottom--->|
;	| I.o	|	II.b			|
;   |---------------------------|
	
	IF N_ELEMENTS(tlb_id) NE 0 && WIDGET_INFO(tlb_id,/VALID_ID) THEN BEGIN
		IF KEYWORD_SET(autokill) THEN BEGIN
		topBase = WIDGET_BASE(GROUP_LEADER=tlb_id, TITLE='GBM Blockage',/ROW,MBAR=mbar, $
					KILL_NOTIFY='quit_CollectGarbage')
		ENDIF ELSE topBase = WIDGET_BASE(GROUP_LEADER=tlb_id, TITLE='GBM Blockage',/ROW,MBAR=mbar, /TLB_KILL_REQUEST_EVENTS)

	END ELSE topBase = WIDGET_BASE(TITLE='GBM Blockage',/ROW,MBAR=mbar, KILL_NOTIFY='quit_CollectGarbage')
	
	client_base = topBase
	
	file = WIDGET_BUTTON(mbar,VALUE='File',/MENU)
	quit = WIDGET_BUTTON(file,VALUE='Close',EVENT_PRO='quit_test')
	sset = WIDGET_BUTTON(mbar,VALUE='Sources',/MENU)
	edit = WIDGET_BUTTON(sset, VALUE = 'Edit Names', $
			EVENT_PRO='edit_src_rows')
	load = WIDGET_BUTTON(sset, VALUE = 'Load Set', EVENT_PRO='load_set')
	save = WIDGET_BUTTON(sset, VALUE = 'Save Set', EVENT_PRO='save_set')
	angs = WIDGET_BUTTON(sset, VALUE = 'Detector Angles...', EVENT_PRO='call_angles_window')
	srcs = WIDGET_BUTTON(sset, VALUE = 'Source Separation...', EVENT_PRO='call_sep_window')
	
	;Region I
	leftBase = WIDGET_BASE(topBase,/COLUMN,/ALIGN_CENTER)
	lab = WIDGET_LABEL(leftBase,VALUE='Enter solar panel tilt')
	lab = WIDGET_LABEL(leftBase,VALUE='(degrees from +z):')
	;I.a
	angles = WIDGET_BASE(leftBase,/ROW,/ALIGN_CENTER)
	plusAngles = WIDGET_BASE(angles,/COLUMN,/ALIGN_CENTER, /FRAME)
	lab = WIDGET_LABEL(plusAngles,VALUE='+y Angle:')
	angle1 = WIDGET_TEXT(plusAngles,/EDITABLE,VALUE='0',EVENT_PRO='plot_event',XSIZE=4)
	lab = WIDGET_LABEL(plusAngles, VALUE='Rotate by:')
	del_a1 = WIDGET_TEXT(plusAngles,/EDITABLE,VALUE='0',EVENT_PRO='shift_event',XSIZE=4)
	
	minusAngles = WIDGET_BASE(angles,/COLUMN,/ALIGN_CENTER, /FRAME)
	lab = WIDGET_LABEL(minusAngles,VALUE='-y Angle:')
	angle2 = WIDGET_TEXT(minusAngles,/EDITABLE,VALUE='0',EVENT_PRO='plot_event',XSIZE=4)	
	lab = WIDGET_LABEL(minusAngles, VALUE='Rotate by:')
	del_a2 = WIDGET_TEXT(minusAngles,/EDITABLE,VALUE='0',EVENT_PRO='shift_event',XSIZE=4)
	
	lab = WIDGET_LABEL(leftBase,VALUE='Detector  :')
	

;	detlist = WIDGET_DROPLIST(leftBase,VALUE = dlist,UVALUE=dethash, $
;				EVENT_PRO='detlist_ev', /ALIGN_CENTER, KILL_NOTIFY='quit_HashDelete')
	
	dlist_ptr = PTR_NEW(dlist)
	
	detGroup = CW_BGROUP(leftBase, dlist, /NONEXCLUSIVE, /RETURN_NAME, COLUMN=4, $
						UVALUE=dlist_ptr, EVENT_FUNC='detbgroup_event',/FRAME)
	
	;I.o
	or_base = WIDGET_BASE(leftBase,/COLUMN, /ALIGN_CENTER, /FRAME)
	lab = WIDGET_LABEL(or_base, VALUE='ORIENTATION INFO:')
	flab = WIDGET_LABEL(or_base, VALUE='----', /ALIGN_CENTER, /DYNAMIC_RESIZE)
	baseb = WIDGET_BASE(or_base,/ROW, /ALIGN_CENTER)
	quat = WIDGET_BUTTON(baseb, VALUE = 'Load Poshist',$
			EVENT_PRO='select_poshist', /ALIGN_CENTER)
	quat = WIDGET_BUTTON(baseb, VALUE = 'Load Trigdat',$
			EVENT_PRO='select_trigdat', /ALIGN_CENTER)
			
	or_base_row = WIDGET_BASE(or_base,/ROW)
	lab = WIDGET_LABEL(or_base_row, VALUE="T0 [MET]:")
	reflab = WIDGET_LABEL(or_base_row, VALUE = '----', /DYNAMIC_RESIZE)
	or_base_row = WIDGET_BASE(or_base,/ROW)
	lab = WIDGET_LABEL(or_base_row, VALUE="T0 [UT] :")
	refutc = WIDGET_LABEL(or_base_row, VALUE = '----', /DYNAMIC_RESIZE)	
	or_base_row = WIDGET_BASE(or_base,/ROW)
	lab = WIDGET_LABEL(or_base_row, VALUE="T0 + (s):")
	refoff = WIDGET_TEXT(or_base_row, VALUE = '', /EDITABLE,SENSITIVE = 0, EVENT_PRO = 'data_offset')
	rowlab = WIDGET_LABEL(or_base_row, VALUE = '[]', /DYNAMIC_RESIZE)
	or_base_row = WIDGET_BASE(or_base,/ROW)
	
	lab = WIDGET_LABEL(or_base_row, VALUE="t (MET) =")
	offmet = WIDGET_LABEL(or_base_row, VALUE = '----', /DYNAMIC_RESIZE)	
	or_base_row = WIDGET_BASE(or_base,/ROW)
	lab = WIDGET_LABEL(or_base_row, VALUE="t (UT)  =")
	offutc = WIDGET_LABEL(or_base_row, VALUE = '----', /DYNAMIC_RESIZE)	
	
	or_base_row = WIDGET_BASE(or_base,/ROW)
	
	forw = WIDGET_BUTTON(or_base_row, VALUE='Fwd ', EVENT_PRO = 'data_offset')
	back = WIDGET_BUTTON(or_base_row, VALUE='Back', EVENT_PRO = 'data_offset')
	rowinc = WIDGET_TEXT(or_base_row, VALUE='1',/EDITABLE, XSIZE=10, UVALUE=1, $
		EVENT_PRO='valtouvalf', /ALL_EVENTS)
	lab = WIDGET_LABEL(or_base_row, VALUE = '[Rows]')
	
	;STOPALL = WIDGET_BUTTON(base, VALUE = 'INTERRUPT',EVENT_PRO = 'interrupt')
	;STOPALL = WIDGET_BUTTON(base, VALUE = 'BUCKETS',EVENT_PRO = 'buckets')
	
	
	;Region II
	base2 = WIDGET_BASE(topBase,/COLUMN, /ALIGN_CENTER)
	
	;II.Top
	topPanel = WIDGET_BASE(base2,/COLUMN)
	
	;II.Tables
	topTables = WIDGET_BASE(topPanel, /ROW)
	
	;II.s
	tTs = WIDGET_BASE(topTables,/COLUMN, /ALIGN_CENTER)
	lab = WIDGET_LABEL(tTs,VALUE='Sources:')
	table = WIDGET_TABLE(tTs, VALUE = sr_sph[0:1,*], $
			/RESIZEABLE_COLUMNS, COLUMN_LABELS=['Azimuth', 'Zenith'], FORMAT='(F0.2)', $
			/EDITABLE, BACKGROUND_COLOR = srcInitTextColor,$
			EVENT_PRO='source_enter')
			
	;II.c
	tTc = WIDGET_BASE(topTables,/COLUMN, /ALIGN_CENTER)
	lab = WIDGET_LABEL(tTc,VALUE='Specify Quaternion to use RA,Dec')
	table2 = WIDGET_TABLE(tTc, VALUE = sr_celest[0:1,*], $
			/RESIZEABLE_COLUMNS, COLUMN_LABELS=['RA', 'Dec'], FORMAT='(F0.2)', EDITABLE = 0, $
			FOREGROUND_COLOR = [138,138,138], BACKGROUND_COLOR = [255,255,255], $
			EVENT_PRO='source_enter', SENSITIVE = 0)
	
	;II.Buttons
	topButtons = WIDGET_BASE(topPanel,/ROW, /ALIGN_LEFT)
	plot = WIDGET_BUTTON(topButtons, VALUE = 'USE SELECTED (above)', $
			EVENT_PRO='plot_select', /ALIGN_CENTER)
	plot2 = WIDGET_BUTTON(topButtons, VALUE = 'USE ALL', $
			EVENT_PRO='reselect_all', /ALIGN_CENTER)
	
	IF KEYWORD_SET(noplot) THEN val = 'Show Plot' ELSE val = 'Hide Plot'
		
	toggle = WIDGET_BUTTON(topButtons, VALUE = val, $
			EVENT_PRO='toggle_plot')
	lab = WIDGET_LABEL(topButtons,VALUE='Source Set: ')
	setFile = WIDGET_LABEL(topButtons, VALUE = '-',/DYNAMIC_RESIZE)
	
	;II.Bottom		
	lowPanel = WIDGET_BASE(base2,/COLUMN)
	lab = WIDGET_LABEL(lowPanel,VALUE='Obstructed Detectors:')
	results = WIDGET_TABLE(lowPanel, VALUE = init_blocktable, $
			COLUMN_LABELS=model_columns, COLUMN_WIDTHS=120, $
			/RESIZEABLE_COLUMNS, BACKGROUND_COLOR = [255,255,255] );, $
	;		BACKGROUND_COLOR = resultsTableBG $
	;		)

	WIDGET_CONTROL, results, BACKGROUND_COLOR=colors.rows.active, USE_TABLE_SELECT = [0, 0, model.ncols-1, 0]

	If not obj_valid(object) then object = OBJ_NEW()
	
	extrawin = {angles:0L, separations:0L}

	info = {$
			extrawin:extrawin,$
			noplot : KEYWORD_SET(noplot), $
			geometry: model, $
			geom_dir:geom_dir,$
			dethash:dethash, detkeys: dlist_ptr, $
			;det:detlist, $
			d_selection: PTR_NEW(0), $
			a1:angle1, a2:angle2, $
			d_a1:del_a1, d_a2:del_a2, $
			reflab:reflab, refutc:refutc, refoff:refoff, rowlab:rowlab, rowinc:rowinc, $
			offmet:offmet, offutc: offutc, $
			refrow:{row:0L,time: 0.0D}, file:flab, $
			table:table, radec:table2, results:results, $
			s_xyz:PTR_NEW(sr_rect,/NO_COPY), s_set:s_set, setFile:setFile, $
			colors:colors,$
			datamode:0b, extnum:0, $
			primary:0b, $
			interface:object, $
			fs:OBJ_NEW(), $
			quat:OBJ_NEW() $
			}
	
	state = PTR_NEW(info)
	
	WIDGET_CONTROL,topBase,/REALIZE, SET_UVALUE = state
	
	IF haveTable THEN BEGIN
		names = s_set->get(COL='name')
		insert_src_names, state, (*names)[*]
		PTR_FREE,names
	ENDIF
	
	
	XMANAGER, 'blocktest', topBase, /NO_BLOCK
	
	IF N_ELEMENTS(trigdat) NE 0 THEN err = read_trigdat(state, FILE=trigdat) $
	ELSE IF N_ELEMENTS(poshist) NE 0 THEN err = read_poshist(state, FILE=poshist)

	;WIDGET_CONTROL, detlist, SET_DROPLIST_SELECT=14, SEND_EVENT={ID:0L, TOP:0L, HANDLER:0L, INDEX:14}
	
	initializer = {ID:detGroup, TOP:topBase, SELECT:1b, VALUE:'All'}
	ok = detbgroup_event(initializer)
	
	;WIDGET_CONTROL, detGroup, SEND_EVENT={ID:0L, TOP:topBase, HANDLER:detGroup, SELECT:1b, VALUE:'All'}
	
	id=topBase

END


