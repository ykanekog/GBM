PRO blockage_set__define
	struct = {BLOCKAGE_SET,$
			sc_pos:0,$
			sc_neg:0,$
			lat:0,$
			rad:0 }
END

PRO src_node__define
	struct = {SRC_NODE, $
			name:'',$
			az:0.0,$
			zen:0.0,$
			ra:0.0,$
			dec:0.0,$
			blocked:{BLOCKAGE_SET},$
			active:0b }
END

FUNCTION src_table::init, number
	IF n_elements(number) eq 0 then number = 10
	self.num = number
	self.set = PTR_NEW( REPLICATE({SRC_NODE}, number) )
	RETURN,1
END

FUNCTION src_table::active, count, SUBSET=subset
	IF N_ELEMENTS(subset) GT 0 THEN inds = WHERE( (*self.set)[subset].active, count) $
	ELSE inds = WHERE( (*self.set)[*].active GE 1b, count)
	RETURN, inds
END

FUNCTION src_table::named, count, SUBSET=subset, _REF_EXTRA=ex
	IF N_ELEMENTS(subset) GT 0 THEN inds = WHERE( (*self.set)[subset].name NE '', count, _EXTRA=ex) $
	ELSE inds = WHERE( (*self.set)[*].name NE '', count, _EXTRA=ex)
	RETURN, inds
END

FUNCTION src_table::number & RETURN, self.num & END

PRO src_table::resize, newnumber
	IF newnumber le 0 or newnumber eq self.num then return
	IF newnumber lt self.num then begin
		temp = (*self.set)[0:newnumber-1]
		ptr_free,self.set
		self.set = ptr_new(temp,/no_copy)
		self.num = newnumber
	endif else begin
		temp2 = replicate( {SRC_NODE}, newnumber )
		temp2[0:self.num-1] = (*self.set)
		ptr_free,self.set
		self.set = ptr_new( temp2,/no_copy)
		self.num = newnumber
	endelse
END

PRO src_table::set_active, ROWS=rows
	IF N_ELEMENTS(rows) EQ 0 THEN (*self.set)[*].active = 1 $
	ELSE BEGIN 
		(*self.set)[*].active = 0
		(*self.set)[rows].active = 1
	END
END

PRO src_table::insert, entries, COL=col, ROWS=inds, ALL=all

	ON_ERROR,2

	ncols = N_ELEMENTS(col)

	IF ncols EQ 0 THEN BEGIN
		print, 'No column given'
		RETURN
	END

	IF SIZE(col,/N_DIMENSIONS) EQ 0 THEN col = [col]
	
	nvdims =  SIZE(entries, /N_DIMENSIONS)
	valdims = SIZE(entries, /DIMENSIONS)
	
	IF nvdims EQ 1 THEN valdims = [valdims,1] $
	ELSE IF nvdims EQ 0 THEN valdims = [self.num,1]
	
	IF ncols NE valdims[1]  THEN BEGIN
		print, "SRC_TABLE::ENTER  : dimension of values and COL do not match"
		print, "VALUES dims: ",valdims, "COL dims: ",ncols
		RETURN
	ENDIF
	
	IF KEYWORD_SET(all) THEN inds = indgen(self.num) $
	ELSE IF N_ELEMENTS(inds) EQ 0 THEN inds = indgen(valdims[0])

	nrows = N_ELEMENTS(inds)
	n_inserts = N_ELEMENTS(entries)
	sub_enter = indgen(nrows)
	
	FOR i=0,ncols-1 DO BEGIN
		start = (i MOD valdims[1]) * valdims[0]
		values = entries[ (start + sub_enter) MOD n_inserts ]
		CASE col[i] OF
			'name':(*self.set)[inds].name = values
			'az' :(*self.set)[inds].az  = values
			'zen':(*self.set)[inds].zen = values
			'ra' :(*self.set)[inds].ra  = values
			'dec':(*self.set)[inds].dec = values
			'active' :(*self.set)[inds].active = values
			'blocked':(*self.set)[inds].blocked = values
		ENDCASE
	ENDFOR
	
END

FUNCTION src_table::get, COL=col, ROWS=rows, ACTIVE=active, NAMED=named

	ON_ERROR,2

	ncols = N_ELEMENTS(col)
	
	IF KEYWORD_SET(active) THEN BEGIN
		rows = self->active(count, SUBSET=rows)
		IF count EQ 0 THEN RETURN,-1
	ENDIF
	
	IF KEYWORD_SET(named) THEN BEGIN
		rows = self->named(count, SUBSET=rows)
		IF count EQ 0 THEN RETURN,-1
	ENDIF
	
	result = PTRARR(ncols,/ALLOCATE_HEAP)
	
	IF N_ELEMENTS(rows) EQ 0 THEN BEGIN
		IF ncols EQ 0 THEN BEGIN
			FOR i=0,ncols-1 DO *(result[i]) = (*self.set).(i)
			RETURN,result
		ENDIF ELSE rows = indgen(self.num)
	ENDIF

	FOR i=0,ncols-1 DO BEGIN
		CASE col[i] OF
			'name':*(result[i]) = (*self.set)[rows].name
			'az' :*(result[i]) = (*self.set)[rows].az
			'zen':*(result[i]) = (*self.set)[rows].zen
			'ra' :*(result[i]) = (*self.set)[rows].ra
			'dec':*(result[i]) = (*self.set)[rows].dec
			'active' :*(result[i]) = (*self.set)[rows].active
			'blocked':*(result[i]) = (*self.set)[rows].blocked
		ENDCASE
	ENDFOR

	If ncols EQ 1 THEN RETURN, result[0]
	
	RETURN,result
	
END

FUNCTION src_table::pull_azzen, ACTIVE=active, ROWS=rows

	IF KEYWORD_SET(active) OR N_ELEMENTS(rows) GT 0 THEN BEGIN
		IF N_ELEMENTS(rows) EQ 0 THEN rows = self->active(count)
		IF count EQ 0 THEN RETURN,-1
		
		coords = FLTARR(3,count)
		coords[0,*] = (*self.set)[rows].az
		coords[1,*] = (*self.set)[rows].zen
		coords[2,*] = 1
		RETURN,coords
	ENDIF
	
	coords = FLTARR(3,self.num)
	coords[0,*] = (*self.set).az
	coords[1,*] = (*self.set).zen
	coords[2,*] = 1
	
	RETURN,coords
END

FUNCTION src_table::pull_radec, ACTIVE=active, ROWS=rows

	IF KEYWORD_SET(active) OR N_ELEMENTS(rows) GT 0 THEN BEGIN
		IF N_ELEMENTS(rows) EQ 0 THEN rows = self->active(count)
		IF count EQ 0 THEN RETURN,-1
		
		coords = FLTARR(3,count)
		coords[0,*] = (*self.set)[rows].ra
		coords[1,*] = (*self.set)[rows].dec
		coords[2,*] = 1
		RETURN,coords
	ENDIF
	
	coords = FLTARR(3,self.num)
	coords[0,*] = (*self.set).ra
	coords[1,*] = (*self.set).dec
	coords[2,*] = 1
	
	RETURN,coords
END

PRO src_table::Cleanup
	PTR_FREE,self.set
END

PRO src_table__define
	set = {SRC_TABLE, $
			set:PTR_NEW(),$
			num:0 }
END