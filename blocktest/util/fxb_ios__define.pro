FUNCTION binplace, v,i, edges, nedges
	
	IF v GE edges[nedges-1] THEN RETURN,nedges-1
	IF v LT edges[0] THEN RETURN, 0
	IF i+1 GT nedges THEN i=binplace(v,i-1,edges,nedges)
	
	IF v GE edges[i] && v LT edges[i+1] THEN RETURN,i
	
	IF v LT edges[i] THEN i=binplace(v,i-1,edges,nedges) $
	ELSE IF v GT edges[i] THEN i=binplace(v,i+1,edges,nedges)
	
	RETURN,i
END

FUNCTION fxb_ios::init
	self.lun=-1
	RETURN,1
END

FUNCTION fxb_ios::open, filename, extension, KEEP_BUCKETS=keep, _REF_EXTRA = ex
	ON_ERROR,2
	IF FXBSTATE(self.lun) THEN FXBCLOSE,self.lun
	
	IF N_ELEMENTS(extension) EQ 0 THEN extension = 1
	
	FXBOPEN, lun, filename, extension, _EXTRA = ex
	self.lun = lun
	IF FXBSTATE(self.lun) EQ 0 THEN BEGIN
		self.lun=-1
		RETURN,1b
	ENDIF
	
	hdr = FXBHEADER(self.lun)
	self.MaxRow = FXPAR(hdr, 'NAXIS2')
	
	
	self.path = FILE_DIRNAME(filename)
	self.file = FILE_BASENAME(filename)
	self.cdu = extension
	
	IF NOT KEYWORD_SET(keep) THEN BEGIN
		self.sorting = 0b
		self.n_bucks = 100
		IF self.n_bucks GT self.MaxRow THEN self.n_bucks = self.MaxRow
		buckets = (1+LINDGEN(self.n_bucks)) * self.MaxRow / self.n_bucks
		IF PTR_VALID(self.buckets) THEN PTR_FREE,self.buckets
		self.buckets = PTR_NEW(buckets)
	ENDIF
	
	
	RETURN,0b
END

PRO fxb_ios::moveto_x, ext, status, _REF_EXTRA = ex

	IF FXBSTATE(self.lun) THEN $
		status = FXMOVE(self.lun, ext, _EXTRA = ex)
		
	hdr = FXBHEADER(self.lun)
	self.MaxRow = FXPAR(hdr, 'NAXIS2')
	
END

PRO fxb_ios::keyByCol, col

	IF FXBSTATE(self.lun) EQ 0 THEN RETURN
	
	IF SIZE(col,/TYPE) EQ 7 THEN col = FXBCOLNUM(self.lun, col)
	
	FXBREAD, self.lun, rows, col
	
	nbins = 100
	nbins--
	nrows = n_elements(rows)
	
	IF nrows EQ 0 THEN BEGIN
		MESSAGE,"Error: Data size read with FXBREAD is 0"
		RETURN
	ENDIF
	
	IF self.MaxRow NE nrows THEN BEGIN
		MESSAGE,"Warning, data size read does not match NAXIS2 keyword"
		print, "Read = ",nrows
		print, "NAXIS2 = ",self.MaxRow
		print, self.file
		self.MaxRow = nrows
	ENDIF
	
	IF nbins GT nrows THEN nbins = nrows
	
	buckets = (LINDGEN(nbins)) * nrows / nbins
	
	order = sort(rows)
	buckets = order[TEMPORARY(buckets)]
	keys = rows[buckets]
	
	
	nodes = REPLICATE( {key:keys[0], row:buckets[0]} ,nbins )
	nodes.key = keys
	nodes.row = buckets+1
	
	IF PTR_VALID(self.buckets) THEN PTR_FREE,self.buckets
	
	self.n_bucks = nbins
	self.buckets = PTR_NEW(nodes,/NO_COPY)
	self.refbuck = 0
	self.sorting = 1
	self.keyCol = col
	
	;print, (*self.buckets)
	
END

FUNCTION fxb_ios::bu_search, val
	IF NOT self.sorting THEN $
		RETURN,-1
	
	buckets = (*self.buckets)[*].key
	bucket = binplace(val, self.refbuck, buckets, self.n_bucks )
	
	RETURN,bucket
END

PRO fxb_ios::impute_ref, keyVal

	b = self->bu_search(keyVal)
	start_row = (*self.buckets)[b].row
	self.refbuck = b
	
END

FUNCTION fxb_ios::nearest_row, keyVal, PAD=pad, VALUE=exact, _REF_EXTRA = ex

	IF FXBSTATE(self.lun) EQ 0 THEN RETURN, -1
	IF self.sorting EQ 0 THEN BEGIN
		MESSAGE, "Warning: Attempt to search the table without the use of a Key"
		print, "Use fxb_ios::keyByCol to specify which column to use as the key"
		RETURN,key
	ENDIF

	searchRange = self->find_set(keyVal, PAD=pad)
	lun = self.lun
	
	IF searchRange[0] GT searchRange[1] THEN $
		searchRange = REVERSE(TEMPORARY(searchRange))
	
	FXBREAD, lun, rows, self.keyCol, searchRange, _EXTRA = ex
	
	
	print,"searchRange=",searchRange
	;print, rows - keyVal
	
	res = MIN(ABS(rows - keyVal), index)
	
	exact = rows[index]
	
	row = index + searchRange[0]
	
	RETURN,row

END

FUNCTION fxb_ios::find_set, keyVal, PAD=pad, EXCEPT=except
	IF self.sorting EQ 0 THEN BEGIN
		MESSAGE, "Warning: Attempt to search the table without the use of a Key"
		print, "Use fxb_ios::keyByCol to specify which column to use as the key"
		RETURN,key
	ENDIF
	
	CASE N_ELEMENTS(pad) OF
		1: do_union = 1b
		2: do_union = 2b
		ELSE: do_union = 0b
	ENDCASE
	
	b = self->bu_search(keyVal)
	
	IF keyVal EQ (*self.buckets)[b].key THEN BEGIN
		keyRow = (*self.buckets)[b].row
		CASE do_union OF
			1: BEGIN
				IF pad GT 0 THEN pad = [keyRow, keyRow + pad] $
				ELSE IF pad LT 0 THEN pad = [keyRow + pad, keyRow]
				IF pad[0] LT 1 THEN pad[0] = 1
				IF pad[1] GT self.MaxRow THEN pad[1] = self.MaxRow
				rows = pad
				END
			2: BEGIN
				pad = pad + keyRow
				IF pad[0] LT 1 THEN pad[0] = 1
				IF pad[1] GT self.MaxRow THEN pad[1] = self.MaxRow
				rows = pad
				END
			ElSE: rows = [keyRow,keyRow]
		ENDCASE
		;print, rows
		RETURN, LONG(rows)
	ENDIF
	
	start_row = (*self.buckets)[b].row
	IF b+1 EQ self.n_bucks THEN stop_row = self.MaxRow $
	ELSE stop_row = (*self.buckets)[b+1].row
	
	CASE do_union OF
			1: BEGIN
				IF pad GT 0 THEN pad = [start_row, stop_row + pad] $
				ELSE IF pad LT 0 THEN pad = [start_row + pad, stop_row]
				IF pad[0] LT 1 THEN pad[0] = 1
				IF pad[1] GT self.MaxRow THEN pad[1] = self.MaxRow
				rows = pad
				END
			2: BEGIN
				pad = pad + [start_row, stop_row]
				IF pad[0] LT 1 THEN pad[0] = 1
				IF pad[1] GT self.MaxRow THEN pad[1] = self.MaxRow
				rows = pad
				END
			ELSE: rows = [start_row, stop_row]
		ENDCASE
	
	RETURN, LONG(rows)
END

FUNCTION fxb_ios::fxpar, par, status, _REF_EXTRA = ex
	ON_ERROR,2
	
	IF FXBSTATE(self.lun) EQ 0 THEN RETURN,-1
	
	hdr = FXBHEADER(self.lun)
	
	result = FXPAR(hdr, par)
	status = !err
	
	RETURN,result
END

FUNCTION fxb_ios::lun
	RETURN,self.lun
END

FUNCTION fxb_ios::buckets,number
	number = self.n_bucks
	RETURN, *self.buckets
END

FUNCTION fxb_ios::file, FULL=full
	IF KEYWORD_SET(full) THEN RETURN,self.path+self.file
	RETURN,self.file
END

PRO fxb_ios::close
	FXBCLOSE,self.lun
	self.lun=-1
	self.cdu=0
END

FUNCTION fxb_ios::numRows
	RETURN, self.MaxRow
END

PRO fxb_ios::Cleanup
	IF FXBSTATE(self.lun) THEN FXBCLOSE,self.lun
	IF PTR_VALID(self.store) THEN PTR_FREE,self.store
	IF PTR_VALID(self.order) THEN PTR_FREE,self.order
	IF PTR_VALID(self.buckets) THEN PTR_FREE,self.buckets
END

PRO fxb_ios__define, file

	struct = {FXB_IOS, $
		store:PTR_NEW(),$
		order:PTR_NEW(),$
		MaxRow:0L ,$
		file:'',$
		path:'',$
		lun:0L ,$
		cdu:0L ,$
		sorting: 0b, $
		keyCol:0L, $
		refbuck:0L ,$
		n_bucks:0L ,$
		buckets:PTR_NEW() $
		}

END