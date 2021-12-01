FUNCTION fgeom_quadril::init, verts, _REF_EXTRA=ex
	
	IF N_ELEMENTS(verts) EQ 0 THEN RETURN,1
	
	vsize = SIZE(verts, /DIMENSIONS)
	
	IF (vsize[0] NE 3) THEN BEGIN
		MESSAGE, "verts must be 3 x (M >= 4) array"
		MESSAGE, "Failed to construct object"
		RETURN, 0
	ENDIF
	
	IF vsize[1] LT 4 THEN BEGIN
		MESSAGE, "Number of vertices must be >= four"
		MESSAGE, "Failed to construct object"
		RETURN,0
	ENDIF
	
	obj = self->fgeom_simpleplane::init(verts,_EXTRA=ex)
	
	IF NOT obj THEN RETURN,0
	IF self->nverts() GT 5 THEN MESSAGE,"Warning, unused vertices in FGEOM_QUADRIL object. "+$
										"Object should be closed with only 4 or 5 points."

	RETURN,1
END

PRO fgeom_quadril::plane_def
	
	self.center = ( (*self.verts)[0:2,0] + (*self.verts)[0:2,2]) / 2.0
	
	self->fgeom_simpleplane::plane_def
	
END

PRO fgeom_quadril::set_verts, verts, _REF_EXTRA = ex
	self->fgeom_frame::set_verts,verts, _EXTRA = ex
	self->plane_def
	IF self->nverts() GT 5 AND self.virtualComponents EQ 0 THEN $
		MESSAGE,"Warning, unused vertices in FGEOM_QUADRIL object. Need 4 or 5 points."
END

FUNCTION fgeom_quadril::left_of_path, v, p1, p2, CONST_X=x, CONST_Y=y
	
	quantity = SIZE(v, /N_DIMENSIONS)
	IF quantity EQ 1 THEN nsrc = 1 ELSE nsrc = (SIZE(v, /DIMENSIONS))[1]	
	det = FLTARR(nsrc)

	IF KEYWORD_SET(x) THEN BEGIN
		det[*] = p1[0]*( v[1,*]*(p1[2] - p2[2]) + v[2,*]*(p2[1] - p1[1]) ) + $
			v[0,*]*( p1[1]*p2[2] - p1[2]*p2[1] )
		return, (det GE 0)
	ENDIF
	IF KEYWORD_SET(y) THEN BEGIN
		det[*] = p1[1]*( v[0,*]*(p2[2] - p1[2]) + v[2,*]*(p1[0] - p2[0]) ) + $
			v[1,*]*( p1[2]*p2[0] - p1[0]*p2[2] )
		return, (det GE 0)
	ENDIF
	
	FOR n = 0, nsrc-1 DO BEGIN		
		hull = transpose ([ [v[*,n]], [p1], [p2] ])
		det[n] =  DETERM ( hull, /CHECK )
	ENDFOR
	
	RETURN, (det GT 0)
END

FUNCTION fgeom_quadril::determ_it, sources, rect, COUNT=nblocked, ALIGNED=aligned
	
	IF KEYWORD_SET(aligned) THEN BEGIN
		a = self->left_of_path(sources, rect[*,0], rect[*,1], /CONST_X)
		c = self->left_of_path(sources, rect[*,2], rect[*,3], /CONST_X)
		b = self->left_of_path(sources, rect[*,1], rect[*,2], /CONST_Y)
		d = self->left_of_path(sources, rect[*,3], rect[*,0], /CONST_Y)
	END ELSE BEGIN
		a = self->left_of_path(sources, rect[*,0], rect[*,1])
		c = self->left_of_path(sources, rect[*,2], rect[*,3])
		b = self->left_of_path(sources, rect[*,1], rect[*,2])
		d = self->left_of_path(sources, rect[*,3], rect[*,0])
	END

;	nsrc = (SIZE(sources, /DIMENSIONS))[1]
	n_on_left = total( [ [a], [b], [c], [d] ], 2 )
	
	vtr =  transpose(rect)
	vertsrc_dots = vtr # sources
	;print,vertsrc_dots	
	
;	i = indgen(self.nverts-1)
;	j = indgen(nsrc)
;	
;	vmags = ( vtr # *rect )[i,i]
;	smags = ( transpose(sources) # source )[j,j]
	
	;mags = fltarr(self.nverts-1, nsrc)
	;mags[i,*] = vmags * 
	
	n_inHemiSphere = total(vertsrc_dots GT 0, 1)
	
	quantity = SIZE(sources, /N_DIMENSIONS)
	IF quantity EQ 1 THEN nsrcdot = total(self.normal * sources) $
	ELSE nsrcdot = self.normal # sources
	
;	print, self.name
;	print, 'NLEFT=',n_on_left
;	print, 'N_INH=',([n_inHemiSphere])
	;print, 'Angles:', acos( 
	
	blocked = (n_on_left EQ 4 AND (nsrcdot GT 0) ) OR (n_on_left EQ 0 AND (nsrcdot LT 0) ) AND (n_inHemiSphere GE 3)
	inds = WHERE( blocked, nblocked)
	
	RETURN,blocked
END

FUNCTION fgeom_quadril::composite_itest, sources, COUNT=nblocked, FROM=origin, ALIGNED=aligned
	
	FOR i=0,self.virtualComponents-1 DO BEGIN
		ref_set = (*self.verts)[i]
		
		IF N_ELEMENTS(origin) GT 0 THEN $
			FOR j=0,2 DO (*ref_set)[j,*] -= origin[j]
			
		newblocked = self->determ_it( sources, ref_set, COUNT=nblocked, ALIGNED=aligned )
		
		IF N_ELEMENTS(origin) GT 0 THEN $
			FOR j=0,2 DO (*ref_set)[j,*] += origin[j]
		
		IF N_ELEMENTS(blocked) EQ 0 THEN blocked = newblocked $
		ELSE blocked = blocked OR newblocked
		
	ENDFOR

	RETURN, blocked
END

PRO fgeom_quadril::shared_instance, fgeom, CONNECTIVITY = verts, RESET=reset
	
	IF N_ELEMENTS(verts) LT 5 THEN BEGIN
		MESSAGE, "Must specify a connectivy list of at least 5 elements"
		RETURN
	ENDIF
	
	IF NOT (OBJ_VALID(fgeom) && OBJ_ISA(fgeom, 'FGEOM_FRAME') ) THEN BEGIN
		MESSAGE,'Arg 1 must be a valid FGEOM_FRAME object'
		RETURN
	ENDIF
	
	IF KEYWORD_SET(reset) OR (self.virtualComponents EQ 0 && PTR_VALID(self.verts) ) THEN BEGIN 
		PTR_FREE,self.verts
		self.virtualComponents = 0
	ENDIF
		
	IF (N_ELEMENTS(verts) / 5) LT 1 THEN BEGIN
		MESSAGE, "CONNECTIVITY MUST CONTAIN MULTIPLES OF 5 VERTICES"
		RETURN
	ENDIF
	
	nverts = N_ELEMENTS(verts)
	i = 0	
	REPEAT BEGIN
		n_v = verts[i]
		IF n_v EQ 0 THEN BEGIN 
			i++
			CONTINUE
		ENDIF
		
		rect = fgeom->getverts( /PTR, VERTS = verts[i+1 : i + n_v ] )
		IF NOT PTR_VALID(rect) THEN RETURN
		IF NOT PTR_VALID(self.verts) THEN BEGIN
			self.verts = PTR_NEW([rect])
			self.nverts = n_v
		END ELSE BEGIN
			ref_set = self.verts
			(*ref_set) = [(*ref_set), rect]
			self.nverts += n_v
		END
		++self.virtualComponents
		i = i + n_v + 1
	ENDREP UNTIL ( i GE nverts )
	
END

PRO fgeom_quadril::xplot3d, dummy1, NORMAL=normal, _REF_EXTRA=ex

	verts = *self.verts
	xplot3d_custom,verts[0,*],verts[1,*],verts[2,*], $
		_EXTRA=ex

	IF KEYWORD_SET(normal) THEN BEGIN
		center = self.center
		normv = center[0:2] + 100.0*self.normal	
		nline = [ [ center ], [ normv ] ]

		xplot3d_custom,nline[0,*],nline[1,*],nline[2,*], COLOR = [200,15,15], THICK=1.5, /OVERPLOT
	ENDIF

END

FUNCTION fgeom_quadril::intersect_test, sources, COUNT=nblocked, _REF_EXTRA=ex
	
	IF self.virtualComponents THEN RETURN, self->composite_itest(sources, _EXTRA=ex)
	
	self->getverts, rect, _EXTRA=ex
	blocked = self->determ_it( sources, rect, _EXTRA=ex )
	
	if (self.constraint AND 128B) gt 1 then begin
		blocked = NOT (blocked)
	endif
	
	RETURN,blocked
END

PRO fgeom_quadril__define
	
	struct = {FGEOM_QUADRIL, $
			INHERITS FGEOM_SIMPLEPLANE, $
			center:FLTARR(3), $
			virtualComponents: 0 $
			}

END