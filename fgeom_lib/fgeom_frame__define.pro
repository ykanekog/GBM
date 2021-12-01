FUNCTION fgeom_frame::init, verts, COMP_AND=comp_and, COMP_NOT=comp_not, NAME=name

	IF KEYWORD_SET(comp_and) THEN self.constraint = 1B
	IF KEYWORD_SET(comp_not) THEN self.constraint = self.constraint OR 128B
	if n_elements(name) gt 0 then self.name = name

	;print,self.name+' Constraint=',self.constraint

	IF N_ELEMENTS(verts) EQ 0 THEN RETURN,1
	
	vsize = SIZE(verts, /DIMENSIONS)
	
	IF (vsize[0] NE 3) THEN BEGIN
		MESSAGE, "verts must be 3 x (M >= 1) array"
		MESSAGE, "Failed to construct object"
		RETURN, 0
	ENDIF
	
	IF vsize[1] LT 3 THEN BEGIN
		MESSAGE, "Number of vertices must be greater than three"
		MESSAGE, "Failed to construct object"
		RETURN,0
	ENDIF
	
	self.nverts = vsize[1]
	self.verts =  PTR_NEW(verts)
	
	RETURN,1
END

FUNCTION fgeom_frame::constraint
	RETURN,self.constraint
END

PRO fgeom_frame::getverts, verts, VERTS=index, FROM=origin, PTR=ptr
	
	IF self.nverts EQ 0 THEN BEGIN
		RETURN
	ENDIF

	IF N_ELEMENTS(index) GT 0 THEN verts = (*self.verts)[*, index] $
	ELSE verts = *self.verts
	
	IF N_ELEMENTS(origin) GT 0 THEN $
		FOR i=0,2 DO verts[i,*] -= origin[i]
	
	IF KEYWORD_SET(ptr) THEN BEGIN
		verts = PTR_NEW(verts, /NO_COPY)
	ENDIF
	
	RETURN
END

FUNCTION fgeom_frame::getverts, dummy, VERTS=index, FROM=origin, PTR=ptr
	
	IF self.nverts EQ 0 THEN BEGIN
		IF KEYWORD_SET(ptr) THEN RETURN, PTR_NEW() $
		ELSE RETURN, 0
	ENDIF

	IF N_ELEMENTS(index) GT 0 THEN verts = (*self.verts)[*, index] $
	ELSE verts = *self.verts
	
	IF N_ELEMENTS(origin) GT 0 THEN $
		FOR i=0,2 DO verts[i,*] -= origin[i]
	
	IF ARG_PRESENT(ptr) THEN BEGIN
		IF PTR_VALID(ptr) THEN *ptr = verts $
		ELSE ptr = PTR_NEW(verts, /NO_COPY)
		RETURN,1
	ENDIF
	
	IF KEYWORD_SET(ptr) THEN BEGIN
		RETURN,PTR_NEW(verts, /NO_COPY)
	ENDIF
	
	RETURN, verts
END

PRO fgeom_frame::set_verts, new_verts, TO=origin
	
	dims = SIZE(new_verts,/DIMENSIONS)
	IF (dims[0] NE 3) || (dims[1] LT 3)  THEN BEGIN
		MESSAGE, "new_verts must be 3 x (M >= 3) array"
		MESSAGE, "Failed to set new_verts"
		RETURN
	ENDIF
	
	IF N_ELEMENTS(origin) GT 0 THEN $
		FOR i=0,2 DO new_verts[i,*] += origin[i]
	
	IF PTR_VALID(self.verts) THEN BEGIN
		*self.verts = new_verts
	ENDIF ELSE self.verts = PTR_NEW(new_verts, /NO_COPY)
	
	self.nverts = dims[1]
END

PRO fgeom_frame::xplot3d, dummy, _REF_EXTRA=ex
	verts = *self.verts
	xplot3d_custom,verts[0,*],verts[1,*],verts[2,*], $
		_EXTRA=ex
END

PRO fgeom_frame::setname,name,_REF_EXTRA=ex & self.name = name & END
FUNCTION fgeom_frame::getname,dummy,_REF_EXTRA=ex & RETURN,self.name & END

PRO fgeom_frame::setparent,object,_REF_EXTRA=ex
	self.parent = object
END

FUNCTION fgeom_frame::getparent,dummy,_REF_EXTRA=ex & RETURN,self.parent & END

PRO fgeom_frame::Cleanup
	HEAP_FREE, self.verts
	IF OBJ_VALID(self.parent) THEN self.parent->remove, self
END

PRO fgeom_frame::UseIterator, fgattri
	self.iterand = fgattri
END

PRO fgeom_frame::SetVisualizer, object

END

PRO fgeom_frame__define

	struct = {FGEOM_FRAME, $
		name:'',$
		parent:OBJ_NEW(),$
		constraint:0B,$
		verts: PTR_NEW(),$
		visualizer:OBJ_NEW(),$
		iterand:OBJ_NEW(),$
		nverts: 0 $
	}

END