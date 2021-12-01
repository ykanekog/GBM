FUNCTION FGEOM_SIMPLEPLANE::init, verts, _REF_EXTRA=ex
	
	IF N_ELEMENTS(verts) EQ 0 THEN RETURN,1
	
	vsize = SIZE(verts, /DIMENSIONS)
	
	IF (vsize[0] NE 3) THEN BEGIN
		MESSAGE, "verts must be 3 x (M >= 3) array"
		MESSAGE, "Failed to construct object"
		RETURN, 0
	ENDIF
	
	IF vsize[1] LT 3 THEN BEGIN
		MESSAGE, "Number of vertices must be >= than three"
		MESSAGE, "Failed to construct object"
		RETURN,0
	ENDIF

	r = self->fgeom_frame::init(verts, _EXTRA=ex)
	self->plane_def
	
	RETURN,r
END

PRO FGEOM_SIMPLEPLANE::plane_def
	v1 = (*self.verts)[*,0]
	v2 = (*self.verts)[*,1]
	v3 = (*self.verts)[*,2]
	normal = crossp(v2 - v1,v3 - v2)
	
	self.normal = normal / norm(normal)
	self.const = total(self.normal * v1)
END

FUNCTION FGEOM_SIMPLEPLANE::nverts & RETURN, self.nverts & END
FUNCTION FGEOM_SIMPLEPLANE::normal & RETURN, self.normal & END
FUNCTION FGEOM_SIMPLEPLANE::pl_con & RETURN, self.const & END

FUNCTION FGEOM_SIMPLEPLANE::intersection_point, vectors
	RETURN, self.const / (self.normal # vectors)
END

FUNCTION FGEOM_SIMPLEPLANE::projplan_dist, source_set, point 
	s2 = total(source_set^2)
	segment_sq = total(source_set^2) + total(point^2) - (2 * total(source_set * point))
	RETURN,segment_sq
END

PRO fgeom_simpleplane__define

	struct = {FGEOM_SIMPLEPLANE, $
		INHERITS FGEOM_FRAME, $
		normal: FLTARR(3),$
		const: 0.0 $
	}

END