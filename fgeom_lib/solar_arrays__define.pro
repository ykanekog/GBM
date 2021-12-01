FUNCTION solar_arrays::init

	status = self->fgeom_quadril::init()
	
	self.length = 492.3
	self.width = 151.0
	
	self.pos =	[0.0, 0.0, 41.5]
	self.diag = (self.length)^2 + (self.width)^2
	
	RETURN,status	
END

FUNCTION solar_arrays::get_pos & return, self.pos & END

FUNCTION solar_arrays::get_wxyz, _REF_EXTRA=ex
	RETURN, [ self.angle, self.axis ]
END

FUNCTION solar_arrays::get_angle, _REF_EXTRA=ex
	RETURN, self.angle
END

PRO solar_arrays::set_angle, angle, _REF_EXTRA=ex
	IF N_ELEMENTS(angle) EQ 0 THEN RETURN
	self->reset
	self.angle = angle MOD 360
	self.o_quat->set, ANGLE=angle, RAXIS=self.axis
	status = self.o_quat->rotate(self, OFFSET=self.pos)
	self->FGEOM_SIMPLEPLANE::plane_def
END

PRO solar_arrays::spin, degrees
	IF N_ELEMENTS(degrees) EQ 0 THEN RETURN
	self.o_quat->update_wxyz, [ degrees, self.axis ]
	self.angle += degrees
	self.angle = self.angle MOD 360
	status = self.o_quat->rotate(self, OFFSET=self.pos)
	self->FGEOM_SIMPLEPLANE::plane_def
END

FUNCTION solar_arrays::intersect_test, sources, COUNT=nblocked, ANGLE=angle, _REF_EXTRA=ex

	IF n_elements(angle) gt 0 then begin
		;print,angle
		self->set_angle,angle
	endif
	
	blocked = self->fgeom_quadril::intersect_test(sources, COUNT=nblocked, /ALIGNED, _EXTRA = ex)
	RETURN, blocked
END

FUNCTION solar_arrays::blockage_test, sources, geom, COUNT=nblocked, _REF_EXTRA=ex
	nblocked = 0
	nsrc = (SIZE(sources, /DIMENSIONS))[1]
	IF nsrc EQ 0 THEN RETURN,-1
	
	IF OBJ_VALID(geom) && OBJ_CLASS(geom) EQ 'GBM_GEOMETRY' THEN points = geom->det_pos() $
	ELSE points = geom
	
	ndets = (SIZE(points, /DIMENSIONS))[1]
	IF ndets EQ 0 THEN RETURN,-1
	
	blocked = BYTARR(nsrc,ndets)

	FOR i=0,ndets-1 DO BEGIN
		blocked[*,i] = self->intersect_test(sources, COUNT=count, FROM=points[*,i])
		nblocked += count
	ENDFOR

	RETURN,blocked
END

PRO solar_arrays::xplot3d, dummy1, _EXTRA = ex
	self->fgeom_quadril::xplot3d, COLOR = [0,0,100], THICK=3, _EXTRA = ex

END

PRO solar_arrays::Cleanup
	PTR_FREE,self.verts
	OBJ_DESTROY,self.o_quat
	self->fgeom_frame::Cleanup
END

PRO solar_arrays__define

	struct = {SOLAR_ARRAYS, $
		INHERITS FGEOM_QUADRIL, $
		o_quat: OBJ_NEW(), $
		angle: 0.0, $
		axis: FLTARR(3), $
		pos: FLTARR(3), $
		diag: 0.0, $
		length: 0.0, $
		width : 0.0 $
	}

END