FUNCTION LAT_RADIATOR_MINUS::init
	
	self.name = "-Rad"
	
	;Distance units = cm
	yoff = -96.3 - 5.2 ;Add the thickness
	
	xoff = 190.3 / 2.0
	zht = 158.6
	
	verts = [$
				[xoff, yoff, 0.0],$
				[xoff, yoff, zht],$
				[-xoff,yoff, zht],$
				[-xoff,yoff, 0.0],$
				[xoff, yoff, 0.0] $
			]
	
	;Calculate plane normal with verts:  ( v[1] - v[0] ) x (v[2] - v[1])
	
	RETURN, self->fgeom_quadril::init(verts)
END

FUNCTION LAT_RADIATOR_MINUS::intersect_test, sources, COUNT=nblocked, _REF_EXTRA=ex
	blocked = self->fgeom_quadril::intersect_test(sources, COUNT=nblocked, /ALIGNED, _EXTRA = ex)
	RETURN, blocked
END

PRO LAT_RADIATOR_MINUS::xplot3d, dummy1, _REF_EXTRA = ex
		
	self->fgeom_quadril::xplot3d, COLOR = [139,137,137], THICK=3, _EXTRA = ex

END

PRO lat_radiator_minus__define

	struct = {LAT_RADIATOR_MINUS, $
		INHERITS FGEOM_QUADRIL $
		}
		
END		