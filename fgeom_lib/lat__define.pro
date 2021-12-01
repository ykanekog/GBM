FUNCTION LAT::init

	self.name = "LAT"

	;Centimeters
	zmin = 158.6
	zmax = 158.6 + 106
	w = 190.3 / 2.0

	nodes = [ $	;Top face
				[w, w, zmax], $
				[w,-w, zmax], $
				[-w,-w,zmax], $
				[-w,w, zmax], $
				;Bottom face
				[w, w, zmin], $
				[w,-w, zmin], $
				[-w,-w,zmin], $
				[-w,w, zmin] $
			]
			
	IF NOT (self->fgeom_frame::init(nodes)) THEN RETURN,0
	
;	self.lattice = OBJ_NEW('FGEOM_QUADRIL')
;
;	self.lattice->shared_instance, self, $
;			CONNECTIVITY=[	4,0,1,2,3, $
;							4,0,1,5,4, $
;							4,1,2,6,5, $
;							4,2,3,7,6, $
;							4,3,0,4,7  ]
	
	self.lattice = OBJ_NEW('FGCOMPOSITE')
	self.lattice->Add, OBJ_NEW('FGEOM_QUADRIL', nodes[*,[3,2,1,0]], NAME='LAT <0,0,1>' ) ;Top face
	self.lattice->Add, OBJ_NEW('FGEOM_QUADRIL', nodes[*,[0,1,5,4]], NAME='LAT <1,0,0>' )
	self.lattice->Add, OBJ_NEW('FGEOM_QUADRIL', nodes[*,[1,2,6,5]] ) ;Side 2, normal = [0,-1,0]
	self.lattice->Add, OBJ_NEW('FGEOM_QUADRIL', nodes[*,[2,3,7,6]] ) ;Side 3, normal = [-1,0,0]
	self.lattice->Add, OBJ_NEW('FGEOM_QUADRIL', nodes[*,[3,0,4,7]] ) ;Side 4, normal = [ 1,0,0]
	
	RETURN,1
END

FUNCTION LAT::intersect_test, sources, COUNT=nblocked, _REF_EXTRA=ex
	
	blocked = self.lattice->intersect_test(sources, COUNT=nblocked, _EXTRA=ex)
	
	RETURN,blocked
END


PRO LAT::xplot3d, dummy1, _REF_EXTRA=ex

	;nodes = self.lattice->getverts()
	
	self.lattice->IterateCallPro, 'xplot3d', COLOR=[0,0,150], THICK=3, /OVERPLOT, _EXTRA=ex
	
;	verts = *(nodes[0])
;	xplot3d_custom,verts[0,*],verts[1,*],verts[2,*], $
;			COLOR = [0,0,150],THICK=3, $
;			_EXTRA=ex
;	
;	FOR i=1,4 DO BEGIN
;		verts = *(nodes[i])
;		xplot3d_custom,verts[0,*],verts[1,*],verts[2,*], $
;			COLOR = [0,0,150],THICK=3, $
;			/OVERPLOT,_EXTRA=ex
;	ENDFOR
	
END

PRO LAT::Cleanup
	OBJ_DESTROY,self.lattice
	self->fgeom_frame::Cleanup
END

PRO lat__define

	struct = {LAT, $
		INHERITS FGEOM_FRAME, $
		lattice:OBJ_NEW() $
	}

END